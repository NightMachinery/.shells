package update

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"time"

	"org-paper-metadata/paper"

	"github.com/hexops/gotextdiff"
	"github.com/hexops/gotextdiff/myers"
	"github.com/hexops/gotextdiff/span"
)

type Options struct {
	Write, Force, Diff, NoBibTeX bool
	MaxAge, Interval, Timeout    time.Duration
}
type Stats struct{ Files, Headings, Recent, Updated, Failed, Locked int }
type FetchFunc func(context.Context, string) (paper.Metadata, error)

func editorLocked(path string) bool {
	_, err := os.Lstat(filepath.Join(filepath.Dir(path), ".#"+filepath.Base(path)))
	return err == nil
}

// WriteFile refuses concurrent changes, symlinks, editor locks, and hard links.
// The temporary file is synced and renamed within the destination directory.
func WriteFile(path string, before, after []byte, mode fs.FileMode) error {
	check := func() error {
		info, err := os.Lstat(path)
		if err != nil {
			return err
		}
		if !info.Mode().IsRegular() {
			return errors.New("file is no longer a regular file")
		}
		if info.Mode().Perm()&0222 == 0 {
			return errors.New("file is read-only")
		}
		if stat, ok := info.Sys().(*syscall.Stat_t); ok && stat.Nlink > 1 {
			return errors.New("refusing to replace a hard-linked file")
		}
		if editorLocked(path) {
			return errors.New("file has an Emacs editor lock")
		}
		current, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if !bytes.Equal(current, before) {
			return errors.New("file changed during metadata retrieval")
		}
		return nil
	}
	if err := check(); err != nil {
		return err
	}
	tmp, err := os.CreateTemp(filepath.Dir(path), ".org-paper-metadata-*")
	if err != nil {
		return err
	}
	name := tmp.Name()
	defer os.Remove(name)
	if err = tmp.Chmod(mode.Perm()); err == nil {
		_, err = tmp.Write(after)
	}
	if err == nil {
		err = tmp.Sync()
	}
	if closeErr := tmp.Close(); err == nil {
		err = closeErr
	}
	if err != nil {
		return err
	}
	if err = check(); err != nil {
		return err
	}
	return os.Rename(name, path)
}

func Run(ctx context.Context, root string, o Options, fetch FetchFunc, now time.Time, out, log io.Writer) (Stats, error) {
	var stats Stats
	if o.MaxAge <= 0 || o.Interval < 0 || o.Timeout <= 0 {
		return stats, errors.New("max-age and timeout must be positive; interval must be nonnegative")
	}
	root, err := filepath.Abs(root)
	if err != nil {
		return stats, err
	}
	info, err := os.Stat(root)
	if err != nil {
		return stats, err
	}
	if !info.IsDir() {
		return stats, errors.New("root must be a directory")
	}
	root, err = filepath.EvalSymlinks(root)
	if err != nil {
		return stats, err
	}
	type result struct {
		metadata paper.Metadata
		err      error
	}
	cache := map[string]result{}
	last := time.Time{}
	var failures []error
	err = filepath.WalkDir(root, func(path string, entry fs.DirEntry, walkErr error) error {
		if ctx.Err() != nil {
			return ctx.Err()
		}
		if walkErr != nil {
			stats.Failed++
			failures = append(failures, walkErr)
			fmt.Fprintln(log, walkErr)
			return nil
		}
		if entry.IsDir() {
			if path != root && (strings.HasPrefix(entry.Name(), ".") || entry.Name() == "node_modules") {
				return filepath.SkipDir
			}
			return nil
		}
		if entry.Type()&os.ModeSymlink != 0 || !entry.Type().IsRegular() || !strings.EqualFold(filepath.Ext(path), ".org") {
			return nil
		}
		stats.Files++
		if editorLocked(path) {
			stats.Locked++
			fmt.Fprintf(log, "skip editor-locked file: %s\n", path)
			return nil
		}
		before, err := os.ReadFile(path)
		if err != nil {
			stats.Failed++
			failures = append(failures, err)
			fmt.Fprintln(log, err)
			return nil
		}
		// Unrelated .org files may contain arbitrary data, including non-UTF-8.
		lower := bytes.ToLower(before)
		if !bytes.Contains(lower, []byte("semanticscholar.org/")) && !bytes.Contains(lower, []byte("arxiv.org/")) {
			return nil
		}
		doc, err := Parse(string(before))
		if err != nil {
			stats.Failed++
			failures = append(failures, err)
			fmt.Fprintf(log, "%s: %v\n", path, err)
			return nil
		}
		targets, err := doc.Targets()
		if err != nil {
			stats.Failed++
			failures = append(failures, err)
			fmt.Fprintf(log, "%s: %v\n", path, err)
			return nil
		}
		stats.Headings += len(targets)
		var edits []Edit
		updated := 0
		applyLocal := func(target Target) {
			local := doc.DrawerLinkEdits(target)
			local = append(local, doc.BibTeXEdits(target, nil, o.NoBibTeX)...)
			if len(local) == 0 {
				return
			}
			edits = append(edits, local...)
			updated++
			if !o.Diff {
				action := "would normalize"
				if o.Write {
					action = "normalize"
				}
				fmt.Fprintf(out, "%s %s:%d %s\n", action, path, target.Heading.Start+1, target.URL)
			}
		}
		for _, target := range targets {
			if ctx.Err() != nil {
				return ctx.Err()
			}
			if !o.Force && Recent(target.Properties, now, o.MaxAge) {
				stats.Recent++
				applyLocal(target)
				continue
			}
			key, _ := paper.Identity(target.URL)
			r, cached := cache[key]
			if !cached {
				if wait := o.Interval - time.Since(last); wait > 0 {
					select {
					case <-ctx.Done():
						return ctx.Err()
					case <-time.After(wait):
					}
				}
				last = time.Now()
				fmt.Fprintf(log, "fetch %s:%d %s\n", path, target.Heading.Start+1, target.URL)
				fetchCtx, cancel := context.WithTimeout(ctx, o.Timeout)
				r.metadata, r.err = fetch(fetchCtx, target.URL)
				cancel()
				if ctx.Err() != nil {
					return ctx.Err()
				}
				if r.err == nil {
					r.err = r.metadata.Validate()
				}
				if r.err == nil && r.metadata.First("metadataFetchedAt") == "" {
					r.err = errors.New("metadata has no successful-fetch timestamp")
				}
				cache[key] = r
				if r.err == nil {
					for _, alias := range []string{r.metadata.First("url"), r.metadata.First("arxiv")} {
						if aliasKey, ok := paper.Identity(alias); ok {
							cache[aliasKey] = r
						}
					}
					fmt.Fprintf(log, "fetched in %s: %s\n", time.Since(last).Round(time.Millisecond), target.URL)
				}
			}
			if r.err != nil {
				stats.Failed++
				failures = append(failures, r.err)
				fmt.Fprintf(log, "%s:%d: %v\n", path, target.Heading.Start+1, r.err)
				applyLocal(target)
				continue
			}
			edits = append(edits, doc.PlanWithOptions(target, r.metadata, paper.OrgOptions{NoBibTeX: o.NoBibTeX})...)
			updated++
			if !o.Diff {
				action := "would update"
				if o.Write {
					action = "update"
				}
				fmt.Fprintf(out, "%s %s:%d %s\n", action, path, target.Heading.Start+1, target.URL)
			}
		}
		if ctx.Err() != nil {
			return ctx.Err()
		}
		if len(edits) == 0 {
			return nil
		}
		after, err := doc.Apply(edits)
		if err != nil {
			stats.Failed++
			failures = append(failures, err)
			fmt.Fprintf(log, "%s: %v\n", path, err)
			return nil
		}
		if after == string(before) {
			return nil
		}
		if o.Diff {
			fmt.Fprint(out, Diff(path, string(before), after))
		}
		if o.Write {
			info, err := os.Lstat(path)
			if err == nil {
				err = WriteFile(path, before, []byte(after), info.Mode())
			}
			if err != nil {
				stats.Failed++
				failures = append(failures, err)
				fmt.Fprintf(log, "%s: %v\n", path, err)
				return nil
			}
		}
		stats.Updated += updated
		return nil
	})
	fmt.Fprintf(log, "files=%d paper-headings=%d recent=%d %s=%d failed=%d editor-locked=%d\n", stats.Files, stats.Headings, stats.Recent, map[bool]string{true: "updated", false: "would-update"}[o.Write], stats.Updated, stats.Failed, stats.Locked)
	if err != nil {
		return stats, err
	}
	if len(failures) > 0 {
		return stats, fmt.Errorf("%d metadata or file operations failed; see diagnostics above", len(failures))
	}
	return stats, nil
}

// Diff renders minimal contextual hunks without a shell or external diff tool.
func Diff(path, before, after string) string {
	edits := myers.ComputeEdits(span.URIFromPath(path), before, after)
	return fmt.Sprint(gotextdiff.ToUnified(path, path, before, edits))
}
