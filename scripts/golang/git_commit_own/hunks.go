package main

// Committing chosen change blocks of a file, without writing a patch.
//
// A block is one hunk of `git diff -U0` between the file in HEAD and the file
// on disk: a run of HEAD's lines replaced by a run of the work copy's lines.
// --list-hunks numbers the blocks and gives each a short content id;
// --hunks PATH:SELECTION builds HEAD's copy with only the chosen blocks
// applied, writes it as a blob and puts it in the private index. Nothing is
// handed to `git apply`, so no block can land at a shifted position because a
// block before it was left out. The rest of the change stays on disk.

import (
	"crypto/sha1"
	"encoding/hex"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
)

type hunk struct {
	header             string
	oldStart, oldCount int // 0-based index into HEAD's lines, and how many it replaces
	newStart, newCount int // 0-based index into the work copy's lines, and how many replace them
	body               []string
	id                 string
}

type hunkSpec struct {
	path string   // as given, relative to the current directory
	sel  []string // "3", "2-4" or "@1a2b3c4d"
}

var (
	hunkHeader   = regexp.MustCompile(`^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@`)
	hunkSelector = regexp.MustCompile(`^(\d+(-\d+)?|@[0-9a-f]{8})$`)
)

const listedBodyLines = 12

func parseHunkSpec(s string) (hunkSpec, error) {
	k := strings.LastIndexByte(s, ':')
	if k <= 0 || k == len(s)-1 {
		return hunkSpec{}, fmt.Errorf("--hunks takes PATH:SELECTION, such as notes.md:1,3 or notes.md:@1a2b3c4d")
	}
	spec := hunkSpec{path: s[:k]}
	for _, x := range strings.Split(s[k+1:], ",") {
		x = strings.TrimSpace(x)
		if !hunkSelector.MatchString(x) {
			return hunkSpec{}, fmt.Errorf("--hunks %s: %q is not a block number, a range such as 2-4, or an id such as @1a2b3c4d", s, x)
		}
		spec.sel = append(spec.sel, x)
	}
	return spec, nil
}

// relPath turns a path given relative to the current directory into one
// relative to the top of the work tree, as git stores it.
func (r *repo) relPath(p string) (string, error) {
	var rel string
	if filepath.IsAbs(p) {
		if real, err := filepath.EvalSymlinks(p); err == nil {
			p = real
		}
		var err error
		if rel, err = filepath.Rel(r.top, p); err != nil {
			return "", err
		}
	} else {
		rel = filepath.Join(r.prefix, p)
	}
	rel = filepath.ToSlash(filepath.Clean(rel))
	if rel == "." || rel == ".." || strings.HasPrefix(rel, "../") {
		return "", fmt.Errorf("%s is not a file inside the work tree", p)
	}
	return rel, nil
}

func splitLines(s string) []string {
	lines := strings.SplitAfter(s, "\n")
	if lines[len(lines)-1] == "" {
		lines = lines[:len(lines)-1]
	}
	return lines
}

// fileHunks compares rel in commit with the file on disk, after the clean
// filters git would apply when staging it. It returns HEAD's lines, the work
// copy's lines, the blocks between them and the file mode in HEAD.
func (a *app) fileHunks(r *repo, commit, rel string) ([]string, []string, []hunk, string, error) {
	top := call{dir: r.top, env: []string{"GIT_LITERAL_PATHSPECS=1"}}
	if commit == "" {
		return nil, nil, nil, "", fmt.Errorf("%s: there is no commit yet to compare it with; name the file as a path", rel)
	}
	entry, err := a.line(top, "ls-tree", commit, "--", rel)
	if err != nil {
		return nil, nil, nil, "", err
	}
	f := strings.Fields(strings.SplitN(entry, "\t", 2)[0])
	if len(f) != 3 {
		return nil, nil, nil, "", fmt.Errorf("%s is not in HEAD; a new file is committed whole, by naming it as a path", rel)
	}
	mode, kind, oldBlob := f[0], f[1], f[2]
	if kind != "blob" || (mode != "100644" && mode != "100755") {
		return nil, nil, nil, "", fmt.Errorf("%s is not a regular file in HEAD (mode %s)", rel, mode)
	}
	if st, err := os.Lstat(filepath.Join(r.top, rel)); err != nil {
		return nil, nil, nil, "", fmt.Errorf("%s is not on disk; a deleted file is committed by naming it as a path", rel)
	} else if !st.Mode().IsRegular() {
		return nil, nil, nil, "", fmt.Errorf("%s is not a regular file on disk", rel)
	}
	newBlob, err := a.line(top, "hash-object", "-w", "--path="+rel, "--", rel)
	if err != nil {
		return nil, nil, nil, "", err
	}
	oldText, err := a.git(call{}, "cat-file", "blob", oldBlob)
	if err != nil {
		return nil, nil, nil, "", err
	}
	newText, err := a.git(call{}, "cat-file", "blob", newBlob)
	if err != nil {
		return nil, nil, nil, "", err
	}
	if strings.ContainsRune(oldText, 0) || strings.ContainsRune(newText, 0) {
		return nil, nil, nil, "", fmt.Errorf("%s is binary; commit it whole by naming it as a path", rel)
	}
	diff, err := a.git(call{}, "diff", "--no-color", "--no-ext-diff", "--no-textconv", "--no-renames",
		"--unified=0", "--diff-algorithm=myers", oldBlob, newBlob)
	if err != nil {
		return nil, nil, nil, "", err
	}
	hunks, err := parseHunks(diff)
	if err != nil {
		return nil, nil, nil, "", fmt.Errorf("%s: %v", rel, err)
	}
	oldLines, newLines := splitLines(oldText), splitLines(newText)
	all := make(map[int]bool, len(hunks))
	for i := range hunks {
		all[i] = true
	}
	// Applying every block must give back the work copy exactly; anything
	// else means the blocks were misread, and nothing may be built from them.
	if applyHunks(oldLines, newLines, hunks, all) != newText {
		return nil, nil, nil, "", fmt.Errorf("%s: the blocks read from git diff do not rebuild the file; use --patch instead", rel)
	}
	return oldLines, newLines, hunks, mode, nil
}

func parseHunks(diff string) ([]hunk, error) {
	var hunks []hunk
	for _, line := range strings.Split(strings.TrimSuffix(diff, "\n"), "\n") {
		if m := hunkHeader.FindStringSubmatch(line); m != nil {
			n := func(s string, missing int) int {
				if s == "" {
					return missing
				}
				v, _ := strconv.Atoi(s)
				return v
			}
			h := hunk{header: m[0], oldCount: n(m[2], 1), newCount: n(m[4], 1)}
			// With no lines on a side, -U0 names the line after which the
			// change sits; otherwise it names the first line (1-based).
			h.oldStart, h.newStart = n(m[1], 0), n(m[3], 0)
			if h.oldCount > 0 {
				h.oldStart--
			}
			if h.newCount > 0 {
				h.newStart--
			}
			hunks = append(hunks, h)
			continue
		}
		if len(hunks) > 0 && line != "" && strings.ContainsRune("-+\\", rune(line[0])) {
			last := &hunks[len(hunks)-1]
			last.body = append(last.body, line)
		}
	}
	for i := range hunks {
		sum := sha1.Sum([]byte(strings.Join(hunks[i].body, "\n")))
		hunks[i].id = hex.EncodeToString(sum[:])[:8]
	}
	return hunks, nil
}

// applyHunks returns HEAD's lines with the chosen blocks replaced by their
// work-copy lines.
func applyHunks(oldLines, newLines []string, hunks []hunk, chosen map[int]bool) string {
	var b strings.Builder
	pos := 0
	for i, h := range hunks {
		for _, l := range oldLines[pos:h.oldStart] {
			b.WriteString(l)
		}
		if chosen[i] {
			for _, l := range newLines[h.newStart : h.newStart+h.newCount] {
				b.WriteString(l)
			}
		} else {
			for _, l := range oldLines[h.oldStart : h.oldStart+h.oldCount] {
				b.WriteString(l)
			}
		}
		pos = h.oldStart + h.oldCount
	}
	for _, l := range oldLines[pos:] {
		b.WriteString(l)
	}
	return b.String()
}

func chooseHunks(rel string, hunks []hunk, sel []string) (map[int]bool, error) {
	chosen := map[int]bool{}
	for _, s := range sel {
		if strings.HasPrefix(s, "@") {
			found := -1
			for i, h := range hunks {
				if h.id == s[1:] {
					if found >= 0 {
						return nil, fmt.Errorf("%s: %s names more than one block; select by number", rel, s)
					}
					found = i
				}
			}
			if found < 0 {
				return nil, fmt.Errorf("%s: no block has id %s; the file may have changed, so list its blocks again", rel, s)
			}
			chosen[found] = true
			continue
		}
		lo, hi := s, s
		if k := strings.IndexByte(s, '-'); k >= 0 {
			lo, hi = s[:k], s[k+1:]
		}
		from, _ := strconv.Atoi(lo)
		to, _ := strconv.Atoi(hi)
		if from < 1 || to < from || to > len(hunks) {
			return nil, fmt.Errorf("%s has %d block(s); %s is out of range", rel, len(hunks), s)
		}
		for i := from; i <= to; i++ {
			chosen[i-1] = true
		}
	}
	return chosen, nil
}

// stageHunks puts HEAD's copy of the file, with only the chosen blocks
// applied, into the private index.
func (a *app) stageHunks(r *repo, commit string, private call, spec hunkSpec) error {
	rel, err := r.relPath(spec.path)
	if err != nil {
		return err
	}
	top := call{dir: r.top, env: append(append([]string{}, private.env...), "GIT_LITERAL_PATHSPECS=1")}
	if commit != "" {
		if _, err := a.git(top, "diff", "--cached", "--quiet", commit, "--", rel); err == nil {
			// unchanged so far: good
		} else if exitCode(err) == 1 {
			return fmt.Errorf("%s is also named as a path or changed by --patch; give each file one way", rel)
		} else {
			return err
		}
	}
	oldLines, newLines, hunks, mode, err := a.fileHunks(r, commit, rel)
	if err != nil {
		return err
	}
	if len(hunks) == 0 {
		return fmt.Errorf("%s has no changes against HEAD", rel)
	}
	chosen, err := chooseHunks(rel, hunks, spec.sel)
	if err != nil {
		return err
	}
	content := applyHunks(oldLines, newLines, hunks, chosen)
	// An empty content leaves stdin unset, which reads as empty: still right.
	blob, err := a.line(call{stdin: content}, "hash-object", "-w", "--no-filters", "--stdin")
	if err != nil {
		return err
	}
	_, err = a.git(top, "update-index", "--cacheinfo", mode+","+blob+","+rel)
	return err
}

// listHunks prints the numbered blocks of each named file against HEAD.
func (a *app) listHunks(r *repo, paths []string) error {
	h, err := a.readHead()
	if err != nil {
		return err
	}
	for _, p := range paths {
		rel, err := r.relPath(p)
		if err != nil {
			return err
		}
		_, _, hunks, _, err := a.fileHunks(r, h.commit, rel)
		if err != nil {
			return err
		}
		fmt.Fprintf(a.stdout, "%s: %d block(s) against HEAD\n", rel, len(hunks))
		for i, hk := range hunks {
			fmt.Fprintf(a.stdout, "  [%d] @%s  %s\n", i+1, hk.id, hk.header)
			for j, l := range hk.body {
				if j == listedBodyLines {
					fmt.Fprintf(a.stdout, "      ... %d more line(s)\n", len(hk.body)-j)
					break
				}
				fmt.Fprintf(a.stdout, "      %s\n", l)
			}
		}
	}
	return nil
}
