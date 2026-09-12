// Package session is the contract between the agent_session CLI and the
// per-agent adapters: what an adapter must be able to say about a transcript
// (list it, name it, preview it, tell whether it is live, turn it into a
// document) and the row shapes the zsh side reads back.
package session

import (
	"errors"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"

	"agent_session/internal/turns"
)

// MaxLineBytes is the scanner buffer for transcript lines. Session lines carry
// whole files inline, so the default 64KiB is nowhere near enough.
const MaxLineBytes = 64 << 20

// Info is one row of `list`: a transcript, when it last moved, what it is
// called and what it opened with.
type Info struct {
	Path string
	// Relative to its root, prefixed with the root's label when several roots
	// were listed.
	Rel     string
	Epoch   int64
	Stamp   string
	Name    string
	Snippet string
}

// Row is the TSV line `list` prints: epoch, path, local time, name, relative
// path, snippet.
func (i Info) Row() string {
	return fmt.Sprintf("%d\t%s\t%s\t%s\t%s\t%s", i.Epoch, i.Path, i.Stamp, i.Name, i.Rel, i.Snippet)
}

// Sort orders rows newest first, ties broken by path so the output is stable.
func Sort(infos []Info) {
	sort.SliceStable(infos, func(i, j int) bool {
		if infos[i].Epoch != infos[j].Epoch {
			return infos[i].Epoch > infos[j].Epoch
		}
		return infos[i].Path < infos[j].Path
	})
}

// Live is one row of `live`: a running session, which process it is, where it
// runs and which transcript it writes.
type Live struct {
	PID        int
	ID         string
	Name       string
	Cwd        string
	Transcript string
	// The tmux session it runs in, or "" for none.
	Tmux   string
	Status string
	// KindInteractive or KindBackground for an agent that has the notion,
	// "" for one that does not. A background session runs under a pty host
	// with no terminal of its own, so nothing that types into a pane or a
	// window can reach it; consumers that pick a delivery route need to know.
	Kind string
}

// The two kinds of Claude Code session. The CLI's `claude agents --json`
// spells them "interactive" and "background"; the session record spells the
// second "bg", and the adapter normalizes to these so no consumer has to know.
const (
	KindInteractive = "interactive"
	KindBackground  = "background"
)

// Row is the TSV line `live` prints: pid, id, name, cwd, transcript, tmux
// session or `-`, status or `-`, kind or `-`.
func (l Live) Row() string {
	tmux, status, kind := l.Tmux, l.Status, l.Kind
	if tmux == "" {
		tmux = "-"
	}
	if status == "" {
		status = "-"
	}
	if kind == "" {
		kind = "-"
	}
	return fmt.Sprintf("%d\t%s\t%s\t%s\t%s\t%s\t%s\t%s", l.PID, l.ID, l.Name, l.Cwd, l.Transcript, tmux, status, kind)
}

// Meta is what `meta` says about one transcript.
type Meta struct {
	ID   string
	Name string
	Cwd  string
}

// Row is the TSV line `meta` prints: id, name, cwd.
func (m Meta) Row() string {
	return m.ID + "\t" + m.Name + "\t" + m.Cwd
}

// ListOpts are the `list` flags.
type ListOpts struct {
	// Only sessions that ran in this directory; "" for all of them.
	Cwd string
	// Also list the transcripts of spawned subagents.
	Subagents  bool
	SnippetLen int
	NameLen    int
	Jobs       int
	// List exactly these transcripts instead of walking the roots. What the
	// live pickers want: they have a handful of paths in hand and need the
	// columns for those, and walking a corpus of a thousand-odd sessions to
	// annotate twenty of them was most of what a picker cost.
	Only []string
	// Which record dates a session: [LastByAny] (the default, and what an
	// empty string is read as) or [LastByUser]. The zero value is the old
	// behaviour, so a caller that never heard of this needs no change.
	LastBy string
}

// The values of [ListOpts.LastBy]. A session whose agent has been grinding
// through its own tool calls for an hour was not touched an hour ago, and a
// picker that sorts by conversation rather than by machine noise needs to say
// which of the two it means.
const (
	LastByAny  = "any"
	LastByUser = "user"
)

// UserOnly is whether these options date a session by the last message the
// user typed, rather than by its last record of any kind.
func (o ListOpts) UserOnly() bool { return o.LastBy == LastByUser }

// PreviewOpts are the `preview` flags.
type PreviewOpts struct {
	// How much of the transcript's tail to read; 0 or less reads all of it.
	Bytes int64
	Color bool
	// Lay the body out for a pane too small for the ordinary two columns:
	// short labels, no blank lines between the blocks, and the long fields
	// cut short. See [preview.Layout].
	Compact bool
}

// DocOpts are the `render` flags an adapter cares about; the rest belong to
// [turns.Render].
type DocOpts struct {
	// Inline the transcripts of spawned subagents.
	Subagents bool
}

// An Adapter knows one agent's on-disk session store.
type Adapter interface {
	// List scans the transcripts under roots.
	List(roots []string, o ListOpts) ([]Info, error)
	// Name is the session's name, "" for an unnamed one.
	Name(path string) (string, error)
	Meta(path string) (Meta, error)
	// Preview is the fzf preview body, ready to print.
	Preview(path string, o PreviewOpts) (string, error)
	// Live is the running sessions whose transcripts sit under roots.
	Live(roots []string) ([]Live, error)
	// Document is the transcript as the renderer's model.
	Document(path string, o DocOpts) (*turns.Document, error)
}

// ErrUnsupported is what an adapter returns for a subcommand its agent has no
// equivalent of, so the CLI can say so rather than fail obscurely.
var ErrUnsupported = errors.New("not supported for this agent")

// Fatal prints the message and exits 1.
func Fatal(msg string) {
	fmt.Fprintln(os.Stderr, "agent_session: "+msg)
	os.Exit(1)
}

// Under is the roots each of the given paths sits under, as pairs, keeping the
// order the paths were given in. A path under none of the roots is left out,
// since the columns are relative to a root. For [ListOpts.Only].
func Under(paths, roots []string) [][2]string {
	var out [][2]string
	for _, p := range paths {
		abs := p
		if a, err := filepath.Abs(p); err == nil {
			abs = a
		}
		for _, root := range roots {
			r := filepath.Clean(root)
			if abs == r || strings.HasPrefix(abs, r+string(filepath.Separator)) {
				out = append(out, [2]string{abs, root})
				break
			}
		}
	}
	return out
}

// GuardPathArgs spells dash-leading path arguments as `./…` before flag
// parsing. Every relative path inside ~/.claude/projects begins with a dash,
// because the project directories are named after the cwd they belong to
// (`-Users-evar-scripts`). The flag package would read those as flags, so an
// argument that is not a defined flag and does name an existing file is
// rewritten. `--` still works for anything this misses.
func GuardPathArgs(fs *flag.FlagSet, argv []string) []string {
	out := make([]string, len(argv))
	copy(out, argv)

	for i, a := range out {
		if !strings.HasPrefix(a, "-") || a == "-" || a == "--" {
			continue
		}

		name, _, _ := strings.Cut(strings.TrimLeft(a, "-"), "=")
		if fs.Lookup(name) != nil {
			continue
		}
		if _, err := os.Stat(a); err == nil {
			out[i] = "." + string(filepath.Separator) + a
		}
	}

	return out
}

// ProfileLabels is how each root is labelled in a listing, keyed by root. The
// relative path alone is ambiguous the moment more than one root is listed:
// every Claude profile has a -Users-evar-scripts/ under it, and in project
// scope the roots end in that same component.
//
// So the label is the first path component in which the roots actually
// differ. For ~/.claude/projects and ~/.claude-work/projects that is .claude
// and .claude-work, and it stays right when the caller scopes the roots down
// to one project apiece -- which taking a fixed component would not: their
// parent is then "projects" for both.
//
// Empty for a single root, so single-root output is unchanged.
func ProfileLabels(roots []string) map[string]string {
	labels := make(map[string]string, len(roots))
	if len(roots) < 2 {
		return labels
	}

	split := make([][]string, len(roots))
	shortest := -1
	for i, r := range roots {
		split[i] = strings.Split(filepath.Clean(r), string(filepath.Separator))
		if shortest < 0 || len(split[i]) < shortest {
			shortest = len(split[i])
		}
	}

	at := 0
	for ; at < shortest; at++ {
		same := true
		for i := 1; i < len(split); i++ {
			if split[i][at] != split[0][at] {
				same = false
				break
			}
		}
		if !same {
			break
		}
	}

	// One root is a prefix of another, so there is no differing component for
	// the shorter one; its own last component is the best it has.
	for i, r := range roots {
		idx := at
		if idx >= len(split[i]) {
			idx = len(split[i]) - 1
		}
		labels[r] = split[i][idx]
	}
	return labels
}

// ForEach calls fn for every index below n, from at most jobs goroutines, and
// returns when all have run. Order is not defined; fn writes to its own slot.
func ForEach(n, jobs int, fn func(i int)) {
	if n == 0 {
		return
	}
	if jobs > n {
		jobs = n
	}
	if jobs < 1 {
		jobs = 1
	}

	var wg sync.WaitGroup
	queue := make(chan int)
	for w := 0; w < jobs; w++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for i := range queue {
				fn(i)
			}
		}()
	}
	for i := 0; i < n; i++ {
		queue <- i
	}
	close(queue)
	wg.Wait()
}
