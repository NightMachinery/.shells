package codex

import (
	"errors"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"sync"

	"agent_session/internal/proc"
	"agent_session/internal/session"
)

// ** live
//
// Codex publishes no listing of its running threads, so the process table is
// asked. The thread a process is writing is exact rather than guessed: a
// running Codex holds `thread-writer-locks/<id>.lock` open (a lock nobody has
// open is a leftover), and one `lsof` over the lock files says which pid holds
// which. The holder is the native binary; the process a shell -- and kitty --
// sees is its launcher (`node .../codex.js`), so the row carries the topmost
// Codex ancestor, and gets its cwd and tmux session from there.
//
// The desktop app holds locks too, through its own bundled `codex`. Its rows
// keep the holder's pid: no terminal window's foreground process, so the
// window resolver can never match them, while the picker still lists them.

var codexCmdRe = regexp.MustCompile(`(^|[/ ])codex(\.js)?( |$)`)

// Whether a command line is a Codex process: `codex ...`, `/path/codex.js
// ...`, `node /path/codex.js ...`. Subcommands that are not a session --
// `exec`, `agents`, `mcp`, `login`, `app-server` -- are left out.
func isCodexCmd(cmd string) bool {
	if !codexCmdRe.MatchString(cmd) {
		return false
	}
	f := strings.Fields(cmd)
	for i, w := range f {
		if !codexCmdRe.MatchString(w + " ") {
			continue
		}
		if i+1 < len(f) {
			switch f[i+1] {
			case "exec", "agents", "mcp", "mcp-server", "app-server", "login", "logout", "completion", "debug", "apply", "cloud", "queue", "features":
				return false
			}
		}
		return true
	}
	return false
}

// Every rollout under the roots, by thread id -- the newest when a thread has
// more than one, which is the order [filepath.Glob] already returns.
func rolloutIndex(roots []string) map[string]string {
	out := map[string]string{}
	for _, root := range roots {
		matches, _ := filepath.Glob(filepath.Join(root, "*", "*", "*", "rollout-*.jsonl"))
		for _, p := range matches {
			if id := idOf(p); id != "" {
				out[id] = p
			}
		}
	}
	return out
}

// The pids that could be holding a thread lock: a process whose own command
// mentions Codex, and everything under one. Deliberately looser than
// [isCodexCmd] -- it only has to be a superset, since the ancestor walk and
// the rollout's own metadata decide what is really a session -- and it exists
// because restricting `lsof` to it took the lock trace from 240ms to 40ms
// across 1500 processes.
func codexPids(procs []proc.Process) []int {
	var roots []int
	for _, p := range procs {
		if strings.Contains(strings.ToLower(p.Cmd), "codex") {
			roots = append(roots, p.PID)
		}
	}
	if len(roots) == 0 {
		return nil
	}

	set := proc.Descendants(roots, procs)
	out := make([]int, 0, len(set))
	for pid := range set {
		out = append(out, pid)
	}
	sort.Ints(out)
	return out
}

// The outermost Codex process above pid: the launcher a shell started, which is
// what kitty lists as the window's foreground process.
func topmostCodex(pid int, byPID map[int]proc.Process) int {
	top := pid
	for depth := 0; depth < 16; depth++ {
		p, ok := byPID[top]
		if !ok {
			break
		}
		parent, ok := byPID[p.PPID]
		if !ok || !isCodexCmd(parent.Cmd) {
			break
		}
		top = parent.PID
	}
	return top
}

func (Adapter) Live(roots []string) ([]session.Live, error) {
	if len(roots) == 0 {
		return nil, errors.New("live: no sessions directory given")
	}

	procs, err := proc.ListShared()
	if err != nil {
		return nil, err
	}
	byPID := proc.ByPID(procs)

	type lockInfo struct{ home, root, id string }
	var locks []string
	info := map[string]lockInfo{}
	for _, root := range roots {
		home := filepath.Dir(root)
		matches, _ := filepath.Glob(filepath.Join(home, "thread-writer-locks", "*.lock"))
		for _, l := range matches {
			locks = append(locks, l)
			info[l] = lockInfo{home: home, root: root, id: strings.TrimSuffix(filepath.Base(l), ".lock")}
		}
	}
	// Only Codex's own processes are asked about, which is what makes the
	// trace cheap; no candidates at all means no Codex is running, so the
	// lsof is skipped entirely. See [codexPids].
	cands := codexPids(procs)
	if len(cands) == 0 {
		return nil, nil
	}

	// The two lsof calls have no need of each other's answers, so they run at
	// once: the cwds are asked of every candidate rather than of the holders,
	// which is a slightly wider question and saves waiting for the first call
	// to say who the holders are. The panes come along for the ride.
	var (
		holders map[string][]int
		cwds    map[int]string
		panes   map[int]string
		wg      sync.WaitGroup
	)
	wg.Add(3)
	go func() { defer wg.Done(); holders = proc.Holders(locks, cands) }()
	go func() { defer wg.Done(); cwds = proc.Cwds(cands) }()
	go func() { defer wg.Done(); panes = proc.PanesShared() }()
	wg.Wait()

	tops := map[int]int{}
	for _, hs := range holders {
		for _, h := range hs {
			tops[h] = topmostCodex(h, byPID)
		}
	}

	// One sweep of the date directories, indexed by thread id: a glob per lock
	// walked the same tree once for every live thread.
	rollouts := rolloutIndex(roots)

	var out []session.Live
	for lock, hs := range holders {
		li := info[lock]
		transcript := rollouts[li.id]
		if transcript == "" {
			continue
		}
		meta, _ := readMeta(transcript)
		if meta.ParentThreadID != nil {
			// A subagent's thread is not a window's session.
			continue
		}

		for _, h := range hs {
			top := tops[h]
			cwd := cwds[top]
			if cwd == "" {
				cwd = cwds[h]
			}
			if cwd == "" {
				cwd = meta.Cwd
			}
			out = append(out, session.Live{
				PID:        top,
				ID:         li.id,
				Name:       names(li.home)[li.id],
				Cwd:        cwd,
				Transcript: transcript,
				Tmux:       proc.TmuxOf(top, byPID, panes),
				Status:     "running",
			})
		}
	}

	// Map order is random; the rows are not.
	sort.Slice(out, func(i, j int) bool {
		if out[i].PID != out[j].PID {
			return out[i].PID < out[j].PID
		}
		return out[i].ID < out[j].ID
	})
	return out, nil
}
