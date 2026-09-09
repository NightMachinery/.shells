package codex

import (
	"errors"
	"path/filepath"
	"regexp"
	"sort"
	"strings"

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

	procs, err := proc.List()
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
	holders := proc.Holders(locks)

	// Every pid the rows will name, so one lsof gives all their cwds.
	tops := map[int]int{}
	var pids []int
	for _, hs := range holders {
		for _, h := range hs {
			top := topmostCodex(h, byPID)
			tops[h] = top
			pids = append(pids, h, top)
		}
	}
	cwds := proc.Cwds(pids)
	panes := proc.TmuxPanes()

	var out []session.Live
	for lock, hs := range holders {
		li := info[lock]
		matches, _ := filepath.Glob(filepath.Join(li.root, "*", "*", "*", "rollout-*-"+li.id+".jsonl"))
		if len(matches) == 0 {
			continue
		}
		transcript := matches[len(matches)-1]
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
