package agy

import (
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"

	"agent_session/internal/proc"
	"agent_session/internal/session"
)

// ** live
//
// Antigravity has no listing of what is running and writes no lock per
// conversation, so a running `agy` is paired with a conversation through the
// directory it was started in: `cache/last_conversations.json` maps a workspace
// to the conversation last opened there. That is the same answer `agy` itself
// would resume, so it is the right one whenever a workspace hosts one
// conversation at a time; when it hosts several, the picker and the hooks'
// record on the tmux session settle it, which is what the window resolver falls
// back to.
//
// A conversation with no transcript yet gets no row: there would be nothing to
// open.

var agyCmdRe = regexp.MustCompile(`(^|/)agy( |$)`)

// Whether a command line is an Antigravity session, as opposed to one of its
// helper invocations.
func isAgyCmd(cmd string) bool {
	f := strings.Fields(cmd)
	if len(f) == 0 || !agyCmdRe.MatchString(f[0]+" ") {
		return false
	}
	if len(f) > 1 {
		switch f[1] {
		case "remote-control", "mcp", "login", "logout", "update", "--version", "-v", "--help", "-h":
			return false
		}
	}
	return true
}

// The workspace -> conversation id map Antigravity keeps for "resume here".
func lastConversations(home string) map[string]string {
	out := map[string]string{}
	raw, err := os.ReadFile(filepath.Join(home, "cache", "last_conversations.json"))
	if err != nil {
		return out
	}
	var m map[string]string
	if err := json.Unmarshal(raw, &m); err != nil {
		return out
	}
	for k, v := range m {
		out[filepath.Clean(k)] = v
	}
	return out
}

func (Adapter) Live(roots []string) ([]session.Live, error) {
	if len(roots) == 0 {
		return nil, errors.New("live: no brain directory given")
	}

	procs, err := proc.List()
	if err != nil {
		return nil, err
	}
	byPID := proc.ByPID(procs)

	var pids []int
	for _, p := range procs {
		if isAgyCmd(p.Cmd) {
			pids = append(pids, p.PID)
		}
	}
	if len(pids) == 0 {
		return nil, nil
	}
	cwds := proc.Cwds(pids)
	panes := proc.TmuxPanes()

	var out []session.Live
	for _, root := range roots {
		home := filepath.Dir(root)
		byWorkspace := lastConversations(home)

		for _, pid := range pids {
			cwd := cwds[pid]
			if cwd == "" {
				continue
			}
			id := byWorkspace[filepath.Clean(cwd)]
			if id == "" {
				continue
			}
			transcript := transcriptOf(filepath.Join(root, id))
			if transcript == "" {
				continue
			}

			out = append(out, session.Live{
				PID:        pid,
				ID:         id,
				Name:       nameOf(home, id),
				Cwd:        cwd,
				Transcript: transcript,
				Tmux:       proc.TmuxOf(pid, byPID, panes),
				Status:     "running",
			})
		}
	}

	sort.Slice(out, func(i, j int) bool {
		if out[i].PID != out[j].PID {
			return out[i].PID < out[j].PID
		}
		return out[i].ID < out[j].ID
	})
	return out, nil
}
