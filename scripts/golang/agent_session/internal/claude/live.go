package claude

// ** live
//
// `agent_session claude live <projects-dir>...` prints one row per live Claude
// Code session, tab separated: pid, sessionId, name, cwd, transcript, tmux
// session (or "-"), status.
//
// This exists to make [agfi:h-agent-session-of-kitty-window] fast. The shell
// version asked `claude agents --json` once per config home, serially, and each
// call costs ~180ms, so the resolver spent most of half a second there. The two
// calls are independent, one daemon per config home, so running them
// concurrently in one process roughly halves the wall time (measured 341ms ->
// 183ms), and doing the record reads and transcript derivation here too
// collapses a dozen shell forks into nothing.
//
// `claude agents --json` stays the source of truth deliberately. Which
// sessions are live is decided inside Claude Code's daemon, and there is no
// signal on disk or in the process table that reproduces it: an assigned
// background session and an idle pre-forked spare share the exact same argv
// (`claude bg-spare --bg-spare .../spare/....claim.sock`), so filtering records
// by liveness ourselves would either drop real sessions or surface spares.
// We only make the authoritative call cheaper, never second-guess it.

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"

	"agent_session/internal/proc"
	"agent_session/internal/session"
)

// A config home is the parent of a `projects` directory: `~/.claude-work` for
// `~/.claude-work/projects`.
type profile struct {
	home string // absolute path of the config home
	root string // the projects dir we were handed
}

// The shape of one `claude agents --json` entry that we use. The listing
// carries more; these are the fields the resolver needs.
type agentEntry struct {
	PID       int    `json:"pid"`
	SessionID string `json:"sessionId"`
	Name      string `json:"name"`
	Cwd       string `json:"cwd"`
	Status    string `json:"status"`
}

// The part of a session record (`<home>/sessions/<pid>.json`) we read. The
// listing carries no tmux location at all; this record carries the one the
// session launched in, which is the fallback when the process tree cannot
// answer. See [tmuxOf].
type sessionRecord struct {
	Tmux string `json:"tmux"`
}

// procInfo is the process table and the tmux panes, read once for the whole
// call and shared by every profile. Both are external commands and neither
// needs an answer from `claude agents`, so they are read alongside it and
// waited on only when the rows are built.
type procInfo struct {
	ready chan struct{}
	byPID map[int]proc.Process
	panes map[int]string
}

func readProcInfo() *procInfo {
	pi := &procInfo{ready: make(chan struct{})}
	go func() {
		defer close(pi.ready)
		if procs, err := proc.ListShared(); err == nil {
			pi.byPID = proc.ByPID(procs)
		}
		pi.panes = proc.PanesShared()
	}()
	return pi
}

// wait blocks until the reads are done. Nothing writes the fields afterwards,
// so every profile may read them at once.
func (pi *procInfo) wait() (map[int]proc.Process, map[int]string) {
	<-pi.ready
	return pi.byPID, pi.panes
}

func (Adapter) Live(roots []string) ([]session.Live, error) {
	if len(roots) == 0 {
		return nil, errors.New("live: no projects directory given")
	}

	profiles := make([]profile, 0, len(roots))
	for _, root := range roots {
		profiles = append(profiles, profile{home: filepath.Dir(root), root: root})
	}

	// Started first so it runs under the `claude agents` calls rather than
	// after them. [proc.ListShared] and [proc.PanesShared] read the process
	// table and `tmux list-panes` once per process, so `live-all` pays for
	// neither twice however many adapters ask.
	pi := readProcInfo()

	// One goroutine per config home: the `claude agents` calls are the whole
	// cost and they do not depend on each other.
	var (
		mu       sync.Mutex
		sessions []session.Live
		wg       sync.WaitGroup
	)
	for _, p := range profiles {
		wg.Add(1)
		go func(p profile) {
			defer wg.Done()
			got := liveForProfile(p, pi)
			mu.Lock()
			sessions = append(sessions, got...)
			mu.Unlock()
		}(p)
	}
	wg.Wait()

	return sessions, nil
}

func liveForProfile(p profile, pi *procInfo) []session.Live {
	out, err := runAgents(p.home)
	if err != nil {
		// A profile that cannot be listed is not fatal: the other one may
		// still answer, and a broken listing should never take the hotkey
		// down. The reason goes to stderr for a human, not into the rows.
		fmt.Fprintf(os.Stderr, "agent_session claude live: %s: %v\n", p.home, err)
		return nil
	}

	var entries []agentEntry
	if err := json.Unmarshal(out, &entries); err != nil {
		fmt.Fprintf(os.Stderr, "agent_session claude live: %s: parsing agents json: %v\n", p.home, err)
		return nil
	}

	byPID, panes := pi.wait()

	sessions := make([]session.Live, 0, len(entries))
	for _, e := range entries {
		// A finished session comes back with no pid; nothing to open.
		if e.PID == 0 || e.SessionID == "" {
			continue
		}

		transcript := filepath.Join(p.home, "projects", projectSlug(e.Cwd), e.SessionID+".jsonl")

		sessions = append(sessions, session.Live{
			PID:        e.PID,
			ID:         e.SessionID,
			Name:       e.Name,
			Cwd:        e.Cwd,
			Transcript: transcript,
			Tmux:       tmuxOf(p.home, e.PID, byPID, panes),
			Status:     e.Status,
		})
	}
	return sessions
}

// runAgents runs `claude agents --json` for one config home.
//
// The personal profile (`~/.claude`) is asked with CLAUDE_CONFIG_DIR *unset*:
// Claude Code hashes a set value into its keychain service name and would go
// looking for credentials filed under the bare name. Every other home is asked
// with it set. This mirrors what [agfi:claude-work] does for the same reason.
func runAgents(home string) ([]byte, error) {
	bin, err := exec.LookPath("claude")
	if err != nil {
		return nil, fmt.Errorf("claude not on PATH: %w", err)
	}

	cmd := exec.Command(bin, "agents", "--json")

	env := os.Environ()
	// Drop any inherited CLAUDE_CONFIG_DIR, then re-add it unless this is the
	// bare personal home.
	filtered := env[:0:0]
	for _, kv := range env {
		if strings.HasPrefix(kv, "CLAUDE_CONFIG_DIR=") {
			continue
		}
		filtered = append(filtered, kv)
	}
	if filepath.Clean(home) != filepath.Clean(defaultHome()) {
		filtered = append(filtered, "CLAUDE_CONFIG_DIR="+home)
	}
	cmd.Env = filtered

	return cmd.Output()
}

// defaultHome is the config home Claude Code uses when CLAUDE_CONFIG_DIR is
// unset: `~/.claude`.
func defaultHome() string {
	h, err := os.UserHomeDir()
	if err != nil {
		return ""
	}
	return filepath.Join(h, ".claude")
}

// tmuxOf is the tmux session a Claude Code session runs in, or "" when there
// is none (an attach via `claude agents`, or a session run outside tmux).
//
// The process tree is asked first, and the session's own record
// (`<home>/sessions/<pid>.json`) only when the walk finds nothing. The record
// carries the tmux session name as it stood when the session launched, and the
// autoname hooks rename tmux sessions on every prompt, so that name goes stale
// the moment the work is named: five of seventeen live sessions named a tmux
// session that no longer existed. Walking the parents up to a pane instead
// reports where the process actually sits, whatever the session is called now.
//
// The record is still the answer for a process the table no longer has, and
// for one whose pane cannot be found -- and it is the only answer when tmux is
// not running at all.
func tmuxOf(home string, pid int, byPID map[int]proc.Process, panes map[int]string) string {
	if name := proc.TmuxOf(pid, byPID, panes); name != "" {
		return name
	}
	return recordedTmux(home, pid)
}

// recordedTmux is the tmux session named in the session's own record. The
// field looks like `claude-work-scripts-2:@149.%151`; only the session name
// before the first colon matters to the resolver.
func recordedTmux(home string, pid int) string {
	path := filepath.Join(home, "sessions", fmt.Sprintf("%d.json", pid))
	data, err := os.ReadFile(path)
	if err != nil {
		return ""
	}
	var rec sessionRecord
	if err := json.Unmarshal(data, &rec); err != nil {
		return ""
	}
	if rec.Tmux == "" || rec.Tmux == "-" {
		return ""
	}
	return strings.SplitN(rec.Tmux, ":", 2)[0]
}
