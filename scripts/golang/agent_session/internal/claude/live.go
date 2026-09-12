package claude

// ** live
//
// `agent_session claude live <projects-dir>...` prints one row per live Claude
// Code session, tab separated: pid, sessionId, name, cwd, transcript, tmux
// session (or "-"), status.
//
// This exists to make [agfi:h-agent-session-of-kitty-window] fast. The shell
// version asked `claude agents --json` once per config home, serially, and each
// call costs ~170ms, so the resolver spent most of half a second there.
//
// The rows come from Claude Code's own session records instead:
// `<config-home>/sessions/<pid>.json`, one file per running session, written by
// the session and removed when it exits. Measured against `claude agents
// --json` on this machine, the records are exactly what the listing prints:
// the listing's only extra entries are finished background agents carrying no
// pid, which never had a row anyway. And the record says more than the listing
// does -- busy/idle, the tmux location the session launched in, its kind, its
// version -- so the tmux fallback that used to read these files alongside the
// subprocess is now the same read. A dozen small file reads cost under a
// millisecond, against 170ms per config home for the subprocess, which leaves
// `live` bounded by the one `ps` and the one `tmux list-panes` it shares with
// the other adapters.
//
// The one thing the records do not do for us is notice a session that died
// without cleaning up: a crash or a `kill -9` leaves the file behind. So every
// record is checked against its pid and a dead one is dropped. That is not the
// argv guessing we still refuse to do -- an assigned background session and an
// idle pre-forked spare share the identical command line, so deciding liveness
// from the process table alone would either drop real sessions or surface
// spares -- it only discards records whose process is gone.

import (
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strings"

	"agent_session/internal/proc"
	"agent_session/internal/session"
)

// The part of a session record (`<home>/sessions/<pid>.json`) we read. The file
// carries more -- startedAt, version, entrypoint, the messaging socket path --
// none of which a live row shows.
type sessionRecord struct {
	PID       int    `json:"pid"`
	SessionID string `json:"sessionId"`
	Name      string `json:"name"`
	Cwd       string `json:"cwd"`
	// "interactive" or "bg" (the CLI's `--json` says "background" for the
	// same thing; [normalizeKind] settles on the CLI's word). Nothing filters
	// on it: both kinds are sessions the resolver may be asked to open. It
	// decides the tmux column, though -- see [tmuxOf] -- and is passed on so
	// a consumer that needs a route to the session knows a pane will not do.
	Kind string `json:"kind"`
	// "busy" or "idle", empty in a record too old to carry it.
	Status string `json:"status"`
	// The tmux location the session launched in, `<session>:@<window>.%<pane>`.
	// Only a fallback; see [tmuxOf].
	Tmux string `json:"tmux"`
}

// procInfo is the process table and the tmux panes, read once for the whole
// call and shared by every profile. Both are external commands and now the
// whole cost of `live`, so they are started before the records are read and
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
// so every caller may read them at once.
func (pi *procInfo) wait() (map[int]proc.Process, map[int]string) {
	<-pi.ready
	return pi.byPID, pi.panes
}

func (Adapter) Live(roots []string) ([]session.Live, error) {
	if len(roots) == 0 {
		return nil, errors.New("live: no projects directory given")
	}

	// Started first so the two external commands run under the record reads
	// rather than after them. [proc.ListShared] and [proc.PanesShared] read
	// the process table and `tmux list-panes` once per process, so `live-all`
	// pays for neither twice however many adapters ask.
	pi := readProcInfo()

	// A config home is the parent of a `projects` directory: `~/.claude-work`
	// for `~/.claude-work/projects`. Reading a home is a handful of small
	// files now, so the homes are walked in order rather than one goroutine
	// each; the concurrency that mattered was against the subprocess.
	homes := make([]string, len(roots))
	records := make([][]sessionRecord, len(roots))
	for i, root := range roots {
		homes[i] = filepath.Dir(root)
		records[i] = readSessionRecords(homes[i])
	}

	byPID, panes := pi.wait()

	var sessions []session.Live
	for i, home := range homes {
		for _, rec := range records[i] {
			// A finished background agent has no pid, and a crashed
			// session leaves its record behind: neither names a process
			// the resolver could open.
			if rec.PID == 0 || rec.SessionID == "" || !recordAlive(rec, byPID) {
				continue
			}

			sessions = append(sessions, session.Live{
				PID:        rec.PID,
				ID:         rec.SessionID,
				Name:       rec.Name,
				Cwd:        rec.Cwd,
				Transcript: filepath.Join(home, "projects", projectSlug(rec.Cwd), rec.SessionID+".jsonl"),
				Tmux:       tmuxOf(rec, byPID, panes),
				Status:     rec.Status,
				Kind:       normalizeKind(rec.Kind),
			})
		}
	}
	return sessions, nil
}

// readSessionRecords is every session record in one config home, in the glob's
// order (by file name). The same directory holds `<pid>.<hash>.key` files,
// which the `*.json` pattern leaves alone.
//
// A record that cannot be read or parsed is skipped rather than failing the
// home: the file is rewritten in place on every status change, so a read can
// land mid-write, and one such file should not take the other rows down.
func readSessionRecords(home string) []sessionRecord {
	paths, err := filepath.Glob(filepath.Join(home, "sessions", "*.json"))
	if err != nil {
		return nil
	}
	recs := make([]sessionRecord, 0, len(paths))
	for _, path := range paths {
		data, err := os.ReadFile(path)
		if err != nil {
			continue
		}
		var rec sessionRecord
		if err := json.Unmarshal(data, &rec); err != nil {
			continue
		}
		recs = append(recs, rec)
	}
	return recs
}

// recordAlive reports whether the process a record names is still running.
//
// [pidAlive] asks the kernel directly, which is exact and free. Where the
// platform has no such call it declines to answer and the shared process table
// stands in -- the same `ps` the tmux walk already needs, so this costs
// nothing extra. A process table we failed to read answers nothing at all, and
// dropping every row on that basis would be worse than trusting the records.
func recordAlive(rec sessionRecord, byPID map[int]proc.Process) bool {
	if alive, ok := pidAlive(rec.PID); ok {
		return alive
	}
	if byPID == nil {
		return true
	}
	_, found := byPID[rec.PID]
	return found
}

// tmuxOf is the tmux session a Claude Code session runs in, or "" when there
// is none (an attach via `claude agents`, or a session run outside tmux).
//
// The process tree is asked first, and the session's own record only when the
// walk finds nothing. The record carries the tmux session name as it stood when
// the session launched, and the autoname hooks rename tmux sessions on every
// prompt, so that name goes stale the moment the work is named: five of
// seventeen live sessions named a tmux session that no longer existed. Walking
// the parents up to a pane instead reports where the process actually sits,
// whatever the session is called now.
//
// The record is still the answer for a process the table no longer has, and
// for one whose pane cannot be found -- and it is the only answer when tmux is
// not running at all.
//
// A background session is the exception: it runs under a pty host that is
// parented to init, so the walk never finds a pane, and the record's location
// is where `claude --bg` was *typed* -- a tmux session the conversation does
// not live in. Publishing that would send every consumer that trusts this
// column (the tmux picker, the window resolver, anything typing into a pane)
// to the wrong place, so a background session gets no tmux location at all.
func tmuxOf(rec sessionRecord, byPID map[int]proc.Process, panes map[int]string) string {
	if name := proc.TmuxOf(rec.PID, byPID, panes); name != "" {
		return name
	}
	if normalizeKind(rec.Kind) == session.KindBackground {
		return ""
	}
	return recordedTmux(rec)
}

// normalizeKind maps the record's spelling onto [session.KindInteractive] /
// [session.KindBackground], and leaves anything else (including "") as is.
func normalizeKind(kind string) string {
	switch kind {
	case "bg", session.KindBackground:
		return session.KindBackground
	case session.KindInteractive:
		return session.KindInteractive
	}
	return kind
}

// recordedTmux is the tmux session named in the record. The field looks like
// `claude-work-scripts-2:@149.%151`; only the session name before the first
// colon matters to the resolver.
func recordedTmux(rec sessionRecord) string {
	if rec.Tmux == "" || rec.Tmux == "-" {
		return ""
	}
	return strings.SplitN(rec.Tmux, ":", 2)[0]
}
