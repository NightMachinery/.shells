package claude

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	"agent_session/internal/proc"
	"agent_session/internal/session"
)

// The record's tmux name is what the session launched in, and the autoname
// hooks rename tmux sessions on every prompt, so it goes stale. The process
// tree has to win over it; the record is only the fallback.
func TestTmuxOfPrefersTheProcessTree(t *testing.T) {
	under := sessionRecord{PID: 300, Tmux: "+Claude/default fftmux-agent-old:@1.%2"}
	outside := sessionRecord{PID: 500, Tmux: "detached-at-launch:@3.%4"}

	proc.ResetShared()
	defer proc.ResetShared()
	oldRun := proc.Run
	proc.Run = func(name string, args ...string) ([]byte, error) {
		switch name {
		case "ps":
			return []byte("" +
				"  100     1 tmux\n" +
				"  200   100 -zsh\n" +
				"  300   200 claude\n" +
				"  500     1 claude\n"), nil
		case "tmux":
			// The pane's session has since been renamed; the record has not.
			return []byte("200\t+Claude/default fftmux-agent-now\n"), nil
		}
		return nil, nil
	}
	defer func() { proc.Run = oldRun }()

	procs, err := proc.ListShared()
	if err != nil {
		t.Fatal(err)
	}
	byPID, panes := proc.ByPID(procs), proc.PanesShared()

	// 300 sits under the pane, so the current name wins over the stale record.
	if got := tmuxOf(under, byPID, panes); got != "+Claude/default fftmux-agent-now" {
		t.Errorf("under tmux: got %q, want the pane's current session name", got)
	}
	// 500 is not under any pane: the record is all there is.
	if got := tmuxOf(outside, byPID, panes); got != "detached-at-launch" {
		t.Errorf("outside tmux: got %q, want the recorded name", got)
	}
	// A pid the table never had, whose record names no tmux either.
	if got := tmuxOf(sessionRecord{PID: 999}, byPID, panes); got != "" {
		t.Errorf("unknown pid: got %q, want empty", got)
	}
}

// With tmux not running there are no panes, and every row falls back to its
// record rather than losing its tmux column.
func TestTmuxOfWithoutTmux(t *testing.T) {
	proc.ResetShared()
	defer proc.ResetShared()
	oldRun := proc.Run
	proc.Run = func(name string, args ...string) ([]byte, error) {
		if name == "ps" {
			return []byte("  300     1 claude\n"), nil
		}
		return nil, os.ErrNotExist
	}
	defer func() { proc.Run = oldRun }()

	procs, _ := proc.ListShared()
	rec := sessionRecord{PID: 300, Tmux: "some-session:@1.%2"}
	if got := tmuxOf(rec, proc.ByPID(procs), proc.PanesShared()); got != "some-session" {
		t.Errorf("got %q, want the recorded name", got)
	}
}

// The rows are the session records of the config home, minus the ones whose
// process is gone: a record is removed when its session exits, but a crash
// leaves one behind and it must not become a row.
func TestLiveReadsTheSessionRecords(t *testing.T) {
	home := t.TempDir()

	// A pid above every kernel's range, so it cannot be alive and cannot be
	// reused by something else while the test runs.
	const deadPID = 2147483000

	live := sessionRecord{
		PID:       os.Getpid(),
		SessionID: "11111111-1111-1111-1111-111111111111",
		Name:      "still-here",
		Cwd:       "/Users/evar/tmp/x",
		Kind:      "interactive",
		Status:    "busy",
		Tmux:      "+Claude/work still-here:@375.%381",
	}
	dead := sessionRecord{
		PID:       deadPID,
		SessionID: "22222222-2222-2222-2222-222222222222",
		Name:      "crashed",
		Cwd:       "/Users/evar/tmp/y",
		Kind:      "interactive",
		Status:    "idle",
		Tmux:      "gone:@1.%2",
	}
	writeRecord(t, home, live)
	writeRecord(t, home, dead)
	// The keychain files sit in the same directory and are not records.
	writeSessionFile(t, home, strconv.Itoa(live.PID)+".deadbeef.key", "not json")

	proc.ResetShared()
	defer proc.ResetShared()
	oldRun := proc.Run
	proc.Run = func(name string, args ...string) ([]byte, error) {
		if name == "ps" {
			// The live pid is in the table but under no pane, so its tmux
			// column comes from the record.
			return []byte("  " + strconv.Itoa(live.PID) + "     1 claude\n"), nil
		}
		return nil, os.ErrNotExist
	}
	defer func() { proc.Run = oldRun }()

	rows, err := Adapter{}.Live([]string{filepath.Join(home, "projects")})
	if err != nil {
		t.Fatal(err)
	}
	if len(rows) != 1 {
		t.Fatalf("got %d rows, want only the live one: %+v", len(rows), rows)
	}

	got := rows[0]
	if got.PID != live.PID {
		t.Errorf("pid: got %d, want %d", got.PID, live.PID)
	}
	if got.ID != live.SessionID {
		t.Errorf("id: got %q, want %q", got.ID, live.SessionID)
	}
	if got.Name != live.Name {
		t.Errorf("name: got %q, want %q", got.Name, live.Name)
	}
	if got.Cwd != live.Cwd {
		t.Errorf("cwd: got %q, want %q", got.Cwd, live.Cwd)
	}
	if got.Status != "busy" {
		t.Errorf("status: got %q, want the record's busy", got.Status)
	}
	if got.Tmux != "+Claude/work still-here" {
		t.Errorf("tmux: got %q, want the recorded session name", got.Tmux)
	}
	if got.Kind != session.KindInteractive {
		t.Errorf("kind: got %q, want %q", got.Kind, session.KindInteractive)
	}
	want := filepath.Join(home, "projects", projectSlug(live.Cwd), live.SessionID+".jsonl")
	if got.Transcript != want {
		t.Errorf("transcript: got %q, want %q", got.Transcript, want)
	}
}

func writeRecord(t *testing.T, home string, rec sessionRecord) {
	t.Helper()
	data, err := json.Marshal(rec)
	if err != nil {
		t.Fatal(err)
	}
	writeSessionFile(t, home, strconv.Itoa(rec.PID)+".json", string(data))
}

func writeSessionFile(t *testing.T, home, name, body string) {
	t.Helper()
	dir := filepath.Join(home, "sessions")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, name), []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}
}

func TestLiveBackgroundSessionHasNoTmuxAndSaysSo(t *testing.T) {
	home := t.TempDir()
	if err := os.MkdirAll(filepath.Join(home, "sessions"), 0o755); err != nil {
		t.Fatal(err)
	}

	// A `claude --bg` typed from inside a tmux pane records that pane as its
	// launch location, but the session runs under the pty host, not there.
	bg := sessionRecord{
		PID:       os.Getpid(),
		SessionID: "33333333-3333-3333-3333-333333333333",
		Name:      "in-the-background",
		Cwd:       "/Users/evar/tmp/z",
		Kind:      "bg",
		Status:    "idle",
		Tmux:      "typed-here:@9.%9",
	}
	writeRecord(t, home, bg)

	proc.ResetShared()
	defer proc.ResetShared()
	oldRun := proc.Run
	proc.Run = func(name string, args ...string) ([]byte, error) {
		if name == "ps" {
			return []byte("  " + strconv.Itoa(bg.PID) + "     1 claude\n"), nil
		}
		return nil, os.ErrNotExist
	}
	defer func() { proc.Run = oldRun }()

	rows, err := Adapter{}.Live([]string{filepath.Join(home, "projects")})
	if err != nil {
		t.Fatal(err)
	}
	if len(rows) != 1 {
		t.Fatalf("got %d rows, want one: %+v", len(rows), rows)
	}
	got := rows[0]
	if got.Kind != session.KindBackground {
		t.Errorf("kind: got %q, want %q (record says bg)", got.Kind, session.KindBackground)
	}
	if got.Tmux != "" {
		t.Errorf("tmux: got %q, want none: the launch location is not where a background session lives", got.Tmux)
	}
	cols := strings.Split(got.Row(), "\t")
	if len(cols) != 8 || cols[5] != "-" || cols[7] != session.KindBackground {
		t.Errorf("row: got %q, want eight columns with tmux %q and kind %q", got.Row(), "-", session.KindBackground)
	}
}
