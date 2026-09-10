package claude

import (
	"os"
	"path/filepath"
	"strconv"
	"testing"

	"agent_session/internal/proc"
)

// The record's tmux name is what the session launched in, and the autoname
// hooks rename tmux sessions on every prompt, so it goes stale. The process
// tree has to win over it; the record is only the fallback.
func TestTmuxOfPrefersTheProcessTree(t *testing.T) {
	home := t.TempDir()
	writeRecord(t, home, 300, "+Claude/default fftmux-agent-old:@1.%2")
	writeRecord(t, home, 500, "detached-at-launch:@3.%4")

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
	if got := tmuxOf(home, 300, byPID, panes); got != "+Claude/default fftmux-agent-now" {
		t.Errorf("under tmux: got %q, want the pane's current session name", got)
	}
	// 500 is not under any pane: the record is all there is.
	if got := tmuxOf(home, 500, byPID, panes); got != "detached-at-launch" {
		t.Errorf("outside tmux: got %q, want the recorded name", got)
	}
	// A pid the table never had, with no record either.
	if got := tmuxOf(home, 999, byPID, panes); got != "" {
		t.Errorf("unknown pid: got %q, want empty", got)
	}
}

// With tmux not running there are no panes, and every row falls back to its
// record rather than losing its tmux column.
func TestTmuxOfWithoutTmux(t *testing.T) {
	home := t.TempDir()
	writeRecord(t, home, 300, "some-session:@1.%2")

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
	if got := tmuxOf(home, 300, proc.ByPID(procs), proc.PanesShared()); got != "some-session" {
		t.Errorf("got %q, want the recorded name", got)
	}
}

func writeRecord(t *testing.T, home string, pid int, tmux string) {
	t.Helper()
	dir := filepath.Join(home, "sessions")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	body := `{"tmux":"` + tmux + `"}`
	if err := os.WriteFile(filepath.Join(dir, strconv.Itoa(pid)+".json"), []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}
}
