//go:build darwin || linux

package main

import (
	"bytes"
	"strings"
	"testing"

	"github.com/creack/pty"
)

// requirePTY skips the test on machines (some CI sandboxes) where no pty can
// be allocated at all.
func requirePTY(t *testing.T) {
	t.Helper()
	m, s, err := pty.Open()
	if err != nil {
		t.Skipf("no pty available: %v", err)
	}
	m.Close()
	s.Close()
}

func runSh(t *testing.T, script string, opts runOpts) (int, string) {
	t.Helper()
	var out bytes.Buffer
	code, err := run([]string{"sh", "-c", script}, nil, nil, &out, opts)
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	return code, out.String()
}

func TestRunRewritesChildOutput(t *testing.T) {
	requirePTY(t)
	code, out := runSh(t, `printf '\033[?1003h\033[?1003$p'`, runOpts{maps: mouseMap})
	if code != 0 {
		t.Fatalf("exit code %d, want 0", code)
	}
	if !strings.Contains(out, e+"[?1002h") {
		t.Errorf("output %q does not contain the rewritten DECSET", out)
	}
	if !strings.Contains(out, e+"[?1003$p") {
		t.Errorf("output %q does not contain the untouched DECRQM", out)
	}
}

func TestRunWithoutMapsIsPassThrough(t *testing.T) {
	requirePTY(t)
	code, out := runSh(t, `printf '\033[?1003h'`, runOpts{})
	if code != 0 {
		t.Fatalf("exit code %d, want 0", code)
	}
	if !strings.Contains(out, e+"[?1003h") {
		t.Errorf("output %q does not contain the original DECSET", out)
	}
}

func TestRunReportsExitStatus(t *testing.T) {
	requirePTY(t)
	code, _ := runSh(t, `exit 3`, runOpts{})
	if code != 3 {
		t.Fatalf("exit code %d, want 3", code)
	}
}

func TestRunReportsSignalDeath(t *testing.T) {
	requirePTY(t)
	code, _ := runSh(t, `kill -TERM $$`, runOpts{})
	if code != 143 {
		t.Fatalf("exit code %d, want 143 (128+SIGTERM)", code)
	}
}

func TestRunSetsWindowSizeBeforeStart(t *testing.T) {
	requirePTY(t)
	code, out := runSh(t, `stty size`, runOpts{winsize: &pty.Winsize{Rows: 24, Cols: 100}})
	if code != 0 {
		t.Fatalf("exit code %d, want 0", code)
	}
	if !strings.Contains(out, "24 100") {
		t.Fatalf("stty size reported %q, want 24 100", out)
	}
}

func TestRunMissingCommand(t *testing.T) {
	var out bytes.Buffer
	code, err := run([]string{"decset-rewrite-no-such-command"}, nil, nil, &out, runOpts{})
	if code != 127 || err == nil {
		t.Fatalf("got (%d, %v), want (127, error)", code, err)
	}
}
