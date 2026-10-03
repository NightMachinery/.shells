package main

import (
	"bufio"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"
)

// The streaming API's integration tests; see integration_test.go for the
// variables that point them at a garden. They pass brishz_stream=y
// themselves, so they need no BRISHZGO_IT_STREAM.

// itStarted is a client started in the background, with its stdout piped.
type itStarted struct {
	cmd *exec.Cmd
	out *bufio.Reader
	t0  time.Time
}

// start starts the client with brishz_stream=y and the signals' default
// handling, whatever this test inherited (a background job may have
// SIGINT ignored, which exec keeps).
func (e itEnv) start(t *testing.T, args []string, kv ...string) itStarted {
	t.Helper()
	dfl := []string{"perl", "-e", `$SIG{$_} = "DEFAULT" for qw(INT TERM HUP); exec @ARGV or die`, e.bin}
	cmd := exec.Command(dfl[0], append(dfl[1:], args...)...)
	cmd.Dir = t.TempDir()
	cmd.Env = []string{"PATH=" + os.Getenv("PATH"), "HOME=" + e.home, "PWD=" + cmd.Dir, "bshEndpoint=" + e.endpoint, "brishz_stream=y"}
	for i := 0; i+1 < len(kv); i += 2 {
		cmd.Env = append(cmd.Env, kv[i]+"="+kv[i+1])
	}
	cmd.Stderr = io.Discard
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatal(err)
	}
	s := itStarted{cmd, bufio.NewReader(stdout), time.Now()}
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	return s
}

// firstByte waits for the client's first byte of output, and returns when
// it came, since the start.
func (s itStarted) firstByte(t *testing.T) time.Duration {
	t.Helper()
	if _, err := s.out.ReadByte(); err != nil {
		t.Fatalf("no output: %v", err)
	}
	return time.Since(s.t0)
}

// TestITStreamProgressive: output arrives while the command runs, on a
// garden with the streaming API; elsewhere the client falls back, and it
// arrives at the end.
func TestITStreamProgressive(t *testing.T) {
	e := integration(t)
	s := e.start(t, []string{"eval", "print -r -- one; sleep 2; print -r -- two"})
	first := s.firstByte(t)
	rest, _ := io.ReadAll(s.out)
	err := s.cmd.Wait()
	total := time.Since(s.t0)
	t.Logf("first byte after %v, done after %v", first, total)
	if err != nil || !strings.HasSuffix(string(rest), "two\n") {
		t.Fatalf("%v, rest %q", err, rest)
	}
	if early := total-first > 1500*time.Millisecond; early != e.streamP() {
		t.Errorf("first byte %v before the end; streaming API: %v", total-first, e.streamP())
	}
}

// TestITStreamRetcodes: the exit status is the retcode's low 8 bits, on
// every garden, and stdout and stderr stay apart.
func TestITStreamRetcodes(t *testing.T) {
	e := integration(t)
	for cmd, want := range map[string]int{"return 300": 44, "return 3": 3, "true": 0} {
		got := e.run(t, nil, []string{cmd}, "brishz_stream", "y", "brishz_noquote", "y")
		if got.code != want {
			t.Errorf("%s: exit %d, want %d, err %q", cmd, got.code, want, got.errOut)
		}
	}
	got := e.run(t, nil, []string{"eval", "print -rn -- out; print -rn -- err >&2; (exit 3)"}, "brishz_stream", "y")
	if got.code != 3 || string(got.out) != "out" || string(got.errOut) != "err" {
		t.Errorf("exit %d, out %q, err %q", got.code, got.out, got.errOut)
	}
}

// TestITStreamInterrupt: SIGINT to the client mid stream closes the
// connection, the garden kills the command, and the client dies of the
// signal. The same on the raw API leaves the command running to its end;
// that is the difference the streaming API makes.
func TestITStreamInterrupt(t *testing.T) {
	e := integration(t)
	if !e.streamP() {
		t.Skip("no streaming API")
	}
	for _, c := range []struct {
		api      string
		kv       []string
		sentinel bool
	}{
		{"streaming", nil, false},
		{"raw", []string{"brishz_stream", "n"}, true},
	} {
		sentinel := filepath.Join(t.TempDir(), "sentinel")
		cmd := "print -r -- started; sleep 5; print -r -- ran >> " + quoteSingle(sentinel)
		s := e.start(t, []string{"eval", cmd}, c.kv...)
		if c.api == "streaming" {
			s.firstByte(t)
		} else {
			// The raw API's output only comes at the end.
			time.Sleep(time.Second)
		}
		s.cmd.Process.Signal(syscall.SIGINT)
		s.cmd.Wait()
		ws := s.cmd.ProcessState.Sys().(syscall.WaitStatus)
		if !(ws.Signaled() && ws.Signal() == syscall.SIGINT) && ws.ExitStatus() != 130 {
			t.Errorf("%s: the client did not die of SIGINT: %v", c.api, s.cmd.ProcessState)
		}
		time.Sleep(6 * time.Second)
		if _, err := os.Stat(sentinel); (err == nil) != c.sentinel {
			t.Errorf("%s: sentinel written: %v, want %v", c.api, err == nil, c.sentinel)
		}
	}
	// The worker the kill freed runs the next command.
	got := e.run(t, nil, []string{"print", "-rn", "--", "next"}, "brishz_stream", "y")
	if got.code != 0 || string(got.out) != "next" {
		t.Errorf("after the kill: exit %d, out %q, err %q", got.code, got.out, got.errOut)
	}
}

// TestITStreamFallback: a garden without the streaming API answers 404, and
// the client falls back to the raw API, or with none to the JSON API, with
// all of stdin.
func TestITStreamFallback(t *testing.T) {
	e := integration(t)
	got := e.run(t, []byte("in\x00put"), []string{"od", "-An", "-c"}, "brishz_in", "MAGIC_READ_STDIN", "brishz_stream", "y", "brishz_debug", "y")
	errOut := string(got.errOut)
	toRaw := strings.Contains(errOut, "no streaming API (HTTP 404); falling back to the raw API")
	if toRaw == e.streamP() || got.code != 0 || !strings.Contains(string(got.out), `i   n  \0   p   u   t`) {
		t.Errorf("fell back %v, exit %d, out %q\n%s", toRaw, got.code, got.out, errOut)
	}
	if strings.Contains(string(got.out), "<redacted>") || !strings.Contains(errOut, "<redacted>") {
		t.Errorf("the key header was not redacted in the debug output")
	}
}
