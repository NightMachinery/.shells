package main

import (
	"bytes"
	"crypto/rand"
	"fmt"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"unicode/utf8"
)

// The integration tests run the built binary against a real garden. They
// are skipped unless pointed at one:
//
//	BRISHZGO_IT_ENDPOINT  the garden, such as http://127.0.0.1:7292
//	BRISHZGO_IT_KIND      binary, legacy (BRISH_BINARY=0), old-binary or
//	                      old-legacy (a garden without the raw API, started
//	                      with and without BRISH_BINARY=1)
//	BRISHZGO_IT_HOME      a HOME whose .keys/brishgarden is the garden's key
//
// Every command is inert: cat, print, true, typeset.

type itEnv struct {
	endpoint, kind, home, bin string
}

var (
	buildOnce sync.Once
	builtBin  string
	buildErr  error
)

func TestMain(m *testing.M) {
	code := m.Run()
	if builtBin != "" {
		os.RemoveAll(filepath.Dir(builtBin))
	}
	os.Exit(code)
}

func integration(t *testing.T) itEnv {
	t.Helper()
	e := itEnv{os.Getenv("BRISHZGO_IT_ENDPOINT"), os.Getenv("BRISHZGO_IT_KIND"), os.Getenv("BRISHZGO_IT_HOME"), ""}
	if e.endpoint == "" || e.kind == "" || e.home == "" {
		t.Skip("BRISHZGO_IT_ENDPOINT, BRISHZGO_IT_KIND and BRISHZGO_IT_HOME are not all set")
	}
	e.bin = builtBinary(t)
	return e
}

// builtBinary builds brishzgo once per test run, into a temp dir that
// TestMain removes.
func builtBinary(t *testing.T) string {
	t.Helper()
	buildOnce.Do(func() {
		dir, err := os.MkdirTemp("", "brishzgo-it")
		if err != nil {
			buildErr = err
			return
		}
		builtBin = filepath.Join(dir, "brishzgo")
		out, err := exec.Command("go", "build", "-o", builtBin, ".").CombinedOutput()
		if err != nil {
			buildErr = fmt.Errorf("%v: %s", err, out)
		}
	})
	if buildErr != nil {
		t.Fatal(buildErr)
	}
	return builtBin
}

func (e itEnv) binaryP() bool { return e.kind == "binary" || e.kind == "old-binary" }
func (e itEnv) oldP() bool    { return strings.HasPrefix(e.kind, "old") }

type itResult struct {
	code        int
	out, errOut []byte
}

// run runs the client with stdin piped in, and the given env on top of a
// clean one (so no brishz_* or NIGHT_EMACS_P from the caller leaks in).
func (e itEnv) run(t *testing.T, stdin []byte, args []string, kv ...string) itResult {
	t.Helper()
	cmd := exec.Command(e.bin, args...)
	cmd.Dir = t.TempDir()
	cmd.Env = []string{"PATH=" + os.Getenv("PATH"), "HOME=" + e.home, "PWD=" + cmd.Dir, "bshEndpoint=" + e.endpoint}
	for i := 0; i+1 < len(kv); i += 2 {
		cmd.Env = append(cmd.Env, kv[i]+"="+kv[i+1])
	}
	cmd.Stdin = bytes.NewReader(stdin)
	var out, errb bytes.Buffer
	cmd.Stdout, cmd.Stderr = &out, &errb
	err := cmd.Run()
	code := 0
	if ee, ok := err.(*exec.ExitError); ok {
		code = ee.ExitCode()
	} else if err != nil {
		t.Fatal(err)
	}
	return itResult{code, out.Bytes(), errb.Bytes()}
}

func allBytes() []byte {
	b := make([]byte, 256)
	for i := range b {
		b[i] = byte(i)
	}
	return b
}

func TestITRoundTrips(t *testing.T) {
	e := integration(t)
	cases := map[string][]byte{
		"all 256 bytes":     allBytes(),
		"NUL":               {0},
		"NULs":              []byte("a\x00b\x00"),
		"CR":                []byte("\r"),
		"CRLF":              []byte("a\r\n"),
		"trailing newlines": []byte("x\n\n\n"),
		"newline":           []byte("\n"),
		"empty":             {},
		"text":              []byte("héllo wörld\n"),
	}
	for name, data := range cases {
		// Legacy mode loses NUL (refused), CR and invalid UTF-8. A garden
		// without the raw API gets the JSON API's text reply, which loses
		// invalid UTF-8 even in binary mode (brishz_binary=y keeps it).
		hasNUL := bytes.IndexByte(data, 0) >= 0 || bytes.IndexByte(data, '\r') >= 0
		invalid := !utf8.Valid(data)
		lossy := false
		switch e.kind {
		case "legacy", "old-legacy":
			lossy = hasNUL || invalid
		case "old-binary":
			lossy = invalid
		}
		for _, stream := range []string{"stdout", "stderr"} {
			args := []string{"cat"}
			if stream == "stderr" {
				args = []string{"eval", "cat >&2"}
			}
			got := e.run(t, data, args, "brishz_in", "MAGIC_READ_STDIN")
			gotData := got.out
			if stream == "stderr" {
				gotData = got.errOut
			}
			if lossy {
				// Refused with nothing run (9000, which exits as 40), or
				// passed through text.
				t.Logf("%s via %s (%s, lossy): exit %d, %d bytes", name, stream, e.kind, got.code, len(gotData))
				continue
			}
			if got.code != 0 || !bytes.Equal(gotData, data) {
				t.Errorf("%s via %s: exit %d, got %q", name, stream, got.code, gotData)
			}
		}
	}
}

func TestITLegacyRefusesNUL(t *testing.T) {
	e := integration(t)
	if e.kind != "legacy" {
		t.Skip("legacy only")
	}
	got := e.run(t, []byte("a\x00b"), []string{"cat"}, "brishz_in", "MAGIC_READ_STDIN")
	if got.code != 9000%256 || len(got.out) != 0 {
		t.Errorf("exit %d, out %q, err %q", got.code, got.out, got.errOut)
	}
}

func TestITMiB(t *testing.T) {
	e := integration(t)
	data := make([]byte, 1<<20)
	rand.Read(data)
	var kv []string
	switch e.kind {
	case "binary":
	case "old-binary":
		kv = []string{"brishz_binary", "y"}
	default:
		t.Skip("random bytes need binary mode")
	}
	got := e.run(t, data, []string{"cat"}, append(kv, "brishz_in", "MAGIC_READ_STDIN")...)
	if got.code != 0 || !bytes.Equal(got.out, data) {
		t.Errorf("1 MiB: exit %d, %d bytes back, equal %v", got.code, len(got.out), bytes.Equal(got.out, data))
	}
}

func TestITStreamsAndRetcodes(t *testing.T) {
	e := integration(t)
	got := e.run(t, nil, []string{"eval", "print -rn -- out; print -rn -- err >&2; (exit 3)"})
	if got.code != 3 || string(got.out) != "out" || string(got.errOut) != "err" {
		t.Errorf("exit %d, out %q, err %q", got.code, got.out, got.errOut)
	}
	for _, rc := range []int{0, 1, 42, 255} {
		got := e.run(t, nil, []string{"eval", fmt.Sprintf("(exit %d)", rc)})
		if got.code != rc {
			t.Errorf("(exit %d): exit %d, err %q", rc, got.code, got.errOut)
		}
	}
	// A literal brishz_in, and the arguments quoted.
	got = e.run(t, nil, []string{"print", "-rn", "--", "it's", "$HOME", "a b"}, "brishz_in", "ignored")
	if got.code != 0 || string(got.out) != "it's $HOME a b" {
		t.Errorf("quoting: exit %d, out %q", got.code, got.out)
	}
	got = e.run(t, nil, []string{"cat"}, "brishz_in", "lit\n")
	if got.code != 0 || string(got.out) != "lit\n" {
		t.Errorf("literal stdin: exit %d, out %q", got.code, got.out)
	}
	// The command runs in our working directory.
	got = e.run(t, nil, []string{"eval", "print -rn -- $PWD"})
	if got.code != 0 || !strings.Contains(string(got.out), "/") {
		t.Errorf("pwd: exit %d, out %q", got.code, got.out)
	}
}

// TestITSession: state persists within a session. Through localhost, since
// brishzq.zsh's wrapper for 127.0.0.1 runs each command in a subshell.
func TestITSession(t *testing.T) {
	e := integration(t)
	ep := strings.Replace(e.endpoint, "127.0.0.1", "localhost", 1)
	session := fmt.Sprintf("bzg-it-%d", os.Getpid())
	got := e.run(t, nil, []string{"typeset", "-g", "bzg_it_v=42"}, "bshEndpoint", ep, "brishz_session", session)
	if got.code != 0 {
		t.Fatalf("set: exit %d, %q", got.code, got.errOut)
	}
	got = e.run(t, nil, []string{"eval", "print -rn -- ${bzg_it_v:-unset}"}, "bshEndpoint", ep, "brishz_session", session)
	if string(got.out) != "42" {
		t.Errorf("same session: %q", got.out)
	}
	got = e.run(t, nil, []string{"eval", "print -rn -- ${bzg_it_v:-unset}"}, "bshEndpoint", ep)
	if string(got.out) != "unset" {
		t.Errorf("shared pool sees the session's variable: %q", got.out)
	}
}

func TestITNotice(t *testing.T) {
	e := integration(t)
	got := e.run(t, nil, nil, "brishz_noquote", "y")
	if got.code != 200 || len(got.out) == 0 || got.out[len(got.out)-1] != '\n' {
		t.Errorf("empty command: exit %d, out %q", got.code, got.out)
	}
	t.Logf("notice: %q", got.out)
}

func TestITBinaryOptIn(t *testing.T) {
	e := integration(t)
	data := allBytes()
	got := e.run(t, data, []string{"cat"}, "brishz_in", "MAGIC_READ_STDIN", "brishz_binary", "y")
	if e.binaryP() {
		if got.code != 0 || !bytes.Equal(got.out, data) {
			t.Errorf("exit %d, %q", got.code, got.out)
		}
		return
	}
	if got.code != 201 || len(got.out) != 0 || !strings.Contains(string(got.errOut), "lacks binary support") {
		t.Errorf("exit %d, out %q, err %q", got.code, got.out, got.errOut)
	}
}

func TestITFallback(t *testing.T) {
	e := integration(t)
	got := e.run(t, []byte("in"), []string{"cat"}, "brishz_in", "MAGIC_READ_STDIN", "brishz_debug", "y")
	fellBack := strings.Contains(string(got.errOut), "falling back to the JSON API")
	if fellBack != e.oldP() {
		t.Errorf("fell back: %v, kind %s", fellBack, e.kind)
	}
	if e.oldP() && !strings.Contains(string(got.errOut), "stdin: 2 bytes, 0 of them already read") {
		t.Errorf("the raw request consumed stdin before the 404:\n%s", got.errOut)
	}
	// Debug output must not be in stdout, and must not carry the key.
	if string(got.out) != "in" {
		t.Errorf("out %q", got.out)
	}
	if !strings.Contains(string(got.errOut), "<redacted>") {
		t.Errorf("no redacted key header in debug output")
	}
	got = e.run(t, nil, []string{"true"}, "brishz_raw", "n", "brishz_debug", "y")
	if got.code != 0 || strings.Contains(string(got.errOut), "/zsh/raw/") {
		t.Errorf("brishz_raw=n: exit %d, %s", got.code, got.errOut)
	}
}

func TestITFailures(t *testing.T) {
	e := integration(t)
	l, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	refused := "http://" + l.Addr().String()
	l.Close()
	if got := e.run(t, nil, []string{"true"}, "bshEndpoint", refused); got.code != 7 {
		t.Errorf("refused: exit %d", got.code)
	}
	// No key: the garden refuses with 401 or 403, and nothing runs.
	got := e.run(t, nil, []string{"true"}, "HOME", t.TempDir())
	if got.code != 22 || len(got.out) != 0 {
		t.Errorf("no key: exit %d, out %q", got.code, got.out)
	}
}
