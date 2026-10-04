package main

import (
	"bytes"
	"crypto/rand"
	"crypto/sha256"
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
//	BRISHZGO_IT_ENDPOINT  the garden, such as http://127.0.0.1:7288
//	BRISHZGO_IT_KIND      what the garden is:
//	                      binary           the raw and streaming APIs, in
//	                                       binary mode
//	                      legacy           the same with BRISH_BINARY=0
//	                      preopt-binary    the raw and streaming APIs, but
//	                                       older than their binary=1 option
//	                                       (such as 7de4a42), binary mode
//	                      preopt-legacy    the same with BRISH_BINARY=0
//	                      nostream-binary  the raw API but no streaming API
//	                                       (such as 0fd2752), binary mode
//	                      nostream-legacy  the same with BRISH_BINARY=0
//	                      old-binary       no raw API (such as 42ddc9d),
//	                                       binary mode
//	                      old-legacy       the same with BRISH_BINARY=0
//	                      pre-binary       no binary mode at all (such as
//	                                       cc390bc)
//	BRISHZGO_IT_HOME      a HOME whose .keys/brishgarden is the garden's key
//	BRISHZGO_IT_STREAM    y to run every test with brishz_stream=y, which
//	                      falls back on a garden without the streaming API
//
// Every command is inert: cat, print, true, typeset, od, shasum, sleep, and
// appending to or touching a file in a test's temp dir.

type itEnv struct {
	endpoint, kind, home, bin string
	stream                    bool
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
	e := itEnv{os.Getenv("BRISHZGO_IT_ENDPOINT"), os.Getenv("BRISHZGO_IT_KIND"), os.Getenv("BRISHZGO_IT_HOME"), "",
		boolP(os.Getenv("BRISHZGO_IT_STREAM"))}
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

func (e itEnv) binaryP() bool { return strings.HasSuffix(e.kind, "binary") && e.kind != "pre-binary" }

// oldP: a garden without the raw API.
func (e itEnv) oldP() bool { return strings.HasPrefix(e.kind, "old") || e.kind == "pre-binary" }

// streamP: a garden with the streaming API.
func (e itEnv) streamP() bool {
	return e.kind == "binary" || e.kind == "legacy" || strings.HasPrefix(e.kind, "preopt-")
}

// legacyModeP: a garden whose workers use brish's legacy transport.
func (e itEnv) legacyModeP() bool {
	return strings.HasSuffix(e.kind, "legacy") || e.kind == "pre-binary"
}

// exactAPIP: the client runs commands through an API that carries any
// bytes: the raw or streaming API of a binary-mode garden.
func (e itEnv) exactAPIP() bool {
	return e.kind == "binary" || e.kind == "nostream-binary" || e.kind == "preopt-binary"
}

// textModeP: a legacy-mode garden whose raw API predates binary=1, so it
// ignores that option and runs a brishz_binary=y command in text mode
// (unless brishz_stream=y finds no streaming API there).
func (e itEnv) textModeP() bool { return e.kind == "nostream-legacy" || e.kind == "preopt-legacy" }

// exactP: whether output bytes come back exact. Only the raw and streaming
// APIs of a binary-mode garden carry them all; elsewhere, either the
// garden's text or the JSON API's text reply escapes invalid UTF-8.
func (e itEnv) exactP(data []byte) bool { return e.exactAPIP() || utf8.Valid(data) }

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
	if e.stream {
		cmd.Env = append(cmd.Env, "brishz_stream=y")
	} else {
		cmd.Env = append(cmd.Env, "brishz_stream=n")
	}
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

// itData are the inputs of the round trips.
func itData() map[string][]byte {
	return map[string][]byte{
		"all 256 bytes":     allBytes(),
		"NUL":               {0},
		"NULs":              []byte("a\x00b\x00"),
		"CR":                []byte("\r"),
		"CRLF":              []byte("a\r\n"),
		"trailing newlines": []byte("x\n\n\n"),
		"newline":           []byte("\n"),
		"empty":             {},
		"text":              []byte("héllo wörld\n"),
		"invalid UTF-8":     []byte("\xff\xfe\n"),
	}
}

// TestITRoundTrips: stdin through cat to stdout or stderr. Every garden
// runs it; the output is exact where the garden can carry it (exactP).
//
// Output with a NUL is not sent to a legacy-mode garden: brish's legacy
// transport ends each reply with "\0\n", so a stdout of "\0" fails with
// status 9000, and a stderr that ends in a NUL leaves the worker's stderr
// one reply behind for every later command, whichever client sent it.
func TestITRoundTrips(t *testing.T) {
	e := integration(t)
	for name, data := range itData() {
		if e.legacyModeP() && bytes.IndexByte(data, 0) >= 0 {
			t.Logf("%s: not sent to a legacy-mode garden, which cannot carry a NUL in output", name)
			continue
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
			if got.code != 0 {
				t.Errorf("%s via %s: exit %d, err %q", name, stream, got.code, got.errOut)
				continue
			}
			if !e.exactP(data) {
				t.Logf("%s via %s (%s): %q", name, stream, e.kind, gotData)
				continue
			}
			if !bytes.Equal(gotData, data) {
				t.Errorf("%s via %s: got %q", name, stream, gotData)
			}
		}
	}
}

// TestITStdinExact: stdin reaches the command exact on every garden, NUL
// and invalid UTF-8 included: through the raw API, or through the JSON
// API's temp file after a 404 or a legacy-mode refusal. shasum's output is
// ASCII, so it comes back exact everywhere.
func TestITStdinExact(t *testing.T) {
	e := integration(t)
	data := itData()
	mib := make([]byte, 1<<20)
	rand.Read(mib)
	data["1 MiB of random bytes"] = mib
	for name, d := range data {
		got := e.run(t, d, []string{"shasum", "-a", "256"}, "brishz_in", "MAGIC_READ_STDIN")
		want := fmt.Sprintf("%x  -\n", sha256.Sum256(d))
		if got.code != 0 || string(got.out) != want {
			t.Errorf("%s: exit %d, got %q, want %q, err %q", name, got.code, got.out, want, got.errOut)
		}
	}
}

// TestITCommandBytes: a command that is not valid UTF-8 runs everywhere:
// exact on the raw API of a binary-mode garden, else with each invalid
// sequence as U+FFFD, as brishzq.zsh's JSON request has it.
func TestITCommandBytes(t *testing.T) {
	e := integration(t)
	got := e.run(t, nil, []string{"print", "-rn", "--", "\xff\xfe"})
	want := "\xff\xfe"
	if !e.exactAPIP() {
		want = "\ufffd\ufffd"
	}
	if got.code != 0 || string(got.out) != want {
		t.Errorf("exit %d, got %q, want %q, err %q", got.code, got.out, want, got.errOut)
	}
}

func TestITMiB(t *testing.T) {
	e := integration(t)
	data := make([]byte, 1<<20)
	rand.Read(data)
	if !e.binaryP() {
		t.Skip("random bytes need binary mode")
	}
	// Without the opt-in only the raw and streaming APIs carry them.
	kvs := [][]string{{"brishz_binary", "y"}}
	if e.exactAPIP() {
		kvs = append(kvs, nil)
	}
	for _, kv := range kvs {
		got := e.run(t, data, []string{"cat"}, append(kv, "brishz_in", "MAGIC_READ_STDIN")...)
		if got.code != 0 || !bytes.Equal(got.out, data) {
			t.Errorf("1 MiB %q: exit %d, %d bytes back, equal %v", kv, got.code, len(got.out), bytes.Equal(got.out, data))
		}
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
	// A first word of ! negates.
	for cmd, rc := range map[string]int{"false": 0, "true": 1} {
		if got := e.run(t, nil, []string{"!", cmd}); got.code != rc {
			t.Errorf("! %s: exit %d, err %q", cmd, got.code, got.errOut)
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

// TestITBinaryOptIn: brishz_binary=y goes to the raw API (or with
// brishz_stream=y the streaming API) with binary=1, and to the JSON API's
// binary transport after a 404 or a refusal. A binary-mode garden gives the
// exact bytes; a legacy-mode garden that knows binary=1 and a garden older
// than binary mode run nothing (a sentinel file stays absent) and exit 201;
// a legacy-mode garden older than binary=1 runs the command in text mode,
// and the client withholds its output and exits 201.
func TestITBinaryOptIn(t *testing.T) {
	e := integration(t)
	data := allBytes()
	got := e.run(t, data, []string{"cat"}, "brishz_in", "MAGIC_READ_STDIN", "brishz_binary", "y", "brishz_debug", "y")
	errOut := string(got.errOut)
	api := "raw"
	if e.stream {
		api = "stream"
	}
	if !strings.Contains(errOut, "/zsh/"+api+"/?binary=1") {
		t.Errorf("no binary=1 request to the %s API:\n%s", api, errOut)
	}
	// After a 404 the JSON API, never the raw API after the streaming one.
	// textRun: the API asked for exists, and ignores binary=1.
	hasAPI := !e.oldP() && (!e.stream || e.streamP())
	textRun := hasAPI && e.textModeP()
	if fellBack := strings.Contains(errOut, "(HTTP 404); falling back to the JSON API"); fellBack == hasAPI {
		t.Errorf("fell back after a 404: %v, kind %s, stream %v:\n%s", fellBack, e.kind, e.stream, errOut)
	}
	// The bytes hold a NUL, which a legacy-mode garden with the raw API
	// refuses even when it predates binary=1; the JSON API then refuses
	// binary: 1.
	if refused := strings.Contains(errOut, "request was refused"); refused != (e.kind == "legacy" || textRun) {
		t.Errorf("refused: %v, kind %s:\n%s", refused, e.kind, errOut)
	}
	if e.binaryP() {
		if got.code != 0 || !bytes.Equal(got.out, data) {
			t.Errorf("exit %d, %q", got.code, got.out)
		}
	} else if got.code != 201 || len(got.out) != 0 || !strings.Contains(errOut, "lacks binary support") {
		t.Errorf("exit %d, out %q, err %q", got.code, got.out, got.errOut)
	}

	// Whether the command ran, and its status and stderr: a legacy-mode
	// garden older than binary=1 runs a command in valid UTF-8.
	sentinel := filepath.Join(t.TempDir(), "ran")
	cmd := "touch " + quoteSingle(sentinel) + "; print -rn -- out; print -rn -- err >&2; return 300"
	got = e.run(t, nil, []string{cmd}, "brishz_binary", "y", "brishz_noquote", "y", "brishz_failure_expected", "y")
	_, err := os.Stat(sentinel)
	if ran := err == nil; ran != (e.binaryP() || textRun) {
		t.Errorf("the command ran: %v, kind %s", ran, e.kind)
	}
	switch {
	case e.binaryP():
		if got.code != 44 || string(got.out) != "out" || string(got.errOut) != "err" {
			t.Errorf("return 300: exit %d, out %q, err %q", got.code, got.out, got.errOut)
		}
	case textRun:
		if got.code != 201 || len(got.out) != 0 || !strings.HasSuffix(string(got.errOut), "; retcode 300\n") {
			t.Errorf("return 300: exit %d, out %q, err %q", got.code, got.out, got.errOut)
		}
	default:
		if got.code != 201 || len(got.out) != 0 {
			t.Errorf("return 300: exit %d, out %q, err %q", got.code, got.out, got.errOut)
		}
	}
}

func TestITFallback(t *testing.T) {
	e := integration(t)
	got := e.run(t, []byte("in"), []string{"cat"}, "brishz_in", "MAGIC_READ_STDIN", "brishz_debug", "y")
	fellBack := strings.Contains(string(got.errOut), "falling back to the JSON API")
	if fellBack != e.oldP() {
		t.Errorf("fell back: %v, kind %s", fellBack, e.kind)
	}
	toRaw := strings.Contains(string(got.errOut), "no streaming API (HTTP 404); falling back to the raw API")
	if toRaw != (e.stream && !e.streamP()) {
		t.Errorf("fell back from the streaming API: %v, kind %s, stream %v", toRaw, e.kind, e.stream)
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

	// A legacy-mode garden refuses a NUL, with X-Brish-Refused: 1 and
	// nothing run; the JSON API then runs it, with all of stdin.
	got = e.run(t, []byte("a\x00b"), []string{"od", "-An", "-c"}, "brishz_in", "MAGIC_READ_STDIN", "brishz_debug", "y")
	refused := strings.Contains(string(got.errOut), "request was refused")
	if refused != (e.legacyModeP() && !e.oldP()) || got.code != 0 || !strings.Contains(string(got.out), `a  \0   b`) {
		t.Errorf("NUL stdin: refused %v, exit %d, out %q\n%s", refused, got.code, got.out, got.errOut)
	}
	if refused && !strings.Contains(string(got.errOut), "stdin: 3 bytes, 3 of them already read") {
		t.Errorf("the fallback did not resend what the raw request read:\n%s", got.errOut)
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
