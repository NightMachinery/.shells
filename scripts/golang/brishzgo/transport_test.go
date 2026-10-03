package main

import (
	"bytes"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"sync"
	"testing"
)

// fakeGarden is an httptest server standing in for a garden. It records
// each request, and answers with handler.
type fakeGarden struct {
	*httptest.Server
	mu   sync.Mutex
	reqs []recorded
}

type recorded struct {
	path, query string
	header      http.Header
	body        []byte
}

func newFakeGarden(t *testing.T, handler func(w http.ResponseWriter, r *http.Request, body []byte)) *fakeGarden {
	return newFakeGardenLazy(t, func(w http.ResponseWriter, r *http.Request, body func() []byte) {
		b := body()
		if handler != nil {
			handler(w, r, b)
		}
	})
}

// newFakeGardenLazy is newFakeGarden where the handler reads the body only
// if it wants to, as a garden without the raw API does not.
func newFakeGardenLazy(t *testing.T, handler func(w http.ResponseWriter, r *http.Request, body func() []byte)) *fakeGarden {
	g := &fakeGarden{}
	g.Server = httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		g.mu.Lock()
		g.reqs = append(g.reqs, recorded{r.URL.Path, r.URL.RawQuery, r.Header.Clone(), nil})
		i := len(g.reqs) - 1
		g.mu.Unlock()
		handler(w, r, func() []byte {
			b, _ := io.ReadAll(r.Body)
			g.mu.Lock()
			g.reqs[i].body = b
			g.mu.Unlock()
			return b
		})
	}))
	t.Cleanup(g.Close)
	return g
}

// rawReply writes a raw API reply.
func rawReply(w http.ResponseWriter, out, errOut string, retcode int, binary string) {
	w.Header().Set("X-Brish-Retcode", strconv.Itoa(retcode))
	w.Header().Set("X-Brish-Out-Length", strconv.Itoa(len(out)))
	if binary != "" {
		w.Header().Set("X-Brish-Binary", binary)
	}
	io.WriteString(w, out+errOut)
}

type result struct {
	code        int
	out, errOut string
}

func runWith(t *testing.T, g *fakeGarden, stdin string, args []string, kv ...string) result {
	t.Helper()
	env := envOf(append([]string{"bshEndpoint", g.URL}, kv...)...)
	var out, errb bytes.Buffer
	code := run(args, env, "/synthetic/pwd", t.TempDir(), strings.NewReader(stdin), &out, &errb)
	return result{code, out.String(), errb.String()}
}

func TestRawRoundTrip(t *testing.T) {
	g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
		n, _ := strconv.Atoi(r.Header.Get("X-Brish-Cmd-Length"))
		rawReply(w, "out:"+string(body[n:]), "err\x00\xff", 7, "1")
	})
	got := runWith(t, g, "\x00a\r\n\n", []string{"cat"}, "brishz_in", "MAGIC_READ_STDIN", "brishz_session", "s 1", "brishz_nolog", "y", "brishz_failure_expected", "1")
	if got.code != 7 || got.out != "out:\x00a\r\n\n" || got.errOut != "err\x00\xff" {
		t.Errorf("got %+v", got)
	}
	r := g.reqs[0]
	if r.path != "/zsh/raw/nolog/" || r.query != "failure_expected=1&nolog=1&session=s+1" {
		t.Errorf("request %s?%s", r.path, r.query)
	}
	// Any non-empty value is true, as on the JSON API, whose request
	// carries the value as it is.
	runWith(t, g, "", []string{"true"}, "brishz_nolog", "n", "brishz_failure_expected", "0")
	if r := g.reqs[1]; r.path != "/zsh/raw/nolog/" || r.query != "failure_expected=1&nolog=1" {
		t.Errorf("n and 0: request %s?%s", r.path, r.query)
	}
	g.reqs = nil
	runWith(t, g, "", []string{"true"}, "brishz_nolog", "n", "brishz_failure_expected", "0", "brishz_raw", "n")
	if f := requestField(t, g.reqs[0].body, "failure_expected"); g.reqs[0].path != "/zsh/nolog/" || f != "0" {
		t.Errorf("JSON: %s, failure_expected %q", g.reqs[0].path, f)
	}
	if r.header.Get("Expect") != "100-continue" {
		t.Errorf("streamed stdin without Expect: 100-continue")
	}
	cmdLen, _ := strconv.Atoi(r.header.Get("X-Brish-Cmd-Length"))
	cmd := string(r.body[:cmdLen])
	if !strings.HasPrefix(cmd, "( mark-me 'BRISHZQ_MARKER' cat\ncd '/synthetic/pwd'\n") {
		t.Errorf("command %q", cmd)
	}
}

func TestRawLiteralStdinAndRetcodes(t *testing.T) {
	g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
		n, _ := strconv.Atoi(r.Header.Get("X-Brish-Cmd-Length"))
		rc, _ := strconv.Atoi(string(body[n:]))
		rawReply(w, "", "", rc, "1")
	})
	for _, rc := range []int{0, 1, 255, 9000, -1} {
		got := runWith(t, g, "", []string{"true"}, "brishz_in", strconv.Itoa(rc))
		if got.code != rc {
			t.Errorf("retcode %d: exit %d", rc, got.code)
		}
	}
	if r := g.reqs[0]; r.header.Get("Content-Length") != "" || r.query != "" {
		// Go's server strips Content-Length into the request; the query
		// must be empty without options.
		if r.query != "" {
			t.Errorf("query %q", r.query)
		}
	}
}

func TestRawNotice(t *testing.T) {
	g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
		w.Header().Set("X-Brish-Notice", "1")
		rawReply(w, "Empty command received.\n\n", "", 0, "1")
	})
	got := runWith(t, g, "", nil, "brishz_noquote", "y")
	if got.code != 200 || got.out != "Empty command received.\n" {
		t.Errorf("got %+v", got)
	}
}

func TestRawNotARawReply(t *testing.T) {
	g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
		io.WriteString(w, "hello")
	})
	if got := runWith(t, g, "", []string{"true"}); got.code != 200 || got.out != "hello\n" {
		t.Errorf("got %+v", got)
	}
	// brishz_binary=y goes to the JSON API, which this fake answers the
	// same way, without X-Brish-Binary.
	if got := runWith(t, g, "", []string{"true"}, "brishz_binary", "y"); got.code != 201 || got.out != "" || !strings.Contains(got.errOut, "lacks binary support") {
		t.Errorf("binary: got %+v", got)
	}
	if p := g.reqs[len(g.reqs)-1].path; p != "/zsh/" {
		t.Errorf("brishz_binary=y used %s", p)
	}
}

// TestRawLegacyGarden: a legacy-mode reply is printed; brishz_binary=y never
// uses the raw API, so that a legacy garden runs nothing.
func TestRawLegacyGarden(t *testing.T) {
	g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
		if r.URL.Path == "/zsh/" {
			io.WriteString(w, `{"retcode":9000,"out":"","err":"refused"}`)
			return
		}
		rawReply(w, "text", "", 0, "0")
	})
	if got := runWith(t, g, "", []string{"true"}); got.code != 0 || got.out != "text" {
		t.Errorf("got %+v", got)
	}
	g.reqs = nil
	if got := runWith(t, g, "", []string{"true"}, "brishz_binary", "1"); got.code != 201 || got.out != "" {
		t.Errorf("binary: got %+v", got)
	}
	if len(g.reqs) != 1 || g.reqs[0].path != "/zsh/" {
		t.Errorf("brishz_binary=y sent %d requests, first to %s", len(g.reqs), g.reqs[0].path)
	}
}

func TestHTTPErrors(t *testing.T) {
	for _, status := range []int{400, 401, 403, 500, 502} {
		g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
			w.WriteHeader(status)
		})
		for _, raw := range []string{"y", "n"} {
			if got := runWith(t, g, "", []string{"true"}, "brishz_raw", raw); got.code != 22 || got.out != "" {
				t.Errorf("HTTP %d, raw %s: got %+v", status, raw, got)
			}
		}
	}
}

// jsonGarden answers 404 on the raw routes, like a garden older than the
// raw API, and a canned CmdResult on /zsh/.
func jsonGarden(t *testing.T, reply func(req map[string]any) (string, string)) *fakeGarden {
	return newFakeGardenLazy(t, func(w http.ResponseWriter, r *http.Request, body func() []byte) {
		if strings.HasPrefix(r.URL.Path, "/zsh/raw/") {
			http.NotFound(w, r)
			return
		}
		var req map[string]any
		json.Unmarshal(body(), &req)
		ctype, data := reply(req)
		if req["binary"] != nil {
			w.Header().Set("X-Brish-Binary", "1")
		}
		w.Header().Set("Content-Type", ctype)
		io.WriteString(w, data)
	})
}

// stdinFileRe is the start of a command that reads stdin from a file.
var stdinFileRe = regexp.MustCompile(`^< '([^']*)' \{\n`)

// requestStdin is the stdin of a JSON request: its stdin_b64 or stdin
// field, or the temp file its command reads with `< file { ... }`, which
// is read now, while the request is in flight.
func requestStdin(req map[string]any) string {
	if s, ok := req["stdin_b64"].(string); ok {
		d, _ := base64.StdEncoding.DecodeString(s)
		return string(d)
	}
	if cmd, ok := req["cmd"].(string); ok {
		if m := stdinFileRe.FindStringSubmatch(cmd); m != nil {
			d, err := os.ReadFile(m[1])
			if err != nil {
				return "<" + err.Error() + ">"
			}
			return string(d)
		}
	}
	s, _ := req["stdin"].(string)
	return s
}

func echoReply(req map[string]any) (string, string) {
	b64 := base64.StdEncoding
	stdin := requestStdin(req)
	if req["binary"] != nil {
		d, _ := json.Marshal(map[string]any{"retcode": 3, "out_b64": b64.EncodeToString([]byte(stdin)), "err_b64": b64.EncodeToString([]byte("e\xff"))})
		return "application/json", string(d)
	}
	d, _ := json.Marshal(map[string]any{"retcode": 4, "out": stdin, "err": "e", "cmd": "x"})
	return "application/json", string(d)
}

func TestFallbackToJSON(t *testing.T) {
	g := jsonGarden(t, echoReply)
	big := strings.Repeat("0123456789abcdef", 1<<12)
	for _, c := range []struct {
		name, stdin string
		kv          []string
		code        int
		out, errOut string
	}{
		{"text", "", []string{"brishz_in", "a\nb\n"}, 4, "a\nb\n", "e"},
		{"magic", big, []string{"brishz_in", "MAGIC_READ_STDIN"}, 4, big, "e"},
		// Stdin arrives exact through the temp file; a JSON reply turns
		// \xff into U+FFFD.
		{"magic invalid utf-8", "\xff\x00", []string{"brishz_in", "MAGIC_READ_STDIN"}, 4, "\ufffd\x00", "e"},
		{"binary", "\xff\x00\r", []string{"brishz_in", "MAGIC_READ_STDIN", "brishz_binary", "y"}, 3, "\xff\x00\r", "e\xff"},
	} {
		g.reqs = nil
		got := runWith(t, g, c.stdin, []string{"cat"}, c.kv...)
		if got.code != c.code || got.out != c.out || got.errOut != c.errOut {
			t.Errorf("%s: got %d %q %q", c.name, got.code, trunc(got.out), got.errOut)
		}
		// A 404 from the raw API, then the JSON API; brishz_binary=y goes
		// to the JSON API directly.
		want := 2
		if c.name == "binary" {
			want = 1
		}
		if len(g.reqs) != want || g.reqs[want-1].path != "/zsh/" {
			t.Errorf("%s: requests %d", c.name, len(g.reqs))
		}
	}
}

func trunc(s string) string {
	if len(s) > 40 {
		return s[:40] + "..."
	}
	return s
}

// TestFallbackReplay: a server that reads the raw body before answering
// 404 still leaves the fallback all of stdin, from the replay copy, in
// memory or, past replayLimit, in a temp file, removed afterwards. With the
// JSON API's remote shape too, which reads the copy back.
func TestFallbackReplay(t *testing.T) {
	tmp := t.TempDir()
	t.Setenv("TMPDIR", tmp)
	g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
		if strings.HasPrefix(r.URL.Path, "/zsh/raw/") {
			io.ReadAll(r.Body)
			http.NotFound(w, r)
			return
		}
		var req map[string]any
		json.Unmarshal(body, &req)
		_, d := echoReply(req)
		io.WriteString(w, d)
	})
	stdin := strings.Repeat("0123456789", 10000)
	check := func(name string) {
		t.Helper()
		got := runWith(t, g, stdin, []string{"cat"}, "brishz_in", "MAGIC_READ_STDIN", "brishz_debug", "y")
		if got.code != 4 || got.out != stdin {
			t.Errorf("%s: got %d %q", name, got.code, trunc(got.out))
		}
		if !strings.Contains(got.errOut, fmt.Sprintf("stdin: %d bytes, %d of them already read", len(stdin), len(stdin))) {
			t.Errorf("%s: the raw request did not read all of stdin:\n%s", name, got.errOut)
		}
		if names, _ := os.ReadDir(tmp); len(names) != 0 {
			t.Errorf("%s: temp files left: %v", name, names)
		}
	}
	check("in memory")
	old := replayLimit
	replayLimit = 1000
	defer func() { replayLimit = old }()
	check("in a temp file")

	// all(), for a garden on another machine, reads the spilled copy.
	src := newStdinSource(config{stdinMagic: true}, strings.NewReader(stdin))
	r, err := src.reader()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := io.Copy(io.Discard, io.LimitReader(r, 5000)); err != nil {
		t.Fatal(err)
	}
	if read, spilled := src.alreadyRead(); read != 5000 || !spilled {
		t.Errorf("read %d, spilled %v", read, spilled)
	}
	if data, err := src.all(); err != nil || string(data) != stdin {
		t.Errorf("all: %v, %d bytes", err, len(data))
	}
	temps.removeAll()
}

// TestRawRefused: a garden that refuses the raw request
// (X-Brish-Refused: 1) ran nothing, so the JSON API gets it, with all of
// stdin. A reply without the header is the command's, even with 9000 and
// a refusal's text, and is never sent again.
func TestRawRefused(t *testing.T) {
	refuse := true
	g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
		if strings.HasPrefix(r.URL.Path, "/zsh/raw/") {
			if refuse {
				w.Header().Set("X-Brish-Refused", "1")
			}
			rawReply(w, "", "brishgarden: stdin is not valid utf-8 ...\n", 9000, "0")
			return
		}
		var req map[string]any
		json.Unmarshal(body, &req)
		_, d := echoReply(req)
		io.WriteString(w, d)
	})
	stdin := "a\x00b\xff"
	got := runWith(t, g, stdin, []string{"cat"}, "brishz_in", "MAGIC_READ_STDIN")
	if got.code != 4 || got.out != "a\x00b\ufffd" || got.errOut != "e" || len(g.reqs) != 2 {
		t.Errorf("refused: got %+v after %d requests", got, len(g.reqs))
	}
	refuse = false
	g.reqs = nil
	got = runWith(t, g, stdin, []string{"cat"}, "brishz_in", "MAGIC_READ_STDIN")
	if got.code != 9000 || got.out != "" || !strings.HasPrefix(got.errOut, "brishgarden: ") || len(g.reqs) != 1 {
		t.Errorf("not refused: got %+v after %d requests", got, len(g.reqs))
	}
}

func TestJSONNotices(t *testing.T) {
	g := jsonGarden(t, func(req map[string]any) (string, string) {
		return "text/plain", "Empty command received.\n"
	})
	if got := runWith(t, g, "", []string{"true"}); got.code != 200 || got.out != "Empty command received.\n" {
		t.Errorf("got %+v", got)
	}
	// With brishz_binary, the fake sends the header; a non-CmdResult is a notice.
	if got := runWith(t, g, "", []string{"true"}, "brishz_binary", "y"); got.code != 200 {
		t.Errorf("binary: got %+v", got)
	}
}

func TestJSONBinaryWithoutHeader(t *testing.T) {
	g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
		if strings.HasPrefix(r.URL.Path, "/zsh/raw/") {
			http.NotFound(w, r)
			return
		}
		io.WriteString(w, `{"retcode":0,"out":"","err":"Empty command received."}`)
	})
	want := "brishzgo: garden lacks binary support (no X-Brish-Binary header); it predates binary mode or runs with BRISH_BINARY=0\n"
	if got := runWith(t, g, "", []string{"true"}, "brishz_binary", "y"); got.code != 201 || got.out != "" || got.errOut != want {
		t.Errorf("got %+v", got)
	}
}

func TestJSONRequestShapes(t *testing.T) {
	cfg := config{session: "s", nolog: "", failureExpected: "y"}
	cmd := []byte("cmd")
	for _, c := range []struct {
		kind       jsonBody
		cmd, stdin string
		want       string
	}{
		{jsonText, "cmd", "<in>&", `{"cmd":"cmd","session":"s","stdin":"<in>&","json_output":"1","nolog":"","failure_expected":"y"}`},
		// Invalid UTF-8 becomes U+FFFD, as jq makes it.
		{jsonText, "c\xff", "\xe2\x82", "{\"cmd\":\"c\ufffd\",\"session\":\"s\",\"stdin\":\"\ufffd\",\"json_output\":\"1\",\"nolog\":\"\",\"failure_expected\":\"y\"}"},
		{jsonStdinFile, "< '/f' {\ncmd\xff\n}", "", "{\"cmd\":\"< '/f' {\\ncmd\ufffd\\n}\",\"session\":\"s\",\"json_output\":\"1\",\"nolog\":\"\",\"failure_expected\":\"y\"}"},
		{jsonB64, "cmd", "\xff", `{"cmd_b64":"Y21k","stdin_b64":"/w==","session":"s","json_output":"1","nolog":"","failure_expected":"y"}`},
		{jsonBinary, "cmd", "", `{"cmd_b64":"Y21k","stdin_b64":"","binary":1,"b64_only":1,"session":"s","json_output":"1","nolog":"","failure_expected":"y"}`},
	} {
		if got := string(jsonRequest(cfg, c.kind, []byte(c.cmd), []byte(c.stdin))); got != c.want {
			t.Errorf("%d %q %q:\n got %s\nwant %s", c.kind, c.cmd, c.stdin, got, c.want)
		}
	}
	if got := string(stdinRedirect("/t/it's", cmd)); got != "< '/t/it'\\''s' {\ncmd\n}" {
		t.Errorf("stdinRedirect: %q", got)
	}
}

// TestJSONStdinFile: with MAGIC_READ_STDIN, the JSON API gets brishzq.zsh's
// request, the command reading stdin from a temp file, which is removed
// afterwards; a garden on another machine gets stdin in the request.
func TestJSONStdinFile(t *testing.T) {
	var path string
	g := jsonGarden(t, func(req map[string]any) (string, string) {
		cmd, _ := req["cmd"].(string)
		if m := stdinFileRe.FindStringSubmatch(cmd); m != nil {
			path = m[1]
			if _, ok := req["stdin"]; ok {
				t.Errorf("stdin sent along with the file")
			}
		}
		return echoReply(req)
	})
	stdin := "a\x00b\xff\r\n"
	got := runWith(t, g, stdin, []string{"cat"}, "brishz_in", "MAGIC_READ_STDIN", "brishz_raw", "n")
	if got.code != 4 || got.out != "a\x00b\ufffd\r\n" {
		t.Errorf("got %+v", got)
	}
	if path == "" {
		t.Fatalf("no temp file in %q", g.reqs[0].body)
	}
	if _, err := os.Stat(path); !os.IsNotExist(err) {
		t.Errorf("temp file %s left behind: %v", path, err)
	}
	cmd := requestField(t, g.reqs[0].body, "cmd")
	if want := "< " + quoteSingle(path) + " {\n( mark-me 'BRISHZQ_MARKER' cat\n"; !strings.HasPrefix(cmd, want) || !strings.HasSuffix(cmd, "\n}") {
		t.Errorf("command %q", cmd)
	}
}

func requestField(t *testing.T, body []byte, name string) string {
	t.Helper()
	var req map[string]any
	if err := json.Unmarshal(body, &req); err != nil {
		t.Fatalf("request %q: %v", body, err)
	}
	s, _ := req[name].(string)
	return s
}

// TestJSONRemoteStdin: a garden on another machine cannot read our temp
// files, so MAGIC_READ_STDIN input goes in the request: as text when it is
// valid UTF-8, else as stdin_b64 with cmd_b64.
func TestJSONRemoteStdin(t *testing.T) {
	l, err := net.Listen("tcp", "[::1]:0")
	if err != nil {
		t.Skip("no IPv6 loopback")
	}
	g := &fakeGarden{}
	g.Server = httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		b, _ := io.ReadAll(r.Body)
		g.mu.Lock()
		g.reqs = append(g.reqs, recorded{r.URL.Path, r.URL.RawQuery, r.Header.Clone(), b})
		g.mu.Unlock()
		if strings.HasPrefix(r.URL.Path, "/zsh/raw/") {
			http.NotFound(w, r)
			return
		}
		var req map[string]any
		json.Unmarshal(b, &req)
		_, d := echoReply(req)
		io.WriteString(w, d)
	}))
	g.Listener.Close()
	g.Listener = l
	g.Start()
	defer g.Close()
	for _, c := range []struct{ stdin, field string }{{"text\n", "stdin"}, {"\xff\x00", "stdin_b64"}} {
		g.reqs = nil
		got := runWith(t, g, c.stdin, []string{"cat"}, "brishz_in", "MAGIC_READ_STDIN")
		if got.code != 4 || requestStdin(map[string]any{c.field: requestField(t, g.reqs[1].body, c.field)}) != c.stdin {
			t.Errorf("%q: got %+v, request %s", c.stdin, got, g.reqs[1].body)
		}
		if strings.HasPrefix(requestField(t, g.reqs[1].body, "cmd"), "< ") {
			t.Errorf("%q: a temp file for a remote garden", c.stdin)
		}
	}
}

func TestParseJSONReply(t *testing.T) {
	out, errOut, rc, ok := parseJSONReply([]byte(`{"out":"a","err":"b","retcode":5}`), false)
	if !ok || string(out) != "a" || string(errOut) != "b" || rc != 5 {
		t.Errorf("text: %q %q %d %v", out, errOut, rc, ok)
	}
	if _, _, _, ok := parseJSONReply([]byte(`{"out":"a","err":"b","retcode":5}`), true); ok {
		t.Errorf("binary without _b64 fields parsed")
	}
	if _, _, _, ok := parseJSONReply([]byte(`not json`), false); ok {
		t.Errorf("non-JSON parsed")
	}
	if _, _, _, ok := parseJSONReply([]byte(`{"out":"a","err":"b"}`), false); ok {
		t.Errorf("no retcode parsed")
	}
}

func TestReadHeaderFile(t *testing.T) {
	f := filepath.Join(t.TempDir(), "h")
	os.WriteFile(f, []byte("X-Test-Key:  synthetic\r\nno colon\n\nX-Other: a:b\n"), 0o600)
	got := readHeaderFile(f)
	if len(got) != 2 || got[0] != [2]string{"X-Test-Key", "synthetic"} || got[1] != [2]string{"X-Other", "a:b"} {
		t.Errorf("got %q", got)
	}
}

// TestKeyHeadersSentAndRedacted: the key file's headers reach a local
// garden, and debug output never shows their values.
func TestKeyHeadersSentAndRedacted(t *testing.T) {
	g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
		rawReply(w, "", "", 0, "1")
	})
	home := t.TempDir()
	os.MkdirAll(filepath.Join(home, ".keys"), 0o700)
	os.WriteFile(filepath.Join(home, ".keys", "brishgarden"), []byte("X-Test-Key: synthetic-secret\n"), 0o600)
	var out, errb bytes.Buffer
	code := run([]string{"true"}, envOf("bshEndpoint", g.URL, "brishz_debug", "y"), "/x", home, strings.NewReader(""), &out, &errb)
	if code != 0 || g.reqs[0].header.Get("X-Test-Key") != "synthetic-secret" {
		t.Errorf("exit %d, key header %q", code, g.reqs[0].header.Get("X-Test-Key"))
	}
	if strings.Contains(errb.String(), "synthetic-secret") || !strings.Contains(errb.String(), "<redacted>") {
		t.Errorf("debug output leaks or lacks redaction:\n%s", errb.String())
	}
}

func TestTransportFailures(t *testing.T) {
	l, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	addr := l.Addr().String()
	l.Close()
	for _, c := range []struct {
		endpoint string
		code     int
	}{
		{"http://" + addr, 7},
		{"http://brishzgo-test.invalid:1", 6},
		{"ftp://127.0.0.1:1", 1},
	} {
		var out, errb bytes.Buffer
		got := run([]string{"true"}, envOf("bshEndpoint", c.endpoint), "/x", "", strings.NewReader(""), &out, &errb)
		if got != c.code {
			t.Errorf("%s: exit %d, want %d", c.endpoint, got, c.code)
		}
	}

	// A server that closes the connection without replying: curl's 52.
	l, err = net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer l.Close()
	go func() {
		for {
			conn, err := l.Accept()
			if err != nil {
				return
			}
			buf := make([]byte, 4096)
			conn.Read(buf)
			conn.Close()
		}
	}()
	var out, errb bytes.Buffer
	if got := run([]string{"true"}, envOf("bshEndpoint", "http://"+l.Addr().String()), "/x", "", strings.NewReader(""), &out, &errb); got != 52 {
		t.Errorf("empty reply: exit %d, want 52", got)
	}
}

func TestPartialReply(t *testing.T) {
	g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
		w.Header().Set("X-Brish-Retcode", "0")
		w.Header().Set("X-Brish-Out-Length", "10")
		io.WriteString(w, "abc")
	})
	if got := runWith(t, g, "", []string{"true"}); got.code != 18 {
		t.Errorf("got %+v", got)
	}
}

// replyServer answers every request with reply, whatever it is, and closes
// the connection.
func replyServer(t *testing.T, reply string) string {
	t.Helper()
	l, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { l.Close() })
	go func() {
		for {
			conn, err := l.Accept()
			if err != nil {
				return
			}
			go func() {
				defer conn.Close()
				var req []byte
				buf := make([]byte, 4096)
				for !bytes.Contains(req, []byte("\r\n\r\n")) {
					n, err := conn.Read(buf)
					if err != nil {
						return
					}
					req = append(req, buf[:n]...)
				}
				io.WriteString(conn, reply)
			}()
		}
	}()
	return l.Addr().String()
}

// TestBrokenReplies: replies that are not HTTP, or not well-formed, exit
// with curl's codes; so does an https endpoint answered in plain HTTP.
func TestBrokenReplies(t *testing.T) {
	for _, c := range []struct {
		reply string
		code  int
	}{
		{"NOT HTTP AT ALL\r\n\r\n", 1},
		{"HTTP/1.1 abc OK\r\nContent-Length: 0\r\n\r\n", 1},
		{"HTTP/1.1 200 OK\r\nbad header line\r\nContent-Length: 0\r\n\r\n", 8},
		{"HTTP/1.1 200 OK\r\nContent-Length: zz\r\n\r\n", 8},
	} {
		var out, errb bytes.Buffer
		ep := "http://" + replyServer(t, c.reply)
		if got := run([]string{"true"}, envOf("bshEndpoint", ep), "/x", "", strings.NewReader(""), &out, &errb); got != c.code {
			t.Errorf("%q: exit %d, want %d", c.reply, got, c.code)
		}
	}
	g := newFakeGarden(t, nil)
	var out, errb bytes.Buffer
	ep := strings.Replace(g.URL, "http://", "https://", 1)
	if got := run([]string{"true"}, envOf("bshEndpoint", ep), "/x", "", strings.NewReader(""), &out, &errb); got != 35 {
		t.Errorf("https to an http garden: exit %d, want 35", got)
	}
}
