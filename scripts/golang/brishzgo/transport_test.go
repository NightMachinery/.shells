package main

import (
	"bytes"
	"encoding/base64"
	"encoding/json"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
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
	if r.path != "/zsh/raw/nolog/" || r.query != "failure_expected=1&nolog=y&session=s+1" {
		t.Errorf("request %s?%s", r.path, r.query)
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

func echoReply(req map[string]any) (string, string) {
	b64 := base64.StdEncoding
	if req["binary"] != nil {
		stdin, _ := b64.DecodeString(req["stdin_b64"].(string))
		d, _ := json.Marshal(map[string]any{"retcode": 3, "out_b64": b64.EncodeToString(stdin), "err_b64": b64.EncodeToString([]byte("e\xff"))})
		return "application/json", string(d)
	}
	stdin, _ := req["stdin"].(string)
	if s, ok := req["stdin_b64"].(string); ok {
		d, _ := b64.DecodeString(s)
		stdin = string(d)
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
		// The request carries stdin_b64; a JSON reply turns \xff into U+FFFD.
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
// 404 still leaves the fallback all of stdin, from the replay copy; past
// replayLimit, the fallback refuses.
func TestFallbackReplay(t *testing.T) {
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
	stdin := strings.Repeat("x", 100000)
	if got := runWith(t, g, stdin, []string{"cat"}, "brishz_in", "MAGIC_READ_STDIN"); got.code != 4 || got.out != stdin {
		t.Errorf("replay: got %d %q", got.code, trunc(got.out))
	}

	old := replayLimit
	replayLimit = 1000
	defer func() { replayLimit = old }()
	got := runWith(t, g, stdin, []string{"cat"}, "brishz_in", "MAGIC_READ_STDIN")
	if got.code != 1 || got.out != "" || !strings.Contains(got.errOut, "nothing ran") {
		t.Errorf("overflow: got %d %q %q", got.code, trunc(got.out), got.errOut)
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
	if got := runWith(t, g, "", []string{"true"}, "brishz_binary", "y"); got.code != 201 || got.out != "" {
		t.Errorf("got %+v", got)
	}
}

func TestJSONRequestShapes(t *testing.T) {
	cfg := config{command: []byte("cmd"), session: "s", nolog: "", failureExpected: "y"}
	if got := string(jsonRequest(cfg, []byte("<in>&"))); got != `{"cmd":"cmd","session":"s","stdin":"<in>&","json_output":"1","nolog":"","failure_expected":"y"}` {
		t.Errorf("text: %s", got)
	}
	if got := string(jsonRequest(cfg, []byte{0xff})); got != `{"cmd_b64":"Y21k","stdin_b64":"/w==","session":"s","json_output":"1","nolog":"","failure_expected":"y"}` {
		t.Errorf("b64: %s", got)
	}
	cfg.binary = true
	if got := string(jsonRequest(cfg, nil)); got != `{"cmd_b64":"Y21k","stdin_b64":"","binary":1,"b64_only":1,"session":"s","json_output":"1","nolog":"","failure_expected":"y"}` {
		t.Errorf("binary: %s", got)
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
