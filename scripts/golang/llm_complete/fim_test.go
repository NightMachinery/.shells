package main

import (
	"bytes"
	"context"
	"crypto/x509"
	"encoding/json"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func testConfig(t *testing.T, url, extract string) Config {
	t.Helper()
	t.Setenv("LLM_COMPLETE_CONFIG", filepath.Join(t.TempDir(), "missing"))
	c, err := readConfig()
	if err != nil {
		t.Fatal(err)
	}
	p := c.Providers["codestral"]
	p.Endpoint = url
	p.Extract = extract
	c.Providers["codestral"] = p
	t.Setenv("codestral_api_key", "INERT_KEY")
	return c
}

func TestNetworkErrorCodes(t *testing.T) {
	for _, tc := range []struct {
		err  error
		code int
	}{
		{context.DeadlineExceeded, 28}, {&net.DNSError{Err: "fabricated", Name: "example.invalid"}, 6},
		{x509.UnknownAuthorityError{}, 60}, {&net.OpError{Op: "dial", Err: io.EOF}, 7}, {io.EOF, 52},
	} {
		if got := networkCode(tc.err); got != tc.code {
			t.Fatal(got, tc.code)
		}
	}
	tls := httptest.NewTLSServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {}))
	defer tls.Close()
	l, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	closed := "http://" + l.Addr().String()
	l.Close()
	for _, tc := range []struct {
		endpoint string
		code     int
	}{{tls.URL, 60}, {closed, 7}} {
		c := testConfig(t, tc.endpoint, "chat")
		_, code, msg, _ := performFIM(c, FIMRequest{Prefix: "inert"})
		want := "fim-get: codestral: curl error " + fmtInt(tc.code)
		if code != tc.code || msg != want {
			t.Fatal(code, msg)
		}
	}
}

func TestHTTPProxyEnvironment(t *testing.T) {
	// ProxyFromEnvironment caches its first environment. Exercise each setup
	// in a fresh process, as real CLI requests do.
	if mode := os.Getenv("LLM_TEST_PROXY_CHILD"); mode != "" {
		if mode == "all-only" {
			r, _ := http.NewRequest("POST", "http://example.invalid/fim", nil)
			proxy, err := http.ProxyFromEnvironment(r)
			if err != nil || proxy != nil {
				t.Fatal("ALL_PROXY unexpectedly used")
			}
			return
		}
		c := testConfig(t, "http://example.invalid/fim", "chat")
		out, code, msg, _ := performFIM(c, FIMRequest{Prefix: "inert"})
		if code != 0 || msg != "" || out != " proxied" {
			t.Fatal(out, code, msg)
		}
		return
	}
	proxy := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Host != "example.invalid" || r.Header.Get("Authorization") != "Bearer INERT_KEY" {
			t.Error("proxy request")
		}
		w.Write([]byte(`{"choices":[{"message":{"content":" proxied"}}]}`))
	}))
	defer proxy.Close()
	for _, mode := range []string{"http", "all-only"} {
		cmd := exec.Command(os.Args[0], "-test.run=^TestHTTPProxyEnvironment$")
		for _, e := range os.Environ() {
			k := strings.SplitN(e, "=", 2)[0]
			if !strings.Contains(strings.ToUpper(k), "PROXY") && k != "REQUEST_METHOD" {
				cmd.Env = append(cmd.Env, e)
			}
		}
		cmd.Env = append(cmd.Env, "LLM_TEST_PROXY_CHILD="+mode, "ALL_PROXY="+proxy.URL)
		if mode == "http" {
			cmd.Env = append(cmd.Env, "HTTP_PROXY="+proxy.URL)
		}
		if b, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("%s: %v %s", mode, err, b)
		}
	}
}
func TestFIMErrors(t *testing.T) {
	for _, tc := range []struct{ body, want string }{
		{`{"detail":"Invalid API Key"}`, "Invalid API Key"}, {`{"message":"bad\nrequest"}`, "bad request"}, {`{"error":{"message":"auth fails"}}`, "auth fails"}, {`{"detail":[{"msg":"bad"}]}`, `[{"msg":"bad"}]`}, {`<html>bad gateway</html>`, `<html>bad gateway</html>`}, {"", "(empty response body)"}, {"\n\t  ", "(empty response body)"}, {`{"detail":null,"message":"next"}`, "next"}, {`{"detail":false,"message":"next"}`, "next"}, {`{"detail":""}`, `{"detail":""}`}, {strings.Repeat("é", 201), strings.Repeat("é", 200) + "…"},
	} {
		t.Run(tc.want, func(t *testing.T) {
			srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) { w.WriteHeader(401); w.Write([]byte(tc.body)) }))
			defer srv.Close()
			c := testConfig(t, srv.URL, "chat")
			out, code, msg, _ := performFIM(c, FIMRequest{Prefix: "x"})
			if out != "" || code != 1 || msg != "fim-get: codestral: HTTP 401 — "+tc.want {
				t.Fatalf("%q %d %q", out, code, msg)
			}
		})
	}
	c := testConfig(t, "http://127.0.0.1:1", "chat")
	for _, tc := range []struct {
		r    FIMRequest
		want string
	}{{FIMRequest{Provider: "x"}, "fim-get: unknown provider 'x'; known: codestral deepseek deepseek-flash"}, {FIMRequest{}, "fim-get: needs a prefix, a suffix, or both"}} {
		_, code, msg, _ := performFIM(c, tc.r)
		if code != 1 || msg != tc.want {
			t.Fatalf("%d %q", code, msg)
		}
	}
	t.Setenv("codestral_api_key", "")
	_, code, msg, _ := performFIM(c, FIMRequest{Prefix: "x"})
	if code != 1 || msg != "fim-get: no API key for codestral (expected $codestral_api_key)" {
		t.Fatal(code, msg)
	}
}
func TestFIMBodyAndSpace(t *testing.T) {
	for _, shape := range []string{"chat", "text"} {
		srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			var b map[string]any
			if json.NewDecoder(r.Body).Decode(&b) != nil {
				t.Error("body")
			}
			if b["prompt"] != "prefix" || b["suffix"] != "suffix" || b["max_tokens"] != float64(17) || b["temperature"] != 0.2 || b["stop"] != "STOP" || r.Header.Get("Authorization") != "Bearer INERT_KEY" {
				t.Errorf("%v", b)
			}
			w.Write([]byte(`{"choices":[{"text":" value","message":{"content":" value"}}]}`))
		}))
		c := testConfig(t, srv.URL, shape)
		for _, strip := range []bool{false, true} {
			out, code, msg, _ := performFIM(c, FIMRequest{Prefix: "prefix", Suffix: "suffix", Parameters: Parameters{MaxTokens: ptr(17), Temperature: ptr(0.2), Stop: json.RawMessage(`"STOP"`), StripSpace: &strip}})
			want := " value"
			if strip {
				want = "value"
			}
			if code != 0 || msg != "" || out != want {
				t.Fatalf("%q %d %q", out, code, msg)
			}
		}
		srv.Close()
	}
}
func TestOverridePrecedence(t *testing.T) {
	p := filepath.Join(t.TempDir(), "providers.json")
	os.WriteFile(p, []byte(`{"providers":{"codestral":{"model":"override","max_tokens":23,"temperature":0.4,"stop":["X","Y"]},"local":{"endpoint":"http://localhost/fim","model":"local-model","key_env":"","extract":"text"}},"default_provider":"local","agent_fim":{"prefix_chars":7,"log":false}}`), 0600)
	t.Setenv("LLM_COMPLETE_CONFIG", p)
	c, err := readConfig()
	if err != nil {
		t.Fatal(err)
	}
	if c.Providers["codestral"].KeyVar != "codestral_api_key" || c.DefaultProvider != "local" {
		t.Fatal(c)
	}
	_, b, _, _, err := resolve(c, FIMRequest{Provider: "codestral", Prefix: "x"})
	if err != nil || b["model"] != "override" || b["max_tokens"] != 23 || b["temperature"] != 0.4 {
		t.Fatal(b, err)
	}
	_, b, _, _, err = resolve(c, FIMRequest{Provider: "codestral", Parameters: Parameters{Model: ptr("call"), MaxTokens: ptr(0), Stop: json.RawMessage(`""`), Temperature: ptr(0.0)}})
	if err != nil || b["model"] != "call" || b["temperature"] != 0.0 {
		t.Fatal(b, err)
	}
	if _, ok := b["stop"]; ok {
		t.Fatal(b)
	}
	if _, ok := b["max_tokens"]; ok {
		t.Fatal(b)
	}
	os.WriteFile(p, []byte("broken"), 0600)
	if _, err = readConfig(); err == nil {
		t.Fatal("bad override accepted")
	}
}
func TestFIMResponseAndTimeout(t *testing.T) {
	for _, tc := range []struct {
		body, want string
		code       int
	}{{`{"choices":[]}`, "", 0}, {`{}`, "", 0}, {`garbage`, "fim-get: codestral: unreadable response", 1}} {
		srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) { w.Write([]byte(tc.body)) }))
		c := testConfig(t, srv.URL, "chat")
		out, code, msg, _ := performFIM(c, FIMRequest{Prefix: "x"})
		if out != "" || code != tc.code || msg != tc.want {
			t.Fatal(out, code, msg)
		}
		srv.Close()
	}
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) { time.Sleep(50 * time.Millisecond) }))
	defer srv.Close()
	c := testConfig(t, srv.URL, "chat")
	_, code, msg, _ := performFIM(c, FIMRequest{Prefix: "x", Parameters: Parameters{Timeout: ptr(.005)}})
	if code != 28 || msg != "fim-get: codestral: curl error 28" {
		t.Fatal(code, msg)
	}
}
func TestCLIContract(t *testing.T) {
	t.Setenv("LLM_COMPLETE_CONFIG", filepath.Join(t.TempDir(), "missing"))
	var out, err bytes.Buffer
	code := run([]string{"fim"}, strings.NewReader(`{"provider":"x","prefix":"SENTINEL"}`), &out, &err)
	if code != 1 || out.Len() != 0 || err.String() != "fim-get: unknown provider 'x'; known: codestral deepseek deepseek-flash\n" {
		t.Fatal(code, out.String(), err.String())
	}
	out.Reset()
	err.Reset()
	if run([]string{"fim", "providers", "--json"}, strings.NewReader(""), &out, &err) != 0 || !strings.Contains(out.String(), `"key_env":"codestral_api_key"`) {
		t.Fatal(out.String())
	}
}

func TestShellRequest(t *testing.T) {
	r, err := shellRequest(strings.NewReader("prefix\x00quote\"\nسلام\x00suffix\x00tail\x00max_tokens\x007\x00stop\x00\x00strip_space\x00true\x00"))
	if err != nil || r.Prefix != "quote\"\nسلام" || *r.MaxTokens != 7 || string(r.Stop) != "\"\"" || !*r.StripSpace {
		t.Fatalf("%+v %v", r, err)
	}
	for _, s := range []string{"prefix\x00x", "prefix\x00", "temperature\x00oops\x00"} {
		if _, err := shellRequest(strings.NewReader(s)); err == nil {
			t.Fatal("accepted malformed shell request")
		}
	}
}
