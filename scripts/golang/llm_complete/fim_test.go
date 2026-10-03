package main

import (
	"bytes"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"os"
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
