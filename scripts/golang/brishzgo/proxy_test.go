package main

import (
	"bytes"
	"io"
	"net/http"
	"net/http/httptest"
	"net/url"
	"os/exec"
	"regexp"
	"strings"
	"testing"
)

func proxyFor(t *testing.T, target string, kv ...string) string {
	t.Helper()
	req, err := http.NewRequest("POST", target, nil)
	if err != nil {
		t.Fatal(err)
	}
	u, err := proxyFromEnv(envOf(kv...))(req)
	if err != nil {
		return "error"
	}
	if u == nil {
		return ""
	}
	return u.String()
}

func TestProxyFromEnv(t *testing.T) {
	const h = "http://127.0.0.1:7289/zsh/"
	for _, c := range []struct {
		target string
		kv     []string
		want   string
	}{
		{h, nil, ""},
		{h, []string{"http_proxy", "http://p:1"}, "http://p:1"},
		{h, []string{"HTTP_PROXY", "http://p:1"}, ""},
		{h, []string{"ALL_PROXY", "socks5h://p:1"}, "socks5h://p:1"},
		{h, []string{"all_proxy", "socks5h://a:1", "ALL_PROXY", "socks5h://b:1"}, "socks5h://a:1"},
		{h, []string{"http_proxy", "", "ALL_PROXY", "socks5h://p:1"}, "socks5h://p:1"},
		{h, []string{"http_proxy", "http://p:1", "ALL_PROXY", "socks5h://q:1"}, "http://p:1"},
		{h, []string{"http_proxy", "p"}, "http://p:1080"},
		{h, []string{"http_proxy", "https://p"}, "https://p:443"},
		{h, []string{"http_proxy", "socks4://p:1"}, "error"},
		{h, []string{"http_proxy", "http://p:1", "no_proxy", "127.0.0.1"}, ""},
		{h, []string{"http_proxy", "http://p:1", "no_proxy", "", "NO_PROXY", "127.0.0.1"}, ""},
		{h, []string{"http_proxy", "http://p:1", "no_proxy", "127.0.0.0/8"}, ""},
		{h, []string{"http_proxy", "http://p:1", "no_proxy", "127.0.0.2"}, "http://p:1"},
		{"https://g.example.invalid/zsh/", []string{"https_proxy", "http://a:1", "HTTPS_PROXY", "http://b:1"}, "http://a:1"},
		{"https://g.example.invalid/zsh/", []string{"HTTPS_PROXY", "http://b:1"}, "http://b:1"},
		{"http://www.example.invalid/", []string{"http_proxy", "http://p:1", "no_proxy", "x, .Example.invalid."}, ""},
		{"http://www.example.invalid/", []string{"http_proxy", "http://p:1", "no_proxy", "xample.invalid"}, "http://p:1"},
		{"http://www.example.invalid/", []string{"http_proxy", "http://p:1", "no_proxy", "*"}, ""},
		{"http://www.example.invalid/", []string{"http_proxy", "http://p:1", "no_proxy", "a, *"}, "http://p:1"},
	} {
		if got := proxyFor(t, c.target, c.kv...); got != c.want {
			t.Errorf("%s %q: proxy %q, want %q", c.target, c.kv, got, c.want)
		}
	}
}

// TestProxyMatchesCurl asks curl which address it connects to first (the
// proxy's, or the target's) for each environment, and compares. Skipped
// without curl.
func TestProxyMatchesCurl(t *testing.T) {
	curl, err := exec.LookPath("curl")
	if err != nil {
		t.Skip("curl not found")
	}
	trying := regexp.MustCompile(`Trying \[?([^\]\s]+?)\]?:(\d+)\.\.\.`)
	targets := []string{"http://127.0.0.1:7289/", "http://localhost:7289/", "https://127.0.0.1:7289/"}
	envs := [][]string{
		{"http_proxy", "http://127.0.0.1:7277"},
		{"HTTP_PROXY", "http://127.0.0.1:7277"},
		{"ALL_PROXY", "socks5h://127.0.0.1:7277"},
		{"all_proxy", "socks5h://127.0.0.1:7276", "ALL_PROXY", "socks5h://127.0.0.1:7277"},
		{"http_proxy", "", "ALL_PROXY", "socks5h://127.0.0.1:7277"},
		{"HTTPS_PROXY", "http://127.0.0.1:7277"},
		{"https_proxy", "http://127.0.0.1:7276", "HTTPS_PROXY", "http://127.0.0.1:7277"},
		{"http_proxy", "http://127.0.0.1:7277", "no_proxy", "127.0.0.1,localhost"},
		{"ALL_PROXY", "socks5h://127.0.0.1:7277", "NO_PROXY", "127.0.0.0/8, LOCALHOST."},
		{"ALL_PROXY", "socks5h://127.0.0.1:7277", "no_proxy", "*"},
		{"http_proxy", "127.0.0.1"},
	}
	for _, target := range targets {
		for _, kv := range envs {
			cmd := exec.Command(curl, "-sv", "--max-time", "2", target)
			cmd.Env = []string{"PATH=/usr/bin:/bin"}
			for i := 0; i+1 < len(kv); i += 2 {
				cmd.Env = append(cmd.Env, kv[i]+"="+kv[i+1])
			}
			var errb bytes.Buffer
			cmd.Stderr = &errb
			cmd.Run()
			m := trying.FindStringSubmatch(errb.String())
			if m == nil {
				t.Fatalf("%s %q: no connection attempt in curl's output:\n%s", target, kv, errb.String())
			}
			curlPort := m[2]

			want := "7289" // the target itself
			if p := proxyFor(t, target, kv...); p != "" {
				u, _ := url.Parse(p)
				want = u.Port()
			}
			if curlPort != want {
				t.Errorf("%s %q: curl connects to port %s, brishzgo to %s", target, kv, curlPort, want)
			}
		}
	}
}

// TestProxyUsed: a request through http_proxy reaches the proxy, even for
// 127.0.0.1, as curl's does, with the key headers.
func TestProxyUsed(t *testing.T) {
	var got []string
	proxy := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		got = append(got, r.Method+" "+r.URL.String())
		io.ReadAll(r.Body)
		w.Header().Set("X-Brish-Stream", "1")
		w.Write(frameOf(frameStdout, []byte("via proxy")))
		w.Write(exitFrameOf(0))
	}))
	defer proxy.Close()
	var out, errb bytes.Buffer
	code := run([]string{"true"}, envOf("bshEndpoint", "http://127.0.0.1:1", "http_proxy", proxy.URL), "/x", "", strings.NewReader(""), &out, &errb)
	if code != 0 || out.String() != "via proxy" || len(got) != 1 || got[0] != "POST http://127.0.0.1:1/zsh/stream/" {
		t.Errorf("exit %d, out %q, proxy saw %q", code, out.String(), got)
	}
	// An unsupported proxy is curl's exit 7, and a refused one too.
	for _, p := range []string{"socks4://127.0.0.1:1", "socks5h://127.0.0.1:1"} {
		if code := run([]string{"true"}, envOf("bshEndpoint", "http://127.0.0.1:1", "ALL_PROXY", p), "/x", "", strings.NewReader(""), &out, &errb); code != 7 {
			t.Errorf("%s: exit %d, want 7", p, code)
		}
	}
}
