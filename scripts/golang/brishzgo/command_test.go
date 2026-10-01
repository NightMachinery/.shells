package main

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"
)

func envOf(kv ...string) lookupEnv {
	m := map[string]string{}
	for i := 0; i+1 < len(kv); i += 2 {
		m[kv[i]] = kv[i+1]
	}
	return func(name string) (string, bool) {
		v, ok := m[name]
		return v, ok
	}
}

func TestBuildCommandLocalWrapper(t *testing.T) {
	got := buildCommand([]string{"print", "-r", "--", "it's"}, envOf(), "http://127.0.0.1:7230/zsh/", "/tmp/a b")
	want := "( mark-me 'BRISHZQ_MARKER' print '-r' '--' 'it'\\''s'\n" +
		"cd '/tmp/a b'\n" +
		"print '-r' '--' 'it'\\''s'\n" +
		"ret=$? ; cd /tmp ; return-code $ret )"
	if got != want {
		t.Errorf("got %q\nwant %q", got, want)
	}
}

func TestBuildCommandRemote(t *testing.T) {
	for _, ep := range []string{"http://localhost:7230/zsh/", "https://example.invalid/zsh/"} {
		if got := buildCommand([]string{"true"}, envOf(), ep, "/x"); got != "true" {
			t.Errorf("%s: got %q", ep, got)
		}
	}
	// brishzq.zsh's regex leaves the dots unescaped.
	if got := buildCommand([]string{"true"}, envOf(), "http://127x0y0z1:1/zsh/", "/x"); got == "true" {
		t.Errorf("127x0y0z1 should count as local, as in brishzq.zsh")
	}
}

func TestBuildCommandNoquote(t *testing.T) {
	env := envOf("brishz_noquote", "y", "NIGHT_EMACS_P", "y")
	if got := buildCommand([]string{"echo", "a b", "$x"}, env, "http://127.0.0.1:7230/zsh/", "/x"); got != "echo a b $x" {
		t.Errorf("got %q", got)
	}
	if got := buildCommand(nil, env, "http://127.0.0.1:7230/zsh/", "/x"); got != "" {
		t.Errorf("got %q", got)
	}
}

func TestBuildCommandEmacsForwarding(t *testing.T) {
	env := envOf("NIGHT_EMACS_P", "y", "emacs_night_server_name", "srv 1", "EMACS_SOCKET_NAME", "ignored")
	got := buildCommand([]string{"true"}, env, "http://localhost:1/zsh/", "/x")
	want := "local -x emacs_night_server_name='srv 1'\n" +
		"local -x EMACS_SOCKET_NAME='srv 1'\n" +
		"local -x NIGHT_EMACS_P=y\n" +
		"true"
	if got != want {
		t.Errorf("got %q\nwant %q", got, want)
	}

	// Without emacs_night_server_name, EMACS_SOCKET_NAME is still set, empty.
	got = buildCommand([]string{"true"}, envOf("NIGHT_EMACS_P", "1"), "http://localhost:1/zsh/", "/x")
	want = "local -x EMACS_SOCKET_NAME=''\nlocal -x NIGHT_EMACS_P=1\ntrue"
	if got != want {
		t.Errorf("got %q\nwant %q", got, want)
	}

	// An empty NIGHT_EMACS_P is not emacs.
	if got := buildCommand([]string{"true"}, envOf("NIGHT_EMACS_P", ""), "http://localhost:1/zsh/", "/x"); got != "true" {
		t.Errorf("got %q", got)
	}
}

func TestNewConfig(t *testing.T) {
	home := t.TempDir()
	c := newConfig([]string{"-c", "true"}, envOf(), "/x", home)
	if c.endpoint != "http://127.0.0.1:7230/zsh/" || len(c.args) != 1 || !c.raw || c.binary || c.apikeyFile != "" {
		t.Errorf("defaults: %+v", c)
	}
	c = newConfig(nil, envOf("GARDEN_PORT", "7292", "brishz_raw", "n", "brishz_binary", "No"), "/x", home)
	if c.endpoint != "http://127.0.0.1:7292/zsh/" || c.raw || c.binary {
		t.Errorf("GARDEN_PORT, brishz_raw=n: %+v", c)
	}
	c = newConfig(nil, envOf("bshEndpoint", "https://garden.example.invalid", "GARDEN_PASS0", "p", "brishz_binary", "y", "brishz_raw", ""), "/x", home)
	if c.endpoint != "https://garden.example.invalid/zsh/" || !c.basicAuth || c.basicPass != "p" || !c.binary || !c.raw {
		t.Errorf("bshEndpoint: %+v", c)
	}
	c = newConfig(nil, envOf("brishz_in", "MAGIC_READ_STDIN"), "/x", home)
	if !c.stdinMagic {
		t.Errorf("MAGIC_READ_STDIN: %+v", c)
	}
	c = newConfig(nil, envOf("brishz_in", "abc"), "/x", home)
	if c.stdinMagic || string(c.stdinLiteral) != "abc" {
		t.Errorf("literal stdin: %+v", c)
	}

	if err := os.MkdirAll(filepath.Join(home, ".keys"), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(home, ".keys", "brishgarden"), []byte("X-Test-Key: synthetic\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	for ep, want := range map[string]bool{
		"http://127.0.0.1:1":             true,
		"http://localhost:1":             true,
		"https://garden.example.invalid": false,
	} {
		c = newConfig(nil, envOf("bshEndpoint", ep), "/x", home)
		if (c.apikeyFile != "") != want {
			t.Errorf("%s: apikeyFile %q", ep, c.apikeyFile)
		}
	}
}

func TestBoolP(t *testing.T) {
	for in, want := range map[string]bool{"": false, "n": false, "N": false, "no": false, "NO": false, "0": false, "y": true, "1": true, "false": true, "x": true} {
		if boolP(in) != want {
			t.Errorf("boolP(%q) = %v", in, !want)
		}
	}
}

func TestDisabled(t *testing.T) {
	var out, errb bytes.Buffer
	if code := run(nil, envOf("DISABLE_BRISH", "Y"), "/x", "", nil, &out, &errb); code != 1 {
		t.Errorf("exit %d", code)
	}
}
