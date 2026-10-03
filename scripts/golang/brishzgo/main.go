// Command brishzgo runs one zsh command in a BrishGarden and passes on its
// stdout, stderr and exit status: a faster drop-in for brishzq.zsh, with the
// same argv, environment variables and exit statuses. It talks to the
// garden's raw API, and falls back to the JSON API for a garden without one.
// With brishz_stream=y it uses the streaming API instead, which passes the
// output on as the command makes it. See docs/brishzgo.md in the scripts
// repository.
package main

import (
	"fmt"
	"io"
	"os"
	"regexp"
	"strings"
)

const magicReadStdin = "MAGIC_READ_STDIN"

// boolP is brishzq.zsh's `bool`: n, no and 0 (in any case) and the empty
// string are false, anything else is true. The variables that brishzq.zsh
// reads too (brishz_binary, brishz_raw) are parsed with it, for parity.
func boolP(s string) bool {
	switch strings.ToLower(s) {
	case "", "n", "no", "0":
		return false
	}
	return true
}

// boolCoreP is the `bool` of the scripts' zshlang/basic/core.zsh, which
// also takes false (in any case) as false. brishz_stream is parsed with
// it: brishzq.zsh has no such variable, so there is no parity to keep, and
// brishz_stream=false must not turn on the one mode where an interrupt
// stops the remote command.
func boolCoreP(s string) bool {
	return boolP(s) && strings.ToLower(s) != "false"
}

type config struct {
	env      lookupEnv // for the proxy variables
	args     []string
	endpoint string // ends in /zsh/, as brishzq.zsh's does
	command  []byte

	stdinMagic   bool
	stdinLiteral []byte

	session, nolog, failureExpected string

	binary bool // brishz_binary
	raw    bool // brishz_raw, on unless set to a false value
	stream bool // brishz_stream, off unless set to a true value
	debug  bool // brishz_debug

	// The endpoint is on this machine (apikeyEndpointRe), so the garden
	// can read our temp files.
	sameMachine bool

	apikeyFile string // sent as headers when non-empty
	basicPass  string
	basicAuth  bool
}

var (
	apikeyEndpointRe = regexp.MustCompile(`^https?://(127\.0\.0\.1|localhost)`)
	gardenEndpointRe = regexp.MustCompile(`garden`)
)

func newConfig(args []string, env lookupEnv, pwd, home string) config {
	if len(args) > 0 && args[0] == "-c" {
		args = args[1:]
	}
	c := config{env: env, args: args}

	base := env.get("bshEndpoint")
	if base == "" {
		port := env.get("GARDEN_PORT")
		if port == "" {
			port = "7230"
		}
		base = "http://127.0.0.1:" + port
	}
	c.endpoint = base + "/zsh/"
	c.command = []byte(buildCommand(args, env, c.endpoint, pwd))

	in := env.get("brishz_in")
	if in == magicReadStdin {
		c.stdinMagic = true
	} else {
		c.stdinLiteral = []byte(in)
	}

	c.session = env.get("brishz_session")
	c.nolog = env.get("brishz_nolog")
	c.failureExpected = env.get("brishz_failure_expected")
	c.binary = boolP(env.get("brishz_binary"))
	rawOpt := env.get("brishz_raw")
	c.raw = rawOpt == "" || boolP(rawOpt)
	c.stream = boolCoreP(env.get("brishz_stream"))
	c.debug = boolP(env.get("brishz_debug"))

	// As in brishzq.zsh: local requests send the API key file's header
	// lines; an endpoint whose URL names a garden goes through a proxy
	// with basic auth.
	c.sameMachine = apikeyEndpointRe.MatchString(c.endpoint)
	if c.sameMachine && home != "" {
		f := home + "/.keys/brishgarden"
		if fh, err := os.Open(f); err == nil {
			fh.Close()
			c.apikeyFile = f
		}
	}
	if gardenEndpointRe.MatchString(c.endpoint) {
		c.basicAuth = true
		c.basicPass = env.get("GARDEN_PASS0")
	}
	return c
}

// run is the whole client; it returns the exit status.
func run(args []string, env lookupEnv, pwd, home string, stdin io.Reader, stdout, stderr io.Writer) int {
	if strings.ToLower(env.get("DISABLE_BRISH")) == "y" {
		fmt.Fprintln(stderr, "brishzgo: disabled by DISABLE_BRISH")
		return 1
	}
	cfg := newConfig(args, env, pwd, home)
	defer temps.removeAll()
	cl := newClient(cfg, stdout, stderr)
	in := newStdinSource(cfg, stdin)
	// brishz_binary=y promises that nothing ran on a garden without binary
	// mode. The raw and streaming APIs cannot keep that promise: a
	// legacy-mode garden runs the command (with text decoding) and only
	// says so in its reply. So that opt-in takes the JSON API's binary
	// transport, as brishzq.zsh does, even with brishz_stream=y; without
	// it, the raw and streaming APIs are exact on a binary-mode garden.
	tryRaw := cfg.raw && !cfg.binary
	if cfg.stream && cfg.binary {
		cl.debugf("brishz_binary=y takes the JSON API, so brishz_stream=y does nothing")
	}
	if cfg.stream && !cfg.binary {
		code, fb := cl.stream(in)
		switch fb {
		case noFallback:
			return code
		case fallbackJSON:
			tryRaw = false
		}
		next := "JSON"
		if tryRaw {
			next = "raw"
		}
		cl.debugf("%s; falling back to the %s API", cl.fallbackWhy, next)
	}
	if tryRaw {
		if code, fallback := cl.raw(in); !fallback {
			return code
		}
		cl.debugf("%s; falling back to the JSON API", cl.fallbackWhy)
	}
	return cl.json(in)
}

func main() {
	pwd, err := os.Getwd()
	if err != nil {
		pwd = os.Getenv("PWD")
	}
	home := os.Getenv("HOME")
	os.Exit(run(os.Args[1:], os.LookupEnv, pwd, home, os.Stdin, os.Stdout, os.Stderr))
}
