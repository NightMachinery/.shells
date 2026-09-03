// Command wifi-login-site detects whether this network is behind a captive
// portal -- the kind of public Wi-Fi that intercepts all HTTP until you click
// a button on a login page -- and reports the URL to open, when the network
// bothers to name one.
//
// It only probes. Policy (which URL to fall back on, whether to open a
// browser) lives in zsh; see wifi-login-site-open in
// zshlang/auto-load/others/network.zsh.
//
// Stdlib only, on purpose: the first run of this tool happens exactly when
// there is no internet, so it must build with nothing to download.
package main

import (
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"
	"strings"
	"sync"
	"time"
)

// The aggregate verdict, and each probe's own verdict.
const (
	StateOnline  = "online"  // HTTP is not intercepted; we can reach the internet.
	StatePortal  = "portal"  // HTTP is intercepted. The login URL may or may not be known.
	StateBlocked = "blocked" // Nothing answered at all; not even an interceptor.
	StateUnknown = "unknown"
)

// maxBody caps how much of a response we read. Every sentinel is tiny, so a
// portal that streams a whole login page at us must not be able to make this
// hang or balloon.
const maxBody = 64 << 10

type probe struct {
	Name string
	URL  string
	// ok reports whether this response is the untouched sentinel, i.e. we
	// reached the real endpoint rather than something answering for it.
	ok func(status int, body []byte) bool
}

// Three endpoints rather than one, because portals routinely whitelist
// captive.apple.com specifically to stop macOS from raising its "sign in to
// Wi-Fi" sheet -- and macOS hits it constantly anyway. A portal-free answer
// from Apple alone is therefore the least trustworthy of the three.
func defaultProbes() []probe {
	return []probe{
		{
			Name: "apple",
			URL:  "http://captive.apple.com/hotspot-detect.html",
			ok: func(status int, body []byte) bool {
				return status == http.StatusOK &&
					strings.Contains(string(body), "<TITLE>Success</TITLE>")
			},
		},
		{
			Name: "firefox",
			URL:  "http://detectportal.firefox.com/success.txt",
			ok: func(status int, body []byte) bool {
				return status == http.StatusOK &&
					strings.TrimSpace(string(body)) == "success"
			},
		},
		{
			Name: "gstatic",
			URL:  "http://connectivitycheck.gstatic.com/generate_204",
			ok: func(status int, body []byte) bool {
				return status == http.StatusNoContent && len(body) == 0
			},
		},
	}
}

type ProbeResult struct {
	Name   string `json:"name"`
	URL    string `json:"url"`
	State  string `json:"state"`
	Status int    `json:"status,omitempty"`
	// Location is the login URL the network handed us, when it sent a redirect.
	Location string `json:"location,omitempty"`
	Detail   string `json:"detail,omitempty"`
}

type Result struct {
	State string `json:"state"`
	// URL is the page to open, set only when a probe was redirected to one.
	URL    string        `json:"url,omitempty"`
	Via    string        `json:"via,omitempty"`
	Detail string        `json:"detail,omitempty"`
	Probes []ProbeResult `json:"probes"`
}

// newClient returns a client suitable for probing the network in front of us.
//
// Proxy is nil rather than http.ProxyFromEnvironment deliberately: through a
// proxy we would be measuring the proxy's connectivity, which says nothing
// about whether this Wi-Fi wants a login. Redirects are not followed either --
// a portal's redirect target is the very answer we want, and following it
// would replace that answer with whatever the login page happens to serve.
func newClient(timeout time.Duration) *http.Client {
	return &http.Client{
		Timeout: timeout,
		Transport: &http.Transport{
			Proxy:             nil,
			DisableKeepAlives: true,
		},
		CheckRedirect: func(*http.Request, []*http.Request) error {
			return http.ErrUseLastResponse
		},
	}
}

func runProbe(ctx context.Context, c *http.Client, p probe) ProbeResult {
	out := ProbeResult{Name: p.Name, URL: p.URL}

	req, err := http.NewRequestWithContext(ctx, http.MethodGet, p.URL, nil)
	if err != nil {
		out.State, out.Detail = StateUnknown, err.Error()
		return out
	}
	// Portals serve different things to different agents, and some let the OS
	// probes through untouched. Ask as the OS probe rather than as Go.
	req.Header.Set("User-Agent", "CaptiveNetworkSupport/1.0 wispr")
	req.Header.Set("Cache-Control", "no-store")

	resp, err := c.Do(req)
	if err != nil {
		// Nothing answered: DNS failure, connection refused, or timeout. HTTP
		// is not being intercepted, so there is no login URL to find here.
		out.State, out.Detail = StateBlocked, err.Error()
		return out
	}
	defer resp.Body.Close()

	out.Status = resp.StatusCode
	body, err := io.ReadAll(io.LimitReader(resp.Body, maxBody))
	if err != nil {
		out.State, out.Detail = StateBlocked, err.Error()
		return out
	}

	loc := resp.Header.Get("Location")
	if resp.StatusCode >= 300 && resp.StatusCode < 400 && loc != "" {
		// The good case: the network told us exactly where to log in.
		out.State = StatePortal
		out.Location = absolutize(p.URL, loc)
		return out
	}

	if p.ok(resp.StatusCode, body) {
		out.State = StateOnline
		return out
	}

	// Something answered on the real endpoint's behalf: a transparent in-place
	// hijack. There is no URL to extract, only the knowledge that opening any
	// plain-HTTP page will land on the portal.
	out.State = StatePortal
	out.Detail = "response did not match this endpoint's sentinel"
	return out
}

// absolutize resolves a possibly-relative Location against the request URL.
// Portals send both forms.
func absolutize(base, loc string) string {
	b, err := url.Parse(base)
	if err != nil {
		return loc
	}
	l, err := b.Parse(loc)
	if err != nil {
		return loc
	}
	return l.String()
}

func detect(ctx context.Context, c *http.Client, ps []probe) Result {
	results := make([]ProbeResult, len(ps))

	// Concurrent because every failure mode here is a timeout: a portal that
	// blackholes traffic makes each probe hang to its deadline, so probing in
	// sequence would cost the sum of the timeouts rather than the largest.
	var wg sync.WaitGroup
	for i, p := range ps {
		wg.Add(1)
		go func(i int, p probe) {
			defer wg.Done()
			results[i] = runProbe(ctx, c, p)
		}(i, p)
	}
	wg.Wait()

	return aggregate(results)
}

// aggregate reduces the per-probe verdicts to one.
//
// Precedence: a portal that named its login URL, then a portal that did not,
// then online, then blocked.
//
// Portal outranks online because of the whitelisting above: one probe
// succeeding while another is demonstrably intercepted means the whitelist is
// lying, not that we are online.
//
// Online outranks blocked for the opposite reason. A probe can fail on a
// perfectly healthy network -- gstatic and detectportal are both reachable
// only where Google and Mozilla are, which is not everywhere -- so positive
// evidence that HTTP works beats the absence of an answer elsewhere. Reading
// online+blocked as a portal would misfire under plain censorship.
func aggregate(results []ProbeResult) Result {
	out := Result{Probes: results}

	var online, portal, blocked int
	for _, r := range results {
		switch r.State {
		case StateOnline:
			online++
		case StatePortal:
			portal++
			if r.Location != "" && out.URL == "" {
				out.URL, out.Via = r.Location, r.Name
			}
		case StateBlocked:
			blocked++
		}
	}

	switch {
	case out.URL != "":
		out.State = StatePortal
	case portal > 0:
		out.State = StatePortal
		out.Detail = "HTTP is intercepted but no login URL was offered; open any plain-HTTP page"
	case online > 0:
		out.State = StateOnline
	case blocked > 0:
		out.State = StateBlocked
		out.Detail = "no probe reached a server, and none was intercepted either"
	default:
		out.State = StateUnknown
	}
	return out
}

// probeFlag collects -probe name=url overrides. It exists for testability:
// pointing a probe at a local server is how the portal shapes get exercised
// without a real portal, and without touching /etc/hosts.
type probeFlag map[string]string

func (f probeFlag) String() string { return "" }

func (f probeFlag) Set(v string) error {
	name, u, found := strings.Cut(v, "=")
	if !found || name == "" || u == "" {
		return fmt.Errorf("-probe wants name=url, got %q", v)
	}
	f[name] = u
	return nil
}

func applyOverrides(ps []probe, over probeFlag) ([]probe, error) {
	out := make([]probe, len(ps))
	copy(out, ps)

	for name, u := range over {
		found := false
		for i := range out {
			if out[i].Name == name {
				out[i].URL, found = u, true
				break
			}
		}
		if !found {
			names := make([]string, 0, len(out))
			for _, p := range out {
				names = append(names, p.Name)
			}
			return nil, fmt.Errorf("no probe named %q; have %s", name, strings.Join(names, ", "))
		}
	}
	return out, nil
}

func run(args []string) error {
	cmd := "detect"
	if len(args) > 0 && !strings.HasPrefix(args[0], "-") {
		cmd, args = args[0], args[1:]
	}
	if cmd != "detect" {
		return fmt.Errorf("unknown subcommand %q; only \"detect\"", cmd)
	}

	fs := flag.NewFlagSet("wifi-login-site", flag.ContinueOnError)
	timeout := fs.Duration("timeout", 5*time.Second, "give up on a probe after this long")
	over := probeFlag{}
	fs.Var(&over, "probe", "override a probe's URL, as name=url (repeatable; for testing)")
	fs.Usage = func() {
		fmt.Fprint(fs.Output(), "usage: wifi-login-site detect [-timeout 5s] [-probe name=url]\n\n"+
			"Reports, as JSON, whether this network is behind a captive portal\n"+
			"and which URL to open to log in.\n\n")
		fs.PrintDefaults()
	}
	if err := fs.Parse(args); err != nil {
		return err
	}
	if fs.NArg() > 0 {
		return fmt.Errorf("unexpected argument %q", fs.Arg(0))
	}

	ps, err := applyOverrides(defaultProbes(), over)
	if err != nil {
		return err
	}

	ctx, cancel := context.WithTimeout(context.Background(), *timeout)
	defer cancel()

	// The state is in the payload, not the exit code: "online" is a successful
	// detection, not a failure of this command.
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	return enc.Encode(detect(ctx, newClient(*timeout), ps))
}

func main() {
	if err := run(os.Args[1:]); err != nil {
		fmt.Fprintf(os.Stderr, "wifi-login-site: %v\n", err)
		os.Exit(1)
	}
}
