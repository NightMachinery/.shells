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
	"net"
	"net/http"
	"net/url"
	"os"
	"strings"
	"sync"
	"time"
)

// resolverSystem names the OS resolver in probe results. Any other resolver is
// named by the DNS server address it was given with -dns.
const resolverSystem = "system"

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
	Name string `json:"name"`
	URL  string `json:"url"`
	// Resolver is which DNS answered for this probe: "system", or a server
	// passed with -dns.
	Resolver string `json:"resolver"`
	State    string `json:"state"`
	Status   int    `json:"status,omitempty"`
	// Location is the login URL the network handed us, when it sent a redirect.
	Location string `json:"location,omitempty"`
	Detail   string `json:"detail,omitempty"`
}

type Result struct {
	State string `json:"state"`
	// URL is the page to open, set only when a probe was redirected to one.
	URL    string `json:"url,omitempty"`
	Via    string `json:"via,omitempty"`
	Detail string `json:"detail,omitempty"`
	// Resolvers is each resolver's own verdict, keyed like ProbeResult.Resolver.
	// The caller compares them: "portal" through the network's DNS next to
	// "blocked" through the system's means the system resolver is what this
	// network is blocking, and the browser will need the network's until the
	// login is done.
	Resolvers map[string]string `json:"resolvers,omitempty"`
	Probes    []ProbeResult     `json:"probes"`
}

// resolverClient pairs an HTTP client with the name of the DNS resolver it
// dials through, so results can say which one they came from.
type resolverClient struct {
	Name   string
	Client *http.Client
}

// newClient returns a client suitable for probing the network in front of us.
//
// Proxy is nil rather than http.ProxyFromEnvironment deliberately: through a
// proxy we would be measuring the proxy's connectivity, which says nothing
// about whether this Wi-Fi wants a login. Redirects are not followed either --
// a portal's redirect target is the very answer we want, and following it
// would replace that answer with whatever the login page happens to serve.
//
// dnsAddr, when non-empty, is a host:port DNS server that every hostname in a
// probe URL is resolved through, bypassing the OS resolver. Captive networks
// commonly block outside DNS until you log in while their own resolver keeps
// answering (often with the portal's own address for every name), so the OS
// resolver, when it is pinned to public servers, cannot see the portal at all.
func newClient(timeout time.Duration, dnsAddr string) *http.Client {
	tr := &http.Transport{
		Proxy:             nil,
		DisableKeepAlives: true,
	}
	if dnsAddr != "" {
		r := &net.Resolver{
			// The pure-Go resolver is the only one that honours Dial.
			PreferGo: true,
			Dial: func(ctx context.Context, network, _ string) (net.Conn, error) {
				var d net.Dialer
				return d.DialContext(ctx, network, dnsAddr)
			},
		}
		tr.DialContext = (&net.Dialer{Resolver: r}).DialContext
	}
	return &http.Client{
		Timeout:   timeout,
		Transport: tr,
		CheckRedirect: func(*http.Request, []*http.Request) error {
			return http.ErrUseLastResponse
		},
	}
}

// newClients builds the system-resolver client plus one per -dns server.
func newClients(timeout time.Duration, dnsServers []string) []resolverClient {
	out := []resolverClient{{Name: resolverSystem, Client: newClient(timeout, "")}}
	for _, s := range dnsServers {
		out = append(out, resolverClient{Name: s, Client: newClient(timeout, withDNSPort(s))})
	}
	return out
}

// withDNSPort appends :53 to a bare IP (v4 or v6) and leaves host:port alone.
func withDNSPort(s string) string {
	if _, _, err := net.SplitHostPort(s); err == nil {
		return s
	}
	return net.JoinHostPort(strings.Trim(s, "[]"), "53")
}

func runProbe(ctx context.Context, rc resolverClient, p probe) ProbeResult {
	c := rc.Client
	out := ProbeResult{Name: p.Name, URL: p.URL, Resolver: rc.Name}
	if rc.Name != resolverSystem {
		// Keep the names unique across resolvers, so a reader can tell
		// apple-through-the-network from apple-through-the-system at a glance.
		out.Name = p.Name + "@" + rc.Name
	}

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

// detect runs every probe once through every resolver, all at once.
func detect(ctx context.Context, cs []resolverClient, ps []probe) Result {
	results := make([]ProbeResult, len(cs)*len(ps))

	// Concurrent because every failure mode here is a timeout: a portal that
	// blackholes traffic makes each probe hang to its deadline, so probing in
	// sequence would cost the sum of the timeouts rather than the largest.
	var wg sync.WaitGroup
	for ci, rc := range cs {
		for pi, p := range ps {
			wg.Add(1)
			go func(i int, rc resolverClient, p probe) {
				defer wg.Done()
				results[i] = runProbe(ctx, rc, p)
			}(ci*len(ps)+pi, rc, p)
		}
	}
	wg.Wait()

	return aggregate(results)
}

// aggregate reduces the per-probe verdicts to one, and records each
// resolver's own verdict alongside.
//
// The overall verdict is taken across all resolvers, so a portal seen only
// through the network's DNS still wins: that is the whole point of asking it.
func aggregate(results []ProbeResult) Result {
	out := summarize(results)
	out.Probes = results

	byResolver := map[string][]ProbeResult{}
	for _, r := range results {
		byResolver[r.Resolver] = append(byResolver[r.Resolver], r)
	}
	if len(byResolver) > 0 {
		out.Resolvers = make(map[string]string, len(byResolver))
		for name, rs := range byResolver {
			out.Resolvers[name] = summarize(rs).State
		}
	}
	return out
}

// summarize reduces a set of probe verdicts to one state, URL and detail.
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
func summarize(results []ProbeResult) Result {
	var out Result

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
	var dns listFlag
	fs.Var(&dns, "dns", "also probe through this DNS server, an IP with optional :port (repeatable)")
	fs.Usage = func() {
		fmt.Fprint(fs.Output(), "usage: wifi-login-site detect [-timeout 5s] [-dns ip] [-probe name=url]\n\n"+
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
	return enc.Encode(detect(ctx, newClients(*timeout, dns), ps))
}

// listFlag collects a repeatable string flag.
type listFlag []string

func (f *listFlag) String() string { return strings.Join(*f, ",") }

func (f *listFlag) Set(v string) error {
	if v == "" {
		return fmt.Errorf("empty value")
	}
	*f = append(*f, v)
	return nil
}

func main() {
	if err := run(os.Args[1:]); err != nil {
		fmt.Fprintf(os.Stderr, "wifi-login-site: %v\n", err)
		os.Exit(1)
	}
}
