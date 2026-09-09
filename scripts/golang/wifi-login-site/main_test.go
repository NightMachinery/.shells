package main

import (
	"context"
	"encoding/binary"
	"net"
	"net/http"
	"net/http/httptest"
	"net/url"
	"strings"
	"sync/atomic"
	"testing"
	"time"
)

// probeNamed returns the real probe by name, so tests exercise the production
// sentinels rather than a stand-in that could drift from them.
func probeNamed(t *testing.T, name string) probe {
	t.Helper()
	for _, p := range defaultProbes() {
		if p.Name == name {
			return p
		}
	}
	t.Fatalf("no probe named %q", name)
	return probe{}
}

// probeAgainst runs the named probe against url instead of its real endpoint.
func probeAgainst(t *testing.T, name, url string) ProbeResult {
	t.Helper()
	p := probeNamed(t, name)
	p.URL = url

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	return runProbe(ctx, resolverClient{Name: resolverSystem, Client: newClient(5*time.Second, "")}, p)
}

func TestProbeAbsoluteRedirect(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Location", "http://portal.example/login?mac=aa:bb")
		w.WriteHeader(http.StatusFound)
	}))
	defer srv.Close()

	got := probeAgainst(t, "apple", srv.URL)
	if got.State != StatePortal {
		t.Errorf("state = %q, want %q", got.State, StatePortal)
	}
	if want := "http://portal.example/login?mac=aa:bb"; got.Location != want {
		t.Errorf("location = %q, want %q", got.Location, want)
	}
}

// A portal that redirects to a path rather than a full URL still has to yield
// something a browser can open.
func TestProbeRelativeRedirect(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path == "/login" {
			w.WriteHeader(http.StatusOK)
			return
		}
		http.Redirect(w, r, "/login", http.StatusFound)
	}))
	defer srv.Close()

	got := probeAgainst(t, "apple", srv.URL+"/hotspot-detect.html")
	if got.State != StatePortal {
		t.Errorf("state = %q, want %q", got.State, StatePortal)
	}
	if want := srv.URL + "/login"; got.Location != want {
		t.Errorf("location = %q, want %q", got.Location, want)
	}
}

// The redirect must not be followed: its target is the answer, and following
// it would overwrite that answer with the login page's own response.
func TestProbeDoesNotFollowRedirect(t *testing.T) {
	var hits []string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		hits = append(hits, r.URL.Path)
		http.Redirect(w, r, "/login", http.StatusFound)
	}))
	defer srv.Close()

	probeAgainst(t, "apple", srv.URL+"/hotspot-detect.html")
	if len(hits) != 1 {
		t.Errorf("server saw %d requests (%s), want 1", len(hits), strings.Join(hits, ", "))
	}
}

func TestProbeSentinelMatch(t *testing.T) {
	cases := []struct {
		probe string
		serve http.HandlerFunc
	}{
		{"apple", func(w http.ResponseWriter, r *http.Request) {
			w.Write([]byte("<HTML><HEAD><TITLE>Success</TITLE></HEAD><BODY>Success</BODY></HTML>"))
		}},
		{"firefox", func(w http.ResponseWriter, r *http.Request) {
			w.Write([]byte("success\n"))
		}},
		{"gstatic", func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(http.StatusNoContent)
		}},
	}

	for _, tc := range cases {
		t.Run(tc.probe, func(t *testing.T) {
			srv := httptest.NewServer(tc.serve)
			defer srv.Close()

			if got := probeAgainst(t, tc.probe, srv.URL); got.State != StateOnline {
				t.Errorf("state = %q (detail %q), want %q", got.State, got.Detail, StateOnline)
			}
		})
	}
}

// A transparent hijack: 200, but not what the endpoint actually serves. There
// is no URL to extract, only the verdict.
func TestProbeHijackInPlace(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("<html><body><form>Accept our terms</form></body></html>"))
	}))
	defer srv.Close()

	got := probeAgainst(t, "apple", srv.URL)
	if got.State != StatePortal {
		t.Errorf("state = %q, want %q", got.State, StatePortal)
	}
	if got.Location != "" {
		t.Errorf("location = %q, want empty", got.Location)
	}
}

// A sentinel that arrives with the wrong status is still a hijack: gstatic
// answering 200 is not gstatic.
func TestProbeWrongStatus(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
	}))
	defer srv.Close()

	if got := probeAgainst(t, "gstatic", srv.URL); got.State != StatePortal {
		t.Errorf("state = %q, want %q", got.State, StatePortal)
	}
}

func TestProbeBlocked(t *testing.T) {
	// Close the server to get a port that is certainly free and refusing.
	srv := httptest.NewServer(http.HandlerFunc(func(http.ResponseWriter, *http.Request) {}))
	url := srv.URL
	srv.Close()

	got := probeAgainst(t, "apple", url)
	if got.State != StateBlocked {
		t.Errorf("state = %q, want %q", got.State, StateBlocked)
	}
	if got.Detail == "" {
		t.Error("detail is empty; the transport error should be carried through")
	}
}

func TestAggregatePrecedence(t *testing.T) {
	cases := []struct {
		name    string
		in      []ProbeResult
		want    string
		wantURL string
	}{
		{
			name: "all online",
			in: []ProbeResult{
				{Name: "apple", State: StateOnline},
				{Name: "firefox", State: StateOnline},
			},
			want: StateOnline,
		},
		{
			// The whitelisting case: Apple is let through to suppress the macOS
			// sheet while everything else is intercepted.
			name: "a named URL beats a passing probe",
			in: []ProbeResult{
				{Name: "apple", State: StateOnline},
				{Name: "firefox", State: StatePortal, Location: "http://portal.example/login"},
			},
			want:    StatePortal,
			wantURL: "http://portal.example/login",
		},
		{
			name: "interception with no URL still beats online",
			in: []ProbeResult{
				{Name: "apple", State: StateOnline},
				{Name: "firefox", State: StatePortal},
			},
			want: StatePortal,
		},
		{
			// Censorship or an outage, not a portal: positive evidence that
			// HTTP works outranks the absence of an answer elsewhere.
			name: "online beats blocked",
			in: []ProbeResult{
				{Name: "apple", State: StateOnline},
				{Name: "gstatic", State: StateBlocked, Detail: "no such host"},
			},
			want: StateOnline,
		},
		{
			name: "all blocked",
			in: []ProbeResult{
				{Name: "apple", State: StateBlocked},
				{Name: "firefox", State: StateBlocked},
			},
			want: StateBlocked,
		},
		{
			// A URL from any probe is preferred over none, whichever reported it.
			name: "URL is taken from whichever probe offered one",
			in: []ProbeResult{
				{Name: "apple", State: StatePortal},
				{Name: "firefox", State: StatePortal, Location: "http://portal.example/x"},
			},
			want:    StatePortal,
			wantURL: "http://portal.example/x",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := aggregate(tc.in)
			if got.State != tc.want {
				t.Errorf("state = %q, want %q", got.State, tc.want)
			}
			if got.URL != tc.wantURL {
				t.Errorf("url = %q, want %q", got.URL, tc.wantURL)
			}
		})
	}
}

func TestApplyOverrides(t *testing.T) {
	ps, err := applyOverrides(defaultProbes(), probeFlag{"apple": "http://127.0.0.1:1/x"})
	if err != nil {
		t.Fatalf("applyOverrides: %v", err)
	}
	if got := probeNamed(t, "apple").URL; ps[0].URL == got {
		t.Errorf("override did not take effect: still %q", got)
	}
	if ps[0].URL != "http://127.0.0.1:1/x" {
		t.Errorf("url = %q, want the override", ps[0].URL)
	}
	// The defaults must not be mutated: applyOverrides copies.
	if probeNamed(t, "apple").URL == ps[0].URL {
		t.Error("defaultProbes() was mutated by applyOverrides")
	}

	if _, err := applyOverrides(defaultProbes(), probeFlag{"nope": "http://x/"}); err == nil {
		t.Error("expected an error for an unknown probe name")
	}
}

func TestProbeFlagSet(t *testing.T) {
	f := probeFlag{}
	if err := f.Set("apple=http://x/"); err != nil {
		t.Errorf("Set: %v", err)
	}
	for _, bad := range []string{"apple", "=http://x/", "apple="} {
		if err := f.Set(bad); err == nil {
			t.Errorf("Set(%q) should have failed", bad)
		}
	}
}

// detect must run the probes concurrently: three probes that each hang until
// their deadline have to cost roughly one timeout, not three.
func TestDetectProbesConcurrently(t *testing.T) {
	block := make(chan struct{})

	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		<-block
	}))
	defer srv.Close()
	// Registered last, so it runs first: Close waits for outstanding handlers,
	// which cannot return until block is closed.
	defer close(block)

	ps := defaultProbes()
	for i := range ps {
		ps[i].URL = srv.URL
	}

	const timeout = 300 * time.Millisecond
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()

	start := time.Now()
	got := detect(ctx, newClients(timeout, nil), ps)
	elapsed := time.Since(start)

	if elapsed > 2*timeout {
		t.Errorf("detect took %v for %d probes at a %v timeout; probes look sequential",
			elapsed, len(ps), timeout)
	}
	if got.State != StateBlocked {
		t.Errorf("state = %q, want %q", got.State, StateBlocked)
	}
}

// fakeDNS is the smallest possible DNS server: it answers every A query with
// 127.0.0.1 and every other query with an empty NOERROR, so a hostname in a
// probe URL lands on the local httptest server -- but only when the client
// really does resolve through this server rather than the OS.
func fakeDNS(t *testing.T) (addr string, queries *int32) {
	t.Helper()
	pc, err := net.ListenPacket("udp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	t.Cleanup(func() { pc.Close() })

	var n int32
	go func() {
		buf := make([]byte, 512)
		for {
			ln, from, err := pc.ReadFrom(buf)
			if err != nil {
				return
			}
			q := buf[:ln]
			if len(q) < 12 {
				continue
			}
			atomic.AddInt32(&n, 1)

			// Walk the question name to find its end, then type and class.
			i := 12
			for i < len(q) && q[i] != 0 {
				i += int(q[i]) + 1
			}
			end := i + 5 // the zero label plus qtype and qclass
			if end > len(q) {
				continue
			}
			qtype := binary.BigEndian.Uint16(q[i+1 : i+3])

			resp := make([]byte, 0, end+16)
			resp = append(resp, q[:2]...)     // ID
			resp = append(resp, 0x81, 0x80)   // QR, RD, RA; NOERROR
			resp = append(resp, 0, 1)         // QDCOUNT
			resp = append(resp, 0, 0)         // ANCOUNT, patched below
			resp = append(resp, 0, 0, 0, 0)   // NSCOUNT, ARCOUNT
			resp = append(resp, q[12:end]...) // the question, verbatim
			if qtype == 1 {                   // A
				resp[7] = 1
				resp = append(resp,
					0xC0, 0x0C, // name: pointer to the question
					0, 1, 0, 1, // type A, class IN
					0, 0, 0, 60, // TTL
					0, 4, // RDLENGTH
					127, 0, 0, 1)
			}
			pc.WriteTo(resp, from)
		}
	}()
	return pc.LocalAddr().String(), &n
}

// hostURL swaps the 127.0.0.1 of an httptest URL for a hostname, so the
// request has to go through a resolver.
func hostURL(t *testing.T, srvURL string) string {
	t.Helper()
	u, err := url.Parse(srvURL)
	if err != nil {
		t.Fatalf("parse %q: %v", srvURL, err)
	}
	u.Host = "probe.test:" + u.Port()
	return u.String()
}

// A -dns resolver is really consulted, and the answer really is used.
func TestProbeThroughGivenDNS(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Location", "http://10.0.0.1/login")
		w.WriteHeader(http.StatusFound)
	}))
	defer srv.Close()

	dns, queries := fakeDNS(t)
	rc := resolverClient{Name: dns, Client: newClient(5*time.Second, dns)}
	p := probeNamed(t, "apple")
	p.URL = hostURL(t, srv.URL)

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	got := runProbe(ctx, rc, p)

	if got.State != StatePortal {
		t.Fatalf("state = %q (detail %q), want %q", got.State, got.Detail, StatePortal)
	}
	if got.Location != "http://10.0.0.1/login" {
		t.Errorf("location = %q", got.Location)
	}
	if got.Resolver != dns {
		t.Errorf("resolver = %q, want %q", got.Resolver, dns)
	}
	if want := "apple@" + dns; got.Name != want {
		t.Errorf("name = %q, want %q", got.Name, want)
	}
	if atomic.LoadInt32(queries) == 0 {
		t.Error("the fake DNS server saw no query; the OS resolver was used instead")
	}
}

// A -dns server that does not answer makes the hostname probes blocked, with
// the resolver error carried through, and never falls back to the OS resolver.
func TestProbeThroughDeadDNS(t *testing.T) {
	// A closed UDP port answers with ICMP unreachable, so this fails fast.
	pc, err := net.ListenPacket("udp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	dead := pc.LocalAddr().String()
	pc.Close()

	rc := resolverClient{Name: dead, Client: newClient(2*time.Second, dead)}
	p := probeNamed(t, "apple")
	p.URL = "http://probe.test:1/x"

	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()
	got := runProbe(ctx, rc, p)

	if got.State != StateBlocked {
		t.Errorf("state = %q, want %q", got.State, StateBlocked)
	}
	if !strings.Contains(got.Detail, "probe.test") {
		t.Errorf("detail %q should name the host that failed to resolve", got.Detail)
	}
}

// Every probe runs once per resolver, and each resolver gets its own verdict.
func TestDetectPerResolver(t *testing.T) {
	online := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusNoContent)
	}))
	defer online.Close()

	ps := []probe{probeNamed(t, "gstatic")}
	ps[0].URL = online.URL // an IP literal: no resolver involved, both succeed

	dns, _ := fakeDNS(t)
	cs := newClients(5*time.Second, []string{dns})

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	got := detect(ctx, cs, ps)

	if len(got.Probes) != 2 {
		t.Fatalf("got %d probe results, want 2", len(got.Probes))
	}
	if got.State != StateOnline {
		t.Errorf("state = %q, want %q", got.State, StateOnline)
	}
	for _, name := range []string{resolverSystem, dns} {
		if got.Resolvers[name] != StateOnline {
			t.Errorf("resolvers[%q] = %q, want %q", name, got.Resolvers[name], StateOnline)
		}
	}
	if len(got.Resolvers) != 2 {
		t.Errorf("resolvers = %v, want exactly two entries", got.Resolvers)
	}
}

// The case this exists for: the system resolver is dead, the network's sees the
// portal. The overall verdict must be portal, and the map must show the split.
func TestAggregateSplitResolvers(t *testing.T) {
	got := aggregate([]ProbeResult{
		{Name: "apple", Resolver: resolverSystem, State: StateBlocked, Detail: "i/o timeout"},
		{Name: "firefox", Resolver: resolverSystem, State: StateBlocked, Detail: "i/o timeout"},
		{Name: "apple@10.0.0.1", Resolver: "10.0.0.1", State: StatePortal, Location: "http://10.0.0.1/login"},
		{Name: "firefox@10.0.0.1", Resolver: "10.0.0.1", State: StatePortal},
	})
	if got.State != StatePortal || got.URL != "http://10.0.0.1/login" {
		t.Errorf("state/url = %q/%q, want portal with the login URL", got.State, got.URL)
	}
	if got.Via != "apple@10.0.0.1" {
		t.Errorf("via = %q", got.Via)
	}
	if got.Resolvers[resolverSystem] != StateBlocked {
		t.Errorf("resolvers[system] = %q, want blocked", got.Resolvers[resolverSystem])
	}
	if got.Resolvers["10.0.0.1"] != StatePortal {
		t.Errorf("resolvers[10.0.0.1] = %q, want portal", got.Resolvers["10.0.0.1"])
	}
}

func TestWithDNSPort(t *testing.T) {
	cases := map[string]string{
		"10.0.0.1":                  "10.0.0.1:53",
		"10.0.0.1:5353":             "10.0.0.1:5353",
		"2a01:599:904:408::66":      "[2a01:599:904:408::66]:53",
		"[2a01:599:904:408::66]:53": "[2a01:599:904:408::66]:53",
		"[2a01:599:904:408::66]":    "[2a01:599:904:408::66]:53",
	}
	for in, want := range cases {
		if got := withDNSPort(in); got != want {
			t.Errorf("withDNSPort(%q) = %q, want %q", in, got, want)
		}
	}
}

// Without -dns the output is what it always was: three probes, one resolver.
func TestNewClientsDefault(t *testing.T) {
	cs := newClients(time.Second, nil)
	if len(cs) != 1 || cs[0].Name != resolverSystem {
		t.Errorf("newClients(nil) = %d clients, first %q", len(cs), cs[0].Name)
	}
}
