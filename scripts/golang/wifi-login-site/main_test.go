package main

import (
	"context"
	"net/http"
	"net/http/httptest"
	"strings"
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

	return runProbe(ctx, newClient(5*time.Second), p)
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
	got := detect(ctx, newClient(timeout), ps)
	elapsed := time.Since(start)

	if elapsed > 2*timeout {
		t.Errorf("detect took %v for %d probes at a %v timeout; probes look sequential",
			elapsed, len(ps), timeout)
	}
	if got.State != StateBlocked {
		t.Errorf("state = %q, want %q", got.State, StateBlocked)
	}
}
