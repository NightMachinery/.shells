package paper

import (
	"context"
	"errors"
	"io"
	"net/http"
	"reflect"
	"strings"
	"sync"
	"testing"
	"time"
)

func controlledPacer(c *Client) *time.Time {
	now := time.Date(2026, 10, 21, 12, 0, 0, 0, time.UTC)
	c.pacer.interval = time.Second
	c.pacer.now = func() time.Time { return now }
	c.pacer.sleep = func(ctx context.Context, delay time.Duration) error {
		if err := ctx.Err(); err != nil {
			return err
		}
		now = now.Add(delay)
		return nil
	}
	return &now
}

func TestRequestPacingIncludesRedirectsAndSharedScholarHosts(t *testing.T) {
	var c *Client
	var starts []time.Time
	c = fakeClient(Options{}, func(req *http.Request) (int, string, map[string]string) {
		starts = append(starts, c.pacer.now())
		if req.URL.Path == "/arXiv:2505.00001" {
			return 302, "", map[string]string{"Location": testURL}
		}
		return 200, "{}", nil
	})
	now := controlledPacer(c)
	start := *now
	for _, target := range []struct{ method, url string }{
		{http.MethodHead, "https://api.semanticscholar.org/arXiv:2505.00001"},
		{http.MethodGet, "https://www.semanticscholar.org/api/1/paper/" + testID},
		{http.MethodPost, "https://api.semanticscholar.org/graph/v1/paper/batch"},
		{http.MethodGet, "https://sparql.dblp.org/sparql"},
		{http.MethodGet, "https://dblp.org/rec/conf/example.bib"},
	} {
		if _, _, err := c.request(context.Background(), target.method, target.url, nil, nil); err != nil {
			t.Fatal(err)
		}
	}
	want := []time.Time{start, start.Add(time.Second), start.Add(2 * time.Second), start.Add(3 * time.Second), start.Add(3 * time.Second), start.Add(4 * time.Second)}
	if !reflect.DeepEqual(starts, want) {
		t.Fatalf("request starts %v; want %v", starts, want)
	}
}

func TestDefaultPacingSpacesActualRequests(t *testing.T) {
	c := NewClient(Options{})
	var starts []time.Time
	c.HTTP.Transport = transportFunc(func(req *http.Request) (*http.Response, error) {
		starts = append(starts, time.Now())
		return &http.Response{StatusCode: 200, Header: make(http.Header), Body: io.NopCloser(strings.NewReader("{}")), Request: req}, nil
	})
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	for _, target := range []string{"https://api.semanticscholar.org/graph/v1/paper/batch", testURL} {
		if _, _, err := c.request(ctx, http.MethodGet, target, nil, nil); err != nil {
			t.Fatal(err)
		}
	}
	// Allow scheduling noise between slot acquisition and the mock transport.
	if gap := starts[1].Sub(starts[0]); gap < time.Second-10*time.Millisecond {
		t.Fatalf("default request spacing = %s, want at least 1s", gap)
	}
}

func TestRateLimitRecoveryHonorsRetryAfterAndReplaysPost(t *testing.T) {
	for _, header := range []string{"7", "Wed, 21 Oct 2026 12:01:10 GMT"} {
		t.Run(header, func(t *testing.T) {
			var c *Client
			var starts []time.Time
			c = fakeClient(Options{}, func(req *http.Request) (int, string, map[string]string) {
				starts = append(starts, c.pacer.now())
				body, err := io.ReadAll(req.Body)
				if err != nil || string(body) != `{"ids":["`+testID+`"]}` || req.Header.Get("Content-Type") != "application/json" {
					t.Fatalf("retry lost body/headers: %q %v", body, err)
				}
				if len(starts) == 1 {
					return 429, "slow down", map[string]string{"Retry-After": header}
				}
				return 200, "success", nil
			})
			controlledPacer(c)
			var warnings []string
			c.Warnf = func(format string, args ...any) { warnings = append(warnings, format) }
			data, _, err := c.request(context.Background(), http.MethodPost, "https://api.semanticscholar.org/graph/v1/paper/batch", []byte(`{"ids":["`+testID+`"]}`), map[string]string{"Content-Type": "application/json"})
			if err != nil || string(data) != "success" || len(starts) != 2 || len(warnings) != 1 {
				t.Fatalf("recovery: %q %v %v %v", data, err, starts, warnings)
			}
			want := 7 * time.Second
			if header != "7" {
				want = 70 * time.Second // Long Retry-After is no longer rejected at 30s.
			}
			if got := starts[1].Sub(starts[0]); got != want {
				t.Fatalf("retry after %s, want %s", got, want)
			}
		})
	}
}

func TestRateLimitCooldownSurvivesCanceledRequest(t *testing.T) {
	calls := 0
	c := fakeClient(Options{}, func(req *http.Request) (int, string, map[string]string) {
		calls++
		if calls == 1 {
			return 429, "slow down", map[string]string{"Retry-After": "60"}
		}
		return 200, "success", nil
	})
	now := controlledPacer(c)
	start := *now
	ctx, cancel := context.WithCancel(context.Background())
	c.pacer.sleep = func(ctx context.Context, delay time.Duration) error {
		*now = now.Add(2 * time.Second)
		cancel()
		return ctx.Err()
	}
	_, _, err := c.request(ctx, http.MethodGet, "https://api.semanticscholar.org/graph/v1/paper/"+testID, nil, nil)
	if !errors.Is(err, context.Canceled) || !strings.Contains(err.Error(), "HTTP 429") || calls != 1 {
		t.Fatalf("cancel: %v, calls %d", err, calls)
	}
	c.pacer.sleep = func(ctx context.Context, delay time.Duration) error {
		*now = now.Add(delay)
		return ctx.Err()
	}
	if _, _, err := c.request(context.Background(), http.MethodGet, testURL, nil, nil); err != nil {
		t.Fatal(err)
	}
	if calls != 2 || now.Sub(start) != time.Minute {
		t.Fatalf("next Scholar host ignored cooldown: calls %d, elapsed %s", calls, now.Sub(start))
	}
}

func TestRetryExhaustionRetainsCooldown(t *testing.T) {
	for _, code := range []int{429, 503} {
		calls := 0
		c := fakeClient(Options{}, func(req *http.Request) (int, string, map[string]string) {
			calls++
			if calls <= 3 {
				return code, "retry later", nil
			}
			return 200, "success", nil
		})
		now := controlledPacer(c)
		start := *now
		if _, _, err := c.request(context.Background(), http.MethodGet, testURL, nil, nil); err == nil || calls != 3 {
			t.Fatalf("expected bounded retries for %d, got %v, calls %d", code, err, calls)
		}
		if now.Sub(start) != 3*time.Second {
			t.Fatalf("retry backoff = %s", now.Sub(start))
		}
		if _, _, err := c.request(context.Background(), http.MethodGet, testURL, nil, nil); err != nil {
			t.Fatal(err)
		}
		if now.Sub(start) != 7*time.Second {
			t.Fatalf("final attempt cooldown not retained: %s", now.Sub(start))
		}
	}
}

func TestHeadPaperResolutionRetriesRateLimit(t *testing.T) {
	calls := 0
	c := fakeClient(Options{}, func(req *http.Request) (int, string, map[string]string) {
		calls++
		if calls == 1 {
			return 429, "", nil
		}
		return 200, "", nil
	})
	controlledPacer(c)
	_, resp, err := c.request(context.Background(), http.MethodHead, testURL, nil, nil)
	if err != nil || calls != 2 || resp.StatusCode != 200 {
		t.Fatalf("HEAD treated rate limit as a resolved paper: calls %d, response %v, error %v", calls, resp, err)
	}
}

func TestRequestDeadlineDoesNotSendDuringCooldown(t *testing.T) {
	calls := 0
	c := fakeClient(Options{}, func(req *http.Request) (int, string, map[string]string) {
		calls++
		return 200, "", nil
	})
	c.pacer.pause("api.semanticscholar.org", time.Hour)
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Millisecond)
	defer cancel()
	_, _, err := c.request(ctx, http.MethodGet, testURL, nil, nil)
	if !errors.Is(err, context.DeadlineExceeded) || calls != 0 {
		t.Fatalf("sent during cooldown: calls %d, error %v", calls, err)
	}
}

func TestPacerRechecksExtendedCooldownAndDoesNotBlockOtherServices(t *testing.T) {
	c := NewClient(Options{})
	now := controlledPacer(c)
	start := *now
	p := c.pacer
	if err := p.wait(context.Background(), "www.semanticscholar.org"); err != nil {
		t.Fatal(err)
	}
	sleeps := 0
	p.sleep = func(ctx context.Context, delay time.Duration) error {
		sleeps++
		if sleeps == 1 {
			p.pause("api.semanticscholar.org", 10*time.Second)
			if err := p.wait(ctx, "doi.org"); err != nil {
				t.Fatal(err)
			}
		}
		*now = now.Add(delay)
		return nil
	}
	if err := p.wait(context.Background(), "api.semanticscholar.org"); err != nil {
		t.Fatal(err)
	}
	if sleeps != 2 || now.Sub(start) != 10*time.Second {
		t.Fatalf("extended cooldown not respected: sleeps %d, elapsed %s", sleeps, now.Sub(start))
	}
}

func TestPacerConcurrentWaitersAndDeadline(t *testing.T) {
	p := newRequestPacer()
	p.interval = 5 * time.Millisecond
	var wg sync.WaitGroup
	var mu sync.Mutex
	var reservations []time.Time
	// Observe reservations under the pacer mutex to avoid scheduling noise
	// between acquiring a slot and the caller recording its return time.
	p.now = func() time.Time {
		now := time.Now()
		mu.Lock()
		reservations = append(reservations, now)
		mu.Unlock()
		return now
	}
	for i := 0; i < 5; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			if err := p.wait(context.Background(), "api.semanticscholar.org"); err != nil {
				t.Error(err)
			}
		}()
	}
	wg.Wait()
	// Count successful reservations from their timestamps, excluding early
	// checks made by callers that still had to sleep.
	count := 0
	var last time.Time
	for _, now := range reservations {
		if count == 0 || now.Sub(last) >= p.interval {
			count++
			last = now
		}
	}
	if count != 5 {
		t.Fatalf("expected five spaced slots, got %d: %v", count, reservations)
	}
	p.pause("www.semanticscholar.org", time.Hour)
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Millisecond)
	defer cancel()
	if err := p.wait(ctx, "api.semanticscholar.org"); !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("deadline was not respected: %v", err)
	}
}

func TestRetryDelayInvalidAndOverflow(t *testing.T) {
	now := time.Now()
	for _, header := range []string{"", "garbage", "-1", "0", "Wed, 21 Oct 2015 07:28:00 GMT"} {
		if got := retryDelay(header, now, time.Second); got != time.Second {
			t.Errorf("%q: %s", header, got)
		}
	}
	if got := retryDelay("9223372036854775807", now, time.Second); got <= 0 {
		t.Fatalf("overflowed Retry-After: %s", got)
	}
}
