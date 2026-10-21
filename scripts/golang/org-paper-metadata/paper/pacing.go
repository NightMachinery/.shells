package paper

import (
	"context"
	"fmt"
	"net/http"
	"strconv"
	"strings"
	"sync"
	"time"
)

// One budget covers each service throughout a conversion/update run. In
// particular, the Semantic Scholar website and Graph API share their budget.
type requestPacer struct {
	mu       sync.Mutex
	next     map[string]time.Time
	interval time.Duration
	now      func() time.Time
	sleep    func(context.Context, time.Duration) error
}

func newRequestPacer() *requestPacer {
	return &requestPacer{next: make(map[string]time.Time), interval: time.Second, now: time.Now, sleep: sleepContext}
}

func sleepContext(ctx context.Context, delay time.Duration) error {
	timer := time.NewTimer(delay)
	defer timer.Stop()
	select {
	case <-ctx.Done():
		return ctx.Err()
	case <-timer.C:
		return nil
	}
}

func service(host string) string {
	host = strings.ToLower(host)
	for _, domain := range []string{"semanticscholar.org", "dblp.org"} {
		if host == domain || strings.HasSuffix(host, "."+domain) {
			return domain
		}
	}
	return host
}

func (p *requestPacer) wait(ctx context.Context, host string) error {
	key := service(host)
	for {
		if err := ctx.Err(); err != nil {
			return err
		}
		p.mu.Lock()
		now := p.now()
		delay := p.next[key].Sub(now)
		if delay <= 0 {
			p.next[key] = now.Add(p.interval)
			p.mu.Unlock()
			return nil
		}
		p.mu.Unlock()
		// Do not reserve a future slot: another response may extend the cooldown
		// while we wait, and canceled callers must not delay later requests.
		if err := p.sleep(ctx, delay); err != nil {
			return err
		}
	}
}

func (p *requestPacer) pause(host string, delay time.Duration) {
	p.mu.Lock()
	defer p.mu.Unlock()
	key := service(host)
	until := p.now().Add(delay)
	if until.After(p.next[key]) {
		p.next[key] = until
	}
}

func retryDelay(header string, now time.Time, fallback time.Duration) time.Duration {
	header = strings.TrimSpace(header)
	// Check the bound before multiplying, so malformed headers cannot overflow.
	if seconds, err := strconv.ParseInt(header, 10, 64); err == nil && seconds > 0 {
		const maxDelay = time.Duration(1<<63 - 1)
		if seconds > int64(maxDelay/time.Second) {
			return maxDelay
		}
		if delay := time.Duration(seconds) * time.Second; delay > fallback {
			return delay
		}
	}
	if date, err := http.ParseTime(header); err == nil && date.Sub(now) > fallback {
		return date.Sub(now)
	}
	return fallback
}

// Wrapping RoundTrip rather than Do also paces HTTP redirects. The caller's
// transport, credential redirect checks, and HTTP timeout remain in effect.
type pacedTransport struct {
	base    http.RoundTripper
	pacer   *requestPacer
	backoff time.Duration
}

func (t pacedTransport) RoundTrip(req *http.Request) (*http.Response, error) {
	if err := t.pacer.wait(req.Context(), req.URL.Hostname()); err != nil {
		return nil, fmt.Errorf("waiting for %s request budget: %w", service(req.URL.Hostname()), err)
	}
	resp, err := t.base.RoundTrip(req)
	if err == nil && (resp.StatusCode == http.StatusTooManyRequests || resp.StatusCode >= 500) {
		// Record this even on the final attempt or a failed body read. The next
		// paper/fallback must not ignore an earlier rate-limit response.
		t.pacer.pause(req.URL.Hostname(), retryDelay(resp.Header.Get("Retry-After"), t.pacer.now(), t.backoff))
	}
	return resp, err
}
