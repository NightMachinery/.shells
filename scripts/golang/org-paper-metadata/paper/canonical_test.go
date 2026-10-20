package paper

import (
	"context"
	"errors"
	"net/http"
	"strings"
	"testing"
	"time"
)

func TestCanonicalPageDataRedirect(t *testing.T) {
	oldID := strings.Repeat("b", 40)
	var paths []string
	c := fakeClient(Options{Scraper: "auto"}, func(r *http.Request) (int, string, map[string]string) {
		paths = append(paths, r.URL.Path)
		if strings.HasSuffix(r.URL.Path, oldID) {
			return 200, `{"responseType":"CANONICAL","canonicalId":"` + testID + `","canonicalSlug":"Synthetic-paper"}`, nil
		}
		return 200, pageJSON, nil
	})
	m, err := c.Fetch(context.Background(), "https://www.semanticscholar.org/paper/"+oldID)
	if err != nil || m.First("paperId") != testID || m.First("url") != testURL {
		t.Fatal(m, err)
	}
	if len(paths) != 2 {
		t.Fatal(paths)
	}
}
func TestCanonicalLoopsAndInvalidIDsFail(t *testing.T) {
	for _, id := range []string{testID, "not-an-id", "https://example.org/"} {
		c := fakeClient(Options{Scraper: "page-data"}, func(*http.Request) (int, string, map[string]string) {
			return 200, `{"responseType":"CANONICAL","canonicalId":"` + id + `"}`, nil
		})
		if _, err := c.Fetch(context.Background(), testURL); err == nil {
			t.Fatal("accepted invalid/cyclic canonical ID", id)
		}
	}
}
func TestAutoNeverStartsHTML(t *testing.T) {
	c := fakeClient(Options{Scraper: "auto", APIEnabled: true, DeepSeekKey: "synthetic-key"}, func(*http.Request) (int, string, map[string]string) { return 403, "", nil })
	c.LoadHTML = func(context.Context, string) ([]byte, error) {
		t.Fatal("automatic HTML fallback started")
		return nil, nil
	}
	if _, err := c.Fetch(context.Background(), testURL); err == nil {
		t.Fatal("expected retrieval failure")
	}
}
func TestExplicitHTMLStillWorks(t *testing.T) {
	c := fakeClient(Options{Scraper: "full-html2"}, func(*http.Request) (int, string, map[string]string) {
		t.Fatal("unexpected HTTP request")
		return 0, "", nil
	})
	c.LoadHTML = func(context.Context, string) ([]byte, error) { return []byte(htmlFixture), nil }
	if m, err := c.Fetch(context.Background(), testURL); err != nil || m.First("metadataSource") != "semantic-scholar-html" {
		t.Fatal(m, err)
	}
}
func TestCancellationReturnsWithoutFallback(t *testing.T) {
	calls := 0
	c := NewClient(Options{Scraper: "auto", APIEnabled: true, DeepSeekKey: "synthetic-key"})
	c.HTTP.Transport = transportFunc(func(r *http.Request) (*http.Response, error) {
		calls++
		<-r.Context().Done()
		return nil, r.Context().Err()
	})
	c.LoadHTML = func(context.Context, string) ([]byte, error) { t.Fatal("started HTML"); return nil, nil }
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Millisecond)
	defer cancel()
	if _, err := c.Fetch(ctx, testURL); !errors.Is(err, context.DeadlineExceeded) || calls != 1 {
		t.Fatal(calls, err)
	}
}
