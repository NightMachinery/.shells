package paper

import (
	"context"
	"net/http"
	"strings"
	"testing"
)

func TestCredentialRedirectRefused(t *testing.T) {
	calls := 0
	c := fakeClient(Options{Scraper: "api", APIKey: "synthetic-key"}, func(r *http.Request) (int, string, map[string]string) {
		calls++
		if r.URL.Hostname() != "api.semanticscholar.org" {
			t.Fatal("sent credentials to redirect host")
		}
		return 302, "", map[string]string{"Location": "https://example.org/elsewhere"}
	})
	if _, err := c.Fetch(context.Background(), testURL); err == nil || !strings.Contains(err.Error(), "credentials") {
		t.Fatal(err)
	}
	if calls != 1 {
		t.Fatal(calls)
	}
}
func TestWrongAPIPaperRejected(t *testing.T) {
	c := fakeClient(Options{Scraper: "api"}, func(*http.Request) (int, string, map[string]string) {
		return 200, strings.ReplaceAll(apiJSON, testID, strings.Repeat("b", 40)), nil
	})
	if _, err := c.Fetch(context.Background(), testURL); err == nil {
		t.Fatal("accepted another paper's API response")
	}
}
func TestDeepSeekMissingAbstractFallsBackToPageData(t *testing.T) {
	var calls []string
	c := fakeClient(Options{Scraper: "deepseek", DeepSeekKey: "synthetic-key"}, func(r *http.Request) (int, string, map[string]string) {
		calls = append(calls, r.URL.Hostname())
		if r.URL.Hostname() == "api.deepseek.com" {
			return 200, `{"title":"Synthetic paper | Semantic Scholar","content":"## 48 References\n### Related paper\nRelated abstract"}`, nil
		}
		return 200, pageJSON, nil
	})
	m, err := c.Fetch(context.Background(), testURL)
	if err != nil || m.First("abstract") != "The complete abstract." {
		t.Fatal(m, err)
	}
	if strings.Join(calls, ",") != "api.deepseek.com,www.semanticscholar.org" {
		t.Fatal(calls)
	}
}
