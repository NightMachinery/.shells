package paper

import (
	"context"
	"encoding/json"
	"io"
	"net/http"
	"strings"
	"testing"
)

func TestAnonymousCitationExport(t *testing.T) {
	calls := 0
	citation := "@inproceedings{Source2025, title={Synthetic paper}, booktitle={Example Conference}, year={2025}}"
	c := fakeClient(Options{Scraper: "page-data", FetchBibTeX: true, APIKey: "unused-secret", DeepSeekKey: "unused-secret"}, func(r *http.Request) (int, string, map[string]string) {
		calls++
		if calls == 1 {
			return 200, pageJSON, nil
		}
		if r.Method != "POST" || r.URL.String() != "https://api.semanticscholar.org/graph/v1/paper/batch?fields=externalIds,citationStyles" {
			t.Fatal(r.Method, r.URL)
		}
		if r.Header.Get("Authorization") != "" || r.Header.Get("x-api-key") != "" {
			t.Fatal("citation request leaked credentials")
		}
		body, _ := io.ReadAll(r.Body)
		var payload map[string][]string
		if json.Unmarshal(body, &payload) != nil || len(payload) != 1 || len(payload["ids"]) != 1 || payload["ids"][0] != testID {
			t.Fatalf("unexpected payload %s", body)
		}
		raw, _ := json.Marshal([]any{map[string]any{"paperId": testID, "citationStyles": map[string]string{"bibtex": citation}}})
		return 200, string(raw), nil
	})
	m, err := c.Fetch(context.Background(), testURL)
	if err != nil || calls != 2 || BibTeX(m) != citation {
		t.Fatal(calls, m, err)
	}
}
func TestExportFailureNeverFabricatesOrFailsMetadata(t *testing.T) {
	for _, response := range []string{`[null]`, `[]`, `[{"paperId":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb","citationStyles":{"bibtex":"@article{wrong, title={Wrong}}"}}]`, `[{"paperId":"` + testID + `","citationStyles":{"bibtex":""}}]`, `broken`} {
		calls := 0
		warned := false
		c := fakeClient(Options{Scraper: "page-data", FetchBibTeX: true}, func(r *http.Request) (int, string, map[string]string) {
			calls++
			if calls == 1 {
				return 200, pageJSON, nil
			}
			return 200, response, nil
		})
		c.Warnf = func(string, ...any) { warned = true }
		m, err := c.Fetch(context.Background(), testURL)
		if err != nil || !warned || BibTeX(m) != "" || m.First("metadataFetchedAt") == "" {
			t.Fatal(response, m, err)
		}
	}
}
func TestDisabledCitationExportMakesNoExtraRequest(t *testing.T) {
	calls := 0
	c := fakeClient(Options{Scraper: "page-data"}, func(*http.Request) (int, string, map[string]string) { calls++; return 200, pageJSON, nil })
	m, err := c.Fetch(context.Background(), testURL)
	if err != nil || calls != 1 || BibTeX(m) != "" {
		t.Fatal(calls, m, err)
	}
}
func TestGraphCitationStylesRetainedWithoutSecondRequest(t *testing.T) {
	raw := strings.TrimSuffix(apiJSON, "}") + `,"citationStyles":{"bibtex":"@article{NativeKey, title={API paper}}"}}`
	c := fakeClient(Options{Scraper: "api", FetchBibTeX: true, BibTeXSource: "semantic-scholar"}, func(r *http.Request) (int, string, map[string]string) {
		if r.Method != "GET" || !strings.Contains(r.URL.Query().Get("fields"), "citationStyles") {
			t.Fatal(r.Method, r.URL)
		}
		return 200, raw, nil
	})
	m, err := c.Fetch(context.Background(), testURL)
	if err != nil || BibTeX(m) != "@article{NativeKey, title={API paper}}" {
		t.Fatal(m, err)
	}
}
