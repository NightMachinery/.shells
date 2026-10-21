package paper

import (
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"strings"
	"testing"
)

const botGate = `<!doctype html><html><title>Making sure you&#39;re not a bot!</title><script src="/.within.website/x/cmd/anubis/static/js/main.mjs"></script></html>`

func TestBlockedExportSkippedButMetadataAndCitationsStillRefresh(t *testing.T) {
	exports, metadata, doi := 0, 0, 0
	var notices []string
	c := fakeClient(Options{Scraper: "page-data", EnrichDBLP: true, FetchBibTeX: true}, func(r *http.Request) (int, string, map[string]string) {
		switch r.URL.Host {
		case "www.semanticscholar.org":
			return 200, pageJSON, nil
		case "api.semanticscholar.org":
			return 200, citationBatch(testID, "10.1234/test", metadataKey, nativeCitation), nil
		case "sparql.dblp.org":
			metadata++
			raw, _ := json.Marshal(map[string]any{"results": map[string]any{"bindings": recordRows()}})
			return 200, string(raw), nil
		case "dblp.org":
			exports++
			return 202, botGate, nil
		case "doi.org":
			doi++
			return 200, nativeCitation, nil
		}
		t.Fatal(r.URL)
		return 404, "", nil
	})
	c.Warnf = func(format string, args ...any) { notices = append(notices, format) }
	for i := 0; i < 2; i++ {
		m, err := c.Fetch(context.Background(), testURL)
		if err != nil || m.First("publisher") != "Example Publisher" || m.First("booktitle") != "Proceedings of Example Conference" || m.First("bibtexSource") != "doi" || BibTeX(m) != nativeCitation {
			t.Fatal(m, err)
		}
	}
	if exports != 1 || metadata != 2 || doi != 2 || len(notices) != 1 || !strings.Contains(notices[0], "SPARQL metadata is unaffected") {
		t.Fatal(exports, metadata, doi, notices)
	}
}
func TestMissingRecordDoesNotDisableExport(t *testing.T) {
	for _, code := range []int{200, 404} {
		calls := 0
		c := fakeClient(Options{}, func(*http.Request) (int, string, map[string]string) {
			calls++
			if calls == 1 {
				return code, "<html>Record not found</html>", nil
			}
			return 200, nativeCitation, nil
		})
		if _, err := c.dblpCitation(context.Background(), metadataKey); err == nil {
			t.Fatal("accepted a missing record")
		}
		if bib, err := c.dblpCitation(context.Background(), "conf/example/Other25"); err != nil || bib != nativeCitation || calls != 2 {
			t.Fatal(calls, bib, err)
		}
	}
}
func TestExportBlockResetsWithNewClientAndStillReportsFinalFailure(t *testing.T) {
	c := fakeClient(Options{BibTeXSource: "dblp"}, func(*http.Request) (int, string, map[string]string) { return 200, botGate, nil })
	for i := 0; i < 2; i++ {
		m := Metadata{"dblp": {metadataKey}}
		if err := c.fetchBibTeX(context.Background(), m); !errors.Is(err, errDBLPExportBlocked) {
			t.Fatal("hid a genuine citation failure", err)
		}
	}
	calls := 0
	fresh := fakeClient(Options{}, func(*http.Request) (int, string, map[string]string) { calls++; return 200, nativeCitation, nil })
	if bib, err := fresh.dblpCitation(context.Background(), metadataKey); err != nil || bib != nativeCitation || calls != 1 {
		t.Fatal(calls, bib, err)
	}
}
