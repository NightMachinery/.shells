package paper

import (
	"context"
	"encoding/json"
	"net/http"
	"strings"
	"testing"
)

const metadataKey = "conf/example/Test25"

func metadataRow(scope, predicate, kind, value string) sparqlRow {
	return sparqlRow{"record": {"uri", "https://dblp.org/rec/" + metadataKey}, "scope": {"literal", scope}, "p": {"uri", dblpSchema + predicate}, "o": {kind, value}}
}
func recordRows() []sparqlRow {
	rows := []sparqlRow{
		metadataRow("paper", "title", "literal", "Canonical publication title"), metadataRow("paper", "publishedIn", "literal", "ICLR"),
		metadataRow("paper", "publishedInBook", "literal", "ICLR"), metadataRow("paper", "pagination", "literal", "12-24"),
		metadataRow("paper", "yearOfPublication", "literal", "2025"), metadataRow("paper", "numberOfCreators", "literal", "2"),
		metadataRow("paper", "doi", "uri", "https://doi.org/10.1234/test"), metadataRow("paper", "bibtexType", "uri", "http://purl.org/net/nknouf/ns/bibtex#Inproceedings"),
		metadataRow("parent", "title", "literal", "Proceedings of Example Conference"), metadataRow("parent", "publishedBy", "literal", "Example Publisher"),
		metadataRow("parent", "isbn", "uri", "urn:isbn:9780000000000"),
	}
	for _, item := range []struct{ order, name string }{{"2", "Other Author"}, {"1", "Example Author"}} {
		row := metadataRow("author", "", "", "")
		row["ord"] = rdfValue{"literal", item.order}
		row["name"] = rdfValue{"literal", item.name}
		rows = append(rows, row)
	}
	return rows
}
func TestDBLPMetadataMergePreservesSSContent(t *testing.T) {
	m := Metadata{"title": {"Old title"}, "authors_names": {"Short Author"}, "abstract": {"Full SS abstract."}, "tldr": {"SS summary."}, "citationCount": {"18"}, "url": {testURL}, "publicationDate": {"2025-01-01"}}
	if err := mergeDBLP(m, metadataKey, recordRows()); err != nil {
		t.Fatal(err)
	}
	for key, want := range map[string]string{"title": "Canonical publication title", "venue": "ICLR", "booktitle": "Proceedings of Example Conference", "publisher": "Example Publisher", "pages": "12-24", "publicationType": "Inproceedings", "isbn": "9780000000000", "abstract": "Full SS abstract.", "tldr": "SS summary.", "citationCount": "18", "url": testURL, "publicationDate": "2025-01-01"} {
		if m.First(key) != want {
			t.Fatal(key, m)
		}
	}
	if strings.Join(m["authors_names"], ",") != "Example Author,Other Author" {
		t.Fatal(m)
	}
	m.Put("metadataFetchedAt", "2026-10-21T12:00:00Z")
	out, err := RenderOrgWithOptions(m, OrgOptions{NoBibTeX: true})
	if err != nil || !strings.Contains(out, ":publisher: Example Publisher") || !strings.Contains(out, ":dblp: https://dblp.org/rec/"+metadataKey) {
		t.Fatal(out, err)
	}
}
func TestDBLPMetadataRejectsMismatchesAndPreprintDowngrade(t *testing.T) {
	for _, bad := range []string{"id", "doi"} {
		rows := recordRows()
		m := Metadata{"title": {"Saved title"}, "doi": {"10.1234/test"}}
		if bad == "id" {
			rows[0]["record"] = rdfValue{"uri", "https://dblp.org/rec/conf/example/Wrong"}
		} else {
			m["doi"] = []string{"10.1234/other"}
		}
		if err := mergeDBLP(m, metadataKey, rows); err == nil || m.First("title") != "Saved title" {
			t.Fatal(bad, m, err)
		}
	}
	rows := recordRows()
	key := "journals/corr/abs-2505-00001"
	for i := range rows {
		rows[i]["record"] = rdfValue{"uri", "https://dblp.org/rec/" + key}
	}
	m := Metadata{"title": {"Published title"}, "venue": {"ICLR"}}
	if err := mergeDBLP(m, key, rows); err != nil || m.First("venue") != "ICLR" || m.First("title") != "Published title" {
		t.Fatal(m, err)
	}
}
func TestMetadataUsesPublishedDOIAndOrderedRecord(t *testing.T) {
	c := fakeClient(Options{Scraper: "page-data", EnrichDBLP: true}, func(r *http.Request) (int, string, map[string]string) {
		if strings.Contains(r.URL.Path, "/api/1/") {
			return 200, pageJSON, nil
		}
		if r.Method == "POST" {
			if r.URL.Query().Get("fields") != "externalIds" {
				t.Fatal("requested citations with BibTeX disabled", r.URL)
			}
			return 200, citationBatch(testID, "10.1234/test", "journals/corr/abs-2505-00001", ""), nil
		}
		if r.URL.Host != "sparql.dblp.org" {
			t.Fatal(r.URL)
		}
		if strings.Contains(r.URL.Query().Get("query"), "SELECT DISTINCT") {
			return 200, `{"results":{"bindings":[{"paper":{"type":"uri","value":"https://dblp.org/rec/conf/example/Test25"},"doi":{"type":"uri","value":"https://doi.org/10.1234/Test"}}]}}`, nil
		}
		raw, _ := json.Marshal(map[string]any{"results": map[string]any{"bindings": recordRows()}})
		return 200, string(raw), nil
	})
	m, err := c.Fetch(context.Background(), testURL)
	if err != nil || m.First("venue") != "ICLR" || m.First("booktitle") != "Proceedings of Example Conference" || BibTeX(m) != "" || m.First("abstract") != "The complete abstract." {
		t.Fatal(m, err)
	}
}
func TestDBLPMetadataFailureDoesNotFailSSFetch(t *testing.T) {
	c := fakeClient(Options{Scraper: "page-data", EnrichDBLP: true}, func(r *http.Request) (int, string, map[string]string) {
		if strings.Contains(r.URL.Path, "/api/1/") {
			return 200, pageJSON, nil
		}
		return 403, "", nil
	})
	warned := false
	c.Warnf = func(string, ...any) { warned = true }
	m, err := c.Fetch(context.Background(), testURL)
	if err != nil || !warned || m.First("title") != "Synthetic paper" {
		t.Fatal(m, err)
	}
}

func TestIncompleteDBLPAuthorsRetainOriginalNames(t *testing.T) {
	rows := recordRows()
	rows = rows[:len(rows)-1]
	m := Metadata{"authors_names": {"Original Author", "Another Author"}}
	if err := mergeDBLP(m, metadataKey, rows); err != nil || strings.Join(m["authors_names"], ",") != "Original Author,Another Author" {
		t.Fatal(m, err)
	}
}
