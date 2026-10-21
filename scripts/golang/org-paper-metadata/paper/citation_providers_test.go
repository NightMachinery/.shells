package paper

import (
	"context"
	"encoding/json"
	"net/http"
	"reflect"
	"strings"
	"testing"
)

const nativeCitation = "@inproceedings{NativeKey, title={Synthetic paper}, booktitle={Example Conference}}"

func citationBatch(id, doi, key, bib string) string {
	raw, _ := json.Marshal([]any{map[string]any{"paperId": id, "externalIds": map[string]string{"DOI": doi, "DBLP": key}, "citationStyles": map[string]string{"bibtex": bib}}})
	return string(raw)
}
func TestCitationProviderOrderAndFallbacks(t *testing.T) {
	for _, success := range []string{"dblp", "doi", "semantic-scholar", "none"} {
		t.Run(success, func(t *testing.T) {
			var calls []string
			c := fakeClient(Options{Scraper: "page-data", FetchBibTeX: true, APIKey: "secret-sentinel"}, func(r *http.Request) (int, string, map[string]string) {
				if r.Header.Get("Authorization") != "" || r.Header.Get("x-api-key") != "" {
					t.Fatal("provider received credentials")
				}
				if strings.Contains(r.URL.Path, "/api/1/") {
					return 200, pageJSON, nil
				}
				if r.Method == "POST" {
					calls = append(calls, "ids")
					bib := nativeCitation
					if success == "none" {
						bib = ""
					}
					return 200, citationBatch(testID, "10.1234/synthetic", "conf/example/Synthetic25", bib), nil
				}
				provider := "doi"
				if r.URL.Host == "dblp.org" {
					provider = "dblp"
					if r.URL.Path != "/rec/conf/example/Synthetic25.bib" {
						t.Fatal(r.URL)
					}
				} else if r.URL.Host != "doi.org" || r.URL.Path != "/10.1234/synthetic" {
					t.Fatal(r.URL)
				}
				calls = append(calls, provider)
				if r.Header.Get("Accept") != "application/x-bibtex" {
					t.Fatal(r.Header)
				}
				if provider == success {
					return 200, nativeCitation, nil
				}
				return 200, "<html>Human verification</html>", nil
			})
			m, err := c.Fetch(context.Background(), testURL)
			want := []string{"ids", "dblp"}
			if success != "dblp" {
				want = append(want, "doi")
			}
			if err != nil || !reflect.DeepEqual(calls, want) {
				t.Fatal(calls, want, err)
			}
			if success == "none" {
				if BibTeX(m) != "" {
					t.Fatal(m)
				}
			} else if BibTeX(m) != nativeCitation || m.First("bibtexSource") != success {
				t.Fatal(m)
			}
		})
	}
}
func TestCitationIDsCheckedBeforeProviderRequests(t *testing.T) {
	for _, ids := range []string{
		citationBatch(strings.Repeat("b", 40), "10.1234/wrong", "conf/example/Wrong", nativeCitation),
		citationBatch(testID, "https://private.invalid/secret", "../../private", nativeCitation),
	} {
		c := fakeClient(Options{Scraper: "page-data", FetchBibTeX: true}, func(r *http.Request) (int, string, map[string]string) {
			if strings.Contains(r.URL.Path, "/api/1/") {
				return 200, pageJSON, nil
			}
			if r.Method != "POST" {
				t.Fatal("used an invalid external identifier", r.URL)
			}
			return 200, ids, nil
		})
		if _, err := c.Fetch(context.Background(), testURL); err != nil {
			t.Fatal(err)
		}
	}
}
func TestExplicitCitationProviderSkipsOthers(t *testing.T) {
	for _, provider := range []string{"dblp", "doi", "semantic-scholar"} {
		m := Metadata{"paperId": {testID}, "doi": {"10.1234/synthetic"}, "dblp": {"conf/example/Synthetic25"}, "bibtex": {nativeCitation}}
		var hosts []string
		c := fakeClient(Options{BibTeXSource: provider}, func(r *http.Request) (int, string, map[string]string) {
			hosts = append(hosts, r.URL.Host)
			return 200, nativeCitation, nil
		})
		if err := c.fetchBibTeX(context.Background(), m); err != nil {
			t.Fatal(err)
		}
		want := []string{}
		if provider == "dblp" {
			want = append(want, "dblp.org")
		}
		if provider == "doi" {
			want = append(want, "doi.org")
		}
		if strings.Join(hosts, ",") != strings.Join(want, ",") {
			t.Fatal(provider, hosts)
		}
	}
}
func TestIdentifierDiscoveryWithoutSSBibTeX(t *testing.T) {
	c := fakeClient(Options{Scraper: "page-data", FetchBibTeX: true}, func(r *http.Request) (int, string, map[string]string) {
		if strings.Contains(r.URL.Path, "/api/1/") {
			return 200, pageJSON, nil
		}
		if r.Method == "POST" {
			return 200, citationBatch(testID, "10.1234/synthetic", "", ""), nil
		}
		if r.URL.Host != "doi.org" {
			t.Fatal(r.URL)
		}
		return 200, nativeCitation, nil
	})
	m, err := c.Fetch(context.Background(), testURL)
	if err != nil || m.First("bibtexSource") != "doi" {
		t.Fatal(m, err)
	}
}
func TestProviderFailureKeepsSSFallback(t *testing.T) {
	c := fakeClient(Options{BibTeXSource: "auto"}, func(r *http.Request) (int, string, map[string]string) {
		if r.Method == "POST" {
			return 403, "", nil
		}
		return 404, "", nil
	})
	m := Metadata{"paperId": {testID}, "bibtex": {nativeCitation}, "doi": {"10.1234/synthetic"}}
	if err := c.fetchBibTeX(context.Background(), m); err != nil || BibTeX(m) != nativeCitation || m.First("bibtexSource") != "semantic-scholar" {
		t.Fatal(m, err)
	}
}
func TestCitationIdentifierNormalization(t *testing.T) {
	for in, want := range map[string]string{"10.1234/Example": "10.1234/example", "https://doi.org/10.1234/Example": "10.1234/example", "doi:10.1234/Example": "10.1234/example", "https://private.invalid/10.1234/x": "", "10.1234/x\nprivate": "", "https://doi.org@private.invalid/10.1234/x": ""} {
		if got := normalizeDOI(in); got != want {
			t.Fatal(in, got, want)
		}
	}
	for in, want := range map[string]string{"conf/example/Test25": "conf/example/Test25", "https://dblp.org/rec/conf/example/Test25.html": "conf/example/Test25", "https://dblp.uni-trier.de/rec/conf/example/Test25.bib": "conf/example/Test25", "../../private": "", "conf/../private": "", "https://private.invalid/rec/conf/example/Test25": ""} {
		if got := normalizeDBLP(in); got != want {
			t.Fatal(in, got, want)
		}
	}
}
func TestPageDataAndGraphExternalIDs(t *testing.T) {
	raw := strings.Replace(pageJSON, `"paper":{`, `"paper":{"externalIds":{"DOI":"10.1234/Test","DBLP":"conf/example/Test25"},`, 1)
	m, err := ParsePageData([]byte(raw), testURL)
	if err != nil || m.First("doi") != "10.1234/test" || m.First("dblp") != "conf/example/Test25" {
		t.Fatal(m, err)
	}
	raw = strings.Replace(apiJSON, `"externalIds":{`, `"externalIds":{"DBLP":"conf/example/Test25",`, 1)
	m, err = ParseJSON([]byte(raw), testURL)
	if err != nil || m.First("dblp") != "conf/example/Test25" {
		t.Fatal(m, err)
	}
	m = Metadata{}
	m.AddLink("https://doi.org/10.1234/Test", false)
	m.AddLink("https://dblp.org/rec/conf/example/Test25.html", false)
	if m.First("doi") != "10.1234/test" || m.First("dblp") != "conf/example/Test25" {
		t.Fatal(m)
	}
}

func TestPublishedDOIPrecedesCoRRRecord(t *testing.T) {
	c := fakeClient(Options{BibTeXSource: "auto"}, func(r *http.Request) (int, string, map[string]string) {
		if r.Method == "POST" {
			return 200, citationBatch(testID, "10.1234/published", "journals/corr/abs-2505-00001", nativeCitation), nil
		}
		if r.URL.Host != "doi.org" {
			t.Fatal("preferred a preprint to a published DOI", r.URL)
		}
		return 200, nativeCitation, nil
	})
	m := Metadata{"paperId": {testID}}
	if err := c.fetchBibTeX(context.Background(), m); err != nil || m.First("bibtexSource") != "doi" {
		t.Fatal(m, err)
	}
}
func TestDeepSeekDBLPRecoveryIsSourceOnly(t *testing.T) {
	key := "conf/example/Test25"
	bib := "@inproceedings{DBLP:" + key + ",\n title={Synthetic paper},\n booktitle={Example Conference}\n}"
	c := fakeClient(Options{BibTeXSource: "dblp", DeepSeekKey: "key-sentinel"}, func(r *http.Request) (int, string, map[string]string) {
		if r.URL.Host == "dblp.org" {
			if r.Header.Get("Authorization") != "" {
				t.Fatal("key sent to DBLP")
			}
			return 200, "<html>Anubis</html>", nil
		}
		if r.URL.String() != "https://api.deepseek.com/v1/tools/fetch" || r.Method != "POST" || r.Header.Get("Authorization") != "Bearer key-sentinel" {
			t.Fatal(r.URL, r.Method)
		}
		var body map[string]string
		if json.NewDecoder(r.Body).Decode(&body) != nil || len(body) != 1 || body["url"] != "https://dblp.org/rec/"+key+".bib" {
			t.Fatal(body)
		}
		raw, _ := json.Marshal(map[string]string{"title": "DBLP export", "content": "```bibtex\n" + bib + "\n```"})
		return 200, string(raw), nil
	})
	m := Metadata{"paperId": {testID}, "dblp": {key}}
	if err := c.fetchBibTeX(context.Background(), m); err != nil || BibTeX(m) != bib || m.First("bibtexSource") != "dblp" {
		t.Fatal(m, err)
	}
}
func TestFetchBibTeXRejectsWrongKeyAndTruncation(t *testing.T) {
	for _, content := range []string{"<html>Anubis</html>", "@article{DBLP:conf/example/Wrong, title={Wrong}}", "@article{DBLP:conf/example/Test25, title={Truncated}"} {
		if _, err := fetchedBibTeX(content, "conf/example/Test25"); err == nil {
			t.Fatal(content)
		}
	}
}

func TestDOIPrecedesDeepSeekRecovery(t *testing.T) {
	c := fakeClient(Options{BibTeXSource: "auto", DeepSeekKey: "key-sentinel"}, func(r *http.Request) (int, string, map[string]string) {
		switch r.URL.Host {
		case "api.semanticscholar.org":
			return 200, citationBatch(testID, "10.1234/synthetic", "conf/example/Test25", nativeCitation), nil
		case "dblp.org":
			if r.Header.Get("Authorization") != "" {
				t.Fatal("credential leak")
			}
			return 200, "<html>Anubis</html>", nil
		case "api.deepseek.com":
			if r.Header.Get("Authorization") != "Bearer key-sentinel" {
				t.Fatal("missing Fetch credential")
			}
			return 200, `{"title":"Access denied","content":"Access denied"}`, nil
		case "doi.org":
			if r.Header.Get("Authorization") != "" {
				t.Fatal("credential leak")
			}
			return 200, nativeCitation, nil
		}
		t.Fatal(r.URL)
		return 404, "", nil
	})
	m := Metadata{"paperId": {testID}}
	if err := c.fetchBibTeX(context.Background(), m); err != nil || m.First("bibtexSource") != "doi" {
		t.Fatal(m, err)
	}
}

func TestOptInBrowserRecoversSourceCitation(t *testing.T) {
	key := "conf/example/Test25"
	bib := "@inproceedings{DBLP:" + key + ",\n title={Synthetic & source},\n booktitle={Example Conference}\n}"
	c := fakeClient(Options{BibTeXSource: "dblp", BibTeXBrowser: true}, func(r *http.Request) (int, string, map[string]string) { return 200, "<html>Anubis</html>", nil })
	calls := 0
	c.LoadHTML = func(ctx context.Context, page string) ([]byte, error) {
		calls++
		if page != "https://dblp.uni-trier.de/rec/"+key+".html?view=bibtex" {
			t.Fatal(page)
		}
		return []byte("<html><pre class=verbatim>" + strings.ReplaceAll(bib, "&", "&amp;") + "</pre></html>"), nil
	}
	m := Metadata{"paperId": {testID}, "dblp": {key}}
	if err := c.fetchBibTeX(context.Background(), m); err != nil || BibTeX(m) != bib || calls != 1 {
		t.Fatal(m, calls, err)
	}
	c.Options.BibTeXBrowser = false
	if err := c.fetchBibTeX(context.Background(), m); err == nil || calls != 1 {
		t.Fatal("browser was not opt-in", calls, err)
	}
}

func TestProgrammaticSourcesPrecedeExpensiveRecovery(t *testing.T) {
	var calls []string
	c := fakeClient(Options{BibTeXSource: "auto", DeepSeekKey: "unused", BibTeXBrowser: true}, func(r *http.Request) (int, string, map[string]string) {
		calls = append(calls, r.URL.Host)
		switch r.URL.Host {
		case "api.semanticscholar.org":
			return 200, citationBatch(testID, "10.1234/test", "conf/example/Test25", nativeCitation), nil
		case "dblp.org", "doi.org":
			return 404, "", nil
		default:
			t.Fatal("expensive provider ran before cached SS citation", r.URL)
		}
		return 404, "", nil
	})
	c.LoadHTML = func(context.Context, string) ([]byte, error) { t.Fatal("browser was not last resort"); return nil, nil }
	m := Metadata{"paperId": {testID}}
	if err := c.fetchBibTeX(context.Background(), m); err != nil || m.First("bibtexSource") != "semantic-scholar" || len(calls) != 3 {
		t.Fatal(m, calls, err)
	}
}
func TestExactSPARQLDOILookupBeforeFallback(t *testing.T) {
	c := fakeClient(Options{BibTeXSource: "auto"}, func(r *http.Request) (int, string, map[string]string) {
		if r.Header.Get("Authorization") != "" || r.Header.Get("x-api-key") != "" {
			t.Fatal("credential leak")
		}
		switch r.URL.Host {
		case "api.semanticscholar.org":
			return 200, citationBatch(testID, "", "conf/example/Test25", nativeCitation), nil
		case "dblp.org":
			return 403, "", nil
		case "sparql.dblp.org":
			if r.URL.Query().Get("query") != "SELECT ?doi WHERE { <https://dblp.org/rec/conf/example/Test25> <https://dblp.org/rdf/schema#doi> ?doi } LIMIT 2" {
				t.Fatal(r.URL)
			}
			return 200, `{"results":{"bindings":[{"doi":{"type":"uri","value":"https://doi.org/10.1234/Test"}}]}}`, nil
		case "doi.org":
			if r.URL.Path != "/10.1234/test" {
				t.Fatal(r.URL)
			}
			return 200, nativeCitation, nil
		}
		t.Fatal(r.URL)
		return 404, "", nil
	})
	m := Metadata{"paperId": {testID}}
	if err := c.fetchBibTeX(context.Background(), m); err != nil || m.First("bibtexSource") != "doi" || m.First("doi") != "10.1234/test" {
		t.Fatal(m, err)
	}
}
