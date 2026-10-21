package paper

import (
	"context"
	"encoding/json"
	"io"
	"net/http"
	"strings"
	"testing"
	"time"
)

const testID = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
const testURL = "https://www.semanticscholar.org/paper/" + testID
const pageJSON = `{"paper":{"id":"` + testID + `","title":{"text":"Synthetic paper"},"paperAbstract":{"text":"The complete abstract."},"paperAbstractTruncated":"The complete...","authors":[[{"name":"Example Author"},{"text":"Example Author"}],[{"name":"Other Author"},{"text":"Other Author"}]],"year":{"text":"2025"},"pubDate":"2025-05-01","pubUpdateDate":"2025-06-02","corpusId":12345,"citationStats":{"numCitations":0,"numReferences":48,"numKeyCitations":0,"numKeyReferences":4},"tldr":{"text":"A supplied summary."},"badges":[{"id":"OPEN_ACCESS"}],"fieldsOfStudy":["Computer Science"],"links":[{"url":"https://arxiv.org/pdf/2505.00001.pdf","linkType":"arxiv"}],"primaryPaperLink":{"url":"https://arxiv.org/pdf/2505.00001.pdf","linkType":"arxiv"}}}`
const apiJSON = `{"paperId":"` + testID + `","url":"` + testURL + `","title":"API paper","abstract":"Full API abstract.","authors":[{"name":"Example Author"}],"citationCount":0,"year":2025,"isOpenAccess":false,"externalIds":{"DOI":"10.0000/synthetic","CorpusId":12345,"ArXiv":"2505.00001"},"openAccessPdf":{"url":"https://example.org/paper.pdf"},"journal":{"name":"Test Journal"}}`
const completeMarkdown = "@article{Example2025, title={Synthetic paper}, author={Example Author and Other Author}, year={2025}, abstract={The complete abstract.}}\n0 Citations\n## 48 References\n### Related paper\n19 Citations\n"
const htmlFixture = `<html><head><meta name="citation_title" content="Synthetic [paper] &amp; test"><meta name="citation_author" content="Example Author"><meta name="citation_publication_date" content="2025/05/01"><meta name="citation_pdf_url" content="https://example.org/paper.pdf"><meta name="description" content="Truncated..."></head><body><div data-selenium-selector="abstract-text">Full <b>abstract</b> &amp; $x$.</div><a data-heap-nav="citing-papers">1,234 Citations</a><a data-heap-nav="references">0 References</a></body></html>`

func TestPageData(t *testing.T) {
	m, err := ParsePageData([]byte(pageJSON), testURL)
	if err != nil {
		t.Fatal(err)
	}
	for key, want := range map[string]string{"abstract": "The complete abstract.", "citationCount": "0", "referenceCount": "48", "keyReferenceCount": "4", "publicationUpdateDate": "2025-06-02", "isOpenAccess": "true", "tldr": "A supplied summary.", "arxiv": "https://arxiv.org/abs/2505.00001"} {
		if m.First(key) != want {
			t.Errorf("%s: got %q want %q", key, m.First(key), want)
		}
	}
	if len(m["authors_names"]) != 2 || len(m["links"]) != 1 {
		t.Fatal(m)
	}
	if _, err := ParsePageData([]byte(pageJSON), strings.ReplaceAll(testURL, "a", "b")); err == nil {
		t.Fatal("accepted mismatched paper")
	}
	for _, key := range []string{"paperAbstract", "authors", "title"} {
		var d map[string]any
		json.Unmarshal([]byte(pageJSON), &d)
		delete(object(d["paper"]), key)
		raw, _ := json.Marshal(d)
		if _, err := ParsePageData(raw, testURL); err == nil {
			t.Errorf("accepted missing %s", key)
		}
	}
	for _, raw := range []string{"[]", "{}", "null", "broken"} {
		if _, err := ParsePageData([]byte(raw), testURL); err == nil {
			t.Errorf("accepted %s", raw)
		}
	}
}
func TestAPIAndLegacyJSON(t *testing.T) {
	m, err := ParseJSON([]byte(apiJSON), testURL)
	if err != nil {
		t.Fatal(err)
	}
	for key, want := range map[string]string{"isOpenAccess": "false", "citationCount": "0", "doi": "10.0000/synthetic", "journal_name": "Test Journal", "authors_names": "Example Author", "corpusID": "12345", "openAccessPdf": "https://example.org/paper.pdf"} {
		if m.First(key) != want {
			t.Errorf("%s: %q", key, m.First(key))
		}
	}
	raw, _ := json.Marshal(m)
	round, err := ParseJSON(raw, testURL)
	if err != nil || round.First("abstract") != m.First("abstract") {
		t.Fatal(round, err)
	}
	legacy := `{"title":["Legacy paper"],"abstract":["Legacy abstract"],"authors":[{"name":"Example Author"}],"journal":[{"name":"Legacy Journal"}],"openAccessPdf":[{"url":"https://example.org/legacy.pdf"}],"corpusID":["Corpus ID: 12345"]}`
	old, err := ParseJSON([]byte(legacy), testURL)
	if err != nil || old.First("journal_name") != "Legacy Journal" || old.First("openAccessPdf") != "https://example.org/legacy.pdf" || old.First("corpusID") != "12345" {
		t.Fatal(old, err)
	}
}
func TestHTML(t *testing.T) {
	m, err := ParseHTML([]byte(htmlFixture), testURL)
	if err != nil {
		t.Fatal(err)
	}
	for key, want := range map[string]string{"title": "Synthetic [paper] & test", "abstract": "Full abstract & $x$.", "publicationDate": "2025-05-01", "citationCount": "1234", "referenceCount": "0"} {
		if m.First(key) != want {
			t.Errorf("%s: %q", key, m.First(key))
		}
	}
	ld := `<script type="application/ld+json">{"@graph":[{"@type":"Organization"},{"@type":"ScholarlyArticle","headline":"LD paper","abstract":"LD abstract","author":{"name":"Example Author"},"datePublished":"2025-05-01T12:00:00Z"}]}</script>`
	if m, err = ParseHTML([]byte(ld), testURL); err != nil || m.First("abstract") != "LD abstract" {
		t.Fatal(m, err)
	}
	for _, input := range []string{"", `<title>Human Verification</title>`, strings.Replace(htmlFixture, `<div data-selenium-selector="abstract-text">Full <b>abstract</b> &amp; $x$.</div>`, "", 1)} {
		if _, err := ParseHTML([]byte(input), testURL); err == nil {
			t.Fatal("accepted incomplete/challenged HTML")
		}
	}
}
func TestMarkdown(t *testing.T) {
	page := map[string]string{"title": "Synthetic paper | Semantic Scholar", "content": completeMarkdown}
	raw, _ := json.Marshal(page)
	m, err := ParseFetch(raw, testURL)
	if err != nil {
		t.Fatal(err)
	}
	if m.First("abstract") != "The complete abstract." || m.First("citationCount") != "0" {
		t.Fatal(m)
	}
	for _, content := range []string{"## 48 References\n### Related paper\n" + completeMarkdown, strings.Replace(completeMarkdown, ", abstract={The complete abstract.}", "", 1), "@article{broken"} {
		page["content"] = content
		raw, _ = json.Marshal(page)
		if _, err := ParseFetch(raw, testURL); err == nil {
			t.Fatal("accepted incomplete or related-paper text")
		}
	}
	page["content"] = completeMarkdown
	page["title"] = "Different paper | Semantic Scholar"
	raw, _ = json.Marshal(page)
	if _, err := ParseFetch(raw, testURL); err == nil {
		t.Fatal("accepted title mismatch")
	}
}

func TestOrgFormatting(t *testing.T) {
	m, err := ParsePageData([]byte(pageJSON), testURL)
	if err != nil {
		t.Fatal(err)
	}
	m.Stamp("semantic-scholar-page-data", time.Date(2026, 10, 20, 12, 0, 0, 0, time.UTC))
	out, err := RenderOrg(m)
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{"@citations/0 @2025/May", ":keyReferenceCount: 4", ":metadataFetchedAt: 2026-10-20T12:00:00Z", ":isOpenAccess: true", "* @abstract", "A supplied summary.", "The complete abstract."} {
		if !strings.Contains(out, want) {
			t.Errorf("missing %s", want)
		}
	}
	if strings.Contains(out, "@openAccess") || strings.Contains(out, "* @summary") || strings.Contains(out, ":metadataSource:") {
		t.Fatal(out)
	}
	if strings.Index(out, "org-paper-metadata: summary") > strings.Index(out, "org-paper-metadata: abstract") {
		t.Fatal("summary is after abstract")
	}
	if strings.Count(out, "* @abstract") != 1 {
		t.Fatal(out)
	}
	m["abstract"] = []string{"First\n#+END_QUOTE\n* injected heading"}
	out, _ = RenderOrg(m)
	if !strings.Contains(out, ",#+END_QUOTE") || !strings.Contains(out, ",* injected heading") {
		t.Fatal("unescaped Org structure")
	}
	for input, want := range map[string]string{"International Conference on Learning Representations": "ICLR", "arXiv.org": "arXiv", "Test Venue": "TestVenue"} {
		if got := VenueTag(input); got != want {
			t.Errorf("%s => %s", input, got)
		}
	}
}

type transportFunc func(*http.Request) (*http.Response, error)

func (f transportFunc) RoundTrip(r *http.Request) (*http.Response, error) { return f(r) }
func fakeClient(o Options, serve func(*http.Request) (int, string, map[string]string)) *Client {
	c := NewClient(o)
	c.HTTP.Transport = transportFunc(func(r *http.Request) (*http.Response, error) {
		code, body, headers := serve(r)
		h := http.Header{}
		for k, v := range headers {
			h.Set(k, v)
		}
		return &http.Response{StatusCode: code, Body: io.NopCloser(strings.NewReader(body)), Header: h, Request: r}, nil
	})
	c.Now = func() time.Time { return time.Date(2026, 10, 20, 12, 0, 0, 0, time.UTC) }
	return c
}
func TestDefaultAndFallbackOrder(t *testing.T) {
	for _, success := range []string{"page-data", "api", "deepseek"} {
		t.Run(success, func(t *testing.T) {
			var calls []string
			c := fakeClient(Options{Scraper: "auto", APIEnabled: true, APIKey: "synthetic-key", DeepSeekKey: "synthetic-key"}, func(r *http.Request) (int, string, map[string]string) {
				backend := "page-data"
				body := pageJSON
				if strings.Contains(r.URL.Path, "/graph/") {
					backend = "api"
					body = apiJSON
					if r.Header.Get("x-api-key") != "synthetic-key" {
						t.Error("API key not in header")
					}
				}
				if r.URL.Hostname() == "api.deepseek.com" {
					backend = "deepseek"
					raw, _ := json.Marshal(map[string]string{"title": "Synthetic paper | Semantic Scholar", "content": completeMarkdown})
					body = string(raw)
					if r.Header.Get("Authorization") != "Bearer synthetic-key" {
						t.Error("DeepSeek key not in header")
					}
				}
				calls = append(calls, backend)
				if backend == success {
					return 200, body, nil
				}
				return 403, "", nil
			})
			c.LoadHTML = func(context.Context, string) ([]byte, error) {
				calls = append(calls, "full-html2")
				return []byte(htmlFixture), nil
			}
			m, err := c.Fetch(context.Background(), testURL)
			if err != nil {
				t.Fatal(err)
			}
			order := []string{"page-data", "api", "deepseek"}
			for i, call := range calls {
				if call != order[i] {
					t.Fatal(calls)
				}
			}
			if calls[len(calls)-1] != success || m.First("metadataFetchedAt") == "" {
				t.Fatal(calls, m)
			}
		})
	}
}
func TestResolveUsesHEADRedirectAndNoSearch(t *testing.T) {
	var methods []string
	c := fakeClient(Options{Scraper: "page-data"}, func(r *http.Request) (int, string, map[string]string) {
		methods = append(methods, r.Method)
		if r.URL.Hostname() == "api.semanticscholar.org" {
			if r.URL.Path != "/arXiv:2505.00001" {
				t.Fatal(r.URL)
			}
			return 302, "", map[string]string{"Location": testURL}
		}
		return 202, "", nil
	})
	got, err := c.Resolve(context.Background(), "https://arxiv.org/pdf/2505.00001v2.pdf")
	if err != nil || got != testURL {
		t.Fatal(got, err)
	}
	if strings.Join(methods, ",") != "HEAD,HEAD" {
		t.Fatal(methods)
	}
	for _, input := range []string{testURL, "https://www.semanticscholar.org/paper/Title/" + testID} {
		if got, err = c.Resolve(context.Background(), input); err != nil || got != testURL {
			t.Fatal(got, err)
		}
	}
	for _, input := range []string{"https://example.org/paper/" + testID, "javascript:bad", "https://arxiv.org/abs/not-an-id"} {
		if _, err = c.Resolve(context.Background(), input); err == nil {
			t.Fatal(input)
		}
	}
}
func TestExplicitBackendFailureAndInvalidEnum(t *testing.T) {
	var calls int
	c := fakeClient(Options{Scraper: "page-data"}, func(*http.Request) (int, string, map[string]string) { calls++; return 403, "", nil })
	if _, err := c.Fetch(context.Background(), testURL); err == nil || calls != 1 {
		t.Fatal(calls, err)
	}
	c.Options.Scraper = "typo"
	if _, err := c.Fetch(context.Background(), testURL); err == nil || calls != 1 {
		t.Fatal(calls, err)
	}
}
