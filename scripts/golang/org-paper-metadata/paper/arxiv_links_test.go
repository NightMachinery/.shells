package paper

import (
	"strings"
	"testing"
)

func TestLatestArxivURL(t *testing.T) {
	for raw, want := range map[string]string{
		"https://arxiv.org/abs/2505.00661v2":                  "https://arxiv.org/abs/2505.00661",
		"http://export.arxiv.org/pdf/2505.00661v1.pdf#page=4": "https://arxiv.org/pdf/2505.00661#page=4",
		"https://www.arxiv.org/pdf/hep-th/9901001v3.pdf":      "https://arxiv.org/pdf/hep-th/9901001",
		"https://arxiv.org/html/2505.00661v2?x=1":             "https://arxiv.org/html/2505.00661?x=1",
		"https://arxiv.org/abs/2505.00661":                    "https://arxiv.org/abs/2505.00661",
		"https://arxiv.org.evil.invalid/pdf/2505.00661v1.pdf": "https://arxiv.org.evil.invalid/pdf/2505.00661v1.pdf",
		"https://user:pass@arxiv.org/abs/2505.00661v1":        "https://user:pass@arxiv.org/abs/2505.00661v1",
		"https://arxiv.org/help/versionsv1":                   "https://arxiv.org/help/versionsv1",
	} {
		if got := LatestArxivURL(raw); got != want {
			t.Errorf("%q => %q, want %q", raw, got, want)
		}
	}
	got := LatestArxivURLs("Saved [[https://arxiv.org/pdf/2505.00661v1.pdf][paper]], see https://arxiv.org/abs/2505.00661v2.")
	if got != "Saved [[https://arxiv.org/pdf/2505.00661][paper]], see https://arxiv.org/abs/2505.00661." {
		t.Fatal(got)
	}
}
func TestMetadataLinksCanonicalizeAndDeduplicate(t *testing.T) {
	raw := `{"title":"Synthetic","abstract":"Full abstract.","authors_names":["Example Author"],"url":"` + testURL + `","arxiv":["https://arxiv.org/abs/2505.00661v1"],"links":["http://export.arxiv.org/pdf/2505.00661v1.pdf","https://arxiv.org/pdf/2505.00661v2"],"openAccessPdf":{"url":"https://arxiv.org/pdf/2505.00661v3.pdf"}}`
	m, err := ParseJSON([]byte(raw), testURL)
	if err != nil || len(m["links"]) != 1 || m.First("links") != "https://arxiv.org/pdf/2505.00661" || m.First("pdf_urls") != "https://arxiv.org/pdf/2505.00661" || m.First("openAccessPdf") != "https://arxiv.org/pdf/2505.00661" {
		t.Fatal(m, err)
	}
	out, err := RenderOrgWithOptions(m, OrgOptions{NoBibTeX: true})
	if err != nil || strings.Contains(out, "00661v") {
		t.Fatal(out, err)
	}
}
