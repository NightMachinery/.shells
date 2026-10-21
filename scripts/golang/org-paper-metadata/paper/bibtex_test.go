package paper

import (
	"strings"
	"testing"
)

func TestDefaultBibTeXHasFoldedSiblingAndKnownFields(t *testing.T) {
	m, _ := ParsePageData([]byte(pageJSON), testURL)
	m.Put("bibtex", "@inproceedings{source_key, title={Synthetic paper}, author={Example Author and Other Author}, booktitle={Example Conference}}")
	out, err := RenderOrg(m)
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{"* @bibtex\n:PROPERTIES:\n:visibility: folded\n:orgPaperMetadataGenerated: bibtex\n:END:\n#+BEGIN_SRC bibtex", "@inproceedings{source_key", "author={Example Author and Other Author}", "booktitle={Example Conference}"} {
		if !strings.Contains(out, want) {
			t.Errorf("missing %q\n%s", want, out)
		}
	}
	if strings.Index(out, "* @bibtex") < strings.Index(out, "* @abstract") {
		t.Fatal(out)
	}
	out, err = RenderOrgWithOptions(m, OrgOptions{NoBibTeX: true})
	if err != nil || strings.Contains(out, "@bibtex") || strings.Contains(out, "BEGIN_SRC bibtex") {
		t.Fatal(out, err)
	}
}
func TestSuppliedBibTeXAndOrgEscaping(t *testing.T) {
	bib := "@article{my_custom_key,\n  title={My saved citation}\n}\n#+END_SRC\n* accidental heading"
	m := Metadata{"bibtex": {bib}}
	out := BibTeXSection(m, 2)
	for _, want := range []string{"** @bibtex", "@article{my_custom_key", ",#+END_SRC", ",* accidental heading"} {
		if !strings.Contains(out, want) {
			t.Fatal(out)
		}
	}
}
func TestNoBibTeXSynthesized(t *testing.T) {
	m, _ := ParsePageData([]byte(pageJSON), testURL)
	if BibTeX(m) != "" || BibTeXSection(m, 2) != "" {
		t.Fatal("fabricated a citation")
	}
	out, err := RenderOrg(m)
	if err != nil || strings.Contains(out, "@bibtex") {
		t.Fatal(out, err)
	}
}
