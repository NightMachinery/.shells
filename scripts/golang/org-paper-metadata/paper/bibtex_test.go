package paper

import (
	"strings"
	"testing"
)

func TestDefaultBibTeXHasFoldedSiblingAndKnownFields(t *testing.T) {
	m, _ := ParsePageData([]byte(pageJSON), testURL)
	out, err := RenderOrg(m)
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{"* @bibtex\n:PROPERTIES:\n:visibility: folded\n:orgPaperMetadataGenerated: bibtex\n:END:\n#+BEGIN_SRC bibtex", "@misc{ss" + testID, "author = {Example Author and Other Author}", "title = {{Synthetic paper}}"} {
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
func TestBibTeXFallbackEscapesAndDoesNotInventFields(t *testing.T) {
	m := Metadata{"title": {"A {title} with 50% & $x_1$ # \\ ~ ^"}, "authors_names": {"Example Author", "Other Author"}, "url": {testURL}, "arxiv": {"https://arxiv.org/abs/2505.00001"}, "year": {"2025"}}
	bib := BibTeX(m)
	for _, want := range []string{`\{title\}`, `50\% \& \$x\_1\$ \#`, `\textbackslash{}`, `author = {Example Author and Other Author}`, `eprint = {2505.00001}`, `archivePrefix = {arXiv}`} {
		if !strings.Contains(bib, want) {
			t.Errorf("missing %q\n%s", want, bib)
		}
	}
	if strings.Contains(bib, "journal =") || strings.Contains(bib, "doi =") {
		t.Fatal(bib)
	}
	if BibTeX(m) != bib {
		t.Fatal("unstable key")
	}
}
