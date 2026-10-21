package paper

import (
	"crypto/sha256"
	"fmt"
	"regexp"
	"strings"
)

const GeneratedProperty = "orgPaperMetadataGenerated"

// OrgOptions controls optional generated sections. Zero values keep BibTeX enabled.
type OrgOptions struct{ NoBibTeX bool }

var bibEntryRE = regexp.MustCompile(`(?i)^\s*@[a-z]+\s*[{(]`)
var bibKeyRE = regexp.MustCompile(`^ss[0-9a-f]{40}$`)

func IsBibTeX(text string) bool { return bibEntryRE.MatchString(text) }

// BibTeX prefers supplied citation text, otherwise serializes known fields only.
// @misc avoids inventing a publication type from an ambiguous venue label.
func BibTeX(m Metadata) string {
	if bib := strings.TrimSpace(m.First("bibtex")); IsBibTeX(bib) {
		return bib
	}
	key := "ss" + strings.ToLower(m.First("paperId"))
	if !bibKeyRE.MatchString(key) {
		sum := sha256.Sum256([]byte(m.First("url") + "\n" + m.First("title")))
		key = fmt.Sprintf("paper%x", sum[:10])
	}
	var fields []string
	add := func(key, value string) {
		if value != "" {
			fields = append(fields, "  "+key+" = {"+bibText(value)+"}")
		}
	}
	fields = append(fields, "  title = {{"+bibText(m.First("title"))+"}}")
	var authors []string
	for _, name := range m["authors_names"] {
		authors = append(authors, bibText(name))
	}
	if len(authors) > 0 {
		fields = append(fields, "  author = {"+strings.Join(authors, " and ")+"}")
	}
	year := yearRE.FindString(m.First("year"))
	if year == "" {
		year = yearRE.FindString(m.First("publicationDate"))
	}
	add("year", year)
	add("doi", m.First("doi"))
	add("url", m.First("url"))
	if id, ok := arxivID(m.First("arxiv")); ok {
		add("eprint", id)
		add("archivePrefix", "arXiv")
	}
	venue := m.First("venue")
	if venue == "" {
		venue = m.First("journal_name")
	}
	if venue != "" && !strings.Contains(strings.ToLower(venue), "arxiv") {
		add("howpublished", venue)
	}
	return "@misc{" + key + ",\n" + strings.Join(fields, ",\n") + "\n}"
}

func bibText(text string) string {
	return strings.NewReplacer(`\`, `\textbackslash{}`, `{`, `\{`, `}`, `\}`, `&`, `\&`, `%`, `\%`, `$`, `\$`, `#`, `\#`, `_`, `\_`, `~`, `\textasciitilde{}`, `^`, `\textasciicircum{}`).Replace(OneLine(text))
}

func BibTeXBlock(bib string) string {
	lines := strings.Split(strings.ReplaceAll(bib, "\r\n", "\n"), "\n")
	for i, line := range lines {
		if strings.HasPrefix(strings.TrimLeft(line, " \t"), "#+") || orgHeadingLine.MatchString(line) {
			lines[i] = "," + line
		}
	}
	return "#+BEGIN_SRC bibtex\n" + strings.Join(lines, "\n") + "\n#+END_SRC\n"
}

func BibTeXSection(m Metadata, level int) string {
	return strings.Repeat("*", level) + " @bibtex\n:PROPERTIES:\n:visibility: folded\n:" + GeneratedProperty + ": bibtex\n:END:\n" + BibTeXBlock(BibTeX(m)) + "\n"
}
