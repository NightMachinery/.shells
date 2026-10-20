package paper

import (
	"fmt"
	"regexp"
	"strings"
)

var PropertyKeys = []string{"paperId", "arxiv", "publicationDate", "publicationUpdateDate", "citationCount", "influentialCitationCount", "referenceCount", "keyReferenceCount", "venue", "journal_name", "authors_names", "links", "openAccessPdf", "pdf_urls", "doi", "fieldsOfStudy", "isOpenAccess", "corpusID", "year", "metadataFetchedAt"}

func OneLine(s string) string { return strings.Join(strings.Fields(s), " ") }
func Description(s string) string {
	s = OneLine(s)
	s = strings.ReplaceAll(s, `\`, `\\`)
	s = strings.ReplaceAll(s, "[", `\[`)
	return strings.ReplaceAll(s, "]", `\]`)
}
func PropertyLines(m Metadata) []string {
	var lines []string
	for _, key := range PropertyKeys {
		for i, v := range m[key] {
			name := key
			if i > 0 {
				name += "+"
			}
			if v = OneLine(v); v != "" {
				lines = append(lines, fmt.Sprintf(":%s: %s", name, v))
			}
		}
	}
	return lines
}

var orgHeadingLine = regexp.MustCompile(`^\*+\s`)

func quoteBlock(name, text string) string {
	lines := strings.Split(strings.ReplaceAll(text, "\r\n", "\n"), "\n")
	for i, line := range lines {
		trim := strings.TrimLeft(line, " \t")
		if strings.HasPrefix(trim, "#+") || orgHeadingLine.MatchString(line) {
			lines[i] = "," + line
		}
	}
	return "# org-paper-metadata: " + name + "\n#+BEGIN_QUOTE\n" + strings.Join(lines, "\n") + "\n#+END_QUOTE\n"
}
func ContentBlocks(m Metadata) string {
	var b strings.Builder
	if summary := m.First("tldr"); summary != "" {
		b.WriteString(quoteBlock("summary", "*Semantic Scholar TLDR:* "+summary))
		b.WriteString("\n")
	}
	b.WriteString(quoteBlock("abstract", m.First("abstract")))
	return b.String()
}
func AbstractSection(m Metadata, level int) string {
	return strings.Repeat("*", level) + " @abstract\n:PROPERTIES:\n:visibility: folded\n:END:\n" + ContentBlocks(m) + "\n"
}
func RenderOrg(m Metadata) (string, error) {
	if err := m.Validate(); err != nil {
		return "", err
	}
	url := m.First("url")
	if !webURL(url) {
		return "", fmt.Errorf("invalid paper URL")
	}
	var b strings.Builder
	if tags := HeadingTags(m); len(tags) > 0 {
		b.WriteString(strings.Join(tags, " ") + " ")
	}
	fmt.Fprintf(&b, "[[%s][%s]]\n:PROPERTIES:\n", url, Description(m.First("title")))
	b.WriteString(strings.Join(PropertyLines(m), "\n"))
	b.WriteString("\n:END:\n")
	b.WriteString(AbstractSection(m, 1))
	if bib := m.First("bibtex"); bib != "" {
		b.WriteString("#+BEGIN_SRC bibtex\n" + strings.ReplaceAll(bib, "\n#+", "\n,#+") + "\n#+END_SRC\n")
	}
	return b.String(), nil
}

var venueWords = regexp.MustCompile(`[\s-]+`)

func VenueTag(venue string) string {
	for _, entry := range venuePatterns {
		if entry.re.MatchString(venue) {
			return entry.tag
		}
	}
	words := venueWords.Split(strings.TrimSpace(venue), -1)
	for i, word := range words {
		if word != "" {
			chars := []rune(word)
			words[i] = strings.ToUpper(string(chars[0])) + string(chars[1:])
		}
	}
	return strings.Join(words, "")
}
