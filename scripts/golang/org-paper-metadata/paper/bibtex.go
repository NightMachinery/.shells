package paper

import (
	"regexp"
	"strings"
)

const GeneratedProperty = "orgPaperMetadataGenerated"

// OrgOptions controls optional generated sections. Zero values keep BibTeX enabled.
type OrgOptions struct{ NoBibTeX bool }

var bibEntryRE = regexp.MustCompile(`(?i)^\s*@[a-z]+\s*[{(]`)
var legacyGeneratedBibRE = regexp.MustCompile(`(?i)^\s*@misc\s*\{\s*(?:ss[0-9a-f]{40}|paper[0-9a-f]{20})\s*,`)

func IsBibTeX(text string) bool { return bibEntryRE.MatchString(text) }

// BibTeX returns a supplied citation verbatim. Bibliographic records are never
// synthesized from paper metadata.
func BibTeX(m Metadata) string {
	if bib := strings.TrimSpace(m.First("bibtex")); IsBibTeX(bib) {
		return bib
	}
	return ""
}

// IsLegacyGeneratedBibTeX recognizes only the old local serializer's keys.
// Callers must also verify the section's ownership property before removal.
func IsLegacyGeneratedBibTeX(text string) bool {
	return legacyGeneratedBibRE.MatchString(text)
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
	if BibTeX(m) == "" {
		return ""
	}
	return strings.Repeat("*", level) + " @bibtex\n:PROPERTIES:\n:visibility: folded\n:" + GeneratedProperty + ": bibtex\n:END:\n" + BibTeXBlock(BibTeX(m)) + "\n"
}
