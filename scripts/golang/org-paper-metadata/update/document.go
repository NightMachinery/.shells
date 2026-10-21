// Package update applies narrow, line-preserving changes to existing Org paper headings.
package update

import (
	"errors"
	"fmt"
	"regexp"
	"sort"
	"strings"
	"time"
	"unicode/utf8"

	"org-paper-metadata/paper"
)

type Heading struct {
	Start, End, Level int
	Ignored           bool
}
type Document struct {
	Lines        []string
	Headings     []Heading
	Newline      string
	FinalNewline bool
}
type Target struct {
	Heading                Heading
	URL                    string
	LinkStart, LinkEnd     int
	Bracketed              bool
	Properties             paper.Metadata
	DrawerStart, DrawerEnd int
}
type Edit struct {
	Start, End int
	Lines      []string
	Priority   int
}

var headingRE = regexp.MustCompile(`^(\*+)\s+(.+)$`)
var propertyRE = regexp.MustCompile(`^\s*:([^:\s]+):\s*(.*?)\s*$`)
var planningRE = regexp.MustCompile(`^(?:SCHEDULED|DEADLINE|CLOSED):`)
var urlRE = regexp.MustCompile(`https?://[^\s\[\]<>]+`)
var citationTagRE = regexp.MustCompile(`^@citations/\d+$`)
var dateTagRE = regexp.MustCompile(`^@\d{4}(?:/[A-Za-z]+)?$`)

func Parse(text string) (Document, error) {
	if !utf8.ValidString(text) {
		return Document{}, errors.New("file is not UTF-8; leaving it unchanged")
	}
	d := Document{Newline: "\n", FinalNewline: strings.HasSuffix(text, "\n")}
	if strings.Contains(text, "\r\n") {
		if strings.Contains(strings.ReplaceAll(text, "\r\n", ""), "\n") {
			return d, errors.New("mixed line endings; leaving file unchanged")
		}
		d.Newline = "\r\n"
		text = strings.ReplaceAll(text, "\r\n", "\n")
	}
	if d.FinalNewline {
		text = strings.TrimSuffix(text, "\n")
	}
	if text != "" {
		d.Lines = strings.Split(text, "\n")
	}
	var blocks []string
	drawer := false
	for i, line := range d.Lines {
		upper := strings.ToUpper(strings.TrimSpace(line))
		if len(blocks) > 0 {
			if upper == "#+END_"+blocks[len(blocks)-1] || (blocks[len(blocks)-1] == ":" && upper == "#+END:") {
				blocks = blocks[:len(blocks)-1]
			}
			continue
		}
		if strings.HasPrefix(upper, "#+BEGIN_") {
			fields := strings.Fields(strings.TrimPrefix(upper, "#+BEGIN_"))
			if len(fields) > 0 {
				blocks = append(blocks, fields[0])
			}
			continue
		}
		if strings.HasPrefix(upper, "#+BEGIN:") {
			blocks = append(blocks, ":")
			continue
		}
		if drawer {
			if upper == ":END:" {
				drawer = false
			}
			continue
		}
		if propertyRE.MatchString(line) && upper != ":END:" {
			drawer = true
			continue
		}
		if match := headingRE.FindStringSubmatch(line); match != nil {
			ignored := strings.HasPrefix(match[2], "COMMENT ") || match[2] == "COMMENT" || strings.Contains(match[2], ":ARCHIVE:")
			d.Headings = append(d.Headings, Heading{Start: i, End: len(d.Lines), Level: len(match[1]), Ignored: ignored})
		}
	}
	for i := range d.Headings {
		for j := i + 1; j < len(d.Headings); j++ {
			if d.Headings[j].Level <= d.Headings[i].Level {
				d.Headings[i].End = d.Headings[j].Start
				break
			}
		}
		for j := i - 1; j >= 0; j-- {
			if d.Headings[j].Level < d.Headings[i].Level {
				d.Headings[i].Ignored = d.Headings[i].Ignored || d.Headings[j].Ignored
				break
			}
		}
	}
	return d, nil
}

func (d Document) drawer(h Heading) (int, int, paper.Metadata, error) {
	m := paper.Metadata{}
	i := h.Start + 1
	for i < h.End && (strings.TrimSpace(d.Lines[i]) == "" || planningRE.MatchString(strings.TrimSpace(d.Lines[i]))) {
		i++
	}
	if i >= h.End || !strings.EqualFold(strings.TrimSpace(d.Lines[i]), ":PROPERTIES:") {
		return -1, -1, m, nil
	}
	for j := i + 1; j < h.End; j++ {
		if strings.EqualFold(strings.TrimSpace(d.Lines[j]), ":END:") {
			return i, j + 1, m, nil
		}
		if headingRE.MatchString(d.Lines[j]) {
			break
		}
		match := propertyRE.FindStringSubmatch(d.Lines[j])
		if match == nil {
			continue
		}
		key := strings.TrimSuffix(match[1], "+")
		for _, canonical := range paper.PropertyKeys {
			if strings.EqualFold(key, canonical) {
				key = canonical
				break
			}
		}
		m.Put(key, match[2])
	}
	return -1, -1, nil, errors.New("unterminated properties drawer")
}

func (d Document) Targets() ([]Target, error) {
	var targets []Target
	for _, h := range d.Headings {
		if h.Ignored {
			continue
		}
		line := d.Lines[h.Start]
		var candidates []Target
		identities := map[string]bool{}
		for _, span := range urlRE.FindAllStringIndex(line, -1) {
			raw := strings.TrimRight(line[span[0]:span[1]], ".,;)")
			identity, ok := paper.Identity(raw)
			if !ok || identities[identity] {
				continue
			}
			identities[identity] = true
			t := Target{Heading: h, URL: raw, LinkStart: span[0], LinkEnd: span[0] + len(raw)}
			if span[0] >= 2 && line[span[0]-2:span[0]] == "[[" {
				t.LinkStart = span[0] - 2
				end := t.LinkEnd
				if strings.HasPrefix(line[end:], "]]") {
					t.LinkEnd = end + 2
					t.Bracketed = true
				} else if strings.HasPrefix(line[end:], "][") {
					for j := end + 2; j+1 < len(line); j++ {
						if line[j] == '\\' {
							j++
							continue
						}
						if line[j:j+2] == "]]" {
							t.LinkEnd = j + 2
							t.Bracketed = true
							break
						}
					}
				}
				if !t.Bracketed {
					continue
				}
			}
			candidates = append(candidates, t)
		}
		if len(candidates) != 1 {
			continue
		} // Multiple distinct papers make ownership ambiguous.
		t := candidates[0]
		var err error
		t.DrawerStart, t.DrawerEnd, t.Properties, err = d.drawer(h)
		if err != nil {
			return nil, fmt.Errorf("line %d: %w", h.Start+1, err)
		}
		targets = append(targets, t)
	}
	return targets, nil
}
func Recent(m paper.Metadata, now time.Time, maxAge time.Duration) bool {
	t, err := time.Parse(time.RFC3339, m.First("metadataFetchedAt"))
	return err == nil && !t.After(now.Add(5*time.Minute)) && now.Sub(t) < maxAge
}

func (d Document) Plan(t Target, m paper.Metadata) []Edit {
	return d.PlanWithOptions(t, m, paper.OrgOptions{})
}
func (d Document) PlanWithOptions(t Target, m paper.Metadata, options paper.OrgOptions) []Edit {
	var edits []Edit
	line := d.Lines[t.Heading.Start]
	prefix := line[:t.LinkStart]
	oldTags := map[string]bool{}
	for _, tag := range paper.HeadingTags(t.Properties) {
		oldTags[tag] = true
	}
	oldVenue := t.Properties.First("venue")
	if oldVenue == "" {
		oldVenue = t.Properties.First("journal_name")
	}
	for _, tag := range paper.VenueTagAliases(oldVenue) {
		oldTags["@"+tag] = true
	}
	// Only generated citation/date tags and the old metadata's venue tag are owned.
	var keep strings.Builder
	keptWords := map[string]bool{}
	for _, segment := range regexp.MustCompile(`\S+\s*`).FindAllString(prefix, -1) {
		word := strings.TrimSpace(segment)
		if !oldTags[word] && !citationTagRE.MatchString(word) && !dateTagRE.MatchString(word) {
			keep.WriteString(segment)
			keptWords[word] = true
		}
	}
	newPrefix := strings.TrimRight(keep.String(), " \t")
	tagMetadata := paper.Metadata{}
	for key, values := range t.Properties {
		tagMetadata[key] = values
	}
	for key, values := range m {
		if len(values) > 0 {
			tagMetadata[key] = values
		}
	}
	for _, tag := range paper.HeadingTags(tagMetadata) {
		if !keptWords[tag] {
			newPrefix += " " + tag
		}
	}
	newLine := newPrefix + " [[" + t.URL + "][" + paper.Description(m.First("title")) + "]]" + line[t.LinkEnd:]
	if newLine != line {
		edits = append(edits, Edit{Start: t.Heading.Start, End: t.Heading.Start + 1, Lines: []string{newLine}})
	}
	newProps := paper.PropertyLines(m)
	if t.DrawerStart < 0 {
		pos := t.Heading.Start + 1
		for pos < t.Heading.End && planningRE.MatchString(strings.TrimSpace(d.Lines[pos])) {
			pos++
		}
		props := append([]string{":PROPERTIES:"}, newProps...)
		props = append(props, ":END:")
		edits = append(edits, Edit{Start: pos, End: pos, Lines: props, Priority: 0})
	} else {
		// Drop the retired Org-only property even if the source does not supply it.
		updatedKeys := map[string]bool{"metadatasource": true}
		for _, key := range paper.PropertyKeys {
			if len(m[key]) > 0 {
				updatedKeys[strings.ToLower(key)] = true
			}
		}
		props := []string{d.Lines[t.DrawerStart]}
		for _, line := range d.Lines[t.DrawerStart+1 : t.DrawerEnd-1] {
			match := propertyRE.FindStringSubmatch(line)
			if match != nil && updatedKeys[strings.ToLower(strings.TrimSuffix(match[1], "+"))] {
				continue
			}
			props = append(props, line)
		}
		props = append(props, newProps...)
		props = append(props, d.Lines[t.DrawerEnd-1])
		edits = append(edits, Edit{Start: t.DrawerStart, End: t.DrawerEnd, Lines: props})
	}
	edits = append(edits, d.bodyEdits(t, m)...)
	edits = append(edits, d.BibTeXEdits(t, m, options.NoBibTeX)...)
	return edits
}

func (d Document) bodyEdits(t Target, m paper.Metadata) []Edit {
	child, ambiguous := d.metadataChild(t)
	if ambiguous {
		return nil
	}
	if child == nil {
		body := strings.Split(strings.TrimSuffix(paper.AbstractSection(m, t.Heading.Level+1), "\n"), "\n")
		return []Edit{{Start: t.Heading.End, End: t.Heading.End, Lines: append([]string{""}, body...), Priority: 10000 - 2*t.Heading.Level}}
	}
	start, end, _, err := d.drawer(*child)
	if err != nil {
		return nil
	}
	bodyStart := child.Start + 1
	if start >= 0 {
		bodyStart = end
	}
	var edits []Edit
	foundAbstract, foundSummary := false, false
	for i := bodyStart; i < child.End; i++ {
		name := strings.TrimSpace(d.Lines[i])
		kind := ""
		content := ""
		switch strings.ToLower(name) {
		case "# org-paper-metadata: summary", "#+name: semantic-scholar-summary":
			kind = "summary"
			content = "*Semantic Scholar TLDR:* " + m.First("tldr")
		case "# org-paper-metadata: abstract", "#+name: semantic-scholar-abstract":
			kind = "abstract"
			content = m.First("abstract")
		default:
			continue
		}
		if i+1 >= child.End || !strings.EqualFold(strings.TrimSpace(d.Lines[i+1]), "#+BEGIN_QUOTE") {
			continue
		}
		j := i + 2
		for j < child.End && !strings.EqualFold(strings.TrimSpace(d.Lines[j]), "#+END_QUOTE") {
			j++
		}
		if j == child.End {
			continue
		}
		if kind == "summary" {
			foundSummary = true
			if m.First("tldr") == "" {
				continue
			}
		} else {
			foundAbstract = true
		}
		// Reuse the exact block serializer from the converter.
		temp := paper.Metadata{"abstract": {content}}
		block := paper.ContentBlocks(temp)
		block = strings.Replace(block, "# org-paper-metadata: abstract", "# org-paper-metadata: "+kind, 1)
		edits = append(edits, Edit{Start: i, End: j + 1, Lines: strings.Split(strings.TrimSuffix(block, "\n"), "\n")})
	}
	if foundAbstract {
		if !foundSummary && m.First("tldr") != "" {
			block := strings.SplitN(paper.ContentBlocks(m), "# org-paper-metadata: abstract", 2)[0]
			edits = append(edits, Edit{Start: bodyStart, End: bodyStart, Lines: strings.Split(strings.TrimSuffix(block, "\n"), "\n")})
		}
		if strings.TrimSpace(d.Lines[child.Start][child.Level:]) == "@summary" {
			edits = append(edits, Edit{Start: child.Start, End: child.Start + 1, Lines: []string{strings.Repeat("*", child.Level) + " @abstract"}})
		}
		return edits
	}
	// Legacy output may carry a BibTeX example after its single abstract quote.
	// Replace only that quote; preserve the citation and all surrounding bytes.
	quoteStart := bodyStart
	for quoteStart < child.End && strings.TrimSpace(d.Lines[quoteStart]) == "" {
		quoteStart++
	}
	if legacyCitationTail(d.Lines[quoteStart:child.End]) {
		body := strings.Split(strings.TrimSuffix(paper.ContentBlocks(m), "\n"), "\n")
		result := []Edit{{Start: quoteStart, End: quoteStart, Lines: append(body, "")}}
		if strings.HasSuffix(d.Lines[child.Start], "@summary") {
			result = append(result, Edit{Start: child.Start, End: child.Start + 1, Lines: []string{strings.Repeat("*", child.Level) + " @abstract"}})
		}
		return result
	}
	if quoteStart >= child.End || !strings.EqualFold(strings.TrimSpace(d.Lines[quoteStart]), "#+BEGIN_QUOTE") {
		return nil
	}
	quoteEnd := quoteStart + 1
	for quoteEnd < child.End && !strings.EqualFold(strings.TrimSpace(d.Lines[quoteEnd]), "#+END_QUOTE") {
		if strings.EqualFold(strings.TrimSpace(d.Lines[quoteEnd]), "#+BEGIN_QUOTE") {
			return nil
		}
		quoteEnd++
	}
	if quoteEnd == child.End || !legacyCitationTail(d.Lines[quoteEnd+1:child.End]) {
		return nil
	}
	body := strings.Split(strings.TrimSuffix(paper.ContentBlocks(m), "\n"), "\n")
	result := []Edit{{Start: quoteStart, End: quoteEnd + 1, Lines: body}}
	if strings.HasSuffix(d.Lines[child.Start], "@summary") {
		result = append(result, Edit{Start: child.Start, End: child.Start + 1, Lines: []string{strings.Repeat("*", child.Level) + " @abstract"}})
	}
	return result
}

// Accept whitespace or one legacy BibTeX example, never arbitrary prose/code.
func legacyCitationTail(lines []string) bool {
	tail := strings.TrimSpace(strings.Join(lines, "\n"))
	if tail == "" {
		return true
	}
	parts := strings.Split(tail, "\n")
	if len(parts) < 3 {
		return false
	}
	begin := strings.ToUpper(strings.TrimSpace(parts[0]))
	end := "#+END_EXAMPLE"
	if strings.HasPrefix(begin, "#+BEGIN_SRC ") {
		end = "#+END_SRC"
		if !regexp.MustCompile(`(?i)^#\+BEGIN_SRC\s+bibtex(?:\s+.+)?$`).MatchString(begin) {
			return false
		}
	} else if !regexp.MustCompile(`(?i)^#\+BEGIN_EXAMPLE(?:\s+.+)?$`).MatchString(begin) {
		return false
	}
	if !strings.EqualFold(strings.TrimSpace(parts[len(parts)-1]), end) {
		return false
	}
	content := strings.TrimSpace(strings.Join(parts[1:len(parts)-1], "\n"))
	return paper.IsBibTeX(content) && !strings.Contains(strings.ToUpper(content), end)
}

func (d Document) Apply(edits []Edit) (string, error) {
	// Combine insertions at one boundary, placing properties first and deeper
	// abstract sections before an enclosing paper's abstract section.
	inserts := map[int][]Edit{}
	var changes []Edit
	for _, e := range edits {
		if e.Start == e.End {
			inserts[e.Start] = append(inserts[e.Start], e)
		} else {
			changes = append(changes, e)
		}
	}
	for pos, group := range inserts {
		sort.SliceStable(group, func(i, j int) bool { return group[i].Priority < group[j].Priority })
		e := Edit{Start: pos, End: pos}
		for _, part := range group {
			e.Lines = append(e.Lines, part.Lines...)
		}
		changes = append(changes, e)
	}
	sort.SliceStable(changes, func(i, j int) bool {
		if changes[i].Start == changes[j].Start {
			return changes[i].End > changes[j].End
		}
		return changes[i].Start > changes[j].Start
	})
	lines := append([]string{}, d.Lines...)
	boundary := len(lines)
	for _, e := range changes {
		if e.Start < 0 || e.End < e.Start || e.End > boundary {
			return "", errors.New("overlapping metadata edits; leaving file unchanged")
		}
		newLines := append([]string{}, lines[:e.Start]...)
		newLines = append(newLines, e.Lines...)
		newLines = append(newLines, lines[e.End:]...)
		lines = newLines
		boundary = e.Start
	}
	text := strings.Join(lines, d.Newline)
	if d.FinalNewline {
		text += d.Newline
	} else {
		text = strings.TrimRight(text, "\r\n")
	}
	return text, nil
}
