package update

import (
	"strings"

	"org-paper-metadata/paper"
)

func (d Document) paperChildren(t Target) []Heading {
	var children []Heading
	for _, h := range d.Headings {
		if h.Start > t.Heading.Start && h.Start < t.Heading.End && h.Level == t.Heading.Level+1 && !h.Ignored {
			children = append(children, h)
		}
	}
	return children
}

// A handwritten @summary stays separate. Only known generated layouts qualify
// as an old abstract heading; otherwise a new @abstract child is added.
func (d Document) metadataChild(t Target) (*Heading, bool) {
	var abstracts, summaries []Heading
	for _, h := range d.paperChildren(t) {
		switch strings.TrimSpace(d.Lines[h.Start][h.Level:]) {
		case "@abstract":
			abstracts = append(abstracts, h)
		case "@summary":
			summaries = append(summaries, h)
		}
	}
	if len(abstracts) > 1 {
		return nil, true
	}
	if len(abstracts) == 1 {
		return &abstracts[0], false
	}
	if len(summaries) > 1 {
		return nil, true
	}
	if len(summaries) == 1 {
		h := summaries[0]
		start, end, _, err := d.drawer(h)
		if err != nil {
			return nil, true
		}
		body := h.Start + 1
		if start >= 0 {
			body = end
		}
		for body < h.End && strings.TrimSpace(d.Lines[body]) == "" {
			body++
		}
		if legacyCitationTail(d.Lines[body:h.End]) {
			return &h, false
		}
		for i := body; i < h.End; i++ {
			if strings.EqualFold(strings.TrimSpace(d.Lines[i]), "# org-paper-metadata: abstract") || strings.EqualFold(strings.TrimSpace(d.Lines[i]), "#+name: semantic-scholar-abstract") {
				return &h, false
			}
		}
		if body < h.End && strings.EqualFold(strings.TrimSpace(d.Lines[body]), "#+BEGIN_QUOTE") {
			for i := body + 1; i < h.End; i++ {
				if strings.EqualFold(strings.TrimSpace(d.Lines[i]), "#+BEGIN_QUOTE") {
					break
				}
				if strings.EqualFold(strings.TrimSpace(d.Lines[i]), "#+END_QUOTE") {
					if legacyCitationTail(d.Lines[i+1 : h.End]) {
						return &h, false
					}
					break
				}
			}
		}
	}
	return nil, false
}

func propertyValue(m paper.Metadata, key string) string {
	for k, v := range m {
		if strings.EqualFold(k, key) && len(v) > 0 {
			return v[0]
		}
	}
	return ""
}

type bibBlock struct{ start, end int }

// Only standalone citation blocks in the heading's direct section qualify.
// Quotes and unrelated source/example blocks are skipped as complete blocks.
func (d Document) citationBlocks(h Heading) []bibBlock {
	end := h.End
	for _, child := range d.Headings {
		if child.Start > h.Start && child.Start < end {
			end = child.Start
			break
		}
	}
	var result []bibBlock
	for i := h.Start + 1; i < end; i++ {
		fields := strings.Fields(strings.ToUpper(strings.TrimSpace(d.Lines[i])))
		if len(fields) == 0 || !strings.HasPrefix(fields[0], "#+BEGIN_") {
			continue
		}
		kind := strings.TrimPrefix(fields[0], "#+BEGIN_")
		j := i + 1
		for j < end && !strings.EqualFold(strings.TrimSpace(d.Lines[j]), "#+END_"+kind) {
			j++
		}
		if j == end {
			break
		}
		candidate := kind == "EXAMPLE" || (kind == "SRC" && len(fields) > 1 && fields[1] == "BIBTEX")
		content := strings.TrimSpace(strings.Join(d.Lines[i+1:j], "\n"))
		if candidate && paper.IsBibTeX(content) {
			result = append(result, bibBlock{i, j + 1})
		}
		i = j
	}
	return result
}

// BibTeXEdits uses an ownership property for generated sections. Legacy citation
// blocks are relocated unchanged and remain unmarked, protecting custom keys.
func (d Document) BibTeXEdits(t Target, m paper.Metadata, noBibTeX bool) []Edit {
	children := d.paperChildren(t)
	var generated []Heading
	manual := false
	for _, h := range children {
		_, _, props, err := d.drawer(h)
		if err != nil {
			continue
		}
		if strings.EqualFold(propertyValue(props, paper.GeneratedProperty), "bibtex") {
			generated = append(generated, h)
			continue
		}
		label := strings.TrimSpace(d.Lines[h.Start][h.Level:])
		if label == "@bibtex" {
			manual = true
		}
	}
	// Clean old fabricated citations even when the paper is recent or retrieval
	// failed. The ownership marker AND the old serializer's key must match.
	if noBibTeX || paper.BibTeX(m) == "" {
		var edits []Edit
		for _, h := range generated {
			remove := noBibTeX
			blocks := d.citationBlocks(h)
			if len(blocks) == 1 && paper.IsLegacyGeneratedBibTeX(strings.Join(d.Lines[blocks[0].start+1:blocks[0].end-1], "\n")) {
				remove = true
			}
			if remove {
				edits = append(edits, d.generatedBibTeX(h, nil, true)...)
			}
		}
		if noBibTeX || m == nil || len(generated) > 0 {
			return edits
		}
	}
	abstract, ambiguous := d.metadataChild(t)
	if m == nil || ambiguous {
		return nil
	}
	pos := t.Heading.End
	if abstract != nil {
		pos = abstract.End
	}
	if len(generated) > 1 {
		return nil
	} // Ambiguous ownership, preserve everything.
	if len(generated) == 1 {
		h := generated[0]
		edits := d.generatedBibTeX(h, m, false)
		if len(edits) == 0 {
			return nil
		}
		// Apply internal edits to a copy so relocation cannot overlap body edits.
		snippet := Document{Lines: append([]string{}, d.Lines[h.Start:h.End]...), Newline: "\n", FinalNewline: true}
		for i := range edits {
			edits[i].Start -= h.Start
			edits[i].End -= h.Start
		}
		text, err := snippet.Apply(edits)
		if err != nil {
			return nil
		}
		lines := strings.Split(strings.TrimSuffix(text, "\n"), "\n")
		if pos == h.Start {
			return []Edit{{Start: h.Start, End: h.End, Lines: lines}}
		}
		return []Edit{{Start: h.Start, End: h.End}, {Start: pos, End: pos, Lines: lines, Priority: 10001 - 2*t.Heading.Level}}
	}
	if manual {
		return nil
	}
	if abstract != nil {
		blocks := d.citationBlocks(*abstract)
		if len(blocks) > 1 {
			return nil
		}
		if len(blocks) == 1 {
			b := blocks[0]
			if strings.TrimSpace(strings.Join(d.Lines[b.end:abstract.End], "\n")) != "" {
				return nil
			}
			lines := []string{strings.Repeat("*", t.Heading.Level+1) + " @bibtex", ":PROPERTIES:", ":visibility: folded", ":END:"}
			lines = append(lines, d.Lines[b.start:b.end]...)
			lines = append(lines, "")
			return []Edit{{Start: b.start, End: b.end}, {Start: pos, End: pos, Lines: lines, Priority: 10001 - 2*t.Heading.Level}}
		}
	}
	if paper.BibTeX(m) == "" {
		return nil
	}
	section := strings.Split(strings.TrimSuffix(paper.BibTeXSection(m, t.Heading.Level+1), "\n"), "\n")
	return []Edit{{Start: pos, End: pos, Lines: section, Priority: 10001 - 2*t.Heading.Level}}
}

// A generated section may acquire personal text or IDs. In that case disabling
// BibTeX removes its citation and ownership marker, retaining the user's heading.
func (d Document) generatedBibTeX(h Heading, m paper.Metadata, remove bool) []Edit {
	start, end, _, err := d.drawer(h)
	if err != nil || start < 0 {
		return nil
	}
	blocks := d.citationBlocks(h)
	if len(blocks) > 1 {
		return nil
	}
	owned := map[int]bool{h.Start: true, start: true, end - 1: true}
	marker := -1
	visibility := -1
	for i := start + 1; i < end-1; i++ {
		match := propertyRE.FindStringSubmatch(d.Lines[i])
		if match == nil {
			continue
		}
		if strings.EqualFold(match[1], paper.GeneratedProperty) {
			marker = i
			owned[i] = true
		}
		if strings.EqualFold(match[1], "visibility") {
			visibility = i
			owned[i] = true
		}
	}
	for _, b := range blocks {
		for i := b.start; i < b.end; i++ {
			owned[i] = true
		}
	}
	if remove {
		pure := true
		for i := h.Start; i < h.End; i++ {
			if !owned[i] && strings.TrimSpace(d.Lines[i]) != "" {
				pure = false
				break
			}
		}
		if pure {
			return []Edit{{Start: h.Start, End: h.End}}
		}
		var edits []Edit
		if marker >= 0 {
			edits = append(edits, Edit{Start: marker, End: marker + 1})
		}
		for _, b := range blocks {
			edits = append(edits, Edit{Start: b.start, End: b.end})
		}
		return edits
	}
	block := strings.Split(strings.TrimSuffix(paper.BibTeXBlock(paper.BibTeX(m)), "\n"), "\n")
	var edits []Edit
	if len(blocks) == 1 {
		edits = append(edits, Edit{Start: blocks[0].start, End: blocks[0].end, Lines: block})
	} else {
		edits = append(edits, Edit{Start: end, End: end, Lines: block})
	}
	if visibility >= 0 {
		edits = append(edits, Edit{Start: visibility, End: visibility + 1, Lines: []string{":visibility: folded"}})
	} else {
		edits = append(edits, Edit{Start: end - 1, End: end - 1, Lines: []string{":visibility: folded"}})
	}
	return edits
}
