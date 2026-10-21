package update

import "org-paper-metadata/paper"

// DrawerLinkEdits is local formatting: it never changes the heading URL, prose,
// BibTeX, or the last metadata-fetch time, and needs no network request.
func (d Document) DrawerLinkEdits(t Target) []Edit {
	if t.DrawerStart < 0 {
		return nil
	}
	var edits []Edit
	for i := t.DrawerStart + 1; i < t.DrawerEnd-1; i++ {
		line := d.Lines[i]
		if !propertyRE.MatchString(line) {
			continue
		}
		if normalized := paper.LatestArxivURLs(line); normalized != line {
			edits = append(edits, Edit{Start: i, End: i + 1, Lines: []string{normalized}})
		}
	}
	return edits
}
