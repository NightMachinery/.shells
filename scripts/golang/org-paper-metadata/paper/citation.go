package paper

import (
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"strings"
)

// The public batch export accepts anonymous requests. Send only the resolved
// public paper ID, never local notes, titles, paths, or configured credentials.
func (c *Client) fetchBibTeX(ctx context.Context, m Metadata) error {
	id := strings.ToLower(m.First("paperId"))
	if !idPattern.MatchString(id) {
		return errors.New("no valid Semantic Scholar paper ID for citation export")
	}
	body, _ := json.Marshal(map[string][]string{"ids": {id}})
	raw, _, err := c.request(ctx, http.MethodPost, "https://api.semanticscholar.org/graph/v1/paper/batch?fields=citationStyles", body, map[string]string{"Content-Type": "application/json"})
	if err != nil {
		return err
	}
	var rows []struct {
		PaperID        string `json:"paperId"`
		CitationStyles struct {
			BibTeX string `json:"bibtex"`
		} `json:"citationStyles"`
	}
	if err = json.Unmarshal(raw, &rows); err != nil {
		return err
	}
	if len(rows) != 1 || !strings.EqualFold(rows[0].PaperID, id) {
		return errors.New("citation export paper ID does not match the requested paper")
	}
	bib := strings.TrimSpace(rows[0].CitationStyles.BibTeX)
	if !IsBibTeX(bib) {
		return errors.New("citation export has no valid BibTeX")
	}
	m["bibtex"] = []string{bib}
	return nil
}
