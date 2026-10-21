package paper

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"net/url"
	"strings"
	"time"
)

// Provider order concerns citation selection. Anonymous Semantic Scholar ID
// enrichment may happen first, supplying exact DBLP/DOI identifiers and a cached
// fallback citation. No title search or local note contents are sent anywhere.
func (c *Client) fetchBibTeX(ctx context.Context, m Metadata) error {
	source := c.Options.BibTeXSource
	if source == "" {
		source = "auto"
	}
	fallback := BibTeX(m)
	delete(m, "bibtex")
	delete(m, "bibtexSource")
	var failures []error
	ssLoaded := false
	loadSS := func() {
		if ssLoaded {
			return
		}
		ssLoaded = true
		if fallback != "" && (source == "semantic-scholar" || (normalizeDBLP(m.First("dblp")) != "" && normalizeDOI(m.First("doi")) != "")) {
			return
		}
		requestCtx, cancel := context.WithTimeout(ctx, 8*time.Second)
		bib, err := c.semanticScholarCitation(requestCtx, m)
		cancel()
		if err != nil {
			failures = append(failures, fmt.Errorf("Semantic Scholar: %w", err))
			return
		}
		if IsBibTeX(bib) {
			fallback = bib
		}
	}
	// page-data usually lacks external IDs. Obtain them from the resolved paper,
	// with its own ID checked before trusting either identifiers or citation.
	if source == "auto" {
		loadSS()
	}
	providers := []string{source}
	if source == "auto" {
		providers = []string{"dblp", "doi", "semantic-scholar"}
		// Semantic Scholar may associate the preprint's DBLP key with a published
		// DOI. Prefer the DOI record to a CoRR preprint in that case.
		if strings.HasPrefix(normalizeDBLP(m.First("dblp")), "journals/corr/") && normalizeDOI(m.First("doi")) != "" {
			providers = []string{"doi", "dblp", "semantic-scholar"}
		}
	}
	for _, provider := range providers {
		if ctx.Err() != nil {
			return ctx.Err()
		}
		var bib string
		var err error
		switch provider {
		case "semantic-scholar":
			loadSS()
			bib = fallback
			if !IsBibTeX(bib) {
				err = errors.New("no valid source citation")
			}
		case "dblp", "doi":
			identifier := normalizeDBLP(m.First("dblp"))
			if provider == "doi" {
				identifier = normalizeDOI(m.First("doi"))
			}
			if identifier == "" && source != "auto" {
				loadSS()
				if provider == "dblp" {
					identifier = normalizeDBLP(m.First("dblp"))
				} else {
					identifier = normalizeDOI(m.First("doi"))
				}
			}
			if identifier == "" {
				continue
			}
			duration := 8 * time.Second
			if provider == "dblp" {
				duration = 12 * time.Second
				if c.Options.BibTeXBrowser {
					duration = 36 * time.Second
				}
			}
			requestCtx, cancel := context.WithTimeout(ctx, duration)
			if provider == "dblp" {
				bib, err = c.dblpCitation(requestCtx, identifier)
			} else {
				bib, err = c.doiCitation(requestCtx, identifier)
			}
			cancel()
		}
		if err == nil && IsBibTeX(bib) {
			m["bibtex"] = []string{strings.TrimSpace(bib)}
			m["bibtexSource"] = []string{provider}
			return nil
		}
		if err != nil {
			failures = append(failures, fmt.Errorf("%s: %w", provider, err))
			if c.Warnf != nil && provider != "semantic-scholar" {
				c.Warnf("%s BibTeX export failed: %v; trying fallback if available", provider, err)
			}
		}
	}
	if len(failures) == 0 {
		return errors.New("no verified identifier for the selected citation provider")
	}
	return errors.Join(failures...)
}

// The public batch export accepts anonymous requests. Only the resolved public
// paper ID is sent, never local notes, titles, paths, or configured credentials.
func (c *Client) semanticScholarCitation(ctx context.Context, m Metadata) (string, error) {
	id := strings.ToLower(m.First("paperId"))
	if !idPattern.MatchString(id) {
		return "", errors.New("no valid Semantic Scholar paper ID for citation export")
	}
	body, _ := json.Marshal(map[string][]string{"ids": {id}})
	raw, _, err := c.request(ctx, http.MethodPost, "https://api.semanticscholar.org/graph/v1/paper/batch?fields=externalIds,citationStyles", body, map[string]string{"Content-Type": "application/json"})
	if err != nil {
		return "", err
	}
	var rows []struct {
		PaperID        string         `json:"paperId"`
		ExternalIDs    map[string]any `json:"externalIds"`
		CitationStyles struct {
			BibTeX string `json:"bibtex"`
		} `json:"citationStyles"`
	}
	if err = json.Unmarshal(raw, &rows); err != nil {
		return "", err
	}
	if len(rows) != 1 || !strings.EqualFold(rows[0].PaperID, id) {
		return "", errors.New("citation export paper ID does not match the requested paper")
	}
	m.addExternalIDs(rows[0].ExternalIDs)
	return strings.TrimSpace(rows[0].CitationStyles.BibTeX), nil
}
func (c *Client) dblpCitation(ctx context.Context, key string) (string, error) {
	endpoint := (&url.URL{Scheme: "https", Host: "dblp.org", Path: "/rec/" + key + ".bib"}).String()
	directCtx, cancel := context.WithTimeout(ctx, 4*time.Second)
	bib, err := c.citationExport(directCtx, endpoint)
	cancel()
	if err == nil {
		return bib, nil
	}
	if ctx.Err() != nil {
		return "", err
	}
	if c.Options.BibTeXBrowser {
		if c.Warnf != nil {
			c.Warnf("DBLP direct export failed: %v; trying full-html2", err)
		}
		browserCtx, browserCancel := context.WithTimeout(ctx, 24*time.Second)
		page := (&url.URL{Scheme: "https", Host: "dblp.uni-trier.de", Path: "/rec/" + key + ".html", RawQuery: "view=bibtex"}).String()
		raw, browserErr := c.LoadHTML(browserCtx, page)
		browserCancel()
		if browserErr == nil {
			bib, browserErr = htmlBibTeX(raw, key)
		}
		if browserErr == nil {
			return bib, nil
		}
		err = fmt.Errorf("direct export: %v; full-html2: %w", err, browserErr)
	}
	if c.Options.DeepSeekKey == "" || ctx.Err() != nil {
		return "", err
	}
	if c.Warnf != nil {
		c.Warnf("DBLP direct export failed: %v; trying DeepSeek Fetch", err)
	}
	body, _ := json.Marshal(map[string]string{"url": endpoint})
	raw, _, fetchErr := c.request(ctx, http.MethodPost, "https://api.deepseek.com/v1/tools/fetch", body, map[string]string{"Authorization": "Bearer " + c.Options.DeepSeekKey, "Content-Type": "application/json"})
	if fetchErr != nil {
		return "", fmt.Errorf("direct export: %v; DeepSeek Fetch: %w", err, fetchErr)
	}
	payload, fetchErr := decode(raw)
	if fetchErr != nil {
		return "", fetchErr
	}
	return fetchedBibTeX(str(payload["content"]), key)
}
func (c *Client) doiCitation(ctx context.Context, doi string) (string, error) {
	endpoint := (&url.URL{Scheme: "https", Host: "doi.org", Path: "/" + doi}).String()
	return c.citationExport(ctx, endpoint)
}
func (c *Client) citationExport(ctx context.Context, endpoint string) (string, error) {
	raw, _, err := c.request(ctx, http.MethodGet, endpoint, nil, map[string]string{"Accept": "application/x-bibtex"})
	if err != nil {
		return "", err
	}
	bib := strings.TrimSpace(string(raw))
	if !IsBibTeX(bib) {
		return "", errors.New("export returned no BibTeX (possibly a bot-check page)")
	}
	return bib, nil
}
