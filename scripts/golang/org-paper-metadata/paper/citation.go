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
		if fallback != "" && (source == "semantic-scholar" || (normalizeDBLP(m.First("dblp")) != "")) {
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
		if strings.HasPrefix(normalizeDBLP(m.First("dblp")), "journals/corr/") && normalizeDOI(m.First("doi")) != "" {
			providers = []string{"doi", "dblp", "semantic-scholar"}
		}
	}
	// Expensive retrieval is a last resort, after every selected programmatic
	// provider. Browser use additionally requires an explicit opt-in.
	if source == "auto" || source == "dblp" {
		if c.Options.DeepSeekKey != "" {
			providers = append(providers, "dblp-fetch")
		}
		if c.Options.BibTeXBrowser {
			providers = append(providers, "dblp-browser")
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
		case "dblp", "doi", "dblp-fetch", "dblp-browser":
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
			if provider == "doi" && identifier == "" && normalizeDBLP(m.First("dblp")) != "" {
				lookupCtx, lookupCancel := context.WithTimeout(ctx, 8*time.Second)
				var lookupErr error
				identifier, lookupErr = c.doiFromDBLP(lookupCtx, normalizeDBLP(m.First("dblp")))
				lookupCancel()
				if lookupErr != nil {
					failures = append(failures, fmt.Errorf("DBLP DOI lookup: %w", lookupErr))
				}
				if identifier != "" {
					m["doi"] = []string{identifier}
				}
			}
			if identifier == "" {
				continue
			}
			duration := 8 * time.Second
			if provider == "dblp-browser" {
				duration = 24 * time.Second
			}
			requestCtx, cancel := context.WithTimeout(ctx, duration)
			switch provider {
			case "dblp":
				bib, err = c.dblpCitation(requestCtx, identifier)
			case "doi":
				bib, err = c.doiCitation(requestCtx, identifier)
			case "dblp-fetch":
				bib, err = c.deepseekDBLPCitation(requestCtx, identifier)
			case "dblp-browser":
				bib, err = c.browserDBLPCitation(requestCtx, identifier)
			}
			cancel()
		}
		if err == nil && IsBibTeX(bib) {
			m["bibtex"] = []string{strings.TrimSpace(bib)}
			m["bibtexSource"] = []string{strings.TrimSuffix(strings.TrimSuffix(provider, "-fetch"), "-browser")}
			return nil
		}
		if err != nil {
			failures = append(failures, fmt.Errorf("%s: %w", provider, err))
			if c.Warnf != nil && provider != "semantic-scholar" && !errors.Is(err, errDBLPExportBlocked) {
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
	return c.semanticScholarIdentifiers(ctx, m, true)
}
func (c *Client) semanticScholarIdentifiers(ctx context.Context, m Metadata, withBib bool) (string, error) {
	id := strings.ToLower(m.First("paperId"))
	if !idPattern.MatchString(id) {
		return "", errors.New("no valid Semantic Scholar paper ID for citation export")
	}
	body, _ := json.Marshal(map[string][]string{"ids": {id}})
	fields := "externalIds"
	if withBib {
		fields += ",citationStyles"
	}
	raw, _, err := c.request(ctx, http.MethodPost, "https://api.semanticscholar.org/graph/v1/paper/batch?fields="+fields, body, map[string]string{"Content-Type": "application/json"})
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

var errCitationBotChallenge = errors.New("export returned a bot-check page")
var errDBLPExportBlocked = errors.New("DBLP BibTeX export is bot-blocked for this run")

func (c *Client) dblpCitation(ctx context.Context, key string) (string, error) {
	c.dblpExportMu.Lock()
	if c.dblpExportBlocked {
		c.dblpExportMu.Unlock()
		return "", errDBLPExportBlocked
	}
	requestCtx, cancel := context.WithTimeout(ctx, 4*time.Second)
	endpoint := (&url.URL{Scheme: "https", Host: "dblp.org", Path: "/rec/" + key + ".bib"}).String()
	bib, err := c.citationExport(requestCtx, endpoint)
	cancel()
	blocked := errors.Is(err, errCitationBotChallenge)
	if blocked {
		c.dblpExportBlocked = true
	}
	c.dblpExportMu.Unlock()
	if blocked {
		if c.Warnf != nil {
			c.Warnf("DBLP BibTeX export is bot-blocked; skipping direct exports for the rest of this run (SPARQL metadata is unaffected)")
		}
		return "", errDBLPExportBlocked
	}
	return bib, err
}
func (c *Client) browserDBLPCitation(ctx context.Context, key string) (string, error) {
	if c.Warnf != nil {
		c.Warnf("programmatic citation providers unavailable; trying full-html2 as last resort")
	}
	page := (&url.URL{Scheme: "https", Host: "dblp.uni-trier.de", Path: "/rec/" + key + ".html", RawQuery: "view=bibtex"}).String()
	raw, err := c.LoadHTML(ctx, page)
	if err != nil {
		return "", err
	}
	return htmlBibTeX(raw, key)
}
func (c *Client) deepseekDBLPCitation(ctx context.Context, key string) (string, error) {
	endpoint := (&url.URL{Scheme: "https", Host: "dblp.org", Path: "/rec/" + key + ".bib"}).String()
	body, _ := json.Marshal(map[string]string{"url": endpoint})
	raw, _, err := c.request(ctx, http.MethodPost, "https://api.deepseek.com/v1/tools/fetch", body, map[string]string{"Authorization": "Bearer " + c.Options.DeepSeekKey, "Content-Type": "application/json"})
	if err != nil {
		return "", err
	}
	payload, err := decode(raw)
	if err != nil {
		return "", err
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
		lower := strings.ToLower(bib)
		for _, marker := range []string{"/.within.website/x/", "protected by anubis", "making sure you're not a bot", "making sure you&#39;re not a bot", "human verification", "verify you are human"} {
			if strings.Contains(lower, marker) {
				return "", errCitationBotChallenge
			}
		}
		return "", errors.New("export returned no BibTeX")
	}
	return bib, nil
}
