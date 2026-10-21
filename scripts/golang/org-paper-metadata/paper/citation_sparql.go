package paper

import (
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"net/url"
)

// Exact identifier lookup, without a title search or local citation generation.
func (c *Client) doiFromDBLP(ctx context.Context, key string) (string, error) {
	if normalizeDBLP(key) != key {
		return "", errors.New("invalid DBLP key")
	}
	query := "SELECT ?doi WHERE { <https://dblp.org/rec/" + key + "> <https://dblp.org/rdf/schema#doi> ?doi } LIMIT 2"
	endpoint := "https://sparql.dblp.org/sparql?" + url.Values{"query": {query}}.Encode()
	raw, _, err := c.request(ctx, http.MethodGet, endpoint, nil, map[string]string{"Accept": "application/sparql-results+json"})
	if err != nil {
		return "", err
	}
	var result struct {
		Results struct {
			Bindings []struct {
				DOI struct {
					Type  string `json:"type"`
					Value string `json:"value"`
				} `json:"doi"`
			} `json:"bindings"`
		} `json:"results"`
	}
	if err = json.Unmarshal(raw, &result); err != nil {
		return "", err
	}
	if len(result.Results.Bindings) == 0 {
		return "", nil
	}
	if len(result.Results.Bindings) != 1 {
		return "", errors.New("DBLP has multiple DOI values; refusing an ambiguous citation lookup")
	}
	doi := normalizeDOIURL(result.Results.Bindings[0].DOI.Value)
	if doi == "" || result.Results.Bindings[0].DOI.Type != "uri" {
		return "", errors.New("DBLP returned an invalid DOI identifier")
	}
	return doi, nil
}
