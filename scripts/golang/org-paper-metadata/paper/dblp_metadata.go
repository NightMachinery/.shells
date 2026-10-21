package paper

import (
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"net/url"
	"sort"
	"strconv"
	"strings"
	"time"
)

const dblpSchema = "https://dblp.org/rdf/schema#"

type rdfValue struct {
	Type  string `json:"type"`
	Value string `json:"value"`
}
type sparqlRow map[string]rdfValue

func (c *Client) sparql(ctx context.Context, query string) ([]sparqlRow, error) {
	endpoint := "https://sparql.dblp.org/sparql?" + url.Values{"query": {query}}.Encode()
	raw, _, err := c.request(ctx, http.MethodGet, endpoint, nil, map[string]string{"Accept": "application/sparql-results+json"})
	if err != nil {
		return nil, err
	}
	var result struct {
		Results *struct {
			Bindings []sparqlRow `json:"bindings"`
		} `json:"results"`
	}
	if err = json.Unmarshal(raw, &result); err != nil {
		return nil, err
	}
	if result.Results == nil {
		return nil, errors.New("SPARQL response has no results")
	}
	return result.Results.Bindings, nil
}

func (c *Client) enrichDBLP(ctx context.Context, m Metadata) error {
	key := normalizeDBLP(m.First("dblp"))
	if key == "" && normalizeDOI(m.First("doi")) == "" {
		idsCtx, cancel := context.WithTimeout(ctx, 8*time.Second)
		bib, err := c.semanticScholarIdentifiers(idsCtx, m, c.Options.FetchBibTeX)
		cancel()
		if err != nil {
			return err
		}
		if IsBibTeX(bib) && c.Options.FetchBibTeX {
			m["bibtex"] = []string{bib}
		}
		key = normalizeDBLP(m.First("dblp"))
	}
	// Prefer an exact DOI match to a stale/preprint DBLP association. DOI case is
	// insignificant; bounded variants use indexed equality and are rechecked.
	if doi := normalizeDOI(m.First("doi")); doi != "" && (key == "" || strings.HasPrefix(key, "journals/corr/")) {
		lookupCtx, cancel := context.WithTimeout(ctx, 6*time.Second)
		published, err := c.dblpKeyForDOI(lookupCtx, doi)
		cancel()
		if err != nil {
			return err
		}
		if published != "" {
			key = published
		}
	}
	if key == "" {
		return nil
	}
	recordCtx, cancel := context.WithTimeout(ctx, 8*time.Second)
	defer cancel()
	iri := "<https://dblp.org/rec/" + key + ">"
	query := `PREFIX d: <https://dblp.org/rdf/schema#>
SELECT ?record ?scope ?p ?o ?ord ?name WHERE {
 BIND(` + iri + ` AS ?record)
 { { ` + iri + ` ?p ?o . BIND("paper" AS ?scope) }
 UNION { ` + iri + ` d:publishedAsPartOf ?parent . ?parent ?p ?o . BIND("parent" AS ?scope) }
 UNION { ` + iri + ` d:hasSignature ?s . ?s a d:AuthorSignature ; d:signatureOrdinal ?ord ; d:signatureDblpName ?name . BIND("author" AS ?scope) } }
} LIMIT 1000`
	rows, err := c.sparql(recordCtx, query)
	if err != nil {
		return err
	}
	return mergeDBLP(m, key, rows)
}
func (c *Client) dblpKeyForDOI(ctx context.Context, doi string) (string, error) {
	lower := (&url.URL{Scheme: "https", Host: "doi.org", Path: "/" + doi}).String()
	upper := (&url.URL{Scheme: "https", Host: "doi.org", Path: "/" + strings.ToUpper(doi)}).String()
	query := `PREFIX d: <https://dblp.org/rdf/schema#> SELECT DISTINCT ?paper ?doi WHERE { VALUES ?doi { <` + lower + `> <` + upper + `> } ?paper d:doi ?doi } LIMIT 3`
	rows, err := c.sparql(ctx, query)
	if err != nil {
		return "", err
	}
	keys := map[string]bool{}
	for _, row := range rows {
		key := normalizeDBLPURL(row["paper"].Value)
		if row["paper"].Type != "uri" || row["doi"].Type != "uri" || normalizeDOIURL(row["doi"].Value) != doi || key == "" {
			return "", errors.New("DBLP DOI lookup returned mismatched identifiers")
		}
		keys[key] = true
	}
	if len(keys) > 1 {
		return "", errors.New("multiple DBLP records match this DOI")
	}
	for key := range keys {
		return key, nil
	}
	return "", nil
}
func mergeDBLP(m Metadata, key string, rows []sparqlRow) error {
	if len(rows) == 0 {
		return nil
	}
	if len(rows) >= 1000 {
		return errors.New("DBLP record exceeds the enrichment limit")
	}
	paper, parent := Metadata{}, Metadata{}
	authors := map[int]string{}
	for _, row := range rows {
		if row["record"].Type != "uri" || normalizeDBLPURL(row["record"].Value) != key {
			return errors.New("DBLP metadata record does not match the requested key")
		}
		if row["scope"].Value == "author" {
			order, err := strconv.Atoi(row["ord"].Value)
			if err == nil && order > 0 && row["name"].Type == "literal" {
				if old := authors[order]; old != "" && old != row["name"].Value {
					return errors.New("ambiguous DBLP author order")
				}
				authors[order] = row["name"].Value
			}
			continue
		}
		dest := paper
		if row["scope"].Value == "parent" {
			dest = parent
		} else if row["scope"].Value != "paper" {
			continue
		}
		predicate := strings.TrimPrefix(row["p"].Value, dblpSchema)
		v := row["o"]
		if v.Type == "literal" {
			dest.Put(predicate, v.Value)
		}
		if v.Type == "uri" {
			switch predicate {
			case "doi":
				if doi := normalizeDOIURL(v.Value); doi != "" {
					dest.Put("doi", doi)
				}
			case "isbn", "issn":
				if strings.HasPrefix(v.Value, "urn:"+predicate+":") {
					dest.Put(predicate, strings.TrimPrefix(v.Value, "urn:"+predicate+":"))
				}
			case "bibtexType":
				dest.Put("publicationType", strings.TrimPrefix(v.Value, "http://purl.org/net/nknouf/ns/bibtex#"))
			case "documentPage", "primaryDocumentPage":
				if webURL(v.Value) {
					dest.Put("links", v.Value)
				}
			}
		}
	}
	if paper.First("title") == "" {
		return errors.New("DBLP record has no publication title")
	}
	// Do not let a preprint downgrade known published metadata. Its exact public
	// record link is still useful; no source fields are manufactured.
	if known := normalizeDOI(m.First("doi")); known != "" && !strings.HasPrefix(known, "10.48550/arxiv.") && len(paper["doi"]) == 1 && paper.First("doi") != known {
		return errors.New("DBLP DOI conflicts with the resolved paper's DOI")
	}
	m["dblp"] = []string{"https://dblp.org/rec/" + key}
	if strings.HasPrefix(key, "journals/corr/") {
		return nil
	}
	for target, source := range map[string]string{"title": "title", "year": "yearOfPublication", "venue": "publishedIn", "journal_name": "publishedInJournal", "publicationType": "publicationType", "pages": "pagination", "volume": "publishedInJournalVolume", "number": "publishedInJournalVolumeIssue", "series": "publishedInSeries", "seriesVolume": "publishedInSeriesVolume", "publisher": "publishedBy", "isbn": "isbn", "issn": "issn"} {
		values := paper[source]
		if len(values) == 0 && target != "title" {
			values = parent[source]
		}
		if len(values) > 0 {
			m[target] = append([]string{}, values...)
		}
	}
	if paper.First("publishedInBook") != "" {
		title := parent.First("title")
		if title == "" {
			title = paper.First("publishedInBook")
		}
		m["booktitle"] = []string{title}
	}
	if len(paper["doi"]) == 1 {
		m["doi"] = append([]string{}, paper["doi"]...)
	}
	count, _ := strconv.Atoi(paper.First("numberOfCreators"))
	if count > 0 && count == len(authors) {
		order := make([]int, 0, len(authors))
		for n := range authors {
			order = append(order, n)
		}
		sort.Ints(order)
		names := []string{}
		for i, n := range order {
			if n != i+1 {
				names = nil
				break
			}
			names = append(names, authors[n])
		}
		if len(names) == count {
			m["authors_names"] = names
		}
	}
	for _, link := range paper["links"] {
		m.AddLink(link, false)
	}
	return nil
}
