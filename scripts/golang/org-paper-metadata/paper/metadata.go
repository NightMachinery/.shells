// Package paper fetches, normalizes, and formats Semantic Scholar paper metadata.
package paper

import (
	"encoding/json"
	"errors"
	"fmt"
	"regexp"
	"strconv"
	"strings"
	"time"
)

// Metadata preserves the list-valued JSON interface used by the shell commands.
type Metadata map[string][]string

func (m Metadata) First(key string) string {
	if len(m[key]) > 0 {
		return m[key][0]
	}
	return ""
}
func (m Metadata) Put(key string, values ...string) {
	for _, value := range values {
		value = strings.TrimSpace(value)
		if value == "" {
			continue
		}
		found := false
		for _, old := range m[key] {
			if old == value {
				found = true
				break
			}
		}
		if !found {
			m[key] = append(m[key], value)
		}
	}
}
func (m Metadata) Validate() error {
	var missing []string
	for _, key := range []string{"title", "authors_names", "abstract"} {
		if strings.TrimSpace(m.First(key)) == "" {
			missing = append(missing, key)
		}
	}
	if len(missing) > 0 {
		return fmt.Errorf("incomplete paper metadata: missing %s", strings.Join(missing, ", "))
	}
	return nil
}
func (m Metadata) Stamp(source string, now time.Time) {
	m["metadataSource"] = []string{source}
	m["metadataFetchedAt"] = []string{now.UTC().Format(time.RFC3339)}
}
func str(v any) string {
	switch v := v.(type) {
	case string:
		return v
	case json.Number:
		return string(v)
	case float64:
		return strconv.FormatFloat(v, 'f', -1, 64)
	case bool:
		return strconv.FormatBool(v)
	case map[string]any:
		return str(v["text"])
	}
	return ""
}
func object(v any) map[string]any { m, _ := v.(map[string]any); return m }
func firstObject(v any) map[string]any {
	if a, ok := v.([]any); ok && len(a) > 0 {
		return object(a[0])
	}
	return object(v)
}
func array(v any) []any { a, _ := v.([]any); return a }
func decode(raw []byte) (map[string]any, error) {
	var d map[string]any
	dec := json.NewDecoder(strings.NewReader(string(raw)))
	dec.UseNumber()
	if err := dec.Decode(&d); err != nil {
		return nil, err
	}
	if d == nil {
		return nil, errors.New("metadata must be a JSON object")
	}
	return d, nil
}

func ParsePageData(raw []byte, url string) (Metadata, error) {
	d, err := decode(raw)
	if err != nil {
		return nil, err
	}
	p := object(d["paper"])
	if p == nil {
		return nil, fmt.Errorf("page-data response has no paper (responseType=%q)", str(d["responseType"]))
	}
	id := str(p["id"])
	if id == "" || !strings.EqualFold(id, PaperID(url)) {
		return nil, errors.New("page-data paper ID does not match the requested paper")
	}
	m := Metadata{"paperId": {id}, "url": {url}}
	for key, field := range map[string]string{"title": "title", "abstract": "paperAbstract", "year": "year", "venue": "venue", "corpusID": "corpusId", "publicationDate": "pubDate", "tldr": "tldr"} {
		m.Put(key, str(p[field]))
	}
	// paperAbstractTruncated is a preview string, never a substitute abstract.
	if str(p["pubUpdateDate"]) != m.First("publicationDate") {
		m.Put("publicationUpdateDate", str(p["pubUpdateDate"]))
	}
	for _, row := range array(p["authors"]) {
		if a, ok := row.([]any); ok {
			if len(a) == 0 {
				continue
			}
			row = a[0]
		}
		m.Put("authors_names", str(object(row)["name"]))
	}
	stats := object(p["citationStats"])
	for key, field := range map[string]string{"citationCount": "numCitations", "referenceCount": "numReferences", "influentialCitationCount": "numKeyCitations", "keyReferenceCount": "numKeyReferences"} {
		m.Put(key, str(stats[field]))
	}
	for _, field := range array(p["fieldsOfStudy"]) {
		m.Put("fieldsOfStudy", str(field))
	}
	for _, badge := range array(p["badges"]) {
		if str(object(badge)["id"]) == "OPEN_ACCESS" {
			m.Put("isOpenAccess", "true")
		}
	}
	links := append([]any{}, array(p["links"])...)
	links = append(links, array(p["alternatePaperLinks"])...)
	links = append(links, p["primaryPaperLink"])
	for _, link := range links {
		m.AddLink(str(object(link)["url"]), str(object(link)["linkType"]) == "pdf")
	}
	m.normalizeArxivLinks()
	return m, m.Validate()
}

// ParseJSON accepts both Graph API objects and the previous list-valued schema.
func ParseJSON(raw []byte, fallbackURL string) (Metadata, error) {
	d, err := decode(raw)
	if err != nil {
		return nil, err
	}
	m := Metadata{}
	for key, v := range d {
		if a, ok := v.([]any); ok {
			for _, item := range a {
				m.Put(key, str(item))
			}
		} else {
			m.Put(key, str(v))
		}
	}
	m.Put("bibtex", str(firstObject(d["citationStyles"])["bibtex"]))
	for _, a := range array(d["authors"]) {
		m.Put("authors_names", str(object(a)["name"]))
	}
	for key, field := range map[string]string{"doi": "DOI", "corpusID": "CorpusId"} {
		m.Put(key, str(firstObject(d["externalIds"])[field]))
	}
	if arxiv := str(firstObject(d["externalIds"])["ArXiv"]); arxiv != "" {
		m.Put("arxiv", "https://arxiv.org/abs/"+arxiv)
	}
	if journal := str(firstObject(d["journal"])["name"]); journal != "" {
		m.Put("journal_name", journal)
	}
	if pdf := str(firstObject(d["openAccessPdf"])["url"]); pdf != "" {
		m.Put("openAccessPdf", pdf)
		m.AddLink(pdf, true)
	}
	if m.First("url") == "" {
		m.Put("url", fallbackURL)
	}
	if id := PaperID(m.First("url")); id != "" {
		m.Put("paperId", id)
	}
	for _, link := range m["links"] {
		m.AddLink(link, false)
	}
	if value := m.First("corpusID"); value != "" {
		m["corpusID"] = []string{regexp.MustCompile(`(?i)^\s*Corpus\s*ID:\s*`).ReplaceAllString(value, "")}
	}
	m.normalizeArxivLinks()
	return m, m.Validate()
}

func (m Metadata) AddLink(link string, pdf bool) {
	if !webURL(link) {
		return
	}
	pdf = pdf || arxivPDF(link)
	link = LatestArxivURL(link)
	m.Put("links", link)
	if pdf || strings.HasSuffix(strings.ToLower(strings.Split(link, "?")[0]), ".pdf") {
		m.Put("pdf_urls", link)
	}
	if id, ok := arxivID(link); ok {
		m.Put("arxiv", "https://arxiv.org/abs/"+id)
	}
}

var yearRE = regexp.MustCompile(`\b(?:19|20)\d{2}\b`)

// HeadingTags contains only the tags owned by the paper converter.
func HeadingTags(m Metadata) []string {
	var tags []string
	if n := m.First("citationCount"); n != "" {
		tags = append(tags, "@citations/"+n)
	}
	date := m.First("publicationDate")
	year := yearRE.FindString(date)
	if year == "" {
		year = yearRE.FindString(m.First("year"))
	}
	if year != "" {
		tag := "@" + year
		if d, err := time.Parse("2006-01-02", date); err == nil {
			tag += "/" + d.Month().String()
		}
		tags = append(tags, tag)
	}
	venue := m.First("venue")
	if venue == "" {
		venue = m.First("journal_name")
	}
	if venue != "" {
		tags = append(tags, "@"+VenueTag(venue))
	}
	return tags
}
