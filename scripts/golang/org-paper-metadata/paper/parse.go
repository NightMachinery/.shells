package paper

import (
	"encoding/json"
	"errors"
	"regexp"
	"strings"
	"time"
	"unicode"

	"golang.org/x/net/html"
)

func nodeAttr(n *html.Node, key string) string {
	for _, a := range n.Attr {
		if a.Key == key {
			return a.Val
		}
	}
	return ""
}
func nodeText(n *html.Node) string {
	var b strings.Builder
	var walk func(*html.Node)
	walk = func(n *html.Node) {
		if n.Type == html.TextNode {
			b.WriteString(n.Data)
			b.WriteByte(' ')
		}
		for child := n.FirstChild; child != nil; child = child.NextSibling {
			walk(child)
		}
	}
	walk(n)
	return OneLine(b.String())
}
func normalizeTitle(s string) string {
	return strings.Map(func(r rune) rune {
		if unicode.IsLetter(r) || unicode.IsDigit(r) {
			return unicode.ToLower(r)
		}
		return -1
	}, s)
}

func ParseHTML(raw []byte, url string) (Metadata, error) {
	root, err := html.Parse(strings.NewReader(string(raw)))
	if err != nil {
		return nil, err
	}
	m := Metadata{"url": {url}, "paperId": {PaperID(url)}}
	meta := map[string][]string{}
	visible := map[string][]string{}
	var articles []map[string]any
	var visitJSON func(any)
	visitJSON = func(v any) {
		if a, ok := v.([]any); ok {
			for _, item := range a {
				visitJSON(item)
			}
			return
		}
		d := object(v)
		if d == nil {
			return
		}
		kinds := []any{d["@type"]}
		if a, ok := d["@type"].([]any); ok {
			kinds = a
		}
		for _, kind := range kinds {
			if s := str(kind); s == "ScholarlyArticle" || s == "Article" || strings.HasSuffix(s, "/ScholarlyArticle") {
				articles = append(articles, d)
				break
			}
		}
		visitJSON(d["@graph"])
	}
	var walk func(*html.Node)
	walk = func(n *html.Node) {
		if n.Type == html.ElementNode {
			if n.Data == "meta" {
				name := nodeAttr(n, "name")
				if name == "" {
					name = nodeAttr(n, "property")
				}
				if value := strings.TrimSpace(nodeAttr(n, "content")); value != "" {
					meta[name] = append(meta[name], value)
				}
			}
			if n.Data == "script" && nodeAttr(n, "type") == "application/ld+json" {
				var b strings.Builder
				for child := n.FirstChild; child != nil; child = child.NextSibling {
					b.WriteString(child.Data)
				}
				var v any
				if json.Unmarshal([]byte(b.String()), &v) == nil {
					visitJSON(v)
				}
			}
			if selector := nodeAttr(n, "data-selenium-selector"); selector != "" {
				visible[selector] = append(visible[selector], nodeText(n))
			}
			if key := nodeAttr(n, "data-heap-nav"); key != "" {
				visible[key] = append(visible[key], nodeText(n))
			}
			if nodeAttr(n, "data-heap-id") == "paper-meta-journal" {
				visible["venue"] = append(visible["venue"], nodeText(n))
			}
			if nodeAttr(n, "data-selenium-selector") == "paper-link" {
				m.AddLink(nodeAttr(n, "href"), false)
			}
			if strings.Contains(" "+nodeAttr(n, "class")+" ", " bibtex-citation ") {
				m.Put("bibtex", nodeText(n))
			}
		}
		for child := n.FirstChild; child != nil; child = child.NextSibling {
			walk(child)
		}
	}
	walk(root)
	first := func(a []string) string {
		if len(a) > 0 {
			return a[0]
		}
		return ""
	}
	title := first(meta["citation_title"])
	if title == "" {
		title = first(visible["paper-detail-title"])
	}
	var article map[string]any
	for _, a := range articles {
		candidate := str(a["headline"])
		if candidate == "" {
			candidate = str(a["name"])
		}
		if title == "" || normalizeTitle(candidate) == normalizeTitle(title) {
			article = a
			if title == "" {
				title = candidate
			}
			break
		}
	}
	m.Put("title", title)
	if title == "" {
		return nil, errors.New("no paper metadata found; human verification may be required")
	}
	m.Put("abstract", first(meta["citation_abstract"]))
	if m.First("abstract") == "" {
		m.Put("abstract", str(article["abstract"]))
	}
	if m.First("abstract") == "" {
		m.Put("abstract", first(visible["abstract-text"]))
	}
	for _, name := range meta["citation_author"] {
		m.Put("authors_names", name)
	}
	if m.First("authors_names") == "" {
		authors := array(article["author"])
		if authors == nil && article["author"] != nil {
			authors = []any{article["author"]}
		}
		for _, a := range authors {
			name := str(object(a)["name"])
			if name == "" {
				name = str(a)
			}
			m.Put("authors_names", name)
		}
	}
	if m.First("authors_names") == "" {
		m.Put("authors_names", visible["author-name"]...)
	}
	date := first(meta["citation_publication_date"])
	if date == "" {
		date = first(meta["citation_date"])
	}
	if date == "" {
		date = str(article["datePublished"])
	}
	if date == "" {
		date = first(visible["paper-year"])
	}
	if len(date) > 10 && date[10] == 'T' {
		date = date[:10]
	}
	for _, layout := range []string{"2006-01-02", "2006/01/02", "January 2, 2006", "2 January 2006"} {
		if d, e := time.Parse(layout, date); e == nil {
			m.Put("publicationDate", d.Format("2006-01-02"))
			break
		}
	}
	m.Put("year", yearRE.FindString(date))
	for _, key := range []string{"citation_conference_title", "citation_journal_title"} {
		if m.First("venue") == "" {
			m.Put("venue", first(meta[key]))
		}
	}
	if m.First("venue") == "" {
		m.Put("venue", first(visible["venue"]))
	}
	if m.First("venue") == "" {
		m.Put("venue", str(object(article["isPartOf"])["name"]))
	}
	m.Put("doi", meta["citation_doi"]...)
	for _, pdf := range meta["citation_pdf_url"] {
		m.AddLink(pdf, true)
	}
	m.Put("fieldsOfStudy", visible["entity-name"]...)
	corpus := regexp.MustCompile(`\d+`).FindString(first(visible["corpus-id"]))
	m.Put("corpusID", corpus)
	for key, selectors := range map[string][]string{"citationCount": {"citing-papers", "citation-count"}, "referenceCount": {"references"}} {
		for _, selector := range selectors {
			if n := countRE.FindStringSubmatch(first(visible[selector])); n != nil {
				m.Put(key, strings.ReplaceAll(n[1], ",", ""))
				break
			}
		}
	}
	return m, m.Validate()
}

var countRE = regexp.MustCompile(`^\s*([0-9][0-9,]*)\s+(?:Citations?|References?)\b`)
var escapedPunctuation = regexp.MustCompile(`\\([^\pL\pN_\s])`)
var bibStart = regexp.MustCompile(`@\w+\s*\{`)
var bibField = regexp.MustCompile(`^\s*,?\s*(\w+)\s*=\s*`)
var serviceTitle = regexp.MustCompile(`(?i)^(?:\[PDF\]\s*)?(.+?)\s*\|\s*Semantic Scholar\s*$`)

func ParseFetch(raw []byte, url string) (Metadata, error) {
	d, err := decode(raw)
	if err != nil {
		return nil, err
	}
	match := serviceTitle.FindStringSubmatch(strings.TrimSpace(str(d["title"])))
	if match == nil {
		return nil, errors.New("Fetch did not return a Semantic Scholar paper title; human verification may be required")
	}
	m, err := ParseMarkdown(str(d["content"]), url)
	if err != nil {
		return nil, err
	}
	if normalizeTitle(m.First("title")) != normalizeTitle(match[1]) {
		return nil, errors.New("paper BibTeX does not match the fetched page title")
	}
	return m, m.Validate()
}

func ParseMarkdown(markdown, url string) (Metadata, error) {
	text := escapedPunctuation.ReplaceAllString(markdown, "$1")
	primary := strings.Split(text, "\n### ")[0]
	start := bibStart.FindStringIndex(primary)
	if start == nil {
		return nil, errors.New("fetched page is missing the main paper metadata; title and counts are insufficient")
	}
	end, depth := start[1], 1
	for end < len(primary) && depth > 0 {
		if primary[end] == '{' {
			depth++
		}
		if primary[end] == '}' {
			depth--
		}
		end++
	}
	if depth != 0 {
		return nil, errors.New("incomplete paper BibTeX")
	}
	bib := primary[start[0]:end]
	comma := strings.IndexByte(bib, ',')
	if comma < 0 {
		return nil, errors.New("paper BibTeX has no fields")
	}
	fields := map[string]string{}
	for pos := comma + 1; pos < len(bib); {
		match := bibField.FindStringSubmatchIndex(bib[pos:])
		if match == nil {
			break
		}
		key := strings.ToLower(bib[pos+match[2] : pos+match[3]])
		pos += match[1]
		if pos >= len(bib) {
			break
		}
		begin := pos
		var value string
		switch bib[pos] {
		case '{':
			pos++
			begin = pos
			depth = 1
			for pos < len(bib) && depth > 0 {
				if bib[pos] == '{' {
					depth++
				}
				if bib[pos] == '}' {
					depth--
				}
				pos++
			}
			if depth != 0 {
				return nil, errors.New("incomplete BibTeX field")
			}
			value = bib[begin : pos-1]
		case '"':
			pos++
			begin = pos
			for pos < len(bib) && bib[pos] != '"' {
				pos++
			}
			if pos == len(bib) {
				return nil, errors.New("incomplete quoted BibTeX field")
			}
			value = bib[begin:pos]
			pos++
		default:
			for pos < len(bib) && bib[pos] != ',' && bib[pos] != '}' {
				pos++
			}
			value = bib[begin:pos]
		}
		fields[key] = strings.TrimSpace(value)
	}
	m := Metadata{"url": {url}, "paperId": {PaperID(url)}, "bibtex": {bib}}
	for key, field := range map[string]string{"title": "title", "abstract": "abstract", "year": "year", "venue": "booktitle", "doi": "doi"} {
		m.Put(key, fields[field])
	}
	if m.First("venue") == "" {
		m.Put("venue", fields["journal"])
	}
	for _, name := range regexp.MustCompile(`\s+and\s+`).Split(fields["author"], -1) {
		m.Put("authors_names", name)
	}
	m.AddLink(fields["url"], false)
	inPrimary := true
	for _, line := range strings.Split(text, "\n") {
		if strings.HasPrefix(line, "### ") {
			inPrimary = false
		}
		if !inPrimary && !strings.HasPrefix(line, "## ") {
			continue
		}
		line = strings.TrimLeft(line, "# ")
		if match := countRE.FindStringSubmatch(line); match != nil {
			key := "citationCount"
			if strings.Contains(line, "Reference") {
				key = "referenceCount"
			}
			m.Put(key, strings.ReplaceAll(match[1], ",", ""))
		}
	}
	// A leading TLDR is deliberately not promoted to a complete abstract.
	return m, m.Validate()
}
