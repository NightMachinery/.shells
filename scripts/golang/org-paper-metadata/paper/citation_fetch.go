package paper

import (
	"bytes"
	"errors"
	"regexp"
	"strings"

	"golang.org/x/net/html"
)

var fetchedEntryRE = regexp.MustCompile(`(?im)^\s*@[a-z]+\s*\{`)
var fetchedKeyRE = regexp.MustCompile(`(?is)^\s*@[a-z]+\s*\{\s*([^,\s]+)\s*,`)

// Extract a complete, verbatim source record from Fetch Markdown. The DBLP key
// must equal the requested publication key; related or truncated entries fail.
// No fields, keys, escaping, or citation types are generated or repaired.
func fetchedBibTeX(content, key string) (string, error) {
	var result string
	for _, match := range fetchedEntryRE.FindAllStringIndex(content, -1) {
		start, end := match[0], match[1]
		depth := 1
		for end < len(content) && depth > 0 {
			switch content[end] {
			case '\\':
				end++
				if end >= len(content) {
					break
				}
			case '{':
				depth++
			case '}':
				depth--
			}
			end++
		}
		if depth != 0 {
			continue
		}
		entry := strings.TrimSpace(content[start:end])
		fields := fetchedKeyRE.FindStringSubmatch(entry)
		if len(fields) != 2 || fields[1] != "DBLP:"+key {
			continue
		}
		if result != "" {
			return "", errors.New("Fetch returned multiple records for the requested DBLP key")
		}
		result = entry
	}
	if result == "" {
		return "", errors.New("Fetch has no complete BibTeX record matching the requested DBLP key")
	}
	return result, nil
}

// The browser returns HTML. Preserve text inside each source/pre block exactly,
// decoding HTML entities without collapsing whitespace or rewriting BibTeX.
func htmlBibTeX(raw []byte, key string) (string, error) {
	root, err := html.Parse(bytes.NewReader(raw))
	if err != nil {
		return "", err
	}
	var citation string
	var walk func(*html.Node)
	walk = func(n *html.Node) {
		if n.Type == html.ElementNode && (n.Data == "pre" || n.Data == "code") {
			var b strings.Builder
			var text func(*html.Node)
			text = func(x *html.Node) {
				if x.Type == html.TextNode {
					b.WriteString(x.Data)
				}
				for c := x.FirstChild; c != nil; c = c.NextSibling {
					text(c)
				}
			}
			text(n)
			if bib, e := fetchedBibTeX(b.String(), key); e == nil {
				citation = bib
				return
			}
		}
		for c := n.FirstChild; c != nil && citation == ""; c = c.NextSibling {
			walk(c)
		}
	}
	walk(root)
	if citation == "" {
		return "", errors.New("browser has no source BibTeX matching the requested DBLP key")
	}
	return citation, nil
}
