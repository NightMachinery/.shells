// Port of commonlisp/goodreads_url_json_to_org.lisp: converts Goodreads JSON
// produced by selectors2json.py into an Org-mode entry on stdout.
package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"regexp"
	"strings"
)

var (
	rePandocExtra = regexp.MustCompile(`(?:\A|\n)\s*(?::PROPERTIES:[\s\S]*?:END:|<<[^\n]*?>>)`)
	reNewlines    = regexp.MustCompile(`\n+`)
	reWordBy      = regexp.MustCompile(`\bby\b`)
	reBulletDash  = regexp.MustCompile(`(?m)^\*-\* `)
	reTitleOpen   = regexp.MustCompile(`\\*\[`)
	reTitleClose  = regexp.MustCompile(`\\*\]`)
)

var propertyKeys = []string{
	"authors",
	"rating",
	"ratingCount",
	"reviewCount",
	"series",
	"pageCount",
	"publicationDetails",
	"genres",
	"isbn",
}

func pandocNormalizeWhitespace(s string) string {
	return strings.ReplaceAll(s, "\u00a0", " ")
}

func pandocOrgTrimExtra(s string) string {
	cleaned := rePandocExtra.ReplaceAllString(s, "")
	return strings.Trim(cleaned, " \n")
}

func orgTrimForcedNewlines(s string) string {
	return strings.ReplaceAll(s, "\\", "")
}

func orgTitleEscape(s string) string {
	s = reTitleOpen.ReplaceAllString(s, "{")
	return reTitleClose.ReplaceAllString(s, "}")
}

func orgLinkCreate(url, title string, hasTitle bool) string {
	if hasTitle {
		return fmt.Sprintf("[[%s][%s]]", url, orgTitleEscape(title))
	}
	return fmt.Sprintf("[[%s]]", url)
}

func writeOrgProperties(w io.Writer, keys []string, data map[string][]string) {
	fmt.Fprintln(w, ":PROPERTIES:")
	for _, key := range keys {
		plus := ""
		for _, val := range data[key] {
			if val == "" {
				break
			}
			fmt.Fprintf(w, ":%s%s: %s\n", key, plus, val)
			plus = "+"
		}
	}
	fmt.Fprintln(w, ":visibility: folded")
	fmt.Fprintln(w, ":END:")
}

func writeOrgQuote(w io.Writer, content string, hasContent bool) {
	if !hasContent {
		return
	}
	fmt.Fprintf(w, "#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n", content)
}

func transformValue(key, val string) string {
	val = pandocOrgTrimExtra(pandocNormalizeWhitespace(val))
	switch key {
	case "title":
		return strings.TrimLeft(val, "* ")
	case "authors":
		val = reNewlines.ReplaceAllString(val, " ")
		val = reWordBy.ReplaceAllString(val, "")
		return strings.Trim(val, " ")
	case "description", "descriptionShort":
		val = orgTrimForcedNewlines(val)
		return reBulletDash.ReplaceAllString(val, "- ")
	default:
		return val
	}
}

func parseInput(raw []byte) (map[string][]string, error) {
	trimmed := bytes.TrimSpace(raw)
	if len(trimmed) == 0 {
		return nil, errors.New("empty JSON input")
	}

	var rawMap map[string][]string
	if err := json.Unmarshal(trimmed, &rawMap); err != nil {
		return nil, err
	}
	if rawMap == nil {
		return nil, errors.New("JSON input must be an object")
	}

	out := make(map[string][]string, len(rawMap))
	for key, vs := range rawMap {
		if len(vs) == 0 {
			out[key] = nil
			continue
		}
		processed := make([]string, len(vs))
		for i, v := range vs {
			processed[i] = transformValue(key, v)
		}
		out[key] = processed
	}
	return out, nil
}

func firstVal(data map[string][]string, key string) (string, bool) {
	vs := data[key]
	if len(vs) == 0 {
		return "", false
	}
	return vs[0], true
}

func convert(raw []byte, url string, w io.Writer) error {
	data, err := parseInput(raw)
	if err != nil {
		return err
	}

	title, hasTitle := firstVal(data, "title")
	fmt.Fprintln(w, orgLinkCreate(url, title, hasTitle))

	writeOrgProperties(w, propertyKeys, data)

	desc, hasDesc := firstVal(data, "description")
	if desc == "" {
		desc, hasDesc = firstVal(data, "descriptionShort")
	}
	writeOrgQuote(w, desc, hasDesc)
	return nil
}

func run(args []string, stdin io.Reader, stdout io.Writer) error {
	raw, err := io.ReadAll(stdin)
	if err != nil {
		return err
	}
	url := ""
	if len(args) > 0 {
		url = args[0]
	}
	return convert(raw, url, stdout)
}

func main() {
	if err := run(os.Args[1:], os.Stdin, os.Stdout); err != nil {
		fmt.Fprintf(os.Stderr, "goodreads_url_json_to_org: %v\n", err)
		os.Exit(1)
	}
}
