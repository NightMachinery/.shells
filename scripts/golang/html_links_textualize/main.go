package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"regexp"
	"strings"

	"golang.org/x/net/html"
	"golang.org/x/net/html/atom"
)

var (
	fullDocRe       = regexp.MustCompile(`(?i)<(?:!doctype|html|head|body)(?:[\s>/]|$)`)
	orgTitleOpenRe  = regexp.MustCompile(`\\*\[`)
	orgTitleCloseRe = regexp.MustCompile(`\\*\]`)
)

func boolsh(s string) bool {
	switch strings.ToLower(s) {
	case "", "n", "no", "0":
		return false
	default:
		return true
	}
}

func boolshEnvY(getenv func(string) string, name string) bool {
	v := getenv(name)
	if v == "" {
		return true
	}
	return boolsh(v)
}

func sTruncate(maxLen int, s string) string {
	const ellipsis = "..."
	runes := []rune(s)
	if len(runes) <= maxLen {
		return s
	}
	keep := maxLen - len(ellipsis)
	if keep < 0 {
		keep = 0
	}
	return string(runes[:keep]) + ellipsis
}

func orgLinkEscape(s string) string {
	var b strings.Builder
	b.Grow(len(s))
	for i := 0; i < len(s); i++ {
		c := s[i]
		if (c == '[' || c == ']') && (i == 0 || s[i-1] != '\\') {
			b.WriteByte('\\')
		}
		b.WriteByte(c)
	}
	return b.String()
}

func orgTitleEscape(s string) string {
	s = orgTitleOpenRe.ReplaceAllString(s, "{")
	return orgTitleCloseRe.ReplaceAllString(s, "}")
}

func getAttr(n *html.Node, key string) (string, bool) {
	for _, a := range n.Attr {
		if a.Key == key {
			return a.Val, true
		}
	}
	return "", false
}

func elementText(n *html.Node) string {
	var b strings.Builder
	var walk func(*html.Node)
	walk = func(cur *html.Node) {
		if cur.Type == html.TextNode {
			b.WriteString(cur.Data)
		}
		for c := cur.FirstChild; c != nil; c = c.NextSibling {
			walk(c)
		}
	}
	for c := n.FirstChild; c != nil; c = c.NextSibling {
		walk(c)
	}
	return b.String()
}

func setElementText(n *html.Node, text string) {
	for c := n.FirstChild; c != nil; {
		next := c.NextSibling
		n.RemoveChild(c)
		c = next
	}
	n.AppendChild(&html.Node{
		Type: html.TextNode,
		Data: text,
	})
}

func textualizeNode(n *html.Node, truncateP bool) {
	if n.Type == html.ElementNode && n.Data == "a" {
		href, hasHref := getAttr(n, "href")
		text := elementText(n)
		if hasHref && href != "" && text != "" && !strings.HasPrefix(href, "#") {
			link := href
			if truncateP {
				link = sTruncate(80, link)
			}
			setElementText(n, "[["+orgLinkEscape(link)+"]["+orgTitleEscape(text)+"]]")
		}
	}
	for c := n.FirstChild; c != nil; c = c.NextSibling {
		textualizeNode(c, truncateP)
	}
}

func processHTML(in string, truncateP bool) (string, error) {
	var nodes []*html.Node
	if fullDocRe.MatchString(in) {
		doc, err := html.Parse(strings.NewReader(in))
		if err != nil {
			return "", err
		}
		nodes = []*html.Node{doc}
	} else {
		bodyCtx := &html.Node{Type: html.ElementNode, Data: "body", DataAtom: atom.Body}
		frag, err := html.ParseFragment(strings.NewReader(in), bodyCtx)
		if err != nil {
			return "", err
		}
		nodes = frag
	}

	for _, n := range nodes {
		textualizeNode(n, truncateP)
	}

	var buf bytes.Buffer
	for _, n := range nodes {
		if err := html.Render(&buf, n); err != nil {
			return "", err
		}
	}
	return buf.String(), nil
}

func run(getenv func(string) string, stdin io.Reader, stdout io.Writer) error {
	truncateP := boolshEnvY(getenv, "html_links_textualize_truncate_p")

	rawIn, err := io.ReadAll(stdin)
	if err != nil {
		return err
	}

	out, err := processHTML(string(rawIn), truncateP)
	if err != nil {
		return err
	}
	_, err = fmt.Fprintln(stdout, out)
	return err
}

func main() {
	if err := run(os.Getenv, os.Stdin, os.Stdout); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
