// Command fanficfare2org converts FanFicFare --json-meta JSON on stdin into an
// Org-mode entry (link heading, :PROPERTIES: drawer, and #+BEGIN_QUOTE block).
package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
)

// propertyKeys preserves the exact order and key set from fanficfare2org.lisp.
var propertyKeys = []string{
	"author", "authorUrl",
	"series", "seriesUrl",
	"status", "threadmarks_status",
	"numWords", "estimatedWords",
	"chapterslashtotal", "numChapters",
	"datePublished", "dateUpdated",
	"stars", "likes", "dislikes", "kudos", "reviews", "favs", "follows",
	"views", "total_views", "hits",
	"ships",
	"fandoms",
	"ao3categories",
	"freeformtags", "forumtags", "genre", "extratags",
	"rating",
	"characters",
	"short_description", "threadmarks_description",
	"groups",
	"prequel", "sequels",
	"authorLastLogin",
	"storyId",
	"cover_image",
}

var pandocFilterFiles = []string{
	"drop_empty_inlines.lua",
	"org_math_env.lua",
	"org_soft_linebreaks.lua",
	"org_details.lua",
	"org_sup_sub.lua",
	"org_raw_html.lua",
}

var (
	reTitleOpen       = regexp.MustCompile(`\\*\[`)
	reTitleClose      = regexp.MustCompile(`\\*\]`)
	rePandocOrgExtra  = regexp.MustCompile("(?:\n|\\A)[ \t]*(?:(?s::PROPERTIES:.*?:END:)|<<[^\n]*?>>)[ \t]*|\\^\\{//\\}")
	reInlineDataImage = regexp.MustCompile(`\[\[data:image[^\n]*?\]\]`)
	reListCounter     = regexp.MustCompile(`^([ \t]*\d+\.[ \t]+)\[@\d+\][ \t]`)
	reCiteT           = regexp.MustCompile(`\[cite/t:([^\[\]\n]+)\]`)
	reExampleBlock    = regexp.MustCompile(`(?i)^([ \t]*#\+(?:begin|end)_)example\b`)
	reBeginSrcLine    = regexp.MustCompile(`(?i)^[ \t]*#\+begin_src\b`)
	reStyleAttr       = regexp.MustCompile(`(?i):style`)
)

func main() {
	if len(os.Args) > 1 && (os.Args[1] == "-h" || os.Args[1] == "--help") {
		fmt.Fprintln(os.Stderr, "usage: fanficfare2org < metadata.json")
		return
	}
	if err := run(os.Stdin, os.Stdout, pandocHTMLToOrg); err != nil {
		fmt.Fprintf(os.Stderr, "fanficfare2org: %v\n", err)
		os.Exit(1)
	}
}

func run(r io.Reader, w io.Writer, html2org func(string) (string, error)) error {
	data, err := parseInputJSON(r)
	if err != nil {
		return err
	}

	urlStr := ""
	if rawURL, ok := jsonGet(data, "storyUrl"); ok {
		if s, sok := scalarString(rawURL); sok {
			urlStr = s
		}
	}
	var titlePtr *string
	if rawTitle, ok := jsonGet(data, "title"); ok {
		if s, sok := scalarString(rawTitle); sok {
			titlePtr = &s
		}
	}

	if _, err := io.WriteString(w, orgLinkCreate(urlStr, titlePtr)+"\n"); err != nil {
		return err
	}

	if err := orgPropertiesWrite(w, propertyKeys, func(k string) (any, bool) {
		return jsonGet(data, k)
	}, true); err != nil {
		return err
	}

	if rawDesc, ok := jsonGet(data, "description"); ok {
		if desc, sok := scalarString(rawDesc); sok {
			orgDesc, err := html2org(desc)
			if err != nil {
				return err
			}
			if err := orgQuoteWrite(w, &orgDesc); err != nil {
				return err
			}
		}
	}
	return nil
}

func parseInputJSON(r io.Reader) (map[string]any, error) {
	raw, err := io.ReadAll(r)
	if err != nil {
		return nil, fmt.Errorf("reading stdin: %w", err)
	}
	if len(bytes.TrimSpace(raw)) == 0 {
		return nil, errors.New("empty JSON input")
	}

	dec := json.NewDecoder(bytes.NewReader(raw))
	dec.UseNumber()
	var parsed any
	if err := dec.Decode(&parsed); err != nil {
		return nil, fmt.Errorf("invalid JSON: %w", err)
	}
	obj, ok := parsed.(map[string]any)
	if !ok {
		return nil, fmt.Errorf("expected JSON object, got %T", parsed)
	}
	return obj, nil
}

// jsonGet mirrors jsown:val-safe: missing, null, and false are treated as absent.
func jsonGet(m map[string]any, key string) (any, bool) {
	if m == nil {
		return nil, false
	}
	val, exists := m[key]
	if !exists || val == nil {
		return nil, false
	}
	if b, ok := val.(bool); ok && !b {
		return nil, false
	}
	return val, true
}

func scalarString(v any) (string, bool) {
	if v == nil {
		return "", false
	}
	switch x := v.(type) {
	case bool:
		if !x {
			return "", false
		}
		return "T", true
	case string:
		return x, true
	case json.Number:
		return x.String(), true
	default:
		return fmt.Sprint(x), true
	}
}

func orgTitleEscape(s string) string {
	s = reTitleOpen.ReplaceAllString(s, "{")
	return reTitleClose.ReplaceAllString(s, "}")
}

func orgLinkCreate(url string, title *string) string {
	if title != nil {
		return fmt.Sprintf("[[%s][%s]]", url, orgTitleEscape(*title))
	}
	return fmt.Sprintf("[[%s]]", url)
}

func orgPropertiesWrite(w io.Writer, keys []string, get func(string) (any, bool), folded bool) error {
	if _, err := io.WriteString(w, ":PROPERTIES:\n"); err != nil {
		return err
	}
	for _, key := range keys {
		raw, ok := get(key)
		if !ok {
			continue
		}
		var items []any
		if arr, isArr := raw.([]any); isArr {
			items = arr
		} else {
			items = []any{raw}
		}

		plus := ""
		for _, item := range items {
			val, sok := scalarString(item)
			if !sok || val == "" {
				break
			}
			if _, err := fmt.Fprintf(w, ":%s%s: %s\n", key, plus, val); err != nil {
				return err
			}
			plus = "+"
		}
	}
	if folded {
		if _, err := io.WriteString(w, ":visibility: folded\n"); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, ":END:\n")
	return err
}

func orgQuoteWrite(w io.Writer, content *string) error {
	if content == nil {
		return nil
	}
	_, err := fmt.Fprintf(w, "#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n", *content)
	return err
}

func pandocHTMLToOrg(html string) (string, error) {
	if html == "" {
		return "", nil
	}

	args := []string{"--wrap=none"}
	for _, f := range findPandocFilters() {
		args = append(args, "--lua-filter="+f)
	}
	args = append(args, "--from", "html", "--to", "org", "-", "-o", "-")

	cmd := exec.Command("pandoc", args...)
	cmd.Stdin = strings.NewReader(html)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(stderr.String())
		if msg != "" {
			return "", fmt.Errorf("pandoc failed: %w: %s", err, msg)
		}
		return "", fmt.Errorf("pandoc failed: %w", err)
	}

	out := postprocessPandocOrg(stdout.String())
	return strings.TrimRight(out, "\n"), nil
}

func findPandocFilters() []string {
	var dirs []string
	if nightdir := os.Getenv("NIGHTDIR"); nightdir != "" {
		dirs = append(dirs, filepath.Join(nightdir, "python", "pandoc_filters"))
	}
	if wd, err := os.Getwd(); err == nil {
		for dir := wd; ; {
			dirs = append(dirs, filepath.Join(dir, "python", "pandoc_filters"))
			parent := filepath.Dir(dir)
			if parent == dir {
				break
			}
			dir = parent
		}
	}
	if home, err := os.UserHomeDir(); err == nil && home != "" {
		dirs = append(dirs, filepath.Join(home, "scripts", "python", "pandoc_filters"))
	}

	var filterDir string
	for _, d := range dirs {
		if info, err := os.Stat(d); err == nil && info.IsDir() {
			filterDir = d
			break
		}
	}
	if filterDir == "" {
		return nil
	}

	var out []string
	for _, name := range pandocFilterFiles {
		p := filepath.Join(filterDir, name)
		if info, err := os.Stat(p); err == nil && !info.IsDir() {
			out = append(out, p)
		}
	}
	return out
}

// postprocessPandocOrg mirrors the HTML-to-Org postprocessing in pandoc.zsh.
func postprocessPandocOrg(s string) string {
	s = strings.ReplaceAll(s, "\u00a0", " ")
	s = rePandocOrgExtra.ReplaceAllString(s, "")

	lines := strings.Split(s, "\n")
	for i, line := range lines {
		line = reInlineDataImage.ReplaceAllString(line, "")
		line = reListCounter.ReplaceAllString(line, "$1")
		line = reCiteT.ReplaceAllString(line, "$1")
		line = reExampleBlock.ReplaceAllStringFunc(line, func(m string) string {
			sub := reExampleBlock.FindStringSubmatch(m)
			return strings.ToLower(sub[1]) + "src"
		})
		if reBeginSrcLine.MatchString(line) {
			line += " :eval never"
			line = stripSrcStyleAttr(line)
		}
		lines[i] = line
	}
	return stripDuplicateBlankLines(strings.Join(lines, "\n"))
}

// stripSrcStyleAttr mirrors perl `s/:style.*?(?:$|\s(*pla::))//ig` on #+begin_src lines.
func stripSrcStyleAttr(line string) string {
	var b strings.Builder
	i := 0
	for i < len(line) {
		loc := reStyleAttr.FindStringIndex(line[i:])
		if loc == nil {
			b.WriteString(line[i:])
			break
		}
		start := i + loc[0]
		b.WriteString(line[i:start])

		end := i + loc[1]
		for end < len(line) {
			if (line[end] == ' ' || line[end] == '\t') && end+1 < len(line) && line[end+1] == ':' {
				end++
				break
			}
			end++
		}
		i = end
	}
	return b.String()
}

func stripDuplicateBlankLines(s string) string {
	lines := strings.SplitAfter(s, "\n")
	var b strings.Builder
	seenBlank := false
	for _, line := range lines {
		if line == "" {
			continue
		}
		if strings.TrimSpace(line) != "" {
			b.WriteString(line)
			seenBlank = false
		} else if !seenBlank {
			b.WriteString(line)
			seenBlank = true
		}
	}
	return b.String()
}
