// pathtree2org prints an Org-mode heading tree for a directory hierarchy.
//
// Ported from commonlisp/pathtree2org.lisp.
// Note from the original script: use "..inline_links.." in the output .org
// filename to force Org link previews in Emacs.
package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"regexp"
	"sort"
	"strings"
)

var (
	reTitleOpen  = regexp.MustCompile(`\\*\[`)
	reTitleClose = regexp.MustCompile(`\\*\]`)
)

func main() {
	bw := bufio.NewWriter(os.Stdout)
	code := run(os.Args[1:], bw, os.Stderr)
	if err := bw.Flush(); err != nil {
		fmt.Fprintf(os.Stderr, "pathtree2org: flush stdout: %v\n", err)
		if code == 0 {
			code = 1
		}
	}
	os.Exit(code)
}

func run(args []string, stdout, stderr io.Writer) int {
	if len(args) < 1 || args[0] == "" {
		fmt.Fprintln(stderr, "usage: pathtree2org <dir> [skip-regex...]")
		return 1
	}

	dir := args[0]
	skips := make([]*regexp.Regexp, 0, len(args)-1)
	for _, pat := range args[1:] {
		re, err := regexp.Compile(pat)
		if err != nil {
			fmt.Fprintf(stderr, "pathtree2org: invalid skip regex %q: %v\n", pat, err)
			return 1
		}
		skips = append(skips, re)
	}

	fmt.Fprintf(stdout, "+TITLE: Pathtree of =%s=\n\n", dir)
	listDirs(1, dir, skips, stdout, stderr)
	return 0
}

type dirItem struct {
	fString string
	fn      string
}

func trimTrailingSlashes(p string) string {
	trimmed := strings.TrimRight(p, "/")
	if trimmed == "" && strings.HasPrefix(p, "/") {
		return "/"
	}
	return trimmed
}

func isRealDir(dir string) bool {
	if dir == "" {
		return false
	}
	info, err := os.Lstat(trimTrailingSlashes(dir))
	if err != nil {
		return false
	}
	if info.Mode()&os.ModeSymlink != 0 {
		return false
	}
	return info.IsDir()
}

func listDirs(lv int, dir string, skips []*regexp.Regexp, stdout, stderr io.Writer) {
	if !isRealDir(dir) {
		return
	}

	fmt.Fprintf(stderr, "INFO: Processing %s ...\n\n", dir)

	entries, err := os.ReadDir(dir)
	if err != nil {
		fmt.Fprintf(stderr, "ERROR: reading %s: %v\n", dir, err)
		return
	}

	base := strings.TrimRight(dir, "/")
	items := make([]dirItem, 0, len(entries))
	for _, entry := range entries {
		name := entry.Name()
		fullPath := base + "/" + name
		isDirEntry := entry.IsDir()
		if !isDirEntry && entry.Type()&os.ModeSymlink != 0 {
			if targetInfo, statErr := os.Stat(fullPath); statErr == nil && targetInfo.IsDir() {
				isDirEntry = true
			}
		}
		if isDirEntry {
			fullPath += "/"
		}
		items = append(items, dirItem{
			fString: fullPath,
			fn:      name,
		})
	}

	sort.Slice(items, func(i, j int) bool {
		return items[i].fString < items[j].fString
	})

	stars := strings.Repeat("*", lv)
	for i, item := range items {
		skipMe := false
		for _, re := range skips {
			if re.MatchString(item.fString) {
				skipMe = true
				break
			}
		}
		if skipMe {
			fmt.Fprintf(stderr, "INFO: Skipped %s\n\n", item.fString)
			continue
		}

		fmt.Fprintf(stdout, "%s [[%s][%s]]\n", stars, orgLinkEscape(item.fString), orgTitleEscape(item.fn))
		listDirs(lv+1, item.fString, skips, stdout, stderr)
		if i < len(items)-1 {
			fmt.Fprint(stdout, "\n")
		}
	}
}

func orgTitleEscape(s string) string {
	s = reTitleOpen.ReplaceAllString(s, "{")
	s = reTitleClose.ReplaceAllString(s, "}")
	return s
}

func orgLinkEscape(s string) string {
	if !strings.ContainsAny(s, "[]") {
		return s
	}
	var b strings.Builder
	b.Grow(len(s) + 4)
	for i := 0; i < len(s); i++ {
		c := s[i]
		if (c == '[' || c == ']') && (i == 0 || s[i-1] != '\\') {
			b.WriteByte('\\')
		}
		b.WriteByte(c)
	}
	return b.String()
}
