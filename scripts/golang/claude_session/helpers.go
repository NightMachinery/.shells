package main

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"time"
)

// ** helpers

func humanTimestamp(ts string) string {
	if ts == "" {
		return ""
	}
	t, err := time.Parse(time.RFC3339, ts)
	if err != nil {
		return ts
	}
	return t.Local().Format(orgStamp)
}

func orderedKeys(in map[string]json.RawMessage, preferred []string) []string {
	seen := map[string]bool{}
	var out []string

	for _, k := range preferred {
		if _, ok := in[k]; ok {
			out = append(out, k)
			seen[k] = true
		}
	}

	var rest []string
	for k := range in {
		if !seen[k] {
			rest = append(rest, k)
		}
	}
	sort.Strings(rest)

	return append(out, rest...)
}

func asString(raw json.RawMessage) (string, bool) {
	var s string
	if err := json.Unmarshal(raw, &s); err != nil {
		return "", false
	}
	return s, true
}

func isScalar(raw json.RawMessage) bool {
	s := strings.TrimSpace(string(raw))
	if s == "" {
		return false
	}
	return s[0] != '{' && s[0] != '['
}

func stringAt(in map[string]json.RawMessage, key string) (string, bool) {
	raw, ok := in[key]
	if !ok {
		return "", false
	}
	return asString(raw)
}

func intAt(in map[string]json.RawMessage, key string) (int, bool) {
	raw, ok := in[key]
	if !ok {
		return 0, false
	}
	n, err := strconv.Atoi(strings.TrimSpace(string(raw)))
	if err != nil {
		return 0, false
	}
	return n, true
}

func firstString(in map[string]json.RawMessage, keys ...string) string {
	for _, k := range keys {
		if s, ok := stringAt(in, k); ok && s != "" {
			return s
		}
	}
	return ""
}

var langByExt = map[string]string{
	".c": "c", ".cc": "cpp", ".cpp": "cpp", ".css": "css", ".el": "emacs-lisp",
	".go": "go", ".h": "c", ".hs": "haskell", ".html": "html", ".java": "java",
	".jl": "julia", ".js": "javascript", ".json": "json", ".jsx": "jsx",
	".lua": "lua", ".md": "markdown", ".org": "org", ".pl": "perl",
	".py": "python", ".rb": "ruby", ".rs": "rust", ".scm": "scheme",
	".sh": "sh", ".sql": "sql", ".svelte": "svelte", ".toml": "toml",
	".ts": "typescript", ".tsx": "tsx", ".vim": "vim", ".yaml": "yaml",
	".yml": "yaml", ".zsh": "zsh",
}

func langForPath(p string) string {
	if p == "" {
		return ""
	}
	return langByExt[strings.ToLower(filepath.Ext(p))]
}

func abbrevHome(p string) string {
	home, err := os.UserHomeDir()
	if err != nil || home == "" {
		return p
	}
	if p == home {
		return "~"
	}
	if strings.HasPrefix(p, home+"/") {
		return "~" + p[len(home):]
	}
	return p
}

func splitLines(s string) []string {
	s = strings.TrimSuffix(s, "\n")
	if s == "" {
		return nil
	}
	return strings.Split(s, "\n")
}

func firstLine(s string) string {
	if i := strings.IndexByte(s, '\n'); i >= 0 {
		return strings.TrimSpace(s[:i])
	}
	return strings.TrimSpace(s)
}

var wsRe = regexp.MustCompile(`\s+`)

func oneLine(s string) string {
	return strings.TrimSpace(wsRe.ReplaceAllString(s, " "))
}

func truncate(s string, n int) string {
	runes := []rune(s)
	if len(runes) <= n {
		return s
	}
	return string(runes[:n])
}

func fatal(msg string) {
	fmt.Fprintln(os.Stderr, "claude_session: "+msg)
	os.Exit(1)
}
