package turns

import (
	"encoding/json"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"time"
)

// ** text helpers, shared by every adapter

// HumanTimestamp is an RFC 3339 timestamp as an org inactive stamp in local
// time; anything unparseable is returned as it came.
func HumanTimestamp(ts string) string {
	if ts == "" {
		return ""
	}
	t, err := time.Parse(time.RFC3339, ts)
	if err != nil {
		return ts
	}
	return t.Local().Format(OrgStamp)
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

// AsString decodes a JSON string, reporting false for anything else.
func AsString(raw json.RawMessage) (string, bool) {
	var s string
	if err := json.Unmarshal(raw, &s); err != nil {
		return "", false
	}
	return s, true
}

func asString(raw json.RawMessage) (string, bool) { return AsString(raw) }

func isScalar(raw json.RawMessage) bool {
	s := strings.TrimSpace(string(raw))
	if s == "" {
		return false
	}
	return s[0] != '{' && s[0] != '['
}

// StringAt is the string under key, if there is one and it is a string.
func StringAt(in map[string]json.RawMessage, key string) (string, bool) {
	raw, ok := in[key]
	if !ok {
		return "", false
	}
	return AsString(raw)
}

func stringAt(in map[string]json.RawMessage, key string) (string, bool) {
	return StringAt(in, key)
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

// FirstString is the first non-empty string among keys.
func FirstString(in map[string]json.RawMessage, keys ...string) string {
	for _, k := range keys {
		if s, ok := StringAt(in, k); ok && s != "" {
			return s
		}
	}
	return ""
}

func firstString(in map[string]json.RawMessage, keys ...string) string {
	return FirstString(in, keys...)
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

// LangForPath is the source-block language for a file, by extension.
func LangForPath(p string) string {
	if p == "" {
		return ""
	}
	return langByExt[strings.ToLower(filepath.Ext(p))]
}

// AbbrevHome replaces the home directory prefix with `~`.
func AbbrevHome(p string) string {
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

// SplitLines splits on newlines, dropping one trailing newline; "" is nil.
func SplitLines(s string) []string {
	s = strings.TrimSuffix(s, "\n")
	if s == "" {
		return nil
	}
	return strings.Split(s, "\n")
}

// FirstLine is the first line, trimmed.
func FirstLine(s string) string {
	if i := strings.IndexByte(s, '\n'); i >= 0 {
		return strings.TrimSpace(s[:i])
	}
	return strings.TrimSpace(s)
}

var wsRe = regexp.MustCompile(`\s+`)

// OneLine collapses all whitespace runs to single spaces.
func OneLine(s string) string {
	return strings.TrimSpace(wsRe.ReplaceAllString(s, " "))
}

// Truncate cuts to n runes.
func Truncate(s string, n int) string {
	runes := []rune(s)
	if len(runes) <= n {
		return s
	}
	return string(runes[:n])
}

// Model ids as they read in a heading: the `claude-` prefix and the build date
// are noise when repeated on every turn, and a dotted version is easier on the
// eye than a dashed one. `<synthetic>` marks a message Claude Code wrote
// itself, such as an interruption notice, rather than a model.
var modelRe = regexp.MustCompile(`^(?:claude-)?([a-z]+)-([0-9](?:-[0-9]{1,3})*)(?:-[0-9]{8})?$`)

// ModelLabel is a model id as it reads in a subagent's heading: `Opus5',
// `Fable5.1'. The family capitalised and [ShortModel]'s separator dropped, so
// the tag is one word and scans as a name rather than as a version string.
func ModelLabel(model string) string {
	short := strings.ReplaceAll(ShortModel(model), "-", "")
	if short == "" {
		return ""
	}

	r := []rune(short)
	return strings.ToUpper(string(r[0])) + string(r[1:])
}

// ShortModel is a model id as it reads in a turn heading: `opus-5`,
// `fable-5.1`. Ids that do not follow Anthropic's naming come back with only
// the `claude-` prefix dropped, so another agent's models read as they are.
func ShortModel(model string) string {
	switch model {
	case "":
		return ""
	case "<synthetic>":
		return "synthetic"
	}

	if m := modelRe.FindStringSubmatch(model); m != nil {
		return m[1] + "-" + strings.ReplaceAll(m[2], "-", ".")
	}
	return strings.TrimPrefix(model, "claude-")
}
