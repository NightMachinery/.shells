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
	"unicode/utf8"
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

// Tokens of a model id that are acronyms rather than words, so that
// capitalising a name does not produce `Gpt`.
var modelWords = map[string]string{
	"gpt": "GPT",
	"ai":  "AI",
	"llm": "LLM",
}

// ModelLabel is a model id as it reads in a tag: `Opus5`, `Fable5.1`,
// `CodexAutoReview`. Every dash-separated word of [ShortModel]'s output is
// capitalised and the dashes dropped, so the tag is one word; a version keeps
// the dot ShortModel gave it, since `Fable51` would read as a number.
//
// Capitalising each word rather than only the first is what keeps a
// multi-word id legible: an id that does not follow Anthropic's
// family-and-version shape passes through here too, and `Codexautoreview` is
// not a name anybody can read.
func ModelLabel(model string) string {
	short := ShortModel(model)
	if short == "" {
		return ""
	}

	parts := strings.Split(short, "-")
	for i, p := range parts {
		if p == "" {
			continue
		}
		if w, ok := modelWords[p]; ok {
			parts[i] = w
			continue
		}
		r := []rune(p)
		parts[i] = strings.ToUpper(string(r[0])) + string(r[1:])
	}
	return strings.Join(parts, "")
}

// ModelTag is how a model is named where it stands for whoever spoke: `@Opus5`
// on a turn heading, `@Fable5.1` on a subagent's. The `@` is this
// repository's mark for an agent identity, the same one the tmux session
// names carry, and one word after it reads as a name rather than as a version
// string.
func ModelTag(model string) string {
	if label := ModelLabel(model); label != "" {
		return "@" + label
	}
	return ""
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

// ScrubText makes rendered output safe to open as text.
//
// A single NUL byte anywhere in a file makes emacs decode the *whole* file as
// binary (`no-conversion`), and in that buffer every non-ASCII byte shows as
// an octal escape -- a `·` separator reads as `\302\267`, and so does anything
// the person typed. Tool output carries such bytes whenever a command printed
// a binary file, and invalid UTF-8 has the same class of effect one step down:
// emacs falls back to latin-1 and a `·` reads as `Â·`.
//
// So the renderer emits only what it can honestly show: the C0 controls that
// are never text are dropped, and a byte sequence that is not valid UTF-8
// becomes U+FFFD. Tab, newline and carriage return survive because they are
// layout, and ESC survives because dropping it alone would leave the rest of
// an ANSI sequence behind as literal text.
func ScrubText(s string) string {
	if !needsScrub(s) {
		return s
	}

	var w strings.Builder
	w.Grow(len(s))
	for i := 0; i < len(s); {
		b := s[i]
		if b < utf8.RuneSelf {
			if !dropByte(b) {
				w.WriteByte(b)
			}
			i++
			continue
		}
		r, size := utf8.DecodeRuneInString(s[i:])
		if r == utf8.RuneError && size == 1 {
			w.WriteRune(utf8.RuneError)
			i++
			continue
		}
		w.WriteString(s[i : i+size])
		i += size
	}
	return w.String()
}

// Whether a string has anything [ScrubText] would change. Almost every
// document is clean, and this keeps the common case one pass with no copy.
func needsScrub(s string) bool {
	for i := 0; i < len(s); i++ {
		if b := s[i]; b < utf8.RuneSelf && dropByte(b) {
			return true
		}
	}
	return !utf8.ValidString(s)
}

// A byte that must not reach the output: the C0 controls other than tab,
// newline, carriage return and ESC, plus DEL.
func dropByte(b byte) bool {
	switch b {
	case '\t', '\n', '\r', 0x1b:
		return false
	}
	return b < 0x20 || b == 0x7f
}
