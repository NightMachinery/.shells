// Package codex reads Codex CLI's session store: one rollout `.jsonl` per
// thread under `$CODEX_HOME/sessions/YYYY/MM/DD/`, names in
// `session_index.jsonl`, a lock per live thread in `thread-writer-locks/`.
package codex

import (
	"bufio"
	"bytes"
	"encoding/json"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"agent_session/internal/session"
)

// ** the record model
//
// A rollout is one JSON line per record: `{"timestamp", "type", "payload"}`.
// The payload's shape depends on the type, so it is kept raw and decoded by
// whoever wants it.

type line struct {
	Timestamp string          `json:"timestamp"`
	Type      string          `json:"type"`
	Payload   json.RawMessage `json:"payload"`
}

// The first record of every rollout.
type sessionMeta struct {
	ID             string  `json:"id"`
	Cwd            string  `json:"cwd"`
	Originator     string  `json:"originator"`
	CLIVersion     string  `json:"cli_version"`
	ParentThreadID *string `json:"parent_thread_id"`
}

// Written at the start of each turn: where and as what the model runs.
type turnContext struct {
	Cwd    string `json:"cwd"`
	Model  string `json:"model"`
	Effort string `json:"effort"`
}

// A `response_item`: what went to or came from the model.
type responseItem struct {
	Type string `json:"type"`

	// message
	Role    string        `json:"role"`
	Content []contentPart `json:"content"`

	// reasoning: the summary is what the model shows; the content is the
	// raw trace, present only when the model streams it.
	Summary []contentPart `json:"summary"`

	// function_call, custom_tool_call and their outputs
	Name      string          `json:"name"`
	Arguments string          `json:"arguments"`
	Input     string          `json:"input"`
	CallID    string          `json:"call_id"`
	Output    json.RawMessage `json:"output"`
	Status    string          `json:"status"`

	// local_shell_call (older rollouts): the command lives under `action`.
	Action json.RawMessage `json:"action"`
}

type contentPart struct {
	Type string `json:"type"`
	Text string `json:"text"`
}

// A `compacted` record: the summary that replaced the history before it.
type compacted struct {
	Message string `json:"message"`
}

func readLines(fh *os.File) []line {
	var out []line
	sc := bufio.NewScanner(fh)
	sc.Buffer(make([]byte, 0, 64<<10), session.MaxLineBytes)
	for sc.Scan() {
		raw := strings.TrimSpace(sc.Text())
		if len(raw) == 0 || raw[0] != '{' {
			continue
		}
		var l line
		if err := json.Unmarshal([]byte(raw), &l); err != nil {
			// A truncated or malformed line loses one record, not the file.
			continue
		}
		out = append(out, l)
	}
	return out
}

// The session_meta of a rollout, from its first line only: the one record
// `list` and `live` need from every file, and reading more would make them
// cost the corpus's size rather than its count.
func readMeta(path string) (sessionMeta, bool) {
	fh, err := os.Open(path)
	if err != nil {
		return sessionMeta{}, false
	}
	defer fh.Close()

	sc := bufio.NewScanner(fh)
	sc.Buffer(make([]byte, 0, 64<<10), session.MaxLineBytes)
	for sc.Scan() {
		var l line
		if err := json.Unmarshal(sc.Bytes(), &l); err != nil {
			continue
		}
		if l.Type != "session_meta" {
			// The first line is the meta by construction; anything else
			// means this is not a rollout.
			return sessionMeta{}, false
		}
		var m sessionMeta
		if err := json.Unmarshal(l.Payload, &m); err != nil {
			return sessionMeta{}, false
		}
		if m.ID == "" {
			m.ID = idOf(path)
		}
		return m, true
	}
	return sessionMeta{}, false
}

// `rollout-2026-09-08T12-34-56-<uuid>.jsonl` -> the uuid. A uuid is 36
// characters; anything shorter is taken whole.
func idOf(path string) string {
	stem := strings.TrimSuffix(filepath.Base(path), ".jsonl")
	if len(stem) > 36 {
		return stem[len(stem)-36:]
	}
	return stem
}

// The `$CODEX_HOME` a rollout sits under: `<home>/sessions/YYYY/MM/DD/<file>`.
func homeOf(path string) string {
	abs, err := filepath.Abs(path)
	if err != nil {
		abs = path
	}
	sep := string(filepath.Separator)
	if i := strings.LastIndex(abs, sep+"sessions"+sep); i >= 0 {
		return abs[:i]
	}
	return filepath.Dir(filepath.Dir(filepath.Dir(filepath.Dir(filepath.Dir(abs)))))
}

// ** the scaffolding around a typed message
//
// Codex sends the instructions a session was launched with, and the
// environment it runs in, as part of the *first user message*: the whole
// assembled AGENTS.md inside `<INSTRUCTIONS>`, then an
// `<environment_context>` block of cwd, shell, date and sandbox policy, then
// whatever the person actually typed. Rendered as one lump that reads as if
// the person had typed their own instruction file, and pandoc made it worse:
// a line that starts with a tag is an HTML block, so the markdown headings
// inside arrived unparsed.
//
// So a user message is split. The scaffold sections become a turn of their
// own, folded, and the typed remainder is the user's turn.

// A line that is nothing but an opening `<tag>`. Alone on the line is what
// keeps a `<foo>` inside a sentence from being mistaken for one of these; Go's
// regexp has no backreference, so the matching close is found by hand.
var openTagRe = regexp.MustCompile(`^[ \t]*<([A-Za-z_][A-Za-z0-9_-]*)>[ \t]*$`)

// One `<tag>`-wrapped section of a message.
type section struct {
	tag  string
	body string
}

// The sections of a message and what is left when they are taken out.
func splitSections(text string) ([]section, string) {
	if !strings.Contains(text, "<") {
		return nil, strings.TrimSpace(text)
	}

	lines := strings.Split(text, "\n")
	var out []section
	var rest []string

	for i := 0; i < len(lines); i++ {
		m := openTagRe.FindStringSubmatch(lines[i])
		if m == nil {
			rest = append(rest, lines[i])
			continue
		}
		close := "</" + m[1] + ">"
		end, tail := -1, ""
		for j := i + 1; j < len(lines); j++ {
			trimmed := strings.TrimRight(lines[j], " \t")
			if trimmed == close || strings.TrimSpace(lines[j]) == close {
				end = j
				break
			}
			// Codex closes some sections on the body's last line rather than
			// on one of their own.
			if strings.HasSuffix(trimmed, close) {
				end, tail = j, strings.TrimSuffix(trimmed, close)
				break
			}
		}
		if end < 0 {
			// An opening tag with no close is just a line of text.
			rest = append(rest, lines[i])
			continue
		}
		body := lines[i+1 : end]
		if tail != "" {
			body = append(append([]string{}, body...), tail)
		}
		out = append(out, section{tag: m[1], body: strings.Join(body, "\n")})
		i = end
	}
	return out, strings.TrimSpace(strings.Join(rest, "\n"))
}

// Whether a section is the launch scaffold rather than something that happened
// during the session: the instructions and the environment description, which
// are the same every turn and are nobody's message.
func scaffoldSection(tag string) bool {
	t := strings.ToLower(tag)
	switch t {
	case "environment_context", "filesystem", "sandbox_policy", "workspace_roots":
		return true
	}
	return strings.Contains(t, "instruction") || strings.Contains(t, "guideline") ||
		strings.Contains(t, "agents_md") || strings.Contains(t, "user_prompt_prefix")
}

// Whether a section's body is prose to be converted, or data to be shown as it
// is. The instruction files are markdown; the environment blocks are XML.
func proseSection(tag string) bool {
	t := strings.ToLower(tag)
	return strings.Contains(t, "instruction") || strings.Contains(t, "guideline") ||
		strings.Contains(t, "agents_md")
}

// `environment_context` -> `Environment context`.
func sectionLabel(tag string) string {
	t := strings.ReplaceAll(strings.ToLower(tag), "_", " ")
	t = strings.ReplaceAll(t, "-", " ")
	if t == "" {
		return "Section"
	}
	r := []rune(t)
	return strings.ToUpper(string(r[0])) + string(r[1:])
}

// Whether a user message is nothing but scaffolding, so it is not a turn of
// its own. Kept for the listing and the preview, which want the first thing
// the person actually typed and do not build turns.
func scaffoldText(s string) bool {
	_, typed := splitSections(s)
	return strings.TrimSpace(typed) == ""
}

// The text of a message's parts, joined; images become a marker.
func partsText(parts []contentPart) string {
	var b strings.Builder
	for _, p := range parts {
		switch p.Type {
		case "input_image", "image":
			if b.Len() > 0 {
				b.WriteString("\n")
			}
			b.WriteString("[image]")
		default:
			if p.Text == "" {
				continue
			}
			if b.Len() > 0 {
				b.WriteString("\n")
			}
			b.WriteString(p.Text)
		}
	}
	return b.String()
}

// A tool output as text: a plain string, or the `{"output": ..., "metadata":
// ...}` object older rollouts wrote.
// The text of a tool result, with the layers Codex wraps it in taken off and
// what is left pretty-printed when it is JSON.
//
// Codex hands a result back in whatever shape the tool produced: a plain
// string, an argv-style object with an `output` field, or a list of content
// parts each with a `text` -- and a part's text is itself often another JSON
// document, since MCP tools answer in JSON. Rendered raw that is one
// unreadable line, so each layer is unwrapped and the innermost JSON is
// indented. Returns the body and the language for its block: `json` for
// something this pretty-printed, empty for text nobody can vouch for.
func outputText(raw json.RawMessage) (string, string) {
	if len(raw) == 0 {
		return "", ""
	}
	return unwrapJSON(strings.TrimSpace(string(raw)), 0)
}

// How deep the unwrapping goes. Three layers covers list-of-parts, a part's
// own JSON document and one `output` inside that; more than that is a payload
// nobody is reading in a transcript anyway.
const unwrapDepth = 3

func unwrapJSON(s string, depth int) (string, string) {
	if depth > unwrapDepth || !looksJSON(s) {
		return s, ""
	}
	raw := json.RawMessage(s)

	// A JSON string: its content is the result, and may itself be JSON.
	var str string
	if json.Unmarshal(raw, &str) == nil {
		return unwrapJSON(strings.TrimSpace(str), depth+1)
	}

	// A list of content parts, each with its own text. Only when *every*
	// element is one: an ordinary JSON array is a document, and taking it
	// apart would lose the array itself.
	var items []json.RawMessage
	if json.Unmarshal(raw, &items) == nil {
		texts, ok := contentParts(items)
		if !ok {
			return indentJSON(raw), "json"
		}
		parts := make([]string, 0, len(texts))
		lang := "json"
		for _, text := range texts {
			body, l := unwrapJSON(strings.TrimSpace(text), depth+1)
			parts = append(parts, body)
			if l == "" {
				lang = ""
			}
		}
		if len(parts) == 1 {
			return parts[0], lang
		}
		// Several parts are several documents; the block cannot be one
		// language, and blank lines keep them apart.
		return strings.Join(parts, "\n\n"), ""
	}

	// An object: the `output` field when it has one, since that is the text
	// the tool printed, and the whole document indented otherwise.
	var obj map[string]json.RawMessage
	if json.Unmarshal(raw, &obj) == nil {
		if out, ok := obj["output"]; ok {
			var text string
			if json.Unmarshal(out, &text) == nil {
				body, lang := unwrapJSON(strings.TrimSpace(text), depth+1)
				return body, lang
			}
		}
		return indentJSON(raw), "json"
	}
	return s, ""
}

// The texts of a list of content parts, and whether the list is one: every
// element an object carrying a non-empty `text`.
func contentParts(items []json.RawMessage) ([]string, bool) {
	if len(items) == 0 {
		return nil, false
	}
	out := make([]string, 0, len(items))
	for _, it := range items {
		var obj struct {
			Text string `json:"text"`
		}
		if json.Unmarshal(it, &obj) != nil || obj.Text == "" {
			return nil, false
		}
		out = append(out, obj.Text)
	}
	return out, true
}

// Whether a string is worth handing to the JSON decoder at all.
func looksJSON(s string) bool {
	if s == "" {
		return false
	}
	switch s[0] {
	case '{', '[', '"':
		return true
	}
	return false
}

// A JSON document, indented. The document itself when it cannot be parsed, so
// this never loses anything.
func indentJSON(raw json.RawMessage) string {
	var buf bytes.Buffer
	if err := json.Indent(&buf, raw, "", "  "); err != nil {
		return string(raw)
	}
	return buf.String()
}
