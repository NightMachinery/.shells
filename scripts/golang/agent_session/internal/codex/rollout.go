// Package codex reads Codex CLI's session store: one rollout `.jsonl` per
// thread under `$CODEX_HOME/sessions/YYYY/MM/DD/`, names in
// `session_index.jsonl`, a lock per live thread in `thread-writer-locks/`.
package codex

import (
	"bufio"
	"encoding/json"
	"os"
	"path/filepath"
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

// Whether a user message is scaffolding Codex injects -- `<environment_context>`,
// `<user_instructions>`, `<turn_aborted>` -- rather than something typed. The
// whole text is one tag, so a real message that merely starts with `<` passes.
func scaffoldText(s string) bool {
	s = strings.TrimSpace(s)
	if !strings.HasPrefix(s, "<") {
		return false
	}
	end := strings.IndexByte(s, '>')
	if end < 1 {
		return false
	}
	tag := s[1:end]
	for _, c := range tag {
		if !(c == '_' || c == '-' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) {
			return false
		}
	}
	return strings.HasSuffix(s, "</"+tag+">")
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
func outputText(raw json.RawMessage) string {
	if len(raw) == 0 {
		return ""
	}
	var s string
	if err := json.Unmarshal(raw, &s); err == nil {
		return s
	}
	var obj struct {
		Output string `json:"output"`
	}
	if err := json.Unmarshal(raw, &obj); err == nil && obj.Output != "" {
		return obj.Output
	}
	return string(raw)
}
