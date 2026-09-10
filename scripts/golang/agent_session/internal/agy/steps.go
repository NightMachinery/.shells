// Package agy reads Antigravity's session store. A conversation is a directory
// under `~/.gemini/antigravity-cli/brain/<conversationId>/`, and the readable
// record of it is the JSONL transcript its agent writes under
// `.system_generated/logs/`: `transcript_full.jsonl`, with
// `transcript.jsonl` the same steps with long fields truncated. Titles come
// from `cache/conversation_metadata.json` (the SQLite summaries mirror), the
// working directory from `history.jsonl`, and what is running from the process
// table.
//
// The protobuf stores under `conversations/` are not touched: everything a
// transcript view needs is in the JSONL.
package agy

import (
	"bufio"
	"encoding/json"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"agent_session/internal/session"
)

// ** the step model
//
// One JSON object per line. `type` is an open enum -- the agent adds kinds --
// so an unknown one still renders, titled by its own name.

type step struct {
	Index int `json:"step_index"`
	// USER_EXPLICIT, MODEL or SYSTEM.
	Source string `json:"source"`
	Type   string `json:"type"`
	// DONE, RUNNING or ERROR.
	Status    string     `json:"status"`
	CreatedAt string     `json:"created_at"`
	Content   string     `json:"content"`
	Thinking  string     `json:"thinking"`
	ToolCalls []toolCall `json:"tool_calls"`
	// Present on a compact transcript's line whose fields were cut.
	TruncatedFields []string `json:"truncated_fields"`
}

type toolCall struct {
	Name string `json:"name"`
	// An object, or a JSON-encoded string of one.
	Args json.RawMessage `json:"args"`
}

const (
	sourceUser   = "USER_EXPLICIT"
	sourceModel  = "MODEL"
	sourceSystem = "SYSTEM"
)

const (
	typeUserInput       = "USER_INPUT"
	typePlanner         = "PLANNER_RESPONSE"
	typeConversationLog = "CONVERSATION_HISTORY"
	typeSubagent        = "INVOKE_SUBAGENT"
)

// ** prose or output
//
// A step's content is one of two things, and they have to be told apart: a web
// search summary or a system message is *markdown* and reads as a document,
// while a command's output, a file's contents or a grep's hits are text that
// must be shown exactly as it came. Rendering the first kind verbatim was
// visible in an org file as a block of `###` headings and `*` bullets that
// nothing had converted.
//
// The type decides it where the type is known. It is an open enum, so an
// unknown kind is judged by its body instead.

// Types whose content is markdown.
var proseTypes = map[string]bool{
	typePlanner:         true,
	typeConversationLog: true,
	typeSubagent:        true,
	"SEARCH_WEB":        true,
	"WEB_SEARCH":        true,
	"SYSTEM_MESSAGE":    true,
	"USER_INPUT":        true,
	"MEMORY":            true,
	"PLAN":              true,
}

// Types whose content is output, whatever it happens to look like.
var outputTypes = map[string]bool{
	"RUN_COMMAND":    true,
	"VIEW_FILE":      true,
	"LIST_DIRECTORY": true,
	"GREP_SEARCH":    true,
	"CODE_ACTION":    true,
	"EDIT_FILE":      true,
	"WRITE_FILE":     true,
	"TERMINAL":       true,
}

// Whether a step's body should be rendered as markdown rather than kept
// verbatim.
func proseStep(t, body string) bool {
	u := strings.ToUpper(t)
	if proseTypes[u] {
		return true
	}
	if outputTypes[u] {
		return false
	}
	return markdownish(body)
}

// Whether a body carries enough markdown to be worth converting: a heading, a
// bullet list, bold, or an inline link. Output that happens to contain one of
// these reads no worse for it; prose rendered verbatim reads much worse.
func markdownish(body string) bool {
	if strings.Contains(body, "](http") || strings.Contains(body, "**") {
		return true
	}
	for _, ln := range strings.Split(body, "\n") {
		if mdLineRe.MatchString(strings.TrimLeft(ln, " \t")) {
			return true
		}
	}
	return false
}

// A heading or a bullet, by CommonMark's rule rather than by eye: `#include`
// is not a heading and `-fPIC` is not a list item, and both turn up in output.
var mdLineRe = regexp.MustCompile(`^(?:#{1,6} |[-*+] )`)

// The transcript of a conversation directory: the full one, else the compact
// one. "" when the conversation has written neither, which is the normal state
// of a brain directory that never got going.
func transcriptOf(dir string) string {
	logs := filepath.Join(dir, ".system_generated", "logs")
	for _, name := range []string{"transcript_full.jsonl", "transcript.jsonl"} {
		p := filepath.Join(logs, name)
		if st, err := os.Stat(p); err == nil && !st.IsDir() {
			return p
		}
	}
	return ""
}

// `<brain>/<id>/.system_generated/logs/transcript_full.jsonl` -> the id.
func idOf(path string) string {
	abs, err := filepath.Abs(path)
	if err != nil {
		abs = path
	}
	// logs -> .system_generated -> <id>
	return filepath.Base(filepath.Dir(filepath.Dir(filepath.Dir(abs))))
}

// The `antigravity-cli` state directory a transcript sits under.
func homeOf(path string) string {
	abs, err := filepath.Abs(path)
	if err != nil {
		abs = path
	}
	sep := string(filepath.Separator)
	if i := strings.LastIndex(abs, sep+"brain"+sep); i >= 0 {
		return abs[:i]
	}
	return ""
}

func readSteps(path string) []step {
	fh, err := os.Open(path)
	if err != nil {
		return nil
	}
	defer fh.Close()

	var out []step
	sc := bufio.NewScanner(fh)
	sc.Buffer(make([]byte, 0, 64<<10), session.MaxLineBytes)
	for sc.Scan() {
		raw := strings.TrimSpace(sc.Text())
		if len(raw) == 0 || raw[0] != '{' {
			continue
		}
		var s step
		if err := json.Unmarshal([]byte(raw), &s); err != nil {
			// A malformed line loses one step, not the file.
			continue
		}
		out = append(out, s)
	}
	return out
}

// What the user actually typed. Antigravity wraps a prompt in
// `<USER_REQUEST>`, and appends its own notes about the environment after the
// closing tag; both are scaffolding around the one thing worth showing.
func userText(content string) string {
	const open, close = "<USER_REQUEST>", "</USER_REQUEST>"
	if i := strings.Index(content, open); i >= 0 {
		rest := content[i+len(open):]
		if j := strings.Index(rest, close); j >= 0 {
			return strings.TrimSpace(rest[:j])
		}
		return strings.TrimSpace(rest)
	}
	return strings.TrimSpace(content)
}

// A step type as a heading: `RUN_COMMAND` -> `Run command`. An unknown kind
// reads as well as a known one, which is the point of not keying on the enum.
func typeLabel(t string) string {
	if t == "" {
		return "Step"
	}
	words := strings.Split(strings.ToLower(t), "_")
	if len(words) == 0 {
		return t
	}
	first := words[0]
	words[0] = strings.ToUpper(first[:1]) + first[1:]
	return strings.Join(words, " ")
}

// A tool call's arguments as an object: Antigravity sends either the object or
// a JSON-encoded string of one.
func callArgs(raw json.RawMessage) json.RawMessage {
	raw = json.RawMessage(strings.TrimSpace(string(raw)))
	if len(raw) == 0 {
		return nil
	}
	if raw[0] == '{' {
		return raw
	}
	var s string
	if err := json.Unmarshal(raw, &s); err != nil {
		return raw
	}
	s = strings.TrimSpace(s)
	if strings.HasPrefix(s, "{") {
		return json.RawMessage(s)
	}
	// Not an object at all; give the renderer something it can show.
	out, err := json.Marshal(map[string]string{"args": s})
	if err != nil {
		return nil
	}
	return out
}
