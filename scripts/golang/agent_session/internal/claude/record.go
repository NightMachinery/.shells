package claude

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"strings"
	"time"

	"agent_session/internal/session"
	"agent_session/internal/turns"
)

// ** the record model
//
// A Claude Code transcript is one JSON record per line. Messages carry their
// content blocks in `message.content`; everything else -- titles, events,
// bookkeeping -- rides on records of its own type.

type record struct {
	Type      string   `json:"type"`
	Subtype   string   `json:"subtype"`
	IsMeta    bool     `json:"isMeta"`
	Timestamp string   `json:"timestamp"`
	Message   *message `json:"message"`

	// `system` records keep their payload at the top level, unlike messages.
	// Raw, because other record types put an object here.
	Content json.RawMessage `json:"content"`

	nameFields

	Attachment      *attachment      `json:"attachment"`
	CompactMetadata *compactMetadata `json:"compactMetadata"`
	DurationMs      int64            `json:"durationMs"`

	PRNumber     int    `json:"prNumber"`
	PRUrl        string `json:"prUrl"`
	PRRepository string `json:"prRepository"`
}

// The four ways Claude Code names a session. Its own struct, embedded rather
// than spelled out in `record`, because the tail scan in `list` wants exactly
// these fields and nothing else: decoding a full record per line would copy
// every message body in its window just to read a title.
type nameFields struct {
	Slug        string `json:"slug"`
	AITitle     string `json:"aiTitle"`
	CustomTitle string `json:"customTitle"`
	AgentName   string `json:"agentName"`
}

type message struct {
	Content json.RawMessage `json:"content"`
	Model   string          `json:"model"`
}

// The message fields the two tail scans -- `list`'s and `preview`'s -- both
// key on: who wrote the record, whether the harness wrote it rather than a
// person, and the content itself. Embedded by each of them rather than spelled
// out twice, so what counts as a typed prompt is decided in one place.
//
// Narrow on purpose. Decoding the full [record] per line would copy every
// message body in the window; the content here is kept raw and only decoded
// for the records that turn out to be worth reading.
type msgRecord struct {
	Type    string   `json:"type"`
	IsMeta  bool     `json:"isMeta"`
	Message *message `json:"message"`
}

// Whether this record is a message the user actually typed, which is what
// `-last-by user` dates a session by and what the preview shows as the last
// prompt. Two things wear the `user` type without being that: the harness's
// own meta records, and the tool results, which come back as user turns
// because that is how they are sent to the model.
func (r msgRecord) typed() bool {
	if r.Type != "user" || r.IsMeta || r.Message == nil || len(r.Message.Content) == 0 {
		return false
	}

	// A bare string is a prompt and nothing else; only the block form can
	// carry a tool result, so that is the only shape worth decoding.
	var s string
	if json.Unmarshal(r.Message.Content, &s) == nil {
		return true
	}
	var blocks []struct {
		Type string `json:"type"`
	}
	if json.Unmarshal(r.Message.Content, &blocks) != nil {
		return false
	}
	for _, b := range blocks {
		if b.Type == "tool_result" {
			return false
		}
	}
	return true
}

// The text of a message, its text blocks joined. Everything else a turn can
// carry -- a tool call, a thinking block, a tool result -- has no text of its
// own, so a record holding only those reads as empty here and the caller can
// look further back for one that says something.
func (r msgRecord) text() string {
	var parts []string
	for _, b := range decodeBlocks(r.Message) {
		if b.Type == "text" && strings.TrimSpace(b.Text) != "" {
			parts = append(parts, b.Text)
		}
	}
	return strings.Join(parts, "\n")
}

// `system` record subtypes that carry something worth reading. The rest are
// bookkeeping: `stop_hook_summary` never has content (0 of 225 locally), and
// `turn_duration` is folded into the heading of the turn it measures.
const (
	subtypeRecap    = "away_summary"
	subtypeCompact  = "compact_boundary"
	subtypeCommand  = "local_command"
	subtypeInfo     = "informational"
	subtypeFallback = "model_consent_fallback"
	subtypeDuration = "turn_duration"
)

type attachment struct {
	Type        string `json:"type"`
	Filename    string `json:"filename"`
	DisplayPath string `json:"displayPath"`
	Snippet     string `json:"snippet"`

	// task_reminder: the outstanding task list Claude is reminded of. Every
	// one of the 485 locally is empty, because the task tools go unused here,
	// but a populated one is worth reading rather than dropping.
	Content   json.RawMessage `json:"content"`
	ItemCount int             `json:"itemCount"`
}

type compactMetadata struct {
	Trigger    string `json:"trigger"`
	PreTokens  int    `json:"preTokens"`
	PostTokens int    `json:"postTokens"`
}

func readRecords(fh *os.File) []record {
	var out []record
	sc := bufio.NewScanner(fh)
	sc.Buffer(make([]byte, 0, 64<<10), session.MaxLineBytes)
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if len(line) == 0 || line[0] != '{' {
			continue
		}
		var rec record
		if err := json.Unmarshal([]byte(line), &rec); err != nil {
			// A truncated or malformed line loses one message, not the file.
			continue
		}
		out = append(out, rec)
	}
	return out
}

func parseRecord(line string) (record, bool) {
	line = strings.TrimSpace(line)
	if len(line) == 0 || line[0] != '{' {
		return record{}, false
	}
	var rec record
	if err := json.Unmarshal([]byte(line), &rec); err != nil {
		return record{}, false
	}
	return rec, true
}

// The content blocks of a message. A bare string is one text block; the
// Anthropic block schema decodes straight into [turns.Block].
func decodeBlocks(m *message) []turns.Block {
	if m == nil || len(m.Content) == 0 {
		return nil
	}

	var s string
	if err := json.Unmarshal(m.Content, &s); err == nil {
		return []turns.Block{{Type: "text", Text: s}}
	}

	var blocks []turns.Block
	if err := json.Unmarshal(m.Content, &blocks); err != nil {
		return nil
	}
	return blocks
}

// The records that make up the conversation: the messages, plus the events
// that punctuate them.
//
// Everything else in a transcript is bookkeeping and stays out: `mode`,
// `permission-mode`, `agent-name`/`agent-color`/`agent-setting`,
// `bridge-session`, `file-history-snapshot`/`-delta`, and `last-prompt`, which
// only repeats the message next to it. `queue-operation` is left out for a
// subtler reason: half of what gets enqueued is delivered and so already shows
// up as an ordinary user message, and the other half was withdrawn before it
// was ever sent. Most `attachment` payloads are harness internals
// (`task_reminder`, `skill_listing`, `deferred_tools_delta`); only a file you
// edited yourself says anything about the conversation.
func conversationRecords(all []record) []record {
	var out []record
	for _, rec := range all {
		switch rec.Type {
		case "user", "assistant":
			if rec.IsMeta {
				continue
			}
		case "system":
			switch rec.Subtype {
			case subtypeRecap, subtypeCompact, subtypeCommand, subtypeInfo,
				subtypeFallback, subtypeDuration:
			default:
				continue
			}
		case "pr-link":
		case "attachment":
			if rec.Attachment == nil {
				continue
			}
			switch rec.Attachment.Type {
			case "edited_text_file":
			case "task_reminder":
				if rec.Attachment.ItemCount == 0 {
					continue
				}
			default:
				continue
			}
		default:
			continue
		}
		out = append(out, rec)
	}
	return out
}

// Results arrive as user turns, because that is how they are sent back to the
// model. Indexing them by the call they answer lets them be rendered under it
// instead of as a message nobody wrote.
func indexResults(records []record, blocks [][]turns.Block) map[string]turns.ToolResult {
	calls := map[string]bool{}
	for _, bs := range blocks {
		for _, b := range bs {
			if b.Type == "tool_use" && b.ID != "" {
				calls[b.ID] = true
			}
		}
	}

	out := map[string]turns.ToolResult{}
	for i, bs := range blocks {
		for _, b := range bs {
			if b.Type != "tool_result" || !calls[b.ToolUseID] {
				continue
			}
			out[b.ToolUseID] = turns.ToolResult{
				Body:    turns.FlattenResult(b.Content),
				IsError: b.IsError,
				TS:      records[i].Timestamp,
			}
		}
	}
	return out
}

// Consecutive records that share a role become one turn. Claude Code writes
// one record per content block, so without this an assistant turn becomes a
// run of near-identical headings.
func buildTurns(records []record, blocks [][]turns.Block, results map[string]turns.ToolResult) []turns.Turn {
	var out []turns.Turn

	for i, rec := range records {
		if rec.Type != "user" && rec.Type != "assistant" {
			out = appendEvent(out, rec)
			continue
		}

		model := ""
		if rec.Message != nil {
			model = rec.Message.Model
		}

		var keep []turns.TimedBlock
		for _, b := range blocks[i] {
			// Nested under its call; an orphan with no matching call still
			// gets rendered where it sits.
			if b.Type == "tool_result" {
				if _, nested := results[b.ToolUseID]; nested {
					continue
				}
			}
			keep = append(keep, turns.TimedBlock{B: b, TS: rec.Timestamp})
		}
		if len(keep) == 0 {
			continue
		}

		// Merging stops at a model change, so a switch mid-answer starts a
		// new heading rather than hiding inside one. Annotating sub-headings
		// instead would miss a switch that lands on a plain text block, which
		// has no heading to annotate.
		if n := len(out); n > 0 && out[n-1].Role == rec.Type && out[n-1].Model == model {
			out[n-1].Blocks = append(out[n-1].Blocks, keep...)
			continue
		}
		out = append(out, turns.Turn{Role: rec.Type, TS: rec.Timestamp, Model: model, Blocks: keep})
	}

	return out
}

// An event record -- a notice, a compaction, a slash command, a PR link --
// attached to the turn it belongs with, or standing as a turn of its own.
func appendEvent(out []turns.Turn, rec record) []turns.Turn {
	var text string
	json.Unmarshal(rec.Content, &text)
	text = strings.TrimSpace(text)

	attach := func(b turns.Block) []turns.Turn {
		tb := turns.TimedBlock{B: b, TS: rec.Timestamp}
		if n := len(out); n > 0 {
			out[n-1].Blocks = append(out[n-1].Blocks, tb)
			return out
		}
		return append(out, turns.Turn{Role: "assistant", TS: rec.Timestamp, Blocks: []turns.TimedBlock{tb}})
	}

	switch {
	case rec.Type == "pr-link":
		label := rec.PRRepository
		if label == "" {
			label = "Pull request"
		}
		return attach(turns.Block{Type: "pr", Name: fmt.Sprintf("%s#%d", label, rec.PRNumber), Text: rec.PRUrl})

	case rec.Type == "attachment" && rec.Attachment.Type == "task_reminder":
		var pretty strings.Builder
		enc := json.NewEncoder(&pretty)
		enc.SetIndent("", "  ")
		if enc.Encode(rec.Attachment.Content) != nil {
			return out
		}
		return attach(turns.Block{
			Type: "tasks",
			Name: fmt.Sprintf("%d outstanding", rec.Attachment.ItemCount),
			Text: strings.TrimRight(pretty.String(), "\n"),
		})

	case rec.Type == "attachment":
		path := rec.Attachment.DisplayPath
		if path == "" {
			path = rec.Attachment.Filename
		}
		return attach(turns.Block{Type: "file-edit", Name: path, Text: rec.Attachment.Snippet})

	case rec.Subtype == subtypeDuration:
		// Belongs to the turn it measures, in its heading.
		if n := len(out); n > 0 && rec.DurationMs > 0 {
			out[n-1].Duration = time.Duration(rec.DurationMs) * time.Millisecond
		}
		return out

	case rec.Subtype == subtypeCompact:
		note := ""
		if m := rec.CompactMetadata; m != nil {
			if m.Trigger != "" {
				note = m.Trigger
			}
			if m.PreTokens > 0 {
				if note != "" {
					note += " · "
				}
				note += fmt.Sprintf("%d → %d tokens", m.PreTokens, m.PostTokens)
			}
		}
		return append(out, turns.Turn{
			Role: "system", Heading: "Context compacted", Note: note, TS: rec.Timestamp,
		})

	case rec.Subtype == subtypeCommand:
		// The payload is the same `<command-name>` scaffolding the picker
		// strips, so it reduces to the command that was run.
		if s := snippetText(text); s != "" {
			return attach(turns.Block{Type: "command", Text: s})
		}
		return out

	case text != "":
		// away_summary, informational, model_consent_fallback.
		kind := "Notice"
		switch rec.Subtype {
		case subtypeRecap:
			kind = "Recap"
		case subtypeFallback:
			kind = "Model fallback"
		}
		return attach(turns.Block{Type: "notice", Name: kind, Text: text})
	}

	return out
}

// Where each tool call sits in the transcript, so subagents can be ordered by
// the call that spawned them.
func toolCallOrder(blocks [][]turns.Block) map[string]int {
	order := map[string]int{}
	n := 0
	for _, bs := range blocks {
		for _, b := range bs {
			if b.Type == "tool_use" && b.ID != "" {
				order[b.ID] = n
				n++
			}
		}
	}
	return order
}
