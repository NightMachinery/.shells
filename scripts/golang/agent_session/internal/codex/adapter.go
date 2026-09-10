package codex

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"time"

	"agent_session/internal/session"
	"agent_session/internal/turns"
)

// Adapter implements [session.Adapter] for Codex CLI.
type Adapter struct{}

var _ session.Adapter = Adapter{}

func (Adapter) Name(path string) (string, error) {
	if _, err := os.Stat(path); err != nil {
		return "", err
	}
	return names(homeOf(path))[idOf(path)], nil
}

func (a Adapter) Meta(path string) (session.Meta, error) {
	m, ok := readMeta(path)
	if !ok {
		if _, err := os.Stat(path); err != nil {
			return session.Meta{}, err
		}
		m.ID = idOf(path)
	}
	return session.Meta{
		ID:   m.ID,
		Name: names(homeOf(path))[m.ID],
		Cwd:  m.Cwd,
	}, nil
}

// Document turns a rollout into the renderer's model. Consecutive records of
// one role merge into a turn, as they do for Claude: Codex writes reasoning,
// tool calls and the final message as separate records.
func (a Adapter) Document(path string, o session.DocOpts) (*turns.Document, error) {
	fh, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer fh.Close()

	lines := readLines(fh)
	doc := &turns.Document{}
	doc.Turns, doc.Results = buildTurns(lines)

	if o.Subagents {
		var meta sessionMeta
		for _, l := range lines {
			if l.Type == "session_meta" {
				json.Unmarshal(l.Payload, &meta)
				break
			}
		}
		if meta.ID == "" {
			meta.ID = idOf(path)
		}
		for _, sub := range subRollouts(filepath.Join(homeOf(path), "sessions"), meta.ID) {
			doc.Subagents = append(doc.Subagents, subdoc(sub))
		}
	}
	return doc, nil
}

// The turns and the results index of one rollout.
func buildTurns(lines []line) ([]turns.Turn, map[string]turns.ToolResult) {
	var out []turns.Turn
	results := map[string]turns.ToolResult{}
	model := ""

	add := func(role, ts string, b turns.Block) {
		tb := turns.TimedBlock{B: b, TS: ts}
		if n := len(out); n > 0 && out[n-1].Role == role && out[n-1].Heading == "" &&
			(role != "assistant" || out[n-1].Model == model) {
			out[n-1].Blocks = append(out[n-1].Blocks, tb)
			return
		}
		t := turns.Turn{Role: role, TS: ts, Blocks: []turns.TimedBlock{tb}}
		if role == "assistant" {
			t.Model = model
		}
		out = append(out, t)
	}

	for _, l := range lines {
		switch l.Type {
		case "turn_context":
			var tc turnContext
			if json.Unmarshal(l.Payload, &tc) == nil && tc.Model != "" {
				model = tc.Model
			}

		case "compacted":
			var c compacted
			json.Unmarshal(l.Payload, &c)
			out = append(out, turns.Turn{
				Role: "system", Heading: "Context compacted", TS: l.Timestamp,
				Blocks: []turns.TimedBlock{{B: turns.Block{Type: "notice", Name: "Summary", Text: c.Message}, TS: l.Timestamp}},
			})

		case "response_item":
			var it responseItem
			if err := json.Unmarshal(l.Payload, &it); err != nil {
				continue
			}
			switch it.Type {
			case "message":
				text := partsText(it.Content)
				switch it.Role {
				case "user":
					if strings.TrimSpace(text) == "" {
						continue
					}
					// The launch scaffold rides along in the first user
					// message; it is not something anybody typed, so it gets
					// its own folded turn rather than opening the transcript
					// as if it were a prompt. See [splitSections].
					sections, typed := splitSections(text)
					for _, t := range scaffoldTurns(sections, l.Timestamp) {
						out = append(out, t)
					}
					if strings.TrimSpace(typed) == "" {
						continue
					}
					add("user", l.Timestamp, turns.Block{Type: "text", Text: typed})
				case "assistant":
					add("assistant", l.Timestamp, turns.Block{Type: "text", Text: text})
				}
				// developer/system messages are instructions, not conversation.

			case "reasoning":
				text := partsText(it.Summary)
				if text == "" {
					text = partsText(it.Content)
				}
				if strings.TrimSpace(text) == "" {
					continue
				}
				add("assistant", l.Timestamp, turns.Block{Type: "thinking", Thinking: text})

			case "function_call":
				add("assistant", l.Timestamp, turns.Block{
					Type: "tool_use", Name: it.Name, ID: it.CallID,
					Input: normalizeInput(it.Name, it.Arguments),
				})

			case "custom_tool_call":
				in, _ := json.Marshal(map[string]string{"input": it.Input})
				add("assistant", l.Timestamp, turns.Block{
					Type: "tool_use", Name: it.Name, ID: it.CallID, Input: in,
				})

			case "local_shell_call":
				add("assistant", l.Timestamp, turns.Block{
					Type: "tool_use", Name: "shell", ID: it.CallID,
					Input: normalizeInput("shell", string(it.Action)),
				})

			case "function_call_output", "custom_tool_call_output", "local_shell_call_output":
				if it.CallID == "" {
					continue
				}
				body, lang := outputText(it.Output)
				results[it.CallID] = turns.ToolResult{
					Body:    body,
					Lang:    lang,
					IsError: strings.EqualFold(it.Status, "failed") || strings.EqualFold(it.Status, "error"),
					TS:      l.Timestamp,
				}
			}
		}
	}

	return out, results
}

// The turns a user message's `<tag>` sections become: one folded turn for the
// launch scaffold, and one turn each for anything else the harness wrapped in
// a tag, such as an aborted turn.
func scaffoldTurns(sections []section, ts string) []turns.Turn {
	var out []turns.Turn
	var scaffold []turns.TimedBlock

	for _, sec := range sections {
		kind := "event"
		if proseSection(sec.tag) {
			kind = "notice"
		}
		b := turns.Block{Type: kind, Name: sectionLabel(sec.tag), Text: sec.body}

		if scaffoldSection(sec.tag) {
			scaffold = append(scaffold, turns.TimedBlock{B: b, TS: ts})
			continue
		}
		out = append(out, turns.Turn{
			Role: "system", Heading: sectionLabel(sec.tag), TS: ts,
			Blocks: []turns.TimedBlock{{B: b, TS: ts}},
		})
	}

	if len(scaffold) > 0 {
		out = append([]turns.Turn{{
			Role: "system", Heading: "Session instructions", TS: ts,
			Folded: true, Blocks: scaffold,
		}}, out...)
	}
	return out
}

// A tool's arguments as the renderer wants them. Codex sends `arguments` as a
// JSON string; a `command` given as an argv array is joined into one line so
// it renders as a shell block like every other command, and `workdir` is
// dropped when it is the only other key (it repeats the cwd).
func normalizeInput(name, arguments string) json.RawMessage {
	arguments = strings.TrimSpace(arguments)
	if arguments == "" {
		return nil
	}

	var in map[string]json.RawMessage
	if err := json.Unmarshal([]byte(arguments), &in); err != nil {
		raw, _ := json.Marshal(map[string]string{"arguments": arguments})
		return raw
	}

	if raw, ok := in["command"]; ok {
		var argv []string
		if err := json.Unmarshal(raw, &argv); err == nil {
			// `bash -lc '<script>'` is how Codex runs everything; the script is
			// what a reader wants to see.
			cmd := strings.Join(argv, " ")
			if len(argv) == 3 && (argv[0] == "bash" || argv[0] == "zsh" || argv[0] == "sh") && argv[1] == "-lc" {
				cmd = argv[2]
			}
			in["command"], _ = json.Marshal(cmd)
		}
	}

	out, err := json.Marshal(in)
	if err != nil {
		return json.RawMessage(arguments)
	}
	return out
}

// Rollouts of the subagents this thread spawned: every rollout under the store
// whose session_meta names this thread as its parent, in file order (which is
// start order, since the timestamp leads the name).
func subRollouts(sessionsDir, parentID string) []string {
	if parentID == "" {
		return nil
	}
	matches, _ := filepath.Glob(filepath.Join(sessionsDir, "*", "*", "*", "rollout-*.jsonl"))

	var out []string
	for _, p := range matches {
		m, ok := readMeta(p)
		if !ok || m.ParentThreadID == nil || *m.ParentThreadID != parentID {
			continue
		}
		out = append(out, p)
	}
	return out
}

func subdoc(path string) turns.Subdoc {
	title := "Subagent " + idOf(path)
	fh, err := os.Open(path)
	if err != nil {
		return turns.Subdoc{Title: title}
	}
	defer fh.Close()

	lines := readLines(fh)
	ts, results := buildTurns(lines)

	// The model leads the heading, as it does for Claude's subagents.
	for _, l := range lines {
		if l.Type == "turn_context" {
			var tc turnContext
			if json.Unmarshal(l.Payload, &tc) == nil && tc.Model != "" {
				if tag := turns.ModelTag(tc.Model); tag != "" {
					title = tag + " " + title
				}
				break
			}
		}
	}
	if name := names(homeOf(path))[idOf(path)]; name != "" {
		title += " · " + name
	}

	return turns.Subdoc{Title: title, Turns: ts, Results: results}
}

// The newest timestamp among the rollout's records, from its tail. With
// `userOnly` only a message the user typed counts, so the window widens until
// one is found rather than settling for the reasoning and tool output the
// thread has been writing since.
func lastTimestamp(path string, userOnly bool) time.Time {
	fh, err := os.Open(path)
	if err != nil {
		return time.Time{}
	}
	defer fh.Close()

	st, err := fh.Stat()
	if err != nil {
		return time.Time{}
	}
	size := st.Size()

	for window := int64(tailWindow); ; window *= 4 {
		if window > size {
			window = size
		}
		buf := make([]byte, window)
		if _, err := fh.ReadAt(buf, size-window); err != nil {
			return time.Time{}
		}

		var last time.Time
		for _, raw := range strings.Split(string(buf), "\n") {
			raw = strings.TrimSpace(raw)
			if len(raw) == 0 || raw[0] != '{' {
				continue
			}
			var l line
			if json.Unmarshal([]byte(raw), &l) != nil {
				continue
			}
			if userOnly && !typedPrompt(l) {
				continue
			}
			if t, err := time.Parse(time.RFC3339, l.Timestamp); err == nil && t.After(last) {
				last = t
			}
		}
		if !last.IsZero() || window >= size {
			return last
		}
	}
}
