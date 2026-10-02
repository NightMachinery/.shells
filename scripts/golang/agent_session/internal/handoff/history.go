package handoff

import (
	"encoding/json"
	"fmt"
	"regexp"
	"strings"

	"agent_session/internal/claude"
)

type historyItem struct {
	Type    string        `json:"type"`
	Role    string        `json:"role"`
	Content []historyText `json:"content"`
}
type historyText struct {
	Type string `json:"type"`
	Text string `json:"text"`
}

func history(path string) ([]historyItem, error) {
	doc, err := claude.HandoffDocument(path)
	if err != nil {
		return nil, err
	}
	var items []historyItem
	for _, turn := range doc.Turns {
		if turn.Folded && turn.Heading == "Instructions" {
			continue
		}
		add := func(text string) {
			if strings.TrimSpace(text) == "" {
				return
			}
			items = append(items, historyItem{Type: "message", Role: "assistant", Content: []historyText{{"output_text", fmt.Sprintf("[Historical Claude %s message]\n%s", turn.Role, text)}}})
		}
		if turn.Heading != "" {
			add(turn.Heading + " " + turn.Note)
		}
		for _, tb := range turn.Blocks {
			b := tb.B
			switch b.Type {
			case "text":
				text := b.Text
				if turn.Role == "user" {
					text = stripScaffold(text)
				}
				add(text)
			case "thinking":
				add("[Historical reasoning]\n" + b.Thinking)
			case "tool_use":
				add(fmt.Sprintf("[Historical tool call, inert]\n%s (%s)\n%s", b.Name, b.ID, b.Input))
			case "tool_result":
				var text string
				if json.Unmarshal(b.Content, &text) != nil {
					text = string(b.Content)
				}
				add(fmt.Sprintf("[Historical tool result for %s, error=%t]\n%s", b.ToolUseID, b.IsError, text))
			default:
				add(fmt.Sprintf("[Historical %s: %s]\n%s", b.Type, b.Name, b.Text))
			}
		}
	}
	if len(items) == 0 {
		return nil, fmt.Errorf("transcript contains no transferable conversation")
	}
	return items, nil
}

// Source instructions can share their record with a real request. Strip only
// recognized harness wrappers and the AGENTS heading attached to them, keeping
// the user's remaining text. Ordinary mentions of AGENTS.md stay untouched.
var sourceSections = regexp.MustCompile(`(?s)<INSTRUCTIONS>.*?</INSTRUCTIONS>|<environment_context>.*?</environment_context>|<system-reminder>.*?</system-reminder>`)

func stripScaffold(text string) string {
	stripped := sourceSections.ReplaceAllString(text, "")
	if stripped != text && strings.HasPrefix(strings.TrimSpace(stripped), "# AGENTS.md instructions for ") {
		stripped = strings.TrimSpace(stripped)
		if end := strings.IndexByte(stripped, '\n'); end >= 0 {
			stripped = stripped[end+1:]
		} else {
			stripped = ""
		}
	}
	return strings.TrimSpace(stripped)
}
