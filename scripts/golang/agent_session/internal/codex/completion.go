package codex

import (
	"agent_session/internal/session"
	"encoding/json"
	"strings"
)

func (Adapter) CompletionContext(path string) (session.Completion, error) {
	var out session.Completion
	lines, err := session.CompletionLines(path)
	if err != nil {
		return out, err
	}
	for _, raw := range lines {
		var l line
		if json.Unmarshal(raw, &l) != nil || l.Type != "response_item" {
			continue
		}
		var it responseItem
		if json.Unmarshal(l.Payload, &it) != nil || it.Type != "message" {
			continue
		}
		if it.Role != "assistant" && it.Role != "user" {
			continue
		}
		text := partsText(it.Content)
		if strings.TrimSpace(text) == "" || it.Role == "user" && scaffoldText(text) {
			continue
		}
		out.Corpus = append(out.Corpus, text)
		if out.LastReply == "" && it.Role == "assistant" {
			out.LastReply = text
		}
	}
	return out, nil
}
