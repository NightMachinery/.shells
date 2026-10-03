package claude

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
		var rec msgRecord
		if json.Unmarshal(raw, &rec) != nil || rec.IsMeta {
			continue
		}
		if rec.Type != "assistant" && !rec.typed() {
			continue
		}
		text := rec.text()
		if strings.TrimSpace(text) == "" {
			continue
		}
		out.Corpus = append(out.Corpus, text)
		if out.LastReply == "" && rec.Type == "assistant" {
			out.LastReply = text
		}
	}
	return out, nil
}
