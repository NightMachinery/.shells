package codex

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"

	"agent_session/internal/turns"
)

// HandoffDocument rejects malformed and truncated JSONL rather than the
// display renderer's best-effort read. Only visible reasoning summaries are
// exported, never raw or encrypted reasoning, and source instructions stay out.
func HandoffDocument(path string) (*turns.Document, error) {
	return handoffDocument(path, map[string]bool{})
}
func handoffDocument(path string, seen map[string]bool) (*turns.Document, error) {
	path, err := filepath.Abs(path)
	if err != nil {
		return nil, err
	}
	if seen[path] {
		return nil, fmt.Errorf("cyclic subagent rollout %s", path)
	}
	seen[path] = true
	lines, err := strictLines(path)
	if err != nil {
		return nil, err
	}
	doc := &turns.Document{}
	doc.Turns, doc.Results = buildTurns(lines)
	doc.Turns = withoutSourceInstructions(doc.Turns)
	var meta sessionMeta
	for _, l := range lines {
		if l.Type == "session_meta" {
			if err = json.Unmarshal(l.Payload, &meta); err != nil {
				return nil, err
			}
			break
		}
	}
	if meta.ID == "" {
		return nil, fmt.Errorf("rollout %s has no session_meta id", path)
	}
	for _, child := range subRollouts(filepath.Join(homeOf(path), "sessions"), meta.ID) {
		sub, err := handoffDocument(child, seen)
		if err != nil {
			return nil, fmt.Errorf("subagent export: %w", err)
		}
		doc.Subagents = append(doc.Subagents, turns.Subdoc{Title: "Subagent " + idOf(child), Turns: sub.Turns, Results: sub.Results})
		doc.Subagents = append(doc.Subagents, sub.Subagents...)
	}
	return doc, nil
}
func withoutSourceInstructions(in []turns.Turn) []turns.Turn {
	out := make([]turns.Turn, 0, len(in))
	for _, t := range in {
		if t.Heading == "Session instructions" && t.Folded {
			continue
		}
		out = append(out, t)
	}
	return out
}
func strictLines(path string) ([]line, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	r := bufio.NewReader(f)
	var out []line
	for number := 1; ; number++ {
		data, readErr := r.ReadBytes('\n')
		if len(bytes.TrimSpace(data)) > 0 {
			var l line
			if err = json.Unmarshal(data, &l); err != nil {
				return nil, fmt.Errorf("rollout %s line %d: %w", path, number, err)
			}
			if l.Type == "" || len(l.Payload) == 0 {
				return nil, fmt.Errorf("rollout %s line %d: missing type or payload", path, number)
			}
			if l.Type == "response_item" {
				var item responseItem
				if err = json.Unmarshal(l.Payload, &item); err != nil {
					return nil, fmt.Errorf("rollout %s line %d payload: %w", path, number, err)
				}
				if item.Type == "reasoning" {
					item.Content = nil // Raw reasoning is intentionally never an export input.
					l.Payload, err = json.Marshal(item)
					if err != nil {
						return nil, err
					}
				}
			}
			out = append(out, l)
		}
		if readErr == io.EOF {
			break
		}
		if readErr != nil {
			return nil, readErr
		}
	}
	return out, nil
}
