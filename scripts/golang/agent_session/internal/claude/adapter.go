// Package claude reads Claude Code's session store: one `.jsonl` transcript per
// session under `<config-home>/projects/<cwd-slug>/`, with subagent transcripts
// beside it, and the session records under `<config-home>/sessions/` for what
// is live.
package claude

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"agent_session/internal/session"
	"agent_session/internal/turns"
)

// Adapter implements [session.Adapter] for Claude Code.
type Adapter struct{}

var _ session.Adapter = Adapter{}

// Claude Code names a project directory after its cwd with every character
// that is not ASCII alphanumeric replaced by a dash -- slashes, dots, spaces
// and underscores alike. Matched against the live sessions when this was
// written; a cwd with non-ASCII letters is the one case left unverified, and a
// miss there falls through to the picker rather than opening the wrong thing.
var nonAlnum = regexp.MustCompile(`[^A-Za-z0-9]`)

func projectSlug(cwd string) string {
	return nonAlnum.ReplaceAllString(cwd, "-")
}

func (Adapter) Name(path string) (string, error) {
	fh, err := os.Open(path)
	if err != nil {
		return "", err
	}
	defer fh.Close()
	return sessionName(fh), nil
}

// Meta reads the id off the filename, the name with the same full scan `name`
// does, and the cwd from the tail: Claude Code writes it on every message
// record, so the window `preview` reads is enough.
func (a Adapter) Meta(path string) (session.Meta, error) {
	name, err := a.Name(path)
	if err != nil {
		return session.Meta{}, err
	}
	return session.Meta{
		ID:   strings.TrimSuffix(filepath.Base(path), ".jsonl"),
		Name: name,
		Cwd:  scanPreview(path, previewWindow).cwd,
	}, nil
}

func (Adapter) Document(path string, o session.DocOpts) (*turns.Document, error) {
	fh, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer fh.Close()

	// The unfiltered list is kept: the `model` attachment that names the seat
	// is one of the types `conversationRecords` drops, and the context line
	// needs it.
	all := readRecords(fh)
	records := conversationRecords(all)

	// Decoded once: the result index, the turn grouping and the subagent
	// ordering all need the blocks.
	blocks := make([][]turns.Block, len(records))
	for i := range records {
		blocks[i] = decodeBlocks(records[i].Message)
	}

	results := indexResults(records, blocks)
	doc := &turns.Document{
		Turns:   buildTurns(records, blocks, results),
		Results: results,
		Context: lastContextUsage(all, ""),
	}
	if o.Subagents {
		seat := seatModel(all)
		for _, s := range loadSubagents(path, toolCallOrder(blocks)) {
			doc.Subagents = append(doc.Subagents, s.subdoc(seat))
		}
	}
	return doc, nil
}

// HandoffDocument reads every record strictly, preserving tool results at their
// original position. Display-oriented Document deliberately tolerates corrupt
// lines; migration must fail rather than silently omit historical context.
func HandoffDocument(path string) (*turns.Document, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	r := bufio.NewReader(f)
	var all []record
	for line := 1; ; line++ {
		data, readErr := r.ReadBytes('\n')
		if len(strings.TrimSpace(string(data))) > 0 {
			var rec record
			if err := json.Unmarshal(data, &rec); err != nil {
				return nil, fmt.Errorf("transcript line %d: %w", line, err)
			}
			if rec.Type == "" {
				return nil, fmt.Errorf("transcript line %d: missing record type", line)
			}
			if (rec.Type == "user" || rec.Type == "assistant") && rec.Message == nil {
				return nil, fmt.Errorf("transcript line %d: missing message", line)
			}
			// Preserve unsupported content blocks as historical JSON, not executable items.
			if rec.Message != nil {
				var text string
				var blocks []json.RawMessage
				if json.Unmarshal(rec.Message.Content, &text) != nil {
					if json.Unmarshal(rec.Message.Content, &blocks) != nil {
						return nil, fmt.Errorf("transcript line %d: invalid message content", line)
					}
					for _, raw := range blocks {
						var block turns.Block
						if json.Unmarshal(raw, &block) != nil || block.Type == "" {
							return nil, fmt.Errorf("transcript line %d: invalid content block", line)
						}
					}
				}
			}
			all = append(all, rec)
		}
		if readErr == io.EOF {
			break
		}
		if readErr != nil {
			return nil, readErr
		}
	}
	records := conversationRecords(all)
	blocks := make([][]turns.Block, len(records))
	for i, rec := range records {
		blocks[i] = decodeBlocks(rec.Message)
		if rec.Message != nil {
			var raw []json.RawMessage
			if json.Unmarshal(rec.Message.Content, &raw) == nil {
				for j := range blocks[i] {
					switch blocks[i][j].Type {
					case "text", "thinking", "tool_use", "tool_result":
					default:
						blocks[i][j] = turns.Block{Type: "event", Name: "Historical content", Text: string(raw[j])}
					}
				}
			}
		}
	}
	return &turns.Document{Turns: buildTurns(records, blocks, nil)}, nil
}
