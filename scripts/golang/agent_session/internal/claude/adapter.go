// Package claude reads Claude Code's session store: one `.jsonl` transcript per
// session under `<config-home>/projects/<cwd-slug>/`, with subagent transcripts
// beside it, and the session records under `<config-home>/sessions/` for what
// is live.
package claude

import (
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

	records := conversationRecords(readRecords(fh))

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
	}
	if o.Subagents {
		for _, s := range loadSubagents(path, toolCallOrder(blocks)) {
			doc.Subagents = append(doc.Subagents, s.subdoc())
		}
	}
	return doc, nil
}
