package main

import (
	"encoding/json"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// Claude Code writes a subagent's transcript beside its parent, under
// `<session-uuid>/subagents/agent-<id>.jsonl`, with a `.meta.json` sidecar
// naming the agent and, usefully, the id of the tool call that spawned it.
type subagent struct {
	id     string
	path   string
	meta   subagentMeta
	callAt int // position of its spawning call in the parent, for ordering
}

type subagentMeta struct {
	AgentType   string `json:"agentType"`
	Description string `json:"description"`
	ToolUseID   string `json:"toolUseId"`
	SpawnDepth  int    `json:"spawnDepth"`
}

// A subagent's heading: what it was and what it was asked to do.
func (s subagent) title() string {
	parts := []string{}
	if s.meta.AgentType != "" {
		parts = append(parts, s.meta.AgentType)
	}
	if s.meta.Description != "" {
		parts = append(parts, s.meta.Description)
	}
	if len(parts) == 0 {
		return "Subagent " + s.id
	}
	return strings.Join(parts, " · ")
}

// Transcripts of the agents this session spawned, ordered by where their
// spawning call appears in the parent so they read in the order they were
// launched.
func loadSubagents(sessionPath string, callOrder map[string]int) []subagent {
	dir := filepath.Join(strings.TrimSuffix(sessionPath, ".jsonl"), "subagents")

	entries, err := os.ReadDir(dir)
	if err != nil {
		return nil
	}

	var out []subagent
	for _, e := range entries {
		name := e.Name()
		if e.IsDir() || !strings.HasPrefix(name, "agent-") || !strings.HasSuffix(name, ".jsonl") {
			continue
		}

		s := subagent{
			id:     strings.TrimSuffix(strings.TrimPrefix(name, "agent-"), ".jsonl"),
			path:   filepath.Join(dir, name),
			callAt: 1 << 30,
		}

		metaPath := strings.TrimSuffix(s.path, ".jsonl") + ".meta.json"
		if raw, err := os.ReadFile(metaPath); err == nil {
			json.Unmarshal(raw, &s.meta)
		}
		if pos, ok := callOrder[s.meta.ToolUseID]; ok {
			s.callAt = pos
		}

		out = append(out, s)
	}

	sort.SliceStable(out, func(i, j int) bool {
		if out[i].callAt != out[j].callAt {
			return out[i].callAt < out[j].callAt
		}
		return out[i].id < out[j].id
	})
	return out
}

// Where each tool call sits in the transcript, so subagents can be ordered by
// the call that spawned them.
func toolCallOrder(blocks [][]block) map[string]int {
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

// Reads a subagent transcript and renders its turns, indented to sit under its
// heading in the parent document.
func renderSubagent(s subagent, opts renderOpts, jobs int) []string {
	fh, err := os.Open(s.path)
	if err != nil {
		return nil
	}
	defer fh.Close()

	var records []record
	for _, rec := range readRecords(fh) {
		if rec.Type != "user" && rec.Type != "assistant" {
			continue
		}
		if rec.IsMeta {
			continue
		}
		records = append(records, rec)
	}

	blocks := make([][]block, len(records))
	for i := range records {
		blocks[i] = decodeBlocks(records[i].Message)
	}

	results := indexResults(records, blocks)
	turns := buildTurns(records, blocks, results)

	// Under `* Subagents` / `** <agent>`, so the transcript starts at level 3.
	sub := opts
	sub.base = 2
	return renderTurns(turns, results, sub, jobs)
}
