package claude

import (
	"encoding/json"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"agent_session/internal/turns"
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

// A subagent's heading: which model it ran as, what it was, and what it was
// asked to do. The model leads, because it is the thing you scan a list of
// agents for -- which of these was the expensive one, which was the cheap one.
// Empty when the transcript never said, in which case the heading is what it
// always was.
func (s subagent) title(model string) string {
	parts := []string{}
	if tag := turns.ModelTag(model); tag != "" {
		parts = append(parts, tag)
	}

	rest := []string{}
	if s.meta.AgentType != "" {
		rest = append(rest, s.meta.AgentType)
	}
	if s.meta.Description != "" {
		rest = append(rest, s.meta.Description)
	}
	if len(rest) == 0 {
		rest = append(rest, "Subagent "+s.id)
	}

	// A middle dot between what it was and what it did, a space after the
	// model: the tag is an attribute of the agent, not another field of equal
	// weight.
	return strings.TrimSpace(strings.Join(parts, " ") + " " + strings.Join(rest, " · "))
}

// The model a subagent ran as: the most frequent across its own assistant
// messages. Most run one model throughout, but a model switch mid-agent is
// possible and an interrupted one carries `<synthetic>` records, so the mode is
// what survives both without being thrown off by a stray record.
//
// `<synthetic>` does not get a vote. It marks a message Claude Code wrote
// itself -- an interruption notice, say -- and is not a model at all.
//
// Ties go to whichever appeared first, so the answer cannot depend on map
// iteration order: the rendered document has to be byte-identical run to run,
// which is what `TestPandocPathParity` checks.
func modelMode(records []record) string {
	counts := map[string]int{}
	var order []string

	for _, rec := range records {
		if rec.Type != "assistant" || rec.Message == nil {
			continue
		}
		m := rec.Message.Model
		if m == "" || m == "<synthetic>" {
			continue
		}
		if _, seen := counts[m]; !seen {
			order = append(order, m)
		}
		counts[m]++
	}

	best := ""
	for _, m := range order {
		if best == "" || counts[m] > counts[best] {
			best = m
		}
	}
	return best
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

// Reads a subagent transcript into its own document. The model in the heading
// comes out of the same read: reading the file twice for a field that is
// already in hand would be silly. An unreadable transcript still gets its
// heading, with nothing under it.
func (s subagent) subdoc() turns.Subdoc {
	fh, err := os.Open(s.path)
	if err != nil {
		return turns.Subdoc{Title: s.title("")}
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

	blocks := make([][]turns.Block, len(records))
	for i := range records {
		blocks[i] = decodeBlocks(records[i].Message)
	}

	results := indexResults(records, blocks)
	return turns.Subdoc{
		Title:   s.title(modelMode(records)),
		Turns:   buildTurns(records, blocks, results),
		Results: results,
	}
}
