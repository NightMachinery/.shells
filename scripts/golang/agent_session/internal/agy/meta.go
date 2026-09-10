package agy

import (
	"bufio"
	"encoding/json"
	"net/url"
	"os"
	"path/filepath"
	"strings"
	"sync"

	"agent_session/internal/session"
)

// ** names and working directories
//
// Antigravity keeps a conversation's title in a SQLite database
// (`conversation_summaries.db`) and mirrors it into
// `cache/conversation_metadata.json`. The JSON is what this reads: the Go here
// is stdlib-only by design, and the mirror carries the same two fields that
// decide a name. The zsh side asks SQLite directly
// ([agfi:agy-conversation-name]) and wins where the two disagree, which is why
// the precedence below is the same one it uses: a title the user set, else the
// preview the model generated.
//
// Where a conversation ran is not in that mirror -- `WorkspaceURIs` is null on
// every entry seen -- so `history.jsonl` answers it instead: one line per
// conversation with the workspace it was opened in.

type summary struct {
	ID            string   `json:"ID"`
	Title         string   `json:"Title"`
	Preview       string   `json:"Preview"`
	NumSteps      int      `json:"NumSteps"`
	UpdatedAt     string   `json:"UpdatedAt"`
	WorkspaceURIs []string `json:"WorkspaceURIs"`
	AgentName     string   `json:"AgentName"`
	ProjectID     string   `json:"ProjectID"`
}

type metadataFile struct {
	Conversations map[string]struct {
		Summary          summary `json:"summary"`
		IsInternal       bool    `json:"is_internal"`
		LastModifiedTime string  `json:"last_modified_time"`
	} `json:"conversations"`
}

type historyLine struct {
	ConversationID string `json:"conversationId"`
	Display        string `json:"display"`
	Timestamp      int64  `json:"timestamp"`
	Workspace      string `json:"workspace"`
}

// One read per state directory, kept for the process: `list` asks for every
// row and the file is one JSON document.
var (
	metaMu    sync.Mutex
	metaCache = map[string]map[string]summary{}
	cwdMu     sync.Mutex
	cwdCache  = map[string]map[string]string{}
)

func summaries(home string) map[string]summary {
	metaMu.Lock()
	defer metaMu.Unlock()
	if m, ok := metaCache[home]; ok {
		return m
	}
	m := loadSummaries(filepath.Join(home, "cache", "conversation_metadata.json"))
	metaCache[home] = m
	return m
}

func loadSummaries(path string) map[string]summary {
	out := map[string]summary{}
	raw, err := os.ReadFile(path)
	if err != nil {
		return out
	}
	var f metadataFile
	if err := json.Unmarshal(raw, &f); err != nil {
		return out
	}
	for id, entry := range f.Conversations {
		out[id] = entry.Summary
	}
	return out
}

// Which directory each conversation ran in, from `history.jsonl`; the last
// line for an id wins, as it is the most recent time it was opened.
func workspaces(home string) map[string]string {
	cwdMu.Lock()
	defer cwdMu.Unlock()
	if m, ok := cwdCache[home]; ok {
		return m
	}
	m := loadWorkspaces(filepath.Join(home, "history.jsonl"))
	cwdCache[home] = m
	return m
}

func loadWorkspaces(path string) map[string]string {
	out := map[string]string{}
	fh, err := os.Open(path)
	if err != nil {
		return out
	}
	defer fh.Close()

	sc := bufio.NewScanner(fh)
	sc.Buffer(make([]byte, 0, 64<<10), session.MaxLineBytes)
	for sc.Scan() {
		var l historyLine
		if err := json.Unmarshal(sc.Bytes(), &l); err != nil {
			continue
		}
		if l.ConversationID == "" || l.Workspace == "" {
			continue
		}
		out[l.ConversationID] = l.Workspace
	}
	return out
}

// The name of a conversation: the title the user set, else the preview the
// model generated. "" when it has neither, and the caller falls back to the id.
func nameOf(home, id string) string {
	s := summaries(home)[id]
	if t := strings.TrimSpace(s.Title); t != "" {
		return t
	}
	return strings.TrimSpace(s.Preview)
}

// Where a conversation ran: its history entry, else the first workspace URI
// the summary carries (a `file://` URL, when it carries any at all).
func cwdOf(home, id string) string {
	if w := workspaces(home)[id]; w != "" {
		return w
	}
	for _, u := range summaries(home)[id].WorkspaceURIs {
		if p := fileURLPath(u); p != "" {
			return p
		}
	}
	return ""
}

func fileURLPath(raw string) string {
	if !strings.HasPrefix(raw, "file:") {
		return raw
	}
	u, err := url.Parse(raw)
	if err != nil {
		return ""
	}
	return u.Path
}
