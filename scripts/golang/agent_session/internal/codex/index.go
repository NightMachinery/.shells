package codex

import (
	"bufio"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"sync"

	"agent_session/internal/session"
)

// ** names
//
// Codex keeps thread names out of the rollout, in `$CODEX_HOME/
// session_index.jsonl`: append-only, one `{"id", "thread_name", ...}` per
// change, so the last line for an id is its current name and an empty name is
// a cleared one. Loaded once per home and kept for the process.

type indexEntry struct {
	ID         string `json:"id"`
	ThreadID   string `json:"thread_id"`
	ThreadName string `json:"thread_name"`
	Name       string `json:"name"`
	Title      string `json:"title"`
}

var (
	indexMu    sync.Mutex
	indexCache = map[string]map[string]string{}
)

// names is id -> current name for one `$CODEX_HOME`.
func names(home string) map[string]string {
	indexMu.Lock()
	defer indexMu.Unlock()

	if m, ok := indexCache[home]; ok {
		return m
	}
	m := loadIndex(filepath.Join(home, "session_index.jsonl"))
	indexCache[home] = m
	return m
}

func loadIndex(path string) map[string]string {
	out := map[string]string{}
	fh, err := os.Open(path)
	if err != nil {
		return out
	}
	defer fh.Close()

	sc := bufio.NewScanner(fh)
	sc.Buffer(make([]byte, 0, 64<<10), session.MaxLineBytes)
	for sc.Scan() {
		var e indexEntry
		if err := json.Unmarshal(sc.Bytes(), &e); err != nil {
			continue
		}
		id := e.ID
		if id == "" {
			id = e.ThreadID
		}
		if id == "" {
			continue
		}
		name := e.ThreadName
		if name == "" {
			name = e.Name
		}
		if name == "" {
			name = e.Title
		}
		// Last wins, and a line with no name clears it.
		out[id] = strings.TrimSpace(name)
	}
	return out
}
