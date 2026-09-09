package codex

import (
	"bufio"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strings"

	"agent_session/internal/session"
	"agent_session/internal/turns"
)

// How much of a rollout's end is read for its last timestamp, and how far into
// its start the first typed message is looked for.
const (
	tailWindow = 64 << 10
	headWindow = 4 << 20
)

// List walks `<root>/YYYY/MM/DD/rollout-*.jsonl`. Subagent rollouts (a
// session_meta with a parent) are left out unless asked for: render inlines
// them into their parent. With a cwd, only threads started there are listed.
func (Adapter) List(roots []string, o session.ListOpts) ([]session.Info, error) {
	if len(roots) == 0 {
		return nil, errors.New("list: no sessions directory given")
	}
	labels := session.ProfileLabels(roots)

	cwd := ""
	if o.Cwd != "" {
		cwd = filepath.Clean(o.Cwd)
	}

	type found struct{ path, root string }
	var files []found
	for _, root := range roots {
		matches, _ := filepath.Glob(filepath.Join(root, "*", "*", "*", "rollout-*.jsonl"))
		for _, p := range matches {
			files = append(files, found{path: p, root: root})
		}
	}
	if len(files) == 0 {
		return nil, errors.New("list: no rollouts found in: " + strings.Join(roots, " "))
	}

	infos := make([]session.Info, len(files))
	keep := make([]bool, len(files))
	session.ForEach(len(files), o.Jobs, func(i int) {
		f := files[i]
		m, ok := readMeta(f.path)
		if !ok {
			return
		}
		if m.ParentThreadID != nil && !o.Subagents {
			return
		}
		if cwd != "" && filepath.Clean(m.Cwd) != cwd {
			return
		}

		info := session.Info{Path: f.path}
		if rel, err := filepath.Rel(f.root, f.path); err == nil {
			info.Rel = rel
		} else {
			info.Rel = filepath.Base(f.path)
		}
		if l := labels[f.root]; l != "" {
			info.Rel = filepath.Join(l, info.Rel)
		}

		last := lastTimestamp(f.path)
		if last.IsZero() {
			if st, err := os.Stat(f.path); err == nil {
				last = st.ModTime()
			}
		}
		info.Epoch = last.Unix()
		info.Stamp = last.Local().Format(turns.ListStamp)
		info.Name = turns.Truncate(turns.OneLine(names(homeOf(f.path))[m.ID]), o.NameLen)
		info.Snippet = turns.Truncate(turns.OneLine(firstUserText(f.path)), o.SnippetLen)

		infos[i] = info
		keep[i] = true
	})

	out := make([]session.Info, 0, len(infos))
	for i, k := range keep {
		if k {
			out = append(out, infos[i])
		}
	}
	if len(out) == 0 && cwd != "" {
		return nil, errors.New("list: no rollouts started in " + cwd)
	}
	return out, nil
}

// The first message the user typed, skipping the scaffolding Codex injects
// ahead of it. Abandoned once the file stops being worth scanning for one.
func firstUserText(path string) string {
	fh, err := os.Open(path)
	if err != nil {
		return ""
	}
	defer fh.Close()

	sc := bufio.NewScanner(fh)
	sc.Buffer(make([]byte, 0, 64<<10), session.MaxLineBytes)

	read := 0
	for sc.Scan() {
		read += len(sc.Bytes()) + 1
		if read > headWindow {
			return ""
		}
		var l line
		if json.Unmarshal(sc.Bytes(), &l) != nil || l.Type != "response_item" {
			continue
		}
		var it responseItem
		if json.Unmarshal(l.Payload, &it) != nil || it.Type != "message" || it.Role != "user" {
			continue
		}
		text := partsText(it.Content)
		if strings.TrimSpace(text) == "" || scaffoldText(text) {
			continue
		}
		return text
	}
	return ""
}
