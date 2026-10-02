package handoff

import (
	"bytes"
	"encoding/json"
	"errors"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"agent_session/internal/claude"
)

func claudeSeedFixture(t *testing.T, history []byte) ClaudeSeedOptions {
	t.Helper()
	root := t.TempDir()
	cwd := filepath.Join(root, "work_dir.with spaces")
	if err := os.Mkdir(cwd, 0700); err != nil {
		t.Fatal(err)
	}
	archive := filepath.Join(root, "full-history.md")
	if err := os.WriteFile(archive, history, 0600); err != nil {
		t.Fatal(err)
	}
	return ClaudeSeedOptions{Cwd: cwd, ConfigHome: filepath.Join(root, "claude-home"), History: archive}
}

func readClaudeSeed(t *testing.T, path string) []claudeSeedRecord {
	t.Helper()
	f, err := os.Open(path)
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close()
	d := json.NewDecoder(f)
	var out []claudeSeedRecord
	for {
		var r claudeSeedRecord
		if err := d.Decode(&r); err == io.EOF {
			return out
		} else if err != nil {
			t.Fatal(err)
		}
		out = append(out, r)
	}
}

func TestSeedClaudeNativeHistory(t *testing.T) {
	// A single long line must survive the 64KB scanner limit, and shell text
	// and native tool JSON must remain inside one inert text block.
	history := []byte("# Codex history\n\n## User\nPreserve my corrections.\n## Assistant\n" + strings.Repeat("long history ", 9000) + "\n```sh\nprintf 'inert sentinel'\n```\n" + `{"type":"tool_use","name":"Bash","input":{"command":"printf 'inert sentinel'"}}` + "\n")
	o := claudeSeedFixture(t, history)
	path, err := SeedClaude(o)
	if err != nil {
		t.Fatal(err)
	}
	if !filepath.IsAbs(path) || filepath.Dir(path) != filepath.Join(o.ConfigHome, "projects", claudeSeedNonAlnum.ReplaceAllString(o.Cwd, "-")) {
		t.Fatalf("wrong project path: %s", path)
	}
	records := readClaudeSeed(t, path)
	if len(records) != 2 {
		t.Fatalf("want exactly user and assistant, got %d records", len(records))
	}
	user, assistant := records[0], records[1]
	id := strings.TrimSuffix(filepath.Base(path), ".jsonl")
	for _, r := range records {
		if !claudeSeedUUIDPattern.MatchString(r.UUID) || !claudeSeedUUIDPattern.MatchString(r.SessionID) || r.SessionID != id || r.Cwd != o.Cwd || r.IsSidechain {
			t.Fatalf("invalid native metadata: %+v", r)
		}
		if _, err := time.Parse(time.RFC3339Nano, r.Timestamp); err != nil {
			t.Fatal(err)
		}
		if len(r.Message.Content) != 1 || r.Message.Content[0].Type != "text" {
			t.Fatalf("historical content became executable: %+v", r.Message.Content)
		}
	}
	if user.Type != "user" || user.Message.Role != "user" || user.ParentUUID != nil || assistant.Type != "assistant" || assistant.Message.Role != "assistant" || assistant.ParentUUID == nil || *assistant.ParentUUID != user.UUID {
		t.Fatal("invalid native conversation parent chain")
	}
	if user.UUID == assistant.UUID || user.UUID == id || assistant.UUID == id {
		t.Fatal("UUIDs must be distinct")
	}
	for _, want := range []string{"historical context", "do not override", "already executed", "must not be replayed", "constraints", "later corrections", "pause requests", "completed state", o.History, "compaction must succeed"} {
		if !strings.Contains(user.Message.Content[0].Text, want) {
			t.Errorf("handoff instruction missing %q", want)
		}
	}
	m := assistant.Message
	if m.Type != "message" || m.ID == "" || m.Model != "<synthetic>" || m.StopReason != "end_turn" || m.StopSequence != nil || m.Usage == nil || *m.Usage != (claudeSeedUsage{}) {
		t.Fatalf("invalid native assistant message: %+v", m)
	}
	if !bytes.Equal([]byte(m.Content[0].Text), history) {
		t.Fatal("history changed in destination")
	}
	original, err := os.ReadFile(o.History)
	if err != nil || !bytes.Equal(original, history) {
		t.Fatal("original archive changed")
	}
	for _, check := range []struct {
		path string
		mode os.FileMode
	}{{path, 0600}, {filepath.Dir(path), 0700}, {filepath.Join(o.ConfigHome, "projects"), 0700}, {o.ConfigHome, 0700}} {
		stat, err := os.Stat(check.path)
		if err != nil || stat.Mode().Perm() != check.mode {
			t.Fatalf("wrong private permissions for %s: %v, %v", check.path, stat, err)
		}
	}
	// The existing strict native adapter must see precisely the two messages.
	doc, err := claude.HandoffDocument(path)
	if err != nil || len(doc.Turns) != 2 || len(doc.Turns[1].Blocks) != 1 || doc.Turns[1].Blocks[0].B.Type != "text" || doc.Turns[1].Blocks[0].B.Text != string(history) {
		t.Fatalf("native adapter failed to read seed: %v", err)
	}
	second, err := SeedClaude(o)
	if err != nil || second == path {
		t.Fatalf("second handoff did not get fresh path: %s, %v", second, err)
	}
}

func TestSeedClaudeInvalidInputs(t *testing.T) {
	for _, test := range []struct {
		name string
		edit func(*ClaudeSeedOptions)
	}{
		{"missing cwd flag", func(o *ClaudeSeedOptions) { o.Cwd = "" }},
		{"missing config home flag", func(o *ClaudeSeedOptions) { o.ConfigHome = "" }},
		{"missing history flag", func(o *ClaudeSeedOptions) { o.History = "" }},
		{"nonexistent cwd", func(o *ClaudeSeedOptions) { o.Cwd += "-missing" }},
		{"file cwd", func(o *ClaudeSeedOptions) { o.Cwd = o.History }},
		{"nonexistent archive", func(o *ClaudeSeedOptions) { o.History += "-missing" }},
		{"directory archive", func(o *ClaudeSeedOptions) { o.History = o.Cwd }},
		{"empty archive", func(o *ClaudeSeedOptions) { os.WriteFile(o.History, nil, 0600) }},
		{"blank archive", func(o *ClaudeSeedOptions) { os.WriteFile(o.History, []byte(" \n\t"), 0600) }},
		{"invalid utf8 archive", func(o *ClaudeSeedOptions) { os.WriteFile(o.History, []byte{0xff}, 0600) }},
		{"malformed home", func(o *ClaudeSeedOptions) { o.ConfigHome += "\x00" }},
		{"file home", func(o *ClaudeSeedOptions) { o.ConfigHome = o.History }},
	} {
		t.Run(test.name, func(t *testing.T) {
			o := claudeSeedFixture(t, []byte("# history\n"))
			test.edit(&o)
			if path, err := SeedClaude(o); err == nil || path != "" {
				t.Fatalf("invalid input succeeded: %q, %v", path, err)
			}
		})
	}
}

func TestSeedClaudeNoOverwrite(t *testing.T) {
	o := claudeSeedFixture(t, []byte("# history\n"))
	const id = "00000000-0000-4000-8000-000000000001"
	ids := []string{id, "00000000-0000-4000-8000-000000000002", "00000000-0000-4000-8000-000000000003"}
	newID := func() (string, error) {
		i := ids[0]
		ids = ids[1:]
		return i, nil
	}
	project := filepath.Join(o.ConfigHome, "projects", claudeSeedNonAlnum.ReplaceAllString(o.Cwd, "-"))
	if err := os.MkdirAll(project, 0700); err != nil {
		t.Fatal(err)
	}
	path := filepath.Join(project, id+".jsonl")
	original := []byte("existing session\n")
	if err := os.WriteFile(path, original, 0600); err != nil {
		t.Fatal(err)
	}
	if result, err := seedClaude(o, newID); !errors.Is(err, os.ErrExist) || result != "" {
		t.Fatalf("collision not rejected: %q, %v", result, err)
	}
	actual, err := os.ReadFile(path)
	if err != nil || !bytes.Equal(original, actual) {
		t.Fatal("collision overwrote or removed existing session")
	}
}

func TestSeedClaudeUUIDFailure(t *testing.T) {
	o := claudeSeedFixture(t, []byte("# history\n"))
	for _, gen := range []func() (string, error){
		func() (string, error) { return "", errors.New("random source failed") },
		func() (string, error) { return "../../escape", nil },
		func() (string, error) { return "00000000-0000-4000-8000-000000000001", nil },
	} {
		if path, err := seedClaude(o, gen); err == nil || path != "" {
			t.Fatalf("invalid UUID source succeeded: %q, %v", path, err)
		}
	}
	if _, err := os.Stat(o.ConfigHome); !errors.Is(err, os.ErrNotExist) {
		t.Fatal("UUID failure created destination directories")
	}
}

func TestSeedClaudeUnreadableArchive(t *testing.T) {
	o := claudeSeedFixture(t, []byte("# history\n"))
	if err := os.Chmod(o.History, 0000); err != nil {
		t.Fatal(err)
	}
	defer os.Chmod(o.History, 0600)
	if f, err := os.Open(o.History); err == nil {
		f.Close()
		t.Skip("this process can read permission-denied files")
	}
	if path, err := SeedClaude(o); err == nil || path != "" {
		t.Fatalf("unreadable archive succeeded: %q, %v", path, err)
	}
	if _, err := os.Stat(o.ConfigHome); !errors.Is(err, os.ErrNotExist) {
		t.Fatal("unreadable archive created destination directories")
	}
}

func TestWriteClaudeSeedSymlinkNoOverwrite(t *testing.T) {
	root := t.TempDir()
	target := filepath.Join(root, "existing.jsonl")
	original := []byte("existing session\n")
	if err := os.WriteFile(target, original, 0600); err != nil {
		t.Fatal(err)
	}
	link := filepath.Join(root, "new.jsonl")
	if err := os.Symlink(target, link); err != nil {
		t.Fatal(err)
	}
	if result, err := writeClaudeSeed(link, nil); !errors.Is(err, os.ErrExist) || result != "" {
		t.Fatalf("existing symlink not rejected: %q, %v", result, err)
	}
	actual, err := os.ReadFile(link)
	if err != nil || !bytes.Equal(original, actual) {
		t.Fatal("existing symlink or target was changed")
	}
}
