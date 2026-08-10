package main

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// The org-pandoc path splits the document across pandoc processes and wraps
// the result in a skeleton Go builds itself. Both are only safe as long as the
// output stays identical to handing the whole markdown to one pandoc, and both
// have silently broken that in the past — once on a chunk seam, once on a
// stray newline at a seam. This checks it against real transcripts.
//
// Needs data and pandoc, so it only runs when pointed at a corpus:
//
//	CLAUDE_SESSION_CORPUS=~/.claude/projects go test -run Parity ./...
func TestPandocPathParity(t *testing.T) {
	corpus := os.Getenv("CLAUDE_SESSION_CORPUS")
	if corpus == "" {
		t.Skip("set CLAUDE_SESSION_CORPUS to a directory of session transcripts")
	}
	if _, err := exec.LookPath("pandoc"); err != nil {
		t.Skip("pandoc not installed")
	}

	bin := filepath.Join(t.TempDir(), "claude_session")
	if out, err := exec.Command("go", "build", "-o", bin, ".").CombinedOutput(); err != nil {
		t.Fatalf("build: %v\n%s", err, out)
	}

	var files []string
	filepath.WalkDir(corpus, func(p string, d os.DirEntry, err error) error {
		if err == nil && !d.IsDir() && strings.HasSuffix(p, ".jsonl") {
			files = append(files, p)
		}
		return nil
	})
	if len(files) == 0 {
		t.Skipf("no .jsonl transcripts under %s", corpus)
	}

	snap := filepath.Join(t.TempDir(), "snapshot.jsonl")

	for _, f := range files {
		// The corpus is live: a session open in another window grows between
		// reads, and comparing two passes over a moving file fails for no
		// reason. Both paths read one snapshot instead.
		raw, err := os.ReadFile(f)
		if err != nil {
			t.Errorf("%s: %v", f, err)
			continue
		}
		if err := os.WriteFile(snap, raw, 0o600); err != nil {
			t.Fatalf("snapshot: %v", err)
		}

		// Subagents are skipped here: their section is skeleton, which by
		// design only the org path can express. Snapshotting a session
		// without its subagents/ directory would break them anyway.
		want, err := pipeline(bin, snap)
		if err != nil {
			t.Errorf("%s: reference conversion: %v", f, err)
			continue
		}
		got, err := run(bin, "render", "-format=org-pandoc", "-subagents=false", snap)
		if err != nil {
			t.Errorf("%s: org-pandoc: %v", f, err)
			continue
		}
		if got != want {
			t.Errorf("%s: org-pandoc output differs from a single pandoc run", f)
		}
	}
	t.Logf("compared %d transcripts", len(files))
}

func run(bin string, args ...string) (string, error) {
	out, err := exec.Command(bin, args...).Output()
	return string(out), err
}

func pipeline(bin, file string) (string, error) {
	md := exec.Command(bin, "render", "-format=md", "-subagents=false", file)
	pd := exec.Command("pandoc", "--from=gfm-gfm_auto_identifiers", "--to=org", "--wrap=none")

	var err error
	if pd.Stdin, err = md.StdoutPipe(); err != nil {
		return "", err
	}
	if err = md.Start(); err != nil {
		return "", err
	}
	out, err := pd.Output()
	if err != nil {
		return "", err
	}
	return string(out), md.Wait()
}
