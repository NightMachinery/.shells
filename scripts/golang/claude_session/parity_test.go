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
// The reference is the same path run with `-jobs=1`, which is one pandoc for
// the whole document (`pandocChunks` computes n = 1). It used to be
// `-format=md | pandoc`, which no longer works as a reference: the org-pandoc
// path tags its headings and rewrites their levels afterwards, and a bare
// pandoc pipeline has no way to do either. That trade is deliberate — this test
// exists for the chunk seams, which is what has actually broken.
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

		// Subagents are skipped: the snapshot has no subagents/ directory
		// beside it, so their section would be empty on both sides anyway.
		want, err := run(bin, "render", "-format=org-pandoc", "-subagents=false", "-jobs=1", snap)
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
