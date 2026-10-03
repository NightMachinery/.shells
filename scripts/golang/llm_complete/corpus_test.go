package main

import (
	"os"
	"path/filepath"
	"testing"
	"time"
)

func TestGitCorpusIsolationAndDeadline(t *testing.T) {
	dir := t.TempDir()
	script := filepath.Join(dir, "git")
	if err := os.WriteFile(script, []byte("#!/bin/sh\n[ -z \"$GIT_DIR$GIT_WORK_TREE\" ] || exit 1\n[ \"$1\" = -C ] && [ \"$3\" = ls-files ] && [ \"$4\" = -z ] || exit 2\nprintf 'src/alpha.go\\000docs/alpha.md\\000'\n"), 0700); err != nil {
		t.Fatal(err)
	}
	t.Setenv("PATH", dir+":"+os.Getenv("PATH"))
	t.Setenv("GIT_DIR", "fabricated")
	t.Setenv("GIT_WORK_TREE", "fabricated")
	if got := gitPathsWithin("fabricated-cwd", time.Second); got != "src/alpha.go\ndocs/alpha.md\n" {
		t.Fatal(got)
	}
	// A stuck command is excluded rather than delaying a keypress indefinitely.
	os.WriteFile(script, []byte("#!/bin/sh\nexec sleep 5\n"), 0700)
	start := time.Now()
	if gitPaths("fabricated-cwd") != "" || time.Since(start) > time.Second {
		t.Fatal("git deadline")
	}
}

func TestCachedGitIndexValidation(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	dir := t.TempDir()
	index := filepath.Join(dir, "index")
	os.WriteFile(index, []byte("fabricated index"), 0600)
	st, _ := os.Stat(index)
	if err := writePrivate(gitCorpusFile(dir), GitCorpus{Cwd: dir, Index: index, Mtime: st.ModTime().UnixNano(), Size: st.Size(), Paths: "src/alpha.go\n"}); err != nil {
		t.Fatal(err)
	}
	if p, ok := cachedGit(dir); !ok || p != "src/alpha.go\n" {
		t.Fatal(p, ok)
	}
	os.WriteFile(index, []byte("changed index"), 0600)
	if _, ok := cachedGit(dir); ok {
		t.Fatal("changed index reused")
	}
}
