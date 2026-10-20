package update

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"org-paper-metadata/paper"
)

func TestCancellationStopsBeforeNextPaperAndWrites(t *testing.T) {
	root := t.TempDir()
	path := filepath.Join(root, "papers.org")
	before := "* [[" + page + "][First]]\n* [[https://arxiv.org/abs/2505.00002][Second]]\n"
	os.WriteFile(path, []byte(before), 0600)
	ctx, cancel := context.WithCancel(context.Background())
	calls := 0
	var log bytes.Buffer
	_, err := Run(ctx, root, Options{Write: true, MaxAge: time.Hour, Timeout: time.Minute}, func(context.Context, string) (paper.Metadata, error) { calls++; cancel(); return nil, context.Canceled }, now, ioDiscard{}, &log)
	if !errors.Is(err, context.Canceled) || calls != 1 {
		t.Fatal(calls, err)
	}
	after, _ := os.ReadFile(path)
	if string(after) != before {
		t.Fatal("wrote after cancellation")
	}
	resolved, _ := filepath.EvalSymlinks(path)
	if !strings.Contains(log.String(), "fetch "+resolved+":1") {
		t.Fatal("no progress before fetch", log.String())
	}
}
func TestDiffUsesContextAndSeparateHunks(t *testing.T) {
	before := "first old\n"
	for i := 0; i < 20; i++ {
		before += fmt.Sprintf("keep line %d\n", i)
	}
	before += "last old\n"
	after := strings.ReplaceAll(strings.ReplaceAll(before, "first old", "first new"), "last old", "last new")
	patch := Diff("paper.org", before, after)
	if strings.Contains(patch, "-keep line") || strings.Count(patch, "@@") != 4 {
		t.Fatal(patch)
	}
	if Diff("paper.org", before, before) != "" {
		t.Fatal("unchanged file has a patch")
	}
}

func TestUnrelatedNonUTF8OrgDataDoesNotFailRun(t *testing.T) {
	root := t.TempDir()
	os.WriteFile(filepath.Join(root, "results.org"), []byte{0xfe, 0xff, 0, 42}, 0600)
	stats, err := Run(context.Background(), root, Options{MaxAge: time.Hour, Timeout: time.Minute}, func(context.Context, string) (paper.Metadata, error) { t.Fatal("unexpected fetch"); return nil, nil }, now, ioDiscard{}, ioDiscard{})
	if err != nil || stats.Failed != 0 || stats.Updated != 0 {
		t.Fatal(stats, err)
	}
}
