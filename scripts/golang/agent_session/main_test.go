package main

import (
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent_session/internal/session"
)

func capturePreview(t *testing.T, adapter session.Adapter, args ...string) (string, error) {
	t.Helper()
	output, err := os.CreateTemp(t.TempDir(), "preview")
	if err != nil {
		t.Fatal(err)
	}
	defer output.Close()
	original := os.Stdout
	os.Stdout = output
	defer func() { os.Stdout = original }()
	previewErr := cmdPreview(adapter, args)
	if _, err := output.Seek(0, 0); err != nil {
		t.Fatal(err)
	}
	data, err := io.ReadAll(output)
	if err != nil {
		t.Fatal(err)
	}
	return string(data), previewErr
}

func TestPreviewMissingTranscriptFallback(t *testing.T) {
	root := t.TempDir()
	for _, provider := range []string{"claude", "codex", "agy", "-"} {
		for _, path := range []string{"-", filepath.Join(root, "missing.jsonl"), root} {
			t.Run(provider+"/"+filepath.Base(path), func(t *testing.T) {
				output, err := capturePreview(t, adapters[provider], "-color=false", path)
				if err != nil || output != "No transcript available.\n" {
					t.Fatalf("output=%q error=%v", output, err)
				}
			})
		}
	}
}

func TestPreviewRetainsRichConversation(t *testing.T) {
	path := filepath.Join(t.TempDir(), "session.jsonl")
	data := `{"type":"user","sessionId":"fixture","message":{"role":"user","content":"Fixture prompt for rich preview"}}` + "\n"
	if err := os.WriteFile(path, []byte(data), 0600); err != nil {
		t.Fatal(err)
	}
	output, err := capturePreview(t, adapters["claude"], "-color=false", path)
	if err != nil || !strings.Contains(output, "Fixture prompt for rich preview") {
		t.Fatalf("output=%q error=%v", output, err)
	}
	output, err = capturePreview(t, nil, path)
	if err != nil || output != "No session preview available for this provider.\n" {
		t.Fatalf("output=%q error=%v", output, err)
	}
	if _, err := capturePreview(t, adapters["claude"]); err == nil {
		t.Fatal("missing path argument stopped being a usage error")
	}
}
