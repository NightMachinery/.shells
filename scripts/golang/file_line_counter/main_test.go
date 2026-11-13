package main

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func writeTempFile(t *testing.T, dir, name string, content []byte) string {
	t.Helper()
	p := filepath.Join(dir, name)
	if err := os.WriteFile(p, content, 0o600); err != nil {
		t.Fatalf("write %s: %v", p, err)
	}
	return p
}

func TestRunEmptyInput(t *testing.T) {
	for _, input := range []string{"", "\n", "\n\n"} {
		var out bytes.Buffer
		if err := run(strings.NewReader(input), &out); err != nil {
			t.Fatalf("input %q: unexpected error: %v", input, err)
		}
		if got, want := out.String(), "All lines: 0\n"; got != want {
			t.Fatalf("input %q: got %q, want %q", input, got, want)
		}
	}
}

func TestRunCountsAndSorting(t *testing.T) {
	dir := t.TempDir()
	fEmpty := writeTempFile(t, dir, "empty.txt", nil)
	fNoNL := writeTempFile(t, dir, "no_newline.txt", []byte("one line without trailing newline"))
	fOne := writeTempFile(t, dir, "one.txt", []byte("a\n"))
	fThree := writeTempFile(t, dir, "three.txt", []byte("a\nb\nc\n"))
	fCRLF := writeTempFile(t, dir, "crlf.txt", []byte("a\r\nb\r\n"))
	fTie1 := writeTempFile(t, dir, "tie1.txt", []byte("x\ny\n"))
	fTie2 := writeTempFile(t, dir, "tie2.txt", []byte("p\nq\n"))
	fBinary := writeTempFile(t, dir, "binary.bin", []byte{0x00, 0xff, '\n', 0x80, '\n', '\n'})

	// Input without trailing newline is also accepted.
	in := strings.Join([]string{fEmpty, fNoNL, fOne, fTie1, fThree, fCRLF, fTie2, fBinary}, "\n")
	var out bytes.Buffer
	if err := run(strings.NewReader(in), &out); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Totals: 0 + 0 + 1 + 2 + 3 + 2 + 2 + 3 = 13.
	// Ties should preserve input order via SliceStable:
	// 3: fThree, fBinary
	// 2: fTie1, fCRLF, fTie2
	// 1: fOne
	// 0: fEmpty, fNoNL
	want := strings.Join([]string{
		"All lines: 13",
		fmt.Sprintf("3: %s", fThree),
		fmt.Sprintf("3: %s", fBinary),
		fmt.Sprintf("2: %s", fTie1),
		fmt.Sprintf("2: %s", fCRLF),
		fmt.Sprintf("2: %s", fTie2),
		fmt.Sprintf("1: %s", fOne),
		fmt.Sprintf("0: %s", fEmpty),
		fmt.Sprintf("0: %s", fNoNL),
		"",
	}, "\n")

	if got := out.String(); got != want {
		t.Fatalf("output mismatch:\ngot:\n%s\nwant:\n%s", got, want)
	}
}

func TestRunGlobAndSpecialFilenames(t *testing.T) {
	dir := t.TempDir()
	fStar := writeTempFile(t, dir, "file * [1] ?.org", []byte("1\n2\n3\n4\n"))
	fUnicode := writeTempFile(t, dir, "ünïcödé 文件.org", []byte("α\nβ\n"))

	in := fStar + "\n" + fUnicode + "\n\n"
	var out bytes.Buffer
	if err := run(strings.NewReader(in), &out); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	want := fmt.Sprintf("All lines: 6\n4: %s\n2: %s\n", fStar, fUnicode)
	if got := out.String(); got != want {
		t.Fatalf("got %q, want %q", got, want)
	}
}

func TestRunManyFilesWorkerPoolAndLargeFile(t *testing.T) {
	dir := t.TempDir()
	var paths []string
	var total int64

	for i := 0; i < 40; i++ {
		content := strings.Repeat("line\n", i)
		p := writeTempFile(t, dir, fmt.Sprintf("f_%02d.txt", i), []byte(content))
		paths = append(paths, p)
		total += int64(i)
	}

	// Large file (> 64 KiB read buffer) to verify chunked counting across buffer boundaries.
	largeContent := strings.Repeat("0123456789abcdef\n", 10000)
	fLarge := writeTempFile(t, dir, "large.txt", []byte(largeContent))
	paths = append(paths, fLarge)
	total += 10000

	var out bytes.Buffer
	if err := run(strings.NewReader(strings.Join(paths, "\n")+"\n"), &out); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	lines := strings.Split(strings.TrimSuffix(out.String(), "\n"), "\n")
	if len(lines) != 42 {
		t.Fatalf("expected 42 output lines, got %d", len(lines))
	}
	if lines[0] != fmt.Sprintf("All lines: %d", total) {
		t.Fatalf("header mismatch: got %q, want %q", lines[0], fmt.Sprintf("All lines: %d", total))
	}
	if lines[1] != fmt.Sprintf("10000: %s", fLarge) {
		t.Fatalf("top file mismatch: got %q", lines[1])
	}
	if lines[len(lines)-1] != fmt.Sprintf("0: %s", paths[0]) {
		t.Fatalf("last file mismatch: got %q", lines[len(lines)-1])
	}
}

func TestRunErrors(t *testing.T) {
	dir := t.TempDir()
	fOK := writeTempFile(t, dir, "ok.txt", []byte("hi\n"))
	missing := filepath.Join(dir, "does-not-exist.txt")

	t.Run("missing file", func(t *testing.T) {
		var out bytes.Buffer
		err := run(strings.NewReader(fOK+"\n"+missing+"\n"), &out)
		if err == nil {
			t.Fatal("expected error for missing file, got nil")
		}
		if out.Len() != 0 {
			t.Fatalf("expected no stdout on error, got %q", out.String())
		}
	})

	t.Run("blank line in middle", func(t *testing.T) {
		var out bytes.Buffer
		err := run(strings.NewReader(fOK+"\n\n"+fOK+"\n"), &out)
		if err == nil {
			t.Fatal("expected error for empty path in middle, got nil")
		}
	})
}
