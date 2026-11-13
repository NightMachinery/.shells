package main

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestOrgEscapes(t *testing.T) {
	t.Run("orgTitleEscape", func(t *testing.T) {
		cases := []struct {
			in   string
			want string
		}{
			{"plain.txt", "plain.txt"},
			{"Title [Test]", "Title {Test}"},
			{`foo\[bar\].txt`, "foo{bar}.txt"},
			{`foo\\[bar\\].txt`, "foo{bar}.txt"},
			{`foo\\[bar\\]`, "foo{bar}"},
			{"[[a]]", "{{a}}"},
			{"[a] and [b]", "{a} and {b}"},
		}
		for _, tc := range cases {
			if got := orgTitleEscape(tc.in); got != tc.want {
				t.Errorf("orgTitleEscape(%q) = %q, want %q", tc.in, got, tc.want)
			}
		}
	})

	t.Run("orgLinkEscape", func(t *testing.T) {
		cases := []struct {
			in   string
			want string
		}{
			{"/tmp/plain.txt", "/tmp/plain.txt"},
			{"/tmp/file[1].txt", `/tmp/file\[1\].txt`},
			{`/tmp/file\[1\].txt`, `/tmp/file\[1\].txt`},
			{"[start]", `\[start\]`},
			{"[[nested]]", `\[\[nested\]\]`},
			{"/tmp/café[1]/résumé.pdf", `/tmp/café\[1\]/résumé.pdf`},
		}
		for _, tc := range cases {
			if got := orgLinkEscape(tc.in); got != tc.want {
				t.Errorf("orgLinkEscape(%q) = %q, want %q", tc.in, got, tc.want)
			}
		}
	})
}

func TestCLIValidation(t *testing.T) {
	var stdout, stderr bytes.Buffer
	if code := run(nil, &stdout, &stderr); code != 1 {
		t.Fatalf("run(nil) = %d, want 1", code)
	}
	if stdout.Len() != 0 {
		t.Fatalf("expected empty stdout, got %q", stdout.String())
	}
	if !strings.Contains(stderr.String(), "usage:") {
		t.Fatalf("expected usage on stderr, got %q", stderr.String())
	}

	stdout.Reset()
	stderr.Reset()
	if code := run([]string{"/tmp", "[unclosed"}, &stdout, &stderr); code != 1 {
		t.Fatalf("run(invalid regex) = %d, want 1", code)
	}
	if stdout.Len() != 0 {
		t.Fatalf("expected empty stdout on invalid regex, got %q", stdout.String())
	}
	if !strings.Contains(stderr.String(), "invalid skip regex") {
		t.Fatalf("expected invalid regex message, got %q", stderr.String())
	}
}

func TestBasicTreeTraversalAndSorting(t *testing.T) {
	root := t.TempDir()

	// Create files and nested dirs, including name with brackets and sort-order test (a-b vs a/).
	mustMkdirAll(t, filepath.Join(root, "a", "sub"))
	mustWriteFile(t, filepath.Join(root, "a", "sub", "deep.txt"))
	mustWriteFile(t, filepath.Join(root, "a", "x[1].txt"))
	mustWriteFile(t, filepath.Join(root, "a-b"))
	mustMkdirAll(t, filepath.Join(root, "empty"))

	var stdout, stderr bytes.Buffer
	code := run([]string{root}, &stdout, &stderr)
	if code != 0 {
		t.Fatalf("run returned %d, stderr=%s", code, stderr.String())
	}

	wantStdout := strings.Join([]string{
		"+TITLE: Pathtree of =" + root + "=",
		"",
		"* [[" + root + "/a-b][a-b]]",
		"",
		"* [[" + root + "/a/][a]]",
		"** [[" + root + "/a/sub/][sub]]",
		"*** [[" + root + "/a/sub/deep.txt][deep.txt]]",
		"",
		"** [[" + root + `/a/x\[1\].txt][x{1}.txt]]`,
		"",
		"* [[" + root + "/empty/][empty]]",
		"",
	}, "\n")

	if stdout.String() != wantStdout {
		t.Fatalf("stdout mismatch:\ngot:\n%q\nwant:\n%q", stdout.String(), wantStdout)
	}

	wantStderrParts := []string{
		"INFO: Processing " + root + " ...\n\n",
		"INFO: Processing " + root + "/a/ ...\n\n",
		"INFO: Processing " + root + "/a/sub/ ...\n\n",
		"INFO: Processing " + root + "/empty/ ...\n\n",
	}
	for _, part := range wantStderrParts {
		if !strings.Contains(stderr.String(), part) {
			t.Errorf("stderr missing %q; got %q", part, stderr.String())
		}
	}
}

func TestTrailingSlashPreservedInTitleButNotDoubledInLinks(t *testing.T) {
	root := t.TempDir()
	mustWriteFile(t, filepath.Join(root, "file.txt"))

	arg := root + "/"
	var stdout, stderr bytes.Buffer
	if code := run([]string{arg}, &stdout, &stderr); code != 0 {
		t.Fatalf("run returned %d", code)
	}

	want := "+TITLE: Pathtree of =" + arg + "=\n\n* [[" + root + "/file.txt][file.txt]]\n"
	if stdout.String() != want {
		t.Fatalf("got %q, want %q", stdout.String(), want)
	}
	if !strings.Contains(stderr.String(), "INFO: Processing "+arg+" ...\n\n") {
		t.Fatalf("expected stderr to echo arg %q, got %q", arg, stderr.String())
	}
}

func TestSymlinksNotFollowedAndCyclicSafe(t *testing.T) {
	root := t.TempDir()
	realDir := filepath.Join(root, "real")
	mustMkdirAll(t, realDir)
	mustWriteFile(t, filepath.Join(realDir, "inside.txt"))

	// Cyclic symlink inside realDir pointing back to root.
	if err := os.Symlink(root, filepath.Join(realDir, "loop")); err != nil {
		t.Fatalf("symlink loop: %v", err)
	}
	// Symlink to directory at top level.
	if err := os.Symlink(realDir, filepath.Join(root, "linkdir")); err != nil {
		t.Fatalf("symlink linkdir: %v", err)
	}
	// Broken symlink at top level.
	if err := os.Symlink(filepath.Join(root, "missing"), filepath.Join(root, "broken")); err != nil {
		t.Fatalf("symlink broken: %v", err)
	}

	var stdout, stderr bytes.Buffer
	if code := run([]string{root}, &stdout, &stderr); code != 0 {
		t.Fatalf("run returned %d", code)
	}

	got := stdout.String()
	// linkdir is emitted (with trailing / because target is a dir), but NOT traversed into.
	if !strings.Contains(got, "* [["+root+"/linkdir/][linkdir]]\n") {
		t.Errorf("missing linkdir heading in:\n%s", got)
	}
	if strings.Contains(got, "linkdir/inside.txt") {
		t.Errorf("symlink linkdir should not be traversed:\n%s", got)
	}
	// broken symlink is emitted without trailing /.
	if !strings.Contains(got, "* [["+root+"/broken][broken]]\n") {
		t.Errorf("missing broken symlink heading in:\n%s", got)
	}
	// loop inside real is emitted, not traversed.
	if !strings.Contains(got, "** [["+root+"/real/loop/][loop]]\n") {
		t.Errorf("missing loop symlink heading in:\n%s", got)
	}
	if strings.Contains(got, "loop/real") {
		t.Errorf("cyclic symlink should not be traversed:\n%s", got)
	}

	// Passing a symlink (even with trailing slash) as root should not traverse it.
	stdout.Reset()
	stderr.Reset()
	symRoot := filepath.Join(root, "linkdir") + "/"
	if code := run([]string{symRoot}, &stdout, &stderr); code != 0 {
		t.Fatalf("run on symlink root returned %d", code)
	}
	if stdout.String() != "+TITLE: Pathtree of ="+symRoot+"=\n\n" {
		t.Errorf("symlink root should only print title, got %q", stdout.String())
	}
	if stderr.Len() != 0 {
		t.Errorf("symlink root should not log Processing, got %q", stderr.String())
	}
}

func TestOrgmodeZshIgnoreRegexes(t *testing.T) {
	root := t.TempDir()

	// Entries that should be skipped by orgmode.zsh default patterns.
	mustMkdirAll(t, filepath.Join(root, ".Trash"))
	mustWriteFile(t, filepath.Join(root, ".Trash", "deleted.txt"))
	mustMkdirAll(t, filepath.Join(root, ".Trashes"))
	mustWriteFile(t, filepath.Join(root, ".DS_Store"))
	mustWriteFile(t, filepath.Join(root, "._.DS_Store"))
	mustMkdirAll(t, filepath.Join(root, ".mypy_cache"))
	mustMkdirAll(t, filepath.Join(root, ".git"))
	mustWriteFile(t, filepath.Join(root, ".git", "HEAD"))
	mustMkdirAll(t, filepath.Join(root, ".Spotlight-V100"))
	mustMkdirAll(t, filepath.Join(root, ".trash-1000"))
	mustMkdirAll(t, filepath.Join(root, "System Volume Information"))
	mustMkdirAll(t, filepath.Join(root, "$RECYCLE.BIN"))
	mustMkdirAll(t, filepath.Join(root, "_gsdata_"))
	mustMkdirAll(t, filepath.Join(root, "found.000"))
	mustMkdirAll(t, filepath.Join(root, "MyApp.app", "Contents"))
	mustWriteFile(t, filepath.Join(root, "MyApp.app", "Contents", "Info.plist"))

	// Entries that should remain visible.
	mustWriteFile(t, filepath.Join(root, ".gitignore"))
	mustWriteFile(t, filepath.Join(root, "keep.txt"))

	// Exact regexes from orgmode.zsh pathtree2org.
	skips := []string{
		`(?i)/\.((trash(es)?|(_\.)?DS_STORE|mypy_cache)/?$|git|Spotlight-|trash-)`,
		`/System Volume Information/`,
		`/(\$RECYCLE.BIN|_gsdata_|found\.\d+)/`,
		`\.(sparsebundle|photoslibrary|app)/.+`,
	}

	var stdout, stderr bytes.Buffer
	args := append([]string{root}, skips...)
	if code := run(args, &stdout, &stderr); code != 0 {
		t.Fatalf("run failed with %d: %s", code, stderr.String())
	}

	got := stdout.String()
	// .gitignore matches `git` without `$` in orgmode.zsh's regex, so it is also skipped.
	for _, hidden := range []string{
		".Trash", ".Trashes", ".DS_Store", "._.DS_Store", ".mypy_cache",
		".git", ".Spotlight-V100", ".trash-1000", "System Volume Information",
		"$RECYCLE.BIN", "_gsdata_", "found.000", "Contents", "Info.plist", ".gitignore",
	} {
		if strings.Contains(got, hidden) {
			t.Errorf("expected %q to be skipped in stdout:\n%s", hidden, got)
		}
	}

	if !strings.Contains(got, "* [["+root+"/MyApp.app/][MyApp.app]]\n") {
		t.Errorf("expected MyApp.app heading itself to be kept:\n%s", got)
	}
	if !strings.Contains(got, "* [["+root+"/keep.txt][keep.txt]]\n") {
		t.Errorf("expected keep.txt heading:\n%s", got)
	}
	if !strings.Contains(stderr.String(), "INFO: Skipped "+root+"/.DS_Store\n\n") {
		t.Errorf("expected Skipped log on stderr, got:\n%s", stderr.String())
	}
}

func TestSkippedLastItemBlankLineParity(t *testing.T) {
	// In pathtree2org.lisp, `fs-len` is computed before skipping, so when a
	// non-skipped item precedes a skipped last item, a blank line is still
	// printed after the non-skipped item.
	root := t.TempDir()
	mustWriteFile(t, filepath.Join(root, "a.txt"))
	mustWriteFile(t, filepath.Join(root, "z_skip.txt"))

	var stdout, stderr bytes.Buffer
	if code := run([]string{root, `z_skip\.txt$`}, &stdout, &stderr); code != 0 {
		t.Fatalf("run returned %d", code)
	}

	want := "+TITLE: Pathtree of =" + root + "=\n\n* [[" + root + "/a.txt][a.txt]]\n\n"
	if stdout.String() != want {
		t.Fatalf("got %q, want %q", stdout.String(), want)
	}
}

func mustMkdirAll(t *testing.T, p string) {
	t.Helper()
	if err := os.MkdirAll(p, 0o755); err != nil {
		t.Fatalf("mkdir %s: %v", p, err)
	}
}

func mustWriteFile(t *testing.T, p string) {
	t.Helper()
	if err := os.WriteFile(p, []byte("x"), 0o644); err != nil {
		t.Fatalf("write %s: %v", p, err)
	}
}
