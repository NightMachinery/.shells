package cli

import (
	"io"
	"org-paper-metadata/paper"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func capture(t *testing.T, input string, args ...string) (int, string, string) {
	t.Helper()
	return captureCommand(t, input, mainCode, args...)
}
func captureCommand(t *testing.T, input string, run func([]string) int, args ...string) (int, string, string) {
	t.Helper()
	file, err := os.CreateTemp(t.TempDir(), "stdin")
	if err != nil {
		t.Fatal(err)
	}
	file.WriteString(input)
	file.Seek(0, 0)
	defer file.Close()
	stdout, err := os.CreateTemp(t.TempDir(), "stdout")
	if err != nil {
		t.Fatal(err)
	}
	defer stdout.Close()
	stderr, err := os.CreateTemp(t.TempDir(), "stderr")
	if err != nil {
		t.Fatal(err)
	}
	defer stderr.Close()
	oldIn, oldOut, oldErr := os.Stdin, os.Stdout, os.Stderr
	os.Stdin, os.Stdout, os.Stderr = file, stdout, stderr
	defer func() { os.Stdin, os.Stdout, os.Stderr = oldIn, oldOut, oldErr }()
	code := run(args)
	stdout.Seek(0, 0)
	stderr.Seek(0, 0)
	out, _ := io.ReadAll(stdout)
	diagnostics, _ := io.ReadAll(stderr)
	return code, string(out), string(diagnostics)
}
func TestFromJSONUsesNativeRenderer(t *testing.T) {
	code, out, err := capture(t, `{"title":["Synthetic paper"],"abstract":["Full abstract."],"authors_names":["Example Author"],"tldr":["Short summary."],"url":["https://www.semanticscholar.org/paper/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"]}`, "--from-json", "https://www.semanticscholar.org/paper/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
	if code != 0 || !strings.Contains(out, "Short summary.") || !strings.Contains(out, "Full abstract.") {
		t.Fatal(code, out, err)
	}
}
func TestIncompleteJSONHasNoStdout(t *testing.T) {
	code, out, err := capture(t, `{"title":"Only title"}`, "--from-json", "https://www.semanticscholar.org/paper/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
	if code == 0 || out != "" || !strings.Contains(err, "missing authors_names, abstract") {
		t.Fatal(code, out, err)
	}
}
func TestInvalidOptionsBeforeNetwork(t *testing.T) {
	for _, args := range [][]string{{"--scraper", "typo", "https://example.org"}, {"--format", "typo", "https://example.org"}, {"--timeout", "0s", "https://example.org"}, {"--bibtex-source", "typo", "https://example.org"}} {
		code, out, _ := capture(t, "", args...)
		if code == 0 || out != "" {
			t.Fatal(args, code, out)
		}
	}
}

func TestBibTeXFlagsAndLastSwitchWins(t *testing.T) {
	input := `{"bibtex":["@inproceedings{source_key, title={Synthetic paper}}"],"title":["Synthetic paper"],"abstract":["Full abstract."],"authors_names":["Example Author"],"url":["https://www.semanticscholar.org/paper/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"]}`
	for _, tc := range []struct {
		args    []string
		enabled bool
	}{
		{nil, true}, {[]string{"--bibtex"}, true}, {[]string{"--no-bibtex"}, false},
		{[]string{"--bibtex=false"}, false}, {[]string{"--no-bibtex", "--bibtex"}, true},
		{[]string{"--bibtex", "--no-bibtex"}, false},
	} {
		args := append(append([]string{}, tc.args...), "--from-json", "https://www.semanticscholar.org/paper/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
		code, out, err := capture(t, input, args...)
		if code != 0 || strings.Contains(out, ":orgPaperMetadataGenerated: bibtex") != tc.enabled {
			t.Fatal(tc, code, out, err)
		}
	}
}
func TestUpdaterNoBibTeXCleansRecentEntry(t *testing.T) {
	root := t.TempDir()
	path := filepath.Join(root, "papers.org")
	m := paper.Metadata{"title": {"Synthetic paper"}, "authors_names": {"Example Author"}, "abstract": {"Full abstract."}, "url": {"https://www.semanticscholar.org/paper/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}}
	m.Put("bibtex", "@inproceedings{source_key, title={Synthetic paper}}")
	m.Stamp("synthetic", time.Now())
	org, _ := paper.RenderOrg(m)
	text := "* " + strings.ReplaceAll(org, "\n* @", "\n** @")
	os.WriteFile(path, []byte(text), 0600)
	code, out, err := captureCommand(t, "", updaterCode, "--write", "--no-bibtex", root)
	after, _ := os.ReadFile(path)
	if code != 0 || strings.Contains(string(after), "@bibtex") || !strings.Contains(err, "recent=1") {
		t.Fatal(code, out, err, string(after))
	}
}
