package cli

import (
	"io"
	"os"
	"strings"
	"testing"
)

func capture(t *testing.T, input string, args ...string) (int, string, string) {
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
	code := mainCode(args)
	stdout.Seek(0, 0)
	stderr.Seek(0, 0)
	out, _ := io.ReadAll(stdout)
	diagnostics, _ := io.ReadAll(stderr)
	return code, string(out), string(diagnostics)
}
func TestFromJSONUsesNativeRenderer(t *testing.T) {
	code, out, err := capture(t, `{"title":["Synthetic paper"],"abstract":["Full abstract."],"authors_names":["Example Author"],"tldr":["Short summary."],"url":["https://www.semanticscholar.org/paper/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"]}`, "--from-json", "https://www.semanticscholar.org/paper/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
	if code != 0 || !strings.Contains(out, "*Semantic Scholar TLDR:* Short summary.") || !strings.Contains(out, "Full abstract.") {
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
	for _, args := range [][]string{{"--scraper", "typo", "https://example.org"}, {"--format", "typo", "https://example.org"}, {"--timeout", "0s", "https://example.org"}} {
		code, out, _ := capture(t, "", args...)
		if code == 0 || out != "" {
			t.Fatal(args, code, out)
		}
	}
}
