package update

import (
	"bytes"
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"org-paper-metadata/paper"
)

func TestRecentArxivDrawerLinksNormalizeWithoutFetchingOrChangingNotes(t *testing.T) {
	root := t.TempDir()
	path := filepath.Join(root, "papers.org")
	heading := "* [[https://arxiv.org/abs/2505.00661v1][Pinned heading]]"
	notes := "My note specifically cites https://arxiv.org/abs/2505.00661v1.\n** @bibtex\n#+BEGIN_SRC bibtex\n@misc{saved,url={https://arxiv.org/abs/2505.00661v1}}\n#+END_SRC\n"
	stamp := now.Format(time.RFC3339)
	text := heading + "\n:PROPERTIES:\n:ID: keep-id\n:metadataFetchedAt: " + stamp + "\n:links: http://export.arxiv.org/pdf/2505.00661v1.pdf\n:custom: Keep [[https://arxiv.org/abs/2505.00661v2][link]] annotation.\n:END:\n" + notes
	os.WriteFile(path, []byte(text), 0600)
	var out, log bytes.Buffer
	stats, err := Run(context.Background(), root, Options{Write: true, MaxAge: 7 * 24 * time.Hour, Timeout: time.Minute}, func(context.Context, string) (paper.Metadata, error) {
		t.Fatal("URL normalization fetched metadata")
		return nil, nil
	}, now, &out, &log)
	raw, _ := os.ReadFile(path)
	got := string(raw)
	for _, want := range []string{heading, notes, ":ID: keep-id", ":metadataFetchedAt: " + stamp, ":links: https://arxiv.org/pdf/2505.00661", ":custom: Keep [[https://arxiv.org/abs/2505.00661][link]] annotation."} {
		if !strings.Contains(got, want) {
			t.Errorf("lost %q\n%s", want, got)
		}
	}
	if err != nil || stats.Recent != 1 || stats.Updated != 1 || !strings.Contains(out.String(), "normalize") {
		t.Fatal(stats, err, out.String())
	}
}
func TestRefreshNormalizesPreservedOptionalLinks(t *testing.T) {
	text := "* [[" + page + "][Old]]\n:PROPERTIES:\n:openAccessPdf: https://arxiv.org/pdf/2505.00661v1.pdf\n:custom: https://arxiv.org/abs/2505.00661v2\n:END:\n"
	got := transform(t, text)
	if strings.Contains(got, "00661v") || !strings.Contains(got, ":openAccessPdf: https://arxiv.org/pdf/2505.00661") || !strings.Contains(got, ":custom: https://arxiv.org/abs/2505.00661") {
		t.Fatal(got)
	}
}
