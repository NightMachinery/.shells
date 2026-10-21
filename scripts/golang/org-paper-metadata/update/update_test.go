package update

import (
	"bytes"
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"org-paper-metadata/paper"
)

const id = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
const page = "https://www.semanticscholar.org/paper/" + id

var now = time.Date(2026, 10, 20, 12, 0, 0, 0, time.UTC)

func fresh() paper.Metadata {
	return paper.Metadata{"title": {"New title"}, "url": {page}, "paperId": {id}, "authors_names": {"Example Author", "Other Author"}, "abstract": {"The new complete abstract."}, "tldr": {"The new summary."}, "citationCount": {"12"}, "year": {"2026"}, "publicationDate": {"2026-10-15"}, "venue": {"International Conference on Learning Representations"}, "metadataFetchedAt": {now.Format(time.RFC3339)}}
}
func transform(t *testing.T, text string) string {
	t.Helper()
	d, err := Parse(text)
	if err != nil {
		t.Fatal(err)
	}
	targets, err := d.Targets()
	if err != nil {
		t.Fatal(err)
	}
	var edits []Edit
	for _, target := range targets {
		edits = append(edits, d.Plan(target, fresh())...)
	}
	out, err := d.Apply(edits)
	if err != nil {
		t.Fatal(err)
	}
	return out
}

func TestPreserveNotesTagsAndProperties(t *testing.T) {
	text := "* TODO [#A] @great @citations/3 @2025/May @ICLR [[" + page + "][Old title]] :research:\nSCHEDULED: <2026-10-22 Thu>\n:PROPERTIES:\n:ID: synthetic-id\n:custom: Keep this\n:metadataSource: semantic-scholar-page-data\n:venue: International Conference on Learning Representations\n:year: 2025\n:publicationDate: 2025-05-01\n:citationCount: 3\n:END:\nMy notes must stay.\n** @abstract\n:PROPERTIES:\n:visibility: folded\n:ID: abstract-id\n:END:\n#+BEGIN_QUOTE\nOld abstract.\n#+END_QUOTE\n\n** My analysis\nKeep this child.\n* Next heading\nUntouched.\n"
	out := transform(t, text)
	for _, want := range []string{"* TODO [#A] @great @citations/12 @2026/October @ICLR [[" + page + "][New title]] :research:", "SCHEDULED: <2026-10-22 Thu>", ":ID: synthetic-id", ":custom: Keep this", ":ID: abstract-id", "My notes must stay.", "** My analysis\nKeep this child.\n* Next heading\nUntouched.", "The new summary.", "The new complete abstract."} {
		if !strings.Contains(out, want) {
			t.Errorf("missing %q\n%s", want, out)
		}
	}
	if strings.Contains(out, "Old abstract") || strings.Contains(out, ":citationCount: 3") || strings.Contains(out, ":metadataSource:") {
		t.Fatal(out)
	}
	if again := transform(t, out); again != out {
		t.Fatalf("second refresh changed structure:\n%s", again)
	}
}
func TestNamedBlocksPreserveSurroundingText(t *testing.T) {
	m := fresh()
	rendered, _ := paper.RenderOrg(m)
	text := "* " + strings.ReplaceAll(rendered, "\n* @abstract", "\n** @abstract")
	text = strings.Replace(text, "# org-paper-metadata: summary", "A personal note before the blocks.\n# org-paper-metadata: summary", 1)
	text += "Personal note after the blocks.\n*** Nested personal heading\nKeep nested content.\n"
	out := transform(t, text)
	for _, want := range []string{"A personal note before the blocks.", "Personal note after the blocks.", "*** Nested personal heading\nKeep nested content."} {
		if !strings.Contains(out, want) {
			t.Fatal(out)
		}
	}
	if strings.Count(out, "# org-paper-metadata: summary") != 1 || strings.Count(out, "# org-paper-metadata: abstract") != 1 {
		t.Fatal(out)
	}
}
func TestLooseLinksAndProtectedHeadingsIgnored(t *testing.T) {
	text := "Loose [[" + page + "][paper]]\n- " + page + "\n#+BEGIN_SRC org\n* [[" + page + "][example]]\n#+END_SRC\n* COMMENT Archived idea\n** [[" + page + "][ignored]]\n* Saved :ARCHIVE:\n** [[" + page + "][ignored]]\n"
	d, _ := Parse(text)
	targets, err := d.Targets()
	if err != nil || len(targets) != 0 {
		t.Fatal(targets, err)
	}
	if out := transform(t, text); out != text {
		t.Fatal(out)
	}
}
func TestNewDrawersAndNestedPaperHeadings(t *testing.T) {
	text := "* [[" + page + "][Parent]]\n** [[" + page + "][Child]]"
	out := transform(t, text)
	if strings.Count(out, ":metadataFetchedAt:") != 2 || strings.Count(out, "org-paper-metadata: abstract") != 2 {
		t.Fatal(out)
	}
	if strings.Index(out, "*** @abstract") > strings.LastIndex(out, "** @abstract") {
		t.Fatal("nested abstract ordering", out)
	}
	if strings.HasSuffix(out, "\n") {
		t.Fatal("changed final-newline convention")
	}
	if again := transform(t, out); again != out {
		t.Fatal("nested refresh was not idempotent", again)
	}
}
func TestCRLF(t *testing.T) {
	text := "* [[" + page + "][Old]]\r\nText.\r\n"
	out := transform(t, text)
	if strings.Contains(strings.ReplaceAll(out, "\r\n", ""), "\n") {
		t.Fatal("lost CRLF")
	}
	if !strings.Contains(out, "Text.\r\n") {
		t.Fatal(out)
	}
}
func TestManualAbstractNotReplaced(t *testing.T) {
	text := "* [[" + page + "][Old]]\n** @abstract\nHandwritten comparison.\n#+BEGIN_QUOTE\nManually selected quotation.\n#+END_QUOTE\n"
	out := transform(t, text)
	if !strings.Contains(out, "Handwritten comparison.\n#+BEGIN_QUOTE\nManually selected quotation.") {
		t.Fatal(out)
	}
	if strings.Contains(out, "The new complete abstract.") {
		t.Fatal("overwrote an ambiguous manual abstract")
	}
}

func TestHeadingSpacingAndArxivURLPreserved(t *testing.T) {
	text := "* TODO  [#A] @personal   Research idea [[https://arxiv.org/abs/2505.00001][Old title]] :reading:\n"
	out := transform(t, text)
	if !strings.Contains(out, "* TODO  [#A] @personal   Research idea @citations/12") || !strings.Contains(out, "[[https://arxiv.org/abs/2505.00001][New title]] :reading:") {
		t.Fatal(out)
	}
}
func TestRecentBoundaryAndBadTimestamps(t *testing.T) {
	for _, tc := range []struct {
		value string
		want  bool
	}{{now.Add(-7*24*time.Hour + time.Second).Format(time.RFC3339), true}, {now.Add(-7 * 24 * time.Hour).Format(time.RFC3339), false}, {now.Add(24 * time.Hour).Format(time.RFC3339), false}, {"", false}, {"bad", false}} {
		if got := Recent(paper.Metadata{"metadataFetchedAt": {tc.value}}, now, 7*24*time.Hour); got != tc.want {
			t.Errorf("%q: %v", tc.value, got)
		}
	}
}
func TestMalformedAndAmbiguousHeadings(t *testing.T) {
	d, _ := Parse("* [[" + page + "][Old]]\n:PROPERTIES:\n:ID: missing-end\n")
	if _, err := d.Targets(); err == nil {
		t.Fatal("accepted unclosed drawer")
	}
	d, _ = Parse("* [[" + page + "][one]] [[https://arxiv.org/abs/2505.00002][two]]\n")
	if targets, err := d.Targets(); err != nil || len(targets) != 0 {
		t.Fatal(targets, err)
	}
	if _, err := Parse("* Heading\r\nMixed\n"); err == nil {
		t.Fatal("accepted mixed newlines")
	}
}
func TestRunWeeklySkipCacheDryRunAndFailure(t *testing.T) {
	root := t.TempDir()
	path := filepath.Join(root, "papers.org")
	text := "* [[" + page + "][Stale]]\n* [[" + page + "][Duplicate]]\n* [[https://arxiv.org/abs/2505.00002][Recent]]\n:PROPERTIES:\n:metadataFetchedAt: " + now.Add(-24*time.Hour).Format(time.RFC3339) + "\n:END:\n* [[https://arxiv.org/abs/2505.00003][Unavailable]]\nKeep failure notes.\n"
	os.WriteFile(path, []byte(text), 0600)
	var calls int
	fetch := func(_ context.Context, url string) (paper.Metadata, error) {
		calls++
		if strings.Contains(url, "00003") {
			return nil, errors.New("unavailable")
		}
		return fresh(), nil
	}
	o := Options{MaxAge: 7 * 24 * time.Hour, Timeout: time.Minute}
	var out, log bytes.Buffer
	stats, err := Run(context.Background(), root, o, fetch, now, &out, &log)
	if err == nil || calls != 2 || stats.Recent != 1 || stats.Updated != 2 || stats.Failed != 1 {
		t.Fatal(stats, calls, err)
	}
	data, _ := os.ReadFile(path)
	if string(data) != text {
		t.Fatal("dry run changed file")
	}
	o.Write = true
	out.Reset()
	log.Reset()
	calls = 0
	stats, err = Run(context.Background(), root, o, fetch, now, &out, &log)
	if err == nil || stats.Updated != 2 {
		t.Fatal(stats, err)
	}
	data, _ = os.ReadFile(path)
	if !strings.Contains(string(data), "[Unavailable]]\nKeep failure notes.") {
		t.Fatal("failed entry changed")
	}
	info, _ := os.Stat(path)
	if info.Mode().Perm() != 0600 {
		t.Fatal("permissions changed")
	}
	calls = 0
	stats, _ = Run(context.Background(), root, o, fetch, now, &out, &log)
	if stats.Recent != 3 || calls != 1 {
		t.Fatal(stats, calls)
	}
}
func TestLockedFilesAndSymlinksSkipped(t *testing.T) {
	root := t.TempDir()
	path := filepath.Join(root, "locked.org")
	os.WriteFile(path, []byte("* "+page+"\n"), 0600)
	os.Symlink("synthetic-owner", filepath.Join(root, ".#locked.org"))
	os.Symlink(path, filepath.Join(root, "linked.org"))
	stats, err := Run(context.Background(), root, Options{Write: true, MaxAge: time.Hour, Timeout: time.Minute}, func(context.Context, string) (paper.Metadata, error) { t.Fatal("unexpected fetch"); return nil, nil }, now, ioDiscard{}, ioDiscard{})
	if err != nil || stats.Locked != 1 || stats.Updated != 0 {
		t.Fatal(stats, err)
	}
}

type ioDiscard struct{}

func (ioDiscard) Write(p []byte) (int, error) { return len(p), nil }
func TestSafeWriteRefusesConcurrentChangesAndLinks(t *testing.T) {
	root := t.TempDir()
	path := filepath.Join(root, "paper.org")
	before := []byte("original")
	os.WriteFile(path, []byte("concurrent edit"), 0600)
	if err := WriteFile(path, before, []byte("new"), 0600); err == nil {
		t.Fatal("overwrote concurrent edit")
	}
	os.WriteFile(path, before, 0600)
	linked := filepath.Join(root, "hard.org")
	os.Link(path, linked)
	if err := WriteFile(path, before, []byte("new"), 0600); err == nil {
		t.Fatal("replaced hard link")
	}
	os.Remove(linked)
	symlink := filepath.Join(root, "symlink.org")
	os.Symlink(path, symlink)
	if err := WriteFile(symlink, before, []byte("new"), 0600); err == nil {
		t.Fatal("replaced symlink")
	}
	if err := WriteFile(path, before, []byte("new"), 0600); err != nil {
		t.Fatal(err)
	}
	data, _ := os.ReadFile(path)
	if string(data) != "new" {
		t.Fatal(string(data))
	}
}
func TestOverlappingEditsRefused(t *testing.T) {
	d, _ := Parse("a\nb\nc\n")
	if _, err := d.Apply([]Edit{{Start: 0, End: 2}, {Start: 1, End: 3}}); err == nil {
		t.Fatal("accepted overlap")
	}
}

func TestExplicitSymlinkRootAndForce(t *testing.T) {
	realRoot := t.TempDir()
	link := filepath.Join(t.TempDir(), "notes")
	os.Symlink(realRoot, link)
	os.WriteFile(filepath.Join(realRoot, "paper.org"), []byte("* [["+page+"][Fresh]]\n:PROPERTIES:\n:metadataFetchedAt: "+now.Format(time.RFC3339)+"\n:END:\n"), 0600)
	calls := 0
	stats, err := Run(context.Background(), link, Options{Force: true, MaxAge: 7 * 24 * time.Hour, Timeout: time.Minute}, func(context.Context, string) (paper.Metadata, error) { calls++; return fresh(), nil }, now, ioDiscard{}, ioDiscard{})
	if err != nil || stats.Updated != 1 || calls != 1 {
		t.Fatal(stats, calls, err)
	}
}

func TestUnrenderedMetadataKeysDoNotEraseCustomProperties(t *testing.T) {
	custom := ":url: https://example.invalid/reading-copy\n:title: My working title\n:abstract: My own abstract notes\n:tldr: My takeaway\n:authors: Personal author annotation\n:date: [2025-01-01 Wed]\n"
	text := "* [[" + page + "][Old title]]\n:PROPERTIES:\n" + custom + ":END:\n"
	out := transform(t, text)
	if !strings.Contains(out, custom) {
		t.Fatal("erased unrendered custom properties", out)
	}
}

func TestLegacyAbstractWithBibTeXPreserved(t *testing.T) {
	citation := "\n\n#+BEGIN_EXAMPLE bibtex\n@article{synthetic,\n  title={Saved citation},\n  year={2025}\n}\n#+END_EXAMPLE\n\n"
	text := "* [[" + page + "][Old]]\n** @abstract\n:PROPERTIES:\n:ID: saved-abstract\n:END:\n\n#+BEGIN_QUOTE\nOld abstract.\n#+END_QUOTE" + citation + "** My notes\nKeep these notes.\n"
	out := transform(t, text)
	for _, want := range []string{strings.TrimSpace(citation), ":ID: saved-abstract", "The new complete abstract.", "The new summary.", "** My notes\nKeep these notes.\n"} {
		if !strings.Contains(out, want) {
			t.Errorf("lost %q\n%s", want, out)
		}
	}
	if strings.Contains(out, "Old abstract.") {
		t.Fatal(out)
	}
	if again := transform(t, out); again != out {
		t.Fatal("not idempotent", again)
	}
}

func TestLegacyQuoteWithPersonalTailIsPreserved(t *testing.T) {
	for _, tail := range []string{"Personal commentary.\n", "#+BEGIN_EXAMPLE\nPersonal example.\n#+END_EXAMPLE\n", "#+BEGIN_EXAMPLE\n@article{saved}\n#+END_EXAMPLE\nPersonal commentary.\n"} {
		body := "** @abstract\n#+BEGIN_QUOTE\nMy quotation.\n#+END_QUOTE\n" + tail
		out := transform(t, "* [["+page+"][Old]]\n"+body)
		if !strings.Contains(out, body) || strings.Contains(out, "The new complete abstract.") {
			t.Fatal("overwrote ambiguous section", out)
		}
	}
}

func TestEmptyLegacyAbstractAndCitationOnlyGetContent(t *testing.T) {
	for _, citation := range []string{"", "#+BEGIN_EXAMPLE tex\n@article{synthetic, title={Saved citation}}\n#+END_EXAMPLE\n"} {
		text := "* [[" + page + "][Old]]\n** @abstract\n:PROPERTIES:\n:ID: empty-abstract\n:END:\n\n" + citation
		out := transform(t, text)
		for _, want := range []string{citation, ":ID: empty-abstract", "The new complete abstract.", "The new summary."} {
			if !strings.Contains(out, want) {
				t.Errorf("lost %q\n%s", want, out)
			}
		}
		if again := transform(t, out); again != out {
			t.Fatal("not idempotent", again)
		}
	}
}

func TestVenueTagsUseRetainedFieldsWithoutDuplicates(t *testing.T) {
	text := "* @ICLR @arXiv [[" + page + "][Old]]\n:PROPERTIES:\n:venue: arXiv.org\n:END:\n"
	out := transform(t, text)
	if strings.Count(strings.Split(out, "\n")[0], "@ICLR") != 1 || strings.Contains(strings.Split(out, "\n")[0], "@arXiv") {
		t.Fatal(out)
	}
	d, _ := Parse(text)
	targets, _ := d.Targets()
	m := fresh()
	delete(m, "venue")
	out, err := d.Apply(d.Plan(targets[0], m))
	if err != nil || !strings.Contains(strings.Split(out, "\n")[0], "@arXiv") || !strings.Contains(out, ":venue: arXiv.org") {
		t.Fatal(out, err)
	}
}

func TestPersonalSummarySiblingDoesNotBlockAbstractRefresh(t *testing.T) {
	personal := "** @summary\nMy own takeaway stays here.\n"
	text := "* [[" + page + "][Old]]\n** @abstract\n#+BEGIN_QUOTE\nOld abstract.\n#+END_QUOTE\n" + personal
	out := transform(t, text)
	if !strings.Contains(out, personal) || !strings.Contains(out, "The new complete abstract.") || strings.Contains(out, "Old abstract.") {
		t.Fatal(out)
	}
}
