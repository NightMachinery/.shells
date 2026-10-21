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

func withBibOptions(t *testing.T, text string, no bool) string {
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
		edits = append(edits, d.PlanWithOptions(target, freshCitation(), paper.OrgOptions{NoBibTeX: no})...)
	}
	out, err := d.Apply(edits)
	if err != nil {
		t.Fatal(err)
	}
	return out
}
func TestGeneratedBibTeXRefreshPlacementAndRemoval(t *testing.T) {
	text := "* [[" + page + "][Old]]\nMy analysis.\n** @abstract\n#+BEGIN_QUOTE\nOld abstract.\n#+END_QUOTE\n** @summary\nMy takeaway.\n"
	out := withBibOptions(t, text, false)
	if strings.Count(out, ":orgPaperMetadataGenerated: bibtex") != 1 || !(strings.Index(out, "** @abstract") < strings.Index(out, "** @bibtex") && strings.Index(out, "** @bibtex") < strings.Index(out, "** @summary")) {
		t.Fatal(out)
	}
	if again := withBibOptions(t, out, false); again != out {
		t.Fatal("not idempotent", again)
	}
	out = strings.Replace(out, "@inproceedings{native_key", "@inproceedings{obsolete_key", 1)
	out = withBibOptions(t, out, false)
	if strings.Contains(out, "obsolete_key") {
		t.Fatal("citation was not refreshed", out)
	}
	out = withBibOptions(t, out, true)
	if strings.Contains(out, "@bibtex") || !strings.Contains(out, "My analysis.") || !strings.Contains(out, "** @summary\nMy takeaway.") {
		t.Fatal(out)
	}
}
func TestLegacyCitationMovesIntactAndStaysUnmarked(t *testing.T) {
	for _, kind := range []string{"EXAMPLE bibtex", "SRC bibtex"} {
		end := strings.Fields(kind)[0]
		citation := "#+BEGIN_" + kind + "\n@article{my_key,\n title={Saved citation}, year={2024}\n}\n#+END_" + end
		text := "* [[" + page + "][Old]]\n** @abstract\n#+BEGIN_QUOTE\nOld abstract.\n#+END_QUOTE\n\n" + citation + "\n** @summary\nMy own summary.\n"
		out := withBibOptions(t, text, false)
		if !strings.Contains(out, "** @bibtex\n:PROPERTIES:\n:visibility: folded\n:END:\n"+citation) || strings.Contains(out, paper.GeneratedProperty) || !strings.Contains(out, "The new complete abstract.") {
			t.Fatal(out)
		}
		if strings.Count(out, "@article{my_key") != 1 {
			t.Fatal(out)
		}
		if again := withBibOptions(t, out, true); !strings.Contains(again, citation) {
			t.Fatal("removed unmarked citation", again)
		}
	}
}
func TestGeneratedBibTeXPersonalAdditionsSurviveCleanup(t *testing.T) {
	text := withBibOptions(t, "* [["+page+"][Old]]\n", false)
	text = strings.Replace(text, ":orgPaperMetadataGenerated: bibtex", ":orgPaperMetadataGenerated: bibtex\n:ID: my-citation-id\n:custom: saved", 1)
	text += "Personal citation note.\n*** My follow-up\nKeep this child.\n"
	out := withBibOptions(t, text, true)
	for _, want := range []string{"** @bibtex", ":ID: my-citation-id", ":custom: saved", "Personal citation note.", "*** My follow-up\nKeep this child."} {
		if !strings.Contains(out, want) {
			t.Errorf("lost %q\n%s", want, out)
		}
	}
	if strings.Contains(out, "BEGIN_SRC bibtex") || strings.Contains(out, paper.GeneratedProperty) {
		t.Fatal(out)
	}
}
func TestNoBibTeXCleansRecentEntryWithoutFetching(t *testing.T) {
	root := t.TempDir()
	path := filepath.Join(root, "papers.org")
	text := withBibOptions(t, "* [["+page+"][Old]]\nMy notes.\n", false)
	os.WriteFile(path, []byte(text), 0600)
	var log bytes.Buffer
	stats, err := Run(context.Background(), root, Options{Write: true, NoBibTeX: true, MaxAge: 7 * 24 * time.Hour, Timeout: time.Minute}, func(context.Context, string) (paper.Metadata, error) {
		t.Fatal("recent cleanup fetched metadata")
		return nil, nil
	}, now, &log, &log)
	data, _ := os.ReadFile(path)
	if err != nil || stats.Recent != 1 || stats.Updated != 1 || strings.Contains(string(data), "@bibtex") || !strings.Contains(string(data), "My notes.") {
		t.Fatal(stats, err, string(data))
	}
}
func TestNoBibTeXCleansOwnedSectionEvenIfRefreshFails(t *testing.T) {
	root := t.TempDir()
	path := filepath.Join(root, "papers.org")
	text := withBibOptions(t, "* [["+page+"][Old]]\nMy notes.\n", false)
	os.WriteFile(path, []byte(text), 0600)
	var log bytes.Buffer
	stats, err := Run(context.Background(), root, Options{Write: true, Force: true, NoBibTeX: true, MaxAge: time.Hour, Timeout: time.Minute}, func(context.Context, string) (paper.Metadata, error) { return nil, errors.New("unavailable") }, now, &log, &log)
	data, _ := os.ReadFile(path)
	if err == nil || stats.Failed != 1 || strings.Contains(string(data), "@bibtex") || !strings.Contains(string(data), ":metadataFetchedAt: "+now.Format(time.RFC3339)) {
		t.Fatal(stats, err, string(data))
	}
}
func TestNestedBibTeXSectionsStayWithTheirPapers(t *testing.T) {
	out := withBibOptions(t, "* [["+page+"][Parent]]\n** [["+page+"][Child]]\n", false)
	if strings.Count(out, ":orgPaperMetadataGenerated: bibtex") != 2 || !(strings.Index(out, "*** @abstract") < strings.Index(out, "*** @bibtex") && strings.Index(out, "*** @bibtex") < strings.LastIndex(out, "\n** @abstract")) {
		t.Fatal(out)
	}
	if again := withBibOptions(t, out, false); again != out {
		t.Fatal(again)
	}
}

func TestLegacySummaryRenamesAndBibTeXFollowsIt(t *testing.T) {
	citation := "#+BEGIN_EXAMPLE bibtex\n@article{my_key, title={Saved citation}}\n#+END_EXAMPLE\n"
	text := "* [[" + page + "][Old]]\n** @summary\n#+BEGIN_QUOTE\nOld abstract.\n#+END_QUOTE\n" + citation + "** My notes\nKeep this note.\n"
	out := withBibOptions(t, text, false)
	if strings.Contains(out, "** @summary") || !(strings.Index(out, "** @abstract") < strings.Index(out, "** @bibtex") && strings.Index(out, "** @bibtex") < strings.Index(out, "** My notes")) || !strings.Contains(out, "Keep this note.") {
		t.Fatal(out)
	}
}
func TestHandwrittenSummaryOnlyGetsSeparateAbstract(t *testing.T) {
	personal := "** @summary\nMy own takeaway.\n"
	out := withBibOptions(t, "* [["+page+"][Old]]\n"+personal, false)
	if !strings.Contains(out, personal) || !strings.Contains(out, "** @abstract") || !strings.Contains(out, "The new complete abstract.") || strings.Index(out, "** @bibtex") < strings.Index(out, "** @abstract") {
		t.Fatal(out)
	}
}

func freshCitation() paper.Metadata {
	m := fresh()
	m.Put("bibtex", "@inproceedings{native_key, title={New title}, booktitle={Example Conference}}")
	return m
}
func TestLegacyGeneratedMiscCleanup(t *testing.T) {
	for _, personal := range []bool{false, true} {
		for _, failed := range []bool{false, true} {
			root := t.TempDir()
			path := filepath.Join(root, "papers.org")
			text := withBibOptions(t, "* [["+page+"][Old]]\nMy paper notes.\n", false)
			text = strings.Replace(text, "@inproceedings{native_key, title={New title}, booktitle={Example Conference}}", "@misc{ss"+id+", title={{Old title}}}", 1)
			if personal {
				text += "Personal citation note.\n*** Follow-up\nKeep this child.\n"
				text = strings.Replace(text, ":orgPaperMetadataGenerated: bibtex", ":orgPaperMetadataGenerated: bibtex\n:ID: saved-id", 1)
			}
			os.WriteFile(path, []byte(text), 0600)
			var log bytes.Buffer
			stats, err := Run(context.Background(), root, Options{Write: true, Force: failed, MaxAge: 7 * 24 * time.Hour, Timeout: time.Minute}, func(context.Context, string) (paper.Metadata, error) {
				if !failed {
					t.Fatal("recent cleanup fetched")
				}
				return nil, errors.New("unavailable")
			}, now, &log, &log)
			data, _ := os.ReadFile(path)
			out := string(data)
			if (err != nil) != failed || stats.Updated != 1 || strings.Contains(out, "@misc{") || strings.Contains(out, paper.GeneratedProperty) || !strings.Contains(out, "My paper notes.") {
				t.Fatal(stats, err, out)
			}
			if !personal && strings.Contains(out, "@bibtex") {
				t.Fatal(out)
			}
			if personal {
				for _, want := range []string{":ID: saved-id", "Personal citation note.", "*** Follow-up\nKeep this child."} {
					if !strings.Contains(out, want) {
						t.Fatal(out)
					}
				}
			}
		}
	}
}
func TestCitationUnavailablePreservesNativeAndManualCitations(t *testing.T) {
	for _, bib := range []string{"@inproceedings{native_key, title={Conference paper}}", "@misc{ss" + id + ", title={Saved manual record}}"} {
		marked := !strings.HasPrefix(bib, "@misc")
		text := "* [[" + page + "][Old]]\n** @bibtex\n:PROPERTIES:\n:visibility: folded\n"
		if marked {
			text += ":orgPaperMetadataGenerated: bibtex\n"
		}
		text += ":END:\n#+BEGIN_SRC bibtex\n" + bib + "\n#+END_SRC\n"
		d, _ := Parse(text)
		ts, _ := d.Targets()
		out, err := d.Apply(d.PlanWithOptions(ts[0], fresh(), paper.OrgOptions{}))
		if err != nil || !strings.Contains(out, bib) {
			t.Fatal(out, err)
		}
	}
}
func TestLegacyMiscReplacedWithRetrievedCitation(t *testing.T) {
	text := withBibOptions(t, "* [["+page+"][Old]]\n", false)
	text = strings.Replace(text, "@inproceedings{native_key, title={New title}, booktitle={Example Conference}}", "@misc{ss"+id+", title={Old}}", 1)
	out := withBibOptions(t, text, false)
	if strings.Contains(out, "@misc{") || !strings.Contains(out, "@inproceedings{native_key") {
		t.Fatal(out)
	}
}
