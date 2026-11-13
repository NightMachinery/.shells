package main

import (
	"bytes"
	"strings"
	"testing"
)

func TestSortIRCLogsDescendingAndFormat(t *testing.T) {
	input := strings.Join([]string{
		"#python/2023-01-05.log:12:[09:05:01] <alice> earlier day",
		"sub/dir/#go/2023-02-01.log:7:[08:00:59] <bob> newer month",
		"#python/2023-01-05.log:99:[09:05:10] <charlie> later second same minute",
		"2024-01-01/chan//2024-01-01.log:3:[00:00:00] <dave> double slash and prefix",
	}, "\n") + "\n"

	var out bytes.Buffer
	if err := sortIRCLogs(strings.NewReader(input), &out, ""); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	want := strings.Join([]string{
		"2024-01-01/chan/2024-01-01.log:3:[00:00] <dave> double slash and prefix",
		"sub/dir/#go/2023-02-01.log:7:[08:00] <bob> newer month",
		"#python/2023-01-05.log:99:[09:05] <charlie> later second same minute",
		"#python/2023-01-05.log:12:[09:05] <alice> earlier day",
	}, "\n") + "\n"

	if out.String() != want {
		t.Fatalf("mismatch:\ngot:\n%s\nwant:\n%s", out.String(), want)
	}
}

func TestSelfUsernamesCaseInsensitiveFiltering(t *testing.T) {
	input := strings.Join([]string{
		"#chan/2023-05-01.log:1:[10:00:00] <greyrat> my message",
		"#chan/2023-05-01.log:2:[10:01:00] <GREYRAT> uppercase self",
		"#chan/2023-05-01.log:3:[10:02:00] <Lucerne> mixed case self",
		"#chan/2023-05-01.log:4:[10:03:00] <other> keep this",
		"#chan/2023-05-01.log:5:[10:04:00] <greyrat2> partial name kept",
	}, "\n") + "\n"

	var out bytes.Buffer
	if err := sortIRCLogs(strings.NewReader(input), &out, "greyrat|lucerne"); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	want := strings.Join([]string{
		"#chan/2023-05-01.log:5:[10:04] <greyrat2> partial name kept",
		"#chan/2023-05-01.log:4:[10:03] <other> keep this",
	}, "\n") + "\n"

	if out.String() != want {
		t.Fatalf("mismatch:\ngot:\n%s\nwant:\n%s", out.String(), want)
	}
}

func TestSkipsNonMatchingLinesAndNormalizesSpaces(t *testing.T) {
	input := strings.Join([]string{
		"",
		"not a log line",
		"#chan/2023-05-01.log:1:[10:00:00] * alice does an action",
		"#chan/2023-05-01.log:2:[10:00:00]   <alice>   spaced   text",
		"#chan/2023-05-01.log:3:[10:00:01] <bob> ",
		"no-trailing-newline/#chan/2023-05-01.log:4:[10:00:02] <carol> last line without newline",
	}, "\n")

	var out bytes.Buffer
	if err := sortIRCLogs(strings.NewReader(input), &out, ""); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	want := strings.Join([]string{
		"no-trailing-newline/#chan/2023-05-01.log:4:[10:00] <carol> last line without newline",
		"#chan/2023-05-01.log:3:[10:00] <bob> ",
		"#chan/2023-05-01.log:2:[10:00] <alice> spaced   text",
	}, "\n") + "\n"

	if out.String() != want {
		t.Fatalf("mismatch:\ngot:\n%s\nwant:\n%s", out.String(), want)
	}
}

func TestLongLineAndEmptyInput(t *testing.T) {
	var emptyOut bytes.Buffer
	if err := sortIRCLogs(strings.NewReader(""), &emptyOut, "greyrat|"); err != nil {
		t.Fatalf("unexpected error on empty input: %v", err)
	}
	if emptyOut.Len() != 0 {
		t.Fatalf("expected empty output, got %q", emptyOut.String())
	}

	longText := strings.Repeat("x", 128*1024)
	input := "#chan/2023-05-01.log:1:[01:02:03] <alice> " + longText + "\n"
	var out bytes.Buffer
	if err := sortIRCLogs(strings.NewReader(input), &out, ""); err != nil {
		t.Fatalf("unexpected error on long line: %v", err)
	}
	want := "#chan/2023-05-01.log:1:[01:02] <alice> " + longText + "\n"
	if out.String() != want {
		t.Fatalf("long line mismatch (got len %d, want len %d)", out.Len(), len(want))
	}
}
