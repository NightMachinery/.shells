package main

import (
	"bytes"
	"strings"
	"testing"
)

func testEnv(m map[string]string) func(string) string {
	return func(k string) string {
		if m == nil {
			return ""
		}
		return m[k]
	}
}

func TestHelpers(t *testing.T) {
	if got := sTruncate(80, strings.Repeat("a", 80)); len(got) != 80 {
		t.Fatalf("len 80 should not truncate, got %d", len(got))
	}
	if got := sTruncate(80, strings.Repeat("a", 81)); got != strings.Repeat("a", 77)+"..." {
		t.Fatalf("len 81 truncate mismatch: %q", got)
	}
	if got := sTruncate(80, strings.Repeat("é", 81)); got != strings.Repeat("é", 77)+"..." {
		t.Fatalf("multibyte truncate mismatch: %q", got)
	}

	if got := orgLinkEscape(`https://x.com/a[1]\[2]`); got != `https://x.com/a\[1\]\[2\]` {
		t.Fatalf("orgLinkEscape got %q", got)
	}
	if got := orgTitleEscape(`Title [1] \[2\] \\[3\\]`); got != `Title {1} {2} {3}` {
		t.Fatalf("orgTitleEscape got %q", got)
	}
}

func TestReplModeSampleParity(t *testing.T) {
	in := `<p><a href="https://bing.com">bing!</a></p><a href="https://google.com">gingin</a>

<a href="https://google.com/123456789101112131415161718192021222324252627282930/123456789101112131415161718192021222324252627282930">loooong</a>
`

	var out bytes.Buffer
	if err := run(testEnv(nil), strings.NewReader(in), &out); err != nil {
		t.Fatalf("run failed: %v", err)
	}

	want := `<p><a href="https://bing.com">[[https://bing.com][bing!]]</a></p><a href="https://google.com">[[https://google.com][gingin]]</a>

<a href="https://google.com/123456789101112131415161718192021222324252627282930/123456789101112131415161718192021222324252627282930">[[https://google.com/123456789101112131415161718192021222324252627282930/123456...][loooong]]</a>
` + "\n"

	if out.String() != want {
		t.Fatalf("got:\n%q\nwant:\n%q", out.String(), want)
	}
}

func TestTruncateDisabled(t *testing.T) {
	longURL := "https://google.com/" + strings.Repeat("1234567890", 10)
	in := `<a href="` + longURL + `">loooong</a>`

	for _, offVal := range []string{"n", "no", "0", "N"} {
		var out bytes.Buffer
		if err := run(testEnv(map[string]string{"html_links_textualize_truncate_p": offVal}), strings.NewReader(in), &out); err != nil {
			t.Fatalf("run(%q) failed: %v", offVal, err)
		}
		want := `<a href="` + longURL + `">[[` + longURL + `][loooong]]</a>` + "\n"
		if out.String() != want {
			t.Fatalf("offVal=%q: got %q, want %q", offVal, out.String(), want)
		}
	}
}

func TestSkipsEmptyMissingAndHashHrefOrEmptyText(t *testing.T) {
	in := `<a href="">empty href</a><a>no href</a><a href="#section">anchor</a><a href="https://example.com"></a><a href="https://img.example"><img src="x.png"/></a>`
	var out bytes.Buffer
	if err := run(testEnv(nil), strings.NewReader(in), &out); err != nil {
		t.Fatalf("run failed: %v", err)
	}
	want := in + "\n"
	if out.String() != want {
		t.Fatalf("got %q, want %q", out.String(), want)
	}
}

func TestNestedTagsAndEscapingInsideAnchor(t *testing.T) {
	in := `<a href="https://example.com/q?a=[1]"><span>hello</span> <b>[world]</b></a>`
	var out bytes.Buffer
	if err := run(testEnv(nil), strings.NewReader(in), &out); err != nil {
		t.Fatalf("run failed: %v", err)
	}
	want := `<a href="https://example.com/q?a=[1]">[[https://example.com/q?a=\[1\]][hello {world}]]</a>` + "\n"
	if out.String() != want {
		t.Fatalf("got %q, want %q", out.String(), want)
	}
}

func TestFullDocumentPreserved(t *testing.T) {
	in := `<!DOCTYPE html><html><head><title>T</title></head><body><a href="https://example.com">Example</a></body></html>`
	var out bytes.Buffer
	if err := run(testEnv(nil), strings.NewReader(in), &out); err != nil {
		t.Fatalf("run failed: %v", err)
	}
	want := `<!DOCTYPE html><html><head><title>T</title></head><body><a href="https://example.com">[[https://example.com][Example]]</a></body></html>` + "\n"
	if out.String() != want {
		t.Fatalf("got %q, want %q", out.String(), want)
	}
}

func TestTruncateBeforeEscape(t *testing.T) {
	// URL is truncated to 80 chars first, then org-link-escape escapes brackets in the truncated string.
	href := "https://example.com/" + strings.Repeat("a", 58) + "[1]"
	in := `<a href="` + href + `">t</a>`
	var out bytes.Buffer
	if err := run(testEnv(nil), strings.NewReader(in), &out); err != nil {
		t.Fatalf("run failed: %v", err)
	}
	// len("https://example.com/") == 20, + 58 'a' == 78; truncated to 77 + "..." => 77 chars ("https://example.com/" + 57 'a') + "..."
	wantLink := "https://example.com/" + strings.Repeat("a", 57) + "..."
	want := `<a href="` + href + `">[[` + wantLink + `][t]]</a>` + "\n"
	if out.String() != want {
		t.Fatalf("got %q, want %q", out.String(), want)
	}

	// Bracket inside first 77 chars gets escaped even after truncation.
	href2 := "https://example.com/[0]/" + strings.Repeat("a", 70)
	in2 := `<a href="` + href2 + `">t</a>`
	out.Reset()
	if err := run(testEnv(nil), strings.NewReader(in2), &out); err != nil {
		t.Fatalf("run failed: %v", err)
	}
	wantLink2 := `https://example.com/\[0\]/` + strings.Repeat("a", 53) + "..."
	want2 := `<a href="` + href2 + `">[[` + wantLink2 + `][t]]</a>` + "\n"
	if out.String() != want2 {
		t.Fatalf("got %q, want %q", out.String(), want2)
	}
}

func TestEmptyInput(t *testing.T) {
	var out bytes.Buffer
	if err := run(testEnv(nil), strings.NewReader(""), &out); err != nil {
		t.Fatalf("run failed: %v", err)
	}
	if out.String() != "\n" {
		t.Fatalf("got %q, want %q", out.String(), "\n")
	}
}
