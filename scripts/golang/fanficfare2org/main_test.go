package main

import (
	"bytes"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

func strPtr(s string) *string { return &s }

func TestOrgTitleEscape(t *testing.T) {
	cases := []struct {
		in   string
		want string
	}{
		{"Plain Title", "Plain Title"},
		{"Title [Part 1]", "Title {Part 1}"},
		{`Title \[Escaped\]`, "Title {Escaped}"},
		{`Title \\[Double\\]`, "Title {Double}"},
		{"Nested [[a][b]]", "Nested {{a}{b}}"},
		{"", ""},
	}
	for _, tc := range cases {
		if got := orgTitleEscape(tc.in); got != tc.want {
			t.Errorf("orgTitleEscape(%q) = %q, want %q", tc.in, got, tc.want)
		}
	}
}

func TestOrgLinkCreate(t *testing.T) {
	cases := []struct {
		name  string
		url   string
		title *string
		want  string
	}{
		{
			name:  "url and title",
			url:   "https://archiveofourown.org/works/13625910",
			title: strPtr("Yesterday is Tomorrow [Part 1]"),
			want:  "[[https://archiveofourown.org/works/13625910][Yesterday is Tomorrow {Part 1}]]",
		},
		{
			name:  "nil title omits description",
			url:   "https://example.com/story",
			title: nil,
			want:  "[[https://example.com/story]]",
		},
		{
			name:  "empty string title keeps brackets like CL non-nil",
			url:   "https://example.com/story",
			title: strPtr(""),
			want:  "[[https://example.com/story][]]",
		},
		{
			name:  "empty url with title",
			url:   "",
			title: strPtr("Untitled"),
			want:  "[[][Untitled]]",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := orgLinkCreate(tc.url, tc.title); got != tc.want {
				t.Errorf("got %q, want %q", got, tc.want)
			}
		})
	}
}

func TestOrgPropertiesWrite(t *testing.T) {
	raw := `{
		"author": "Alice",
		"authorUrl": "",
		"series": null,
		"status": false,
		"numWords": 12345,
		"numChapters": 4.5,
		"kudos": true,
		"ships": ["A/B", "C/D", "", "E/F"],
		"fandoms": [],
		"freeformtags": "Time Travel",
		"freefromtags": "Should Not Appear",
		"threadmarks_title": "Should Not Appear",
		"comment_count": "99",
		"storyId": "42"
	}`
	m, err := parseInputJSON(strings.NewReader(raw))
	if err != nil {
		t.Fatal(err)
	}

	var out bytes.Buffer
	if err := orgPropertiesWrite(&out, propertyKeys, func(k string) (any, bool) {
		return jsonGet(m, k)
	}, true); err != nil {
		t.Fatal(err)
	}

	want := strings.Join([]string{
		":PROPERTIES:",
		":author: Alice",
		":numWords: 12345",
		":numChapters: 4.5",
		":kudos: T",
		":ships: A/B",
		":ships+: C/D",
		":freeformtags: Time Travel",
		":storyId: 42",
		":visibility: folded",
		":END:",
	}, "\n") + "\n"

	if got := out.String(); got != want {
		t.Fatalf("properties mismatch:\ngot:\n%s\nwant:\n%s", got, want)
	}
}

func TestOrgPropertiesWriteEmptyObject(t *testing.T) {
	var out bytes.Buffer
	if err := orgPropertiesWrite(&out, propertyKeys, func(string) (any, bool) {
		return nil, false
	}, true); err != nil {
		t.Fatal(err)
	}
	want := ":PROPERTIES:\n:visibility: folded\n:END:\n"
	if got := out.String(); got != want {
		t.Fatalf("got %q, want %q", got, want)
	}
}

func TestOrgPropertiesWriteAllKeysOrder(t *testing.T) {
	m := make(map[string]any, len(propertyKeys))
	for _, k := range propertyKeys {
		m[k] = k + "_val"
	}

	var out bytes.Buffer
	if err := orgPropertiesWrite(&out, propertyKeys, func(k string) (any, bool) {
		return jsonGet(m, k)
	}, true); err != nil {
		t.Fatal(err)
	}

	var want strings.Builder
	want.WriteString(":PROPERTIES:\n")
	for _, k := range propertyKeys {
		want.WriteString(":" + k + ": " + k + "_val\n")
	}
	want.WriteString(":visibility: folded\n:END:\n")

	if got := out.String(); got != want.String() {
		t.Fatalf("key order mismatch:\ngot:\n%s\nwant:\n%s", got, want.String())
	}
}

func TestOrgQuoteWrite(t *testing.T) {
	var out bytes.Buffer
	if err := orgQuoteWrite(&out, nil); err != nil {
		t.Fatal(err)
	}
	if out.Len() != 0 {
		t.Fatalf("nil content should emit nothing, got %q", out.String())
	}

	if err := orgQuoteWrite(&out, strPtr("")); err != nil {
		t.Fatal(err)
	}
	if got, want := out.String(), "#+BEGIN_QUOTE\n\n#+END_QUOTE\n"; got != want {
		t.Fatalf("empty string content = %q, want %q", got, want)
	}

	out.Reset()
	if err := orgQuoteWrite(&out, strPtr("line 1\nline 2")); err != nil {
		t.Fatal(err)
	}
	if got, want := out.String(), "#+BEGIN_QUOTE\nline 1\nline 2\n#+END_QUOTE\n"; got != want {
		t.Fatalf("multiline content = %q, want %q", got, want)
	}
}

func TestPostprocessPandocOrg(t *testing.T) {
	in := strings.Join([]string{
		"<<top>>",
		"Hello\u00a0world^{//}",
		"  :PROPERTIES:",
		"  :CUSTOM_ID: sec1",
		"  :END:",
		"",
		"",
		"1. [@4] First [cite/t:@doe2020] [[data:image/png;base64,AAA]] [[https://example.com][link]]",
		"#+BEGIN_EXAMPLE",
		"x = 1",
		"#+END_EXAMPLE",
		"#+begin_src python :style color:red :exports both",
		"print(1)",
		"#+end_src",
		"#+BEGIN_SRC sh :style margin:0",
		"echo ok",
		"#+END_SRC",
		"   ",
		"\t",
		"End",
	}, "\n") + "\n"

	got := strings.TrimRight(postprocessPandocOrg(in), "\n")
	want := strings.Join([]string{
		"",
		"Hello world",
		"",
		"1. First @doe2020  [[https://example.com][link]]",
		"#+begin_src :eval never",
		"x = 1",
		"#+end_src",
		"#+begin_src python :exports both :eval never",
		"print(1)",
		"#+end_src",
		"#+BEGIN_SRC sh :eval never",
		"echo ok",
		"#+END_SRC",
		"   ",
		"End",
	}, "\n")

	if got != want {
		t.Fatalf("postprocess mismatch:\ngot:\n%q\nwant:\n%q", got, want)
	}
}

func TestRunFullSampleFromLisp(t *testing.T) {
	sample := `{
          "ao3categories":"F/M, Gen",
          "author":"IAM_Kneazle (writing_as_tracey), writing_as_tracey",
          "authorUrl":"https://archiveofourown.org/users/writing_as_tracey/pseuds/IAM_Kneazle, https://archiveofourown.org/users/writing_as_tracey/pseuds/writing_as_tracey",
          "chapterslashtotal":"25/?",
          "characters":"Alastor \"Mad-Eye\" Moody, Albus Dumbledore",
          "cover_image":"",
          "datePublished":"2018-02-09",
          "dateUpdated":"2021-05-01",
          "description":"<div class=\"userstuff\"><p>James Potter went five years at Hogwarts without realizing Lily had a little sister.</p></div>",
          "extratags":"FanFiction",
          "fandoms":"Harry Potter - J. K. Rowling",
          "freeformtags":"Time Travel, Tropes",
          "freefromtags":"Time Travel, Tropes",
          "genre":"F/M, Gen, Time Travel",
          "hits":"169393",
          "kudos":"6655",
          "numChapters":"25",
          "numWords":"229,569",
          "rating":"Teen And Up Audiences",
          "series":"",
          "seriesUrl":"",
          "ships":"Hermione Granger/James Potter, Petunia Evans/OC",
          "status":"In-Progress",
          "storyId":"13625910",
          "storyUrl":"https://archiveofourown.org/works/13625910",
          "title":"Yesterday is Tomorrow (everything is connected)",
          "warnings":"Creator Chose Not To Use Archive Warnings"
        }`

	var out bytes.Buffer
	err := run(strings.NewReader(sample), &out, func(html string) (string, error) {
		if !strings.Contains(html, "James Potter") {
			t.Fatalf("unexpected html passed to html2org: %q", html)
		}
		return "James Potter went five years at Hogwarts without realizing Lily had a little sister.", nil
	})
	if err != nil {
		t.Fatal(err)
	}

	want := strings.Join([]string{
		"[[https://archiveofourown.org/works/13625910][Yesterday is Tomorrow (everything is connected)]]",
		":PROPERTIES:",
		":author: IAM_Kneazle (writing_as_tracey), writing_as_tracey",
		":authorUrl: https://archiveofourown.org/users/writing_as_tracey/pseuds/IAM_Kneazle, https://archiveofourown.org/users/writing_as_tracey/pseuds/writing_as_tracey",
		":status: In-Progress",
		":numWords: 229,569",
		":chapterslashtotal: 25/?",
		":numChapters: 25",
		":datePublished: 2018-02-09",
		":dateUpdated: 2021-05-01",
		":kudos: 6655",
		":hits: 169393",
		":ships: Hermione Granger/James Potter, Petunia Evans/OC",
		":fandoms: Harry Potter - J. K. Rowling",
		":ao3categories: F/M, Gen",
		":freeformtags: Time Travel, Tropes",
		":genre: F/M, Gen, Time Travel",
		":extratags: FanFiction",
		":rating: Teen And Up Audiences",
		`:characters: Alastor "Mad-Eye" Moody, Albus Dumbledore`,
		":storyId: 13625910",
		":visibility: folded",
		":END:",
		"#+BEGIN_QUOTE",
		"James Potter went five years at Hogwarts without realizing Lily had a little sister.",
		"#+END_QUOTE",
	}, "\n") + "\n"

	if got := out.String(); got != want {
		t.Fatalf("output mismatch:\ngot:\n%s\nwant:\n%s", got, want)
	}
}

func TestRunMissingAndEmptyFields(t *testing.T) {
	t.Run("missing description omits quote block", func(t *testing.T) {
		var out bytes.Buffer
		err := run(strings.NewReader(`{"storyUrl":"https://example.com/1"}`), &out, func(string) (string, error) {
			t.Fatal("html2org should not be called when description is missing")
			return "", nil
		})
		if err != nil {
			t.Fatal(err)
		}
		want := "[[https://example.com/1]]\n:PROPERTIES:\n:visibility: folded\n:END:\n"
		if got := out.String(); got != want {
			t.Fatalf("got %q, want %q", got, want)
		}
	})

	t.Run("empty description emits empty quote block", func(t *testing.T) {
		var out bytes.Buffer
		err := run(strings.NewReader(`{"storyUrl":"https://example.com/1","title":"T [1]","description":""}`), &out, pandocHTMLToOrg)
		if err != nil {
			t.Fatal(err)
		}
		want := "[[https://example.com/1][T {1}]]\n:PROPERTIES:\n:visibility: folded\n:END:\n#+BEGIN_QUOTE\n\n#+END_QUOTE\n"
		if got := out.String(); got != want {
			t.Fatalf("got %q, want %q", got, want)
		}
	})
}

func TestRunErrors(t *testing.T) {
	cases := []struct {
		name string
		in   string
		fn   func(string) (string, error)
	}{
		{"empty stdin", "", pandocHTMLToOrg},
		{"whitespace stdin", "   \n\t ", pandocHTMLToOrg},
		{"invalid json", "{bad", pandocHTMLToOrg},
		{"json array", "[1,2]", pandocHTMLToOrg},
		{"json null", "null", pandocHTMLToOrg},
		{"json string", `"hello"`, pandocHTMLToOrg},
		{"html2org error", `{"description":"<p>x</p>"}`, func(string) (string, error) {
			return "", errors.New("boom")
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			var out bytes.Buffer
			if err := run(strings.NewReader(tc.in), &out, tc.fn); err == nil {
				t.Fatalf("expected error, got output %q", out.String())
			}
		})
	}
}

func TestPandocHTMLToOrgIntegration(t *testing.T) {
	if _, err := exec.LookPath("pandoc"); err != nil {
		t.Skip("pandoc not installed")
	}

	html := `<div class="userstuff"><p>Hello <b>world</b> &amp; <u></u><sup>[1]</sup>.</p><p>Line&nbsp;two<br>next.</p></div>`
	got, err := pandocHTMLToOrg(html)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(got, "__") {
		t.Errorf("empty underline should be dropped by filter, got %q", got)
	}
	if strings.Contains(got, "\u00a0") {
		t.Errorf("NBSP should be normalized, got %q", got)
	}
	if strings.Contains(got, `\\`) {
		t.Errorf("hard linebreak should become soft break, got %q", got)
	}
	if !strings.Contains(got, "*world*") {
		t.Errorf("expected bold org markup in %q", got)
	}
	if strings.HasSuffix(got, "\n") {
		t.Errorf("trailing newline should be trimmed, got %q", got)
	}
}

func TestCLISubprocess(t *testing.T) {
	if _, err := exec.LookPath("pandoc"); err != nil {
		t.Skip("pandoc not installed")
	}

	bin := filepath.Join(t.TempDir(), "fanficfare2org")
	build := exec.Command("go", "build", "-o", bin, ".")
	if out, err := build.CombinedOutput(); err != nil {
		t.Fatalf("go build failed: %v\n%s", err, out)
	}

	input := `{
		"title": "Test [Story]",
		"storyUrl": "https://archiveofourown.org/works/1",
		"author": "Bob",
		"kudos": "10",
		"description": "<p>Summary text.</p>"
	}`
	cmd := exec.Command(bin)
	cmd.Stdin = strings.NewReader(input)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("cli failed: %v\nstderr: %s", err, stderr.String())
	}

	want := strings.Join([]string{
		"[[https://archiveofourown.org/works/1][Test {Story}]]",
		":PROPERTIES:",
		":author: Bob",
		":kudos: 10",
		":visibility: folded",
		":END:",
		"#+BEGIN_QUOTE",
		"Summary text.",
		"#+END_QUOTE",
	}, "\n") + "\n"
	if got := stdout.String(); got != want {
		t.Fatalf("cli output mismatch:\ngot:\n%s\nwant:\n%s", got, want)
	}

	// Verify help flag exits 0.
	helpCmd := exec.Command(bin, "--help")
	if out, err := helpCmd.CombinedOutput(); err != nil {
		t.Fatalf("--help failed: %v\n%s", err, out)
	} else if !strings.Contains(string(out), "usage:") {
		t.Errorf("expected usage on --help, got %q", string(out))
	}
}

func TestFindPandocFiltersWithoutEnv(t *testing.T) {
	t.Setenv("NIGHTDIR", "")
	t.Setenv("HOME", t.TempDir())
	filters := findPandocFilters()
	if len(filters) == 0 {
		t.Skip("pandoc_filters not found relative to cwd")
	}
	for _, f := range filters {
		if _, err := os.Stat(f); err != nil {
			t.Errorf("filter path %q does not exist: %v", f, err)
		}
	}
}
