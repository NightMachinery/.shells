package main

import (
	"bytes"
	"strings"
	"testing"
)

const goodreadsSampleJSON = `{
  "description": [
    "*Strategies for building large systems that can be easily adapted for new situations with only minor programming modifications.*\\\\\n\\\\\nTime pressures encourage programmers to write code that works well for a narrow purpose, with no room to grow. But the best systems are evolvable; they can be adapted for new situations by adding code, rather than changing the existing code. The authors describe techniques they have found effective--over their combined 100-plus years of programming experience--that will help programmers avoid programming themselves into corners.\\\\\n\\\\\nThe authors explore ways to enhance flexibility by:\\\\\n*-* Organizing systems using combinators to compose mix-and-match parts, ranging from small functions to whole arithmetics, with standardized interfaces\\\\\n*-* Augmenting data with independent annotation layers, such as units of measurement or provenance\\\\\n*-* Combining independent pieces of partial information using unification or propagation\\\\\n*-* Separating control structure from problem domain with domain models, rule systems and pattern matching, propagation, and dependency-directed backtracking\\\\\n*-* Extending the programming language, using dynamically extensible evaluators"
  ],
  "bookFormat": [
    "Hardcover"
  ],
  "pageCount": [
    "448 pages"
  ],
  "publicationDetails": [
    "Published March 9th 2021 by MIT Press"
  ],
  "title": [
    "Software Design for Flexibility: How to Avoid Programming Yourself Into a Corner"
  ],
  "authors": [
    "<<bookAuthors>>\nby\n\n[[https://www.goodreads.com/author/show/8091654.Chris_Hanson][Chris Hanson]],\n\n[[https://www.goodreads.com/author/show/24589.Gerald_Jay_Sussman][Gerald Jay Sussman]]"
  ],
  "rating": [
    "3.06"
  ],
  "ratingCount": [
    "16"
  ],
  "reviewCount": [
    "4"
  ],
  "isbn": [
    "9780262045490"
  ],
  "language": [],
  "series": [],
  "genres": [
    "[[https://www.goodreads.com/genres/computer-science][Computer Science]]",
    "[[https://www.goodreads.com/genres/programming][Programming]]",
    "[[https://www.goodreads.com/genres/computer-science][Computer Science]]",
    "[[https://www.goodreads.com/genres/software][Software]]",
    "[[https://www.goodreads.com/genres/science][Science]]",
    "[[https://www.goodreads.com/genres/computer-science][Computer Science]]",
    "[[https://www.goodreads.com/genres/computer-science][Computer Science]]",
    "[[https://www.goodreads.com/genres/technical][Technical]]",
    "[[https://www.goodreads.com/genres/science][Science]]",
    "[[https://www.goodreads.com/genres/technology][Technology]]",
    "[[https://www.goodreads.com/genres/non-fiction][Nonfiction]]",
    "[[https://www.goodreads.com/genres/computer-science][Computer Science]]",
    "[[https://www.goodreads.com/genres/computers][Computers]]"
  ]
}`

func TestGoodreadsSample(t *testing.T) {
	url := "https://www.goodreads.com/book/show/57372053-software-design-for-flexibility"
	var out bytes.Buffer
	if err := run([]string{url}, strings.NewReader(goodreadsSampleJSON), &out); err != nil {
		t.Fatalf("run failed: %v", err)
	}

	want := strings.Join([]string{
		"[[https://www.goodreads.com/book/show/57372053-software-design-for-flexibility][Software Design for Flexibility: How to Avoid Programming Yourself Into a Corner]]",
		":PROPERTIES:",
		":authors: [[https://www.goodreads.com/author/show/8091654.Chris_Hanson][Chris Hanson]], [[https://www.goodreads.com/author/show/24589.Gerald_Jay_Sussman][Gerald Jay Sussman]]",
		":rating: 3.06",
		":ratingCount: 16",
		":reviewCount: 4",
		":pageCount: 448 pages",
		":publicationDetails: Published March 9th 2021 by MIT Press",
		":genres: [[https://www.goodreads.com/genres/computer-science][Computer Science]]",
		":genres+: [[https://www.goodreads.com/genres/programming][Programming]]",
		":genres+: [[https://www.goodreads.com/genres/computer-science][Computer Science]]",
		":genres+: [[https://www.goodreads.com/genres/software][Software]]",
		":genres+: [[https://www.goodreads.com/genres/science][Science]]",
		":genres+: [[https://www.goodreads.com/genres/computer-science][Computer Science]]",
		":genres+: [[https://www.goodreads.com/genres/computer-science][Computer Science]]",
		":genres+: [[https://www.goodreads.com/genres/technical][Technical]]",
		":genres+: [[https://www.goodreads.com/genres/science][Science]]",
		":genres+: [[https://www.goodreads.com/genres/technology][Technology]]",
		":genres+: [[https://www.goodreads.com/genres/non-fiction][Nonfiction]]",
		":genres+: [[https://www.goodreads.com/genres/computer-science][Computer Science]]",
		":genres+: [[https://www.goodreads.com/genres/computers][Computers]]",
		":isbn: 9780262045490",
		":visibility: folded",
		":END:",
		"#+BEGIN_QUOTE",
		"*Strategies for building large systems that can be easily adapted for new situations with only minor programming modifications.*",
		"",
		"Time pressures encourage programmers to write code that works well for a narrow purpose, with no room to grow. But the best systems are evolvable; they can be adapted for new situations by adding code, rather than changing the existing code. The authors describe techniques they have found effective--over their combined 100-plus years of programming experience--that will help programmers avoid programming themselves into corners.",
		"",
		"The authors explore ways to enhance flexibility by:",
		"- Organizing systems using combinators to compose mix-and-match parts, ranging from small functions to whole arithmetics, with standardized interfaces",
		"- Augmenting data with independent annotation layers, such as units of measurement or provenance",
		"- Combining independent pieces of partial information using unification or propagation",
		"- Separating control structure from problem domain with domain models, rule systems and pattern matching, propagation, and dependency-directed backtracking",
		"- Extending the programming language, using dynamically extensible evaluators",
		"#+END_QUOTE",
		"",
	}, "\n")

	if out.String() != want {
		t.Fatalf("output mismatch:\ngot:\n%s\nwant:\n%s", out.String(), want)
	}
}

func TestTitleEscapingAndTrimming(t *testing.T) {
	input := `{"title": ["  ** Title [Vol. 1] \\\\[Edition\\\\]  "]}`
	var out bytes.Buffer
	if err := run([]string{"https://example.com/book"}, strings.NewReader(input), &out); err != nil {
		t.Fatalf("run failed: %v", err)
	}
	wantHead := "[[https://example.com/book][Title {Vol. 1} {Edition}]]\n"
	if !strings.HasPrefix(out.String(), wantHead) {
		t.Fatalf("got %q, want prefix %q", out.String(), wantHead)
	}
}

func TestMissingTitleAndNoDescription(t *testing.T) {
	input := `{"title": [], "rating": ["4.2"], "description": []}`
	var out bytes.Buffer
	if err := run([]string{"https://example.com/book"}, strings.NewReader(input), &out); err != nil {
		t.Fatalf("run failed: %v", err)
	}
	want := "[[https://example.com/book]]\n:PROPERTIES:\n:rating: 4.2\n:visibility: folded\n:END:\n"
	if out.String() != want {
		t.Fatalf("got:\n%q\nwant:\n%q", out.String(), want)
	}
}

func TestDescriptionShortFallbackAndBullets(t *testing.T) {
	input := `{
  "title": ["Book"],
  "description": ["  <<anchor>>\n  :PROPERTIES:\n  :CUSTOM_ID: desc\n  :END:\n  "],
  "descriptionShort": ["*-* First\\\\\n*-* Second\n  *-* Indented\nInline *-* keep\u00a0here"]
}`
	var out bytes.Buffer
	if err := run([]string{"https://example.com/book"}, strings.NewReader(input), &out); err != nil {
		t.Fatalf("run failed: %v", err)
	}
	wantQuote := strings.Join([]string{
		"#+BEGIN_QUOTE",
		"- First",
		"- Second",
		"  *-* Indented",
		"Inline *-* keep here",
		"#+END_QUOTE",
		"",
	}, "\n")
	if !strings.HasSuffix(out.String(), wantQuote) {
		t.Fatalf("got:\n%s\nwant suffix:\n%s", out.String(), wantQuote)
	}
}

func TestAuthorsWordBoundaryAndNewlines(t *testing.T) {
	input := `{"authors": [":PROPERTIES:\n:ID: x\n:END:\nby\n\nBobby\u00a0Byron, Ruby\nby\nAlice"]}`
	var out bytes.Buffer
	if err := run([]string{"https://example.com/book"}, strings.NewReader(input), &out); err != nil {
		t.Fatalf("run failed: %v", err)
	}
	wantProp := ":authors: Bobby Byron, Ruby  Alice\n"
	if !strings.Contains(out.String(), wantProp) {
		t.Fatalf("got:\n%s\nwant substring %q", out.String(), wantProp)
	}
}

func TestPropertiesStopAtEmptyAndPreserveOrder(t *testing.T) {
	input := `{
  "isbn": ["123"],
  "genres": ["Sci-Fi", "", "Fantasy"],
  "bookFormat": ["Paperback"],
  "language": ["English"],
  "series": ["Dune"]
}`
	var out bytes.Buffer
	if err := run([]string{"https://example.com/book"}, strings.NewReader(input), &out); err != nil {
		t.Fatalf("run failed: %v", err)
	}
	want := strings.Join([]string{
		"[[https://example.com/book]]",
		":PROPERTIES:",
		":series: Dune",
		":genres: Sci-Fi",
		":isbn: 123",
		":visibility: folded",
		":END:",
		"",
	}, "\n")
	if out.String() != want {
		t.Fatalf("got:\n%s\nwant:\n%s", out.String(), want)
	}
}

func TestNoArgsAndErrors(t *testing.T) {
	var out bytes.Buffer
	if err := run(nil, strings.NewReader(`{"title": ["T"]}`), &out); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !strings.HasPrefix(out.String(), "[[][T]]\n") {
		t.Fatalf("got %q", out.String())
	}

	for _, bad := range []string{"", "   ", "null", "[]", `{"title": "not-an-array"}`} {
		out.Reset()
		if err := run([]string{"u"}, strings.NewReader(bad), &out); err == nil {
			t.Fatalf("expected error for input %q", bad)
		}
	}
}
