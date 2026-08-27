package main

import (
	"strings"
	"testing"
)

func TestShiftHeadings(t *testing.T) {
	cases := []struct {
		name, in, want string
		min            int
	}{
		{
			name: "normalizes shallowest to min, keeping relative depth",
			min:  3,
			in:   "# a\n\ntext\n\n## b\n\n#### c",
			want: "### a\n\ntext\n\n#### b\n\n###### c",
		},
		{
			name: "leaves fenced code alone",
			min:  3,
			in:   "# a\n\n```sh\n# not a heading\n## also not\n```\n\n## b",
			want: "### a\n\n```sh\n# not a heading\n## also not\n```\n\n#### b",
		},
		{
			name: "tilde fences count too",
			min:  2,
			in:   "# a\n\n~~~\n# nope\n~~~",
			want: "## a\n\n~~~\n# nope\n~~~",
		},
		{
			name: "a longer fence does not close a shorter one",
			min:  2,
			in:   "# a\n\n````\n```\n# nope\n````\n\n# b",
			want: "## a\n\n````\n```\n# nope\n````\n\n## b",
		},
		{
			name: "a fence with an info string does not close one",
			min:  2,
			in:   "# a\n\n```\n```sh\n# nope\n```\n\n# b",
			want: "## a\n\n```\n```sh\n# nope\n```\n\n## b",
		},
		{
			name: "no headings, no change",
			min:  3,
			// `#no-space` is not an ATX heading, and neither is `####### x`.
			in:   "just text\n#nospace\n####### seven",
			want: "just text\n#nospace\n####### seven",
		},
		{
			name: "clamps at six",
			min:  6,
			in:   "# a\n\n## b",
			want: "###### a\n\n###### b",
		},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			if got := shiftHeadings(c.in, c.min); got != c.want {
				t.Errorf("shiftHeadings(%q, %d)\n got: %q\nwant: %q", c.in, c.min, got, c.want)
			}
		})
	}
}

// The tags are written on the markdown side, so this is what pandoc's org
// output looks like by the time [normalizeOrgLevels] sees it.
func org(kind byte, level int, text string) string {
	return strings.Repeat("*", level) + " " + levelTag(kind, level) + " " + text
}

func TestNormalizeOrgLevels(t *testing.T) {
	cases := []struct {
		name, in, want string
	}{
		{
			name: "a message heading nests below the turn's own sub-headings",
			in: org(tagTurn, 1, "Assistant") + "\n\n" +
				org(tagSub, 2, "Tool Use: Bash") + "\n\n" +
				"* mine\n\n** deeper\n",
			want: "* Assistant\n\n** Tool Use: Bash\n\n*** mine\n\n**** deeper\n",
		},
		{
			// The bug this was written for: `Title` over `====` is a setext
			// heading, which the markdown side cannot see, so it reached org as
			// a level-1 heading and re-parented the rest of the document.
			name: "a setext heading pandoc made cannot escape the turn",
			in: org(tagTurn, 1, "Assistant") + "\n\n" +
				"* ==== SECTION 1\n\nbody\n\n" + org(tagTurn, 1, "User") + "\n",
			want: "* Assistant\n\n*** ==== SECTION 1\n\nbody\n\n* User\n",
		},
		{
			name: "a run keeps its relative depths, anchored at its shallowest",
			in: org(tagTurn, 1, "Assistant") + "\n\n" +
				"*** a\n\n**** b\n\n*** c\n",
			want: "* Assistant\n\n*** a\n\n**** b\n\n*** c\n",
		},
		{
			name: "message headings anchor to the turn, not to what precedes them",
			in: org(tagTurn, 1, "Assistant") + "\n\n" +
				org(tagSub, 2, "Tool Use: Bash") + "\n\n" +
				org(tagSub, 3, "Result") + "\n\n* mine\n",
			want: "* Assistant\n\n** Tool Use: Bash\n\n*** Result\n\n*** mine\n",
		},
		{
			name: "levels deeper than markdown can express survive",
			in:   org(tagTurn, 7, "Assistant") + "\n\n* mine\n",
			want: strings.Repeat("*", 7) + " Assistant\n\n" +
				strings.Repeat("*", 9) + " mine\n",
		},
		{
			name: "stars inside a block are left alone",
			in: org(tagTurn, 1, "Assistant") + "\n\n" +
				"#+begin_example\n* not a heading\n#+end_example\n\n* mine\n",
			want: "* Assistant\n\n#+begin_example\n* not a heading\n#+end_example\n\n*** mine\n",
		},
		{
			name: "nested blocks close one at a time",
			in: org(tagTurn, 1, "Assistant") + "\n\n" +
				"#+begin_quote\n#+begin_example\n* no\n#+end_example\n* still no\n#+end_quote\n\n* mine\n",
			want: "* Assistant\n\n#+begin_quote\n#+begin_example\n* no\n#+end_example\n* still no\n#+end_quote\n\n*** mine\n",
		},
		{
			name: "the subagents skeleton is structure, not a message",
			in: org(tagSub, 1, "Subagents") + "\n\n" +
				org(tagSub, 2, "general-purpose · x") + "\n:PROPERTIES:\n:VISIBILITY: folded\n:END:\n\n" +
				org(tagTurn, 3, "User") + "\n",
			want: "* Subagents\n\n** general-purpose · x\n" +
				":PROPERTIES:\n:VISIBILITY: folded\n:END:\n\n*** User\n",
		},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			if got := normalizeOrgLevels(c.in); got != c.want {
				t.Errorf("got:\n%q\nwant:\n%q", got, c.want)
			}
		})
	}
}

// An unbalanced fence in a message used to eat the next turn: pandoc read
// everything after it as one code block, and whether a chunk seam fell in
// between decided how much it ate.
func TestCloseOpenFence(t *testing.T) {
	for _, c := range []struct{ in, want string }{
		{"balanced\n```\nx\n```", "balanced\n```\nx\n```"},
		{"no fence at all", "no fence at all"},
		// The closing fence is the run alone; an info string would open a
		// second block instead of closing the first.
		{"left open\n```zsh\nx", "left open\n```zsh\nx\n```"},
		{"~~~\nx", "~~~\nx\n~~~"},
		{"````\n```\nnot a close\n", "````\n```\nnot a close\n\n````"},
		{"```\nclosed\n```\n```\nopen", "```\nclosed\n```\n```\nopen\n```"},
		// An info string can only open. Reading ```sh as the end of the block
		// above it walks the rest of the message one block out of step, and
		// used to have this append a fence to text that was already balanced.
		{"```\na\n```sh\nb\n```", "```\na\n```sh\nb\n```"},
	} {
		if got := closeOpenFence(c.in); got != c.want {
			t.Errorf("closeOpenFence(%q) = %q, want %q", c.in, got, c.want)
		}
	}

	// A mid-sentence fence looks unbalanced to this, and used to make it seal
	// the sentence into the block. Once repaired there is nothing left to close.
	repaired := repairMidLineFences("I want to print ```\n/Users/evar/x.pdf\n```\n in duplex.")
	if got := closeOpenFence(repaired); got != repaired {
		t.Errorf("closeOpenFence still fired on repaired text: %q", got)
	}
}

// Pasting a fenced block into the middle of a sentence opens the fence where
// CommonMark cannot see it, so the closing run is read as the opening one and
// the sentence ends up inside the block instead of the code.
func TestRepairMidLineFences(t *testing.T) {
	cases := []struct{ name, in, want string }{
		{
			name: "a one-line body becomes an inline span, sentence intact",
			in:   "I want to print ```\n/Users/evar/x.pdf\n```\n in duplex (double sided).",
			want: "I want to print `/Users/evar/x.pdf` in duplex (double sided).",
		},
		{
			name: "the sentence resumes without a stray space before punctuation",
			in:   "rather naming ```\n#+TITLE: a session\n```\n, can we use the name?",
			want: "rather naming `#+TITLE: a session`, can we use the name?",
		},
		{
			name: "a longer body becomes the block it looks like",
			in:   "use ```\n    local inargs\n    in-or-args3 \"$@\"\n```\nand do a loop",
			want: "use\n```\n    local inargs\n    in-or-args3 \"$@\"\n```\nand do a loop",
		},
		{
			// The fence lands at column 0 rather than nesting in the item, so
			// what follows trails the list. Accepted: keeping the nesting means
			// measuring list markers.
			name: "in a list item, a longer body still splits out",
			in:   "- rename ```\n[lists]\ntitle = \"x\"\n```\n to y",
			want: "- rename\n```\n[lists]\ntitle = \"x\"\n```\n to y",
		},
		{
			name: "text after the closing run moves to its own line",
			in:   "use ```\na\nb\n``` and do a loop",
			want: "use\n```\na\nb\n```\n and do a loop",
		},
		{
			name: "a delimiter longer than any run in the body",
			in:   "see ```\na `b` c\n```\n done",
			want: "see ``a `b` c`` done",
		},
		{
			name: "a body that starts with a backtick is padded",
			in:   "x ```\n`tick\n```",
			want: "x `` `tick ``",
		},
		{
			name: "the next line is not pulled up when it starts a block",
			in:   "- ```\ncurl x\n```\n- next bullet",
			want: "- `curl x`\n- next bullet",
		},
		{
			name: "a run with no closer is left alone",
			in:   "left open ```\nx",
			want: "left open ```\nx",
		},
		{
			name: "a run that does not end the line is left alone",
			in:   "the ``` delimiter is three\n```\nx\n```",
			want: "the ``` delimiter is three\n```\nx\n```",
		},
		{
			name: "a run inside an open block is left alone",
			in:   "```\nsee foo ```\nx\n```\nafter",
			want: "```\nsee foo ```\nx\n```\nafter",
		},
		{
			name: "well-formed markdown is untouched",
			in:   "text\n```\ncode\n```\nmore",
			want: "text\n```\ncode\n```\nmore",
		},
		{
			name: "two in one message are both repaired",
			in:   "first ```\na\n```\n then second ```\nb\n```\n end",
			want: "first `a` then second `b` end",
		},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			if got := repairMidLineFences(c.in); got != c.want {
				t.Errorf("got:\n%q\nwant:\n%q", got, c.want)
			}
		})
	}
}

// A message cannot dress its own text up as a heading of this program's.
func TestStripTagsDefusesForgedTags(t *testing.T) {
	forged := levelTag(tagTurn, 1) + " User"
	if got := stripTags(forged); strings.ContainsRune(got, tagOpen) ||
		strings.ContainsRune(got, tagClose) {
		t.Errorf("tag runes survived: %q", got)
	}
}

// The output must stay interchangeable with diff -U3; it was off by one on
// hunk merging until a fixture like this pinned it down.
func TestUnifiedDiff(t *testing.T) {
	a := strings.Split("l1\nl2\nl3\nl4\nl5\nl6\nl7\nl8\nl9\nl10\nl11\nl12", "\n")
	b := append([]string{}, a...)
	b[1] = "l2 changed"
	b[10] = "l11 changed"

	got, ok := unifiedDiff(a, b, 3)
	if !ok {
		t.Fatal("unifiedDiff declined inputs it should handle")
	}

	want := []string{
		"@@ -1,5 +1,5 @@",
		" l1",
		"-l2",
		"+l2 changed",
		" l3",
		" l4",
		" l5",
		"@@ -8,5 +8,5 @@",
		" l8",
		" l9",
		" l10",
		"-l11",
		"+l11 changed",
		" l12",
	}
	if strings.Join(got, "\n") != strings.Join(want, "\n") {
		t.Errorf("got:\n%s\n\nwant:\n%s", strings.Join(got, "\n"), strings.Join(want, "\n"))
	}
}

// Two changes close enough that their context overlaps belong in one hunk,
// exactly where diff(1) merges them.
func TestUnifiedDiffMergesNearbyHunks(t *testing.T) {
	a := strings.Split("1\n2\n3\n4\n5\n6\n7\n8\n9\n10", "\n")
	b := append([]string{}, a...)
	b[0] = "one"
	b[6] = "seven"

	got, _ := unifiedDiff(a, b, 3)
	hunks := 0
	for _, l := range got {
		if strings.HasPrefix(l, "@@") {
			hunks++
		}
	}
	if hunks != 1 {
		t.Errorf("want 1 merged hunk, got %d:\n%s", hunks, strings.Join(got, "\n"))
	}
}

func TestUnifiedDiffDeclinesHugeInputs(t *testing.T) {
	big := make([]string, 3000)
	for i := range big {
		big[i] = "x"
	}
	if _, ok := unifiedDiff(big, big, 3); ok {
		t.Error("want the quadratic table declined for a 3000x3000 input")
	}
}

// Org reads `*` and `#+` at the start of a block line as structure, including
// after indentation, which is where pandoc puts the comma too.
func TestEscOrgBlock(t *testing.T) {
	in := "plain\n* star\n  * indented\n\t#+kw\n,already\nmid * star"
	want := "plain\n,* star\n  ,* indented\n\t,#+kw\n,already\nmid * star"
	if got := escOrgBlock(in); got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

func TestFenceForOutgrowsBackticks(t *testing.T) {
	for _, c := range []struct {
		body string
		want string
	}{
		{"plain", "```"},
		{"a ` b", "```"},
		{"```\nnested\n```", "````"},
		{"`````", "``````"},
	} {
		if got := fenceFor(c.body); got != c.want {
			t.Errorf("fenceFor(%q) = %q, want %q", c.body, got, c.want)
		}
	}
}

func TestSnippetText(t *testing.T) {
	cases := []struct{ in, want string }{
		{"<command-name>/clear</command-name> <command-message>clear</command-message> <command-args></command-args>", "/clear"},
		{"<system-reminder>ignore me</system-reminder>", ""},
		{"real question here", "real question here"},
		{"<command-name>/loop</command-name>\nand then some prose", "/loop and then some prose"},
		{"Caveat: The messages below were generated by the user.\nactual text", "actual text"},
	}
	for _, c := range cases {
		if got := snippetText(c.in); got != c.want {
			t.Errorf("snippetText(%q) = %q, want %q", c.in, got, c.want)
		}
	}
}

func TestOrgBlockUsesExampleWithoutLanguage(t *testing.T) {
	if got := orgBlock("", "x"); got != "#+begin_example\nx\n#+end_example" {
		t.Errorf("got %q", got)
	}
	if got := orgBlock("zsh", "x"); got != "#+begin_src zsh\nx\n#+end_src" {
		t.Errorf("got %q", got)
	}
}

func TestShortModel(t *testing.T) {
	for _, c := range []struct{ in, want string }{
		{"claude-opus-5", "opus-5"},
		{"claude-fable-5", "fable-5"},
		{"claude-sonnet-5", "sonnet-5"},
		{"claude-haiku-4-5-20251001", "haiku-4.5"},
		{"<synthetic>", "synthetic"},
		{"", ""},
		{"something-unexpected", "something-unexpected"},
	} {
		if got := shortModel(c.in); got != c.want {
			t.Errorf("shortModel(%q) = %q, want %q", c.in, got, c.want)
		}
	}
}
