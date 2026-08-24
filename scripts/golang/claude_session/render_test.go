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
