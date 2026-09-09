package turns

import (
	"os/exec"
	"strconv"
	"strings"
	"testing"
)

func TestEndsWithMarkdownHeading(t *testing.T) {
	cases := []struct {
		name string
		md   string
		want bool
	}{
		{"bodyless heading", "### Result: nothing to say\n\n", true},
		{"heading with a body", "### Tool Use\n\n- *arg*: `x`\n\n", false},
		{"deepest heading", "###### deep\n", true},
		{"seven hashes is not a heading", "####### nope\n", false},
		{"hash without a space", "###nope\n", false},
		{"trailing spaces do not hide it", "## A  \n  \n", true},
		{"empty", "", false},
		{"a hash inside a code fence", "```\n# not a heading\n```\n", false},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := endsWithMarkdownHeading(tc.md); got != tc.want {
				t.Errorf("got %v, want %v for %q", got, tc.want, tc.md)
			}
		})
	}
}

// An org headline is a container, so pandoc writes whatever follows one with no
// blank line, while joining the chunks' outputs always puts one there. That is
// the whole of the difference between the parallel path and a single run, and
// it is why a chunk may not end on a heading.
//
// The regression this pins showed up as a single stray blank line 30686 lines
// into one real transcript, which is not a thing to rediscover by eye.
func TestPandocChunksSeamAfterHeading(t *testing.T) {
	if _, err := exec.LookPath("pandoc"); err != nil {
		t.Skip("pandoc not installed")
	}

	// A part ending in a bodyless heading, followed by one starting with a
	// heading: the shape that broke. Padded past minPandocChunk so the splitter
	// actually wants more than one chunk.
	pad := strings.Repeat("filler paragraph.\n\n", minPandocChunk/16)
	parts := []string{
		"## Turn one\n\n" + pad,
		"### Result: nothing to say\n\n",
		"## Turn two\n\n" + pad,
	}

	one, err := pandocChunks(parts, 1, "pandoc")
	if err != nil {
		t.Fatal(err)
	}
	many, err := pandocChunks(parts, 4, "pandoc")
	if err != nil {
		t.Fatal(err)
	}
	whole := strings.Join(one, "\n\n")
	split := strings.Join(many, "\n\n")

	if whole != split {
		t.Errorf("parallel output differs from a single run:\n%s", firstDifference(whole, split))
	}
	if !strings.Contains(whole, "*** Result: nothing to say\n** Turn two") {
		t.Error("the two headlines should be adjacent, with no blank line between them")
	}
}

// Which line the two outputs first disagree on, so a failure says what went
// wrong rather than dumping two documents.
func firstDifference(a, b string) string {
	la, lb := strings.Split(a, "\n"), strings.Split(b, "\n")
	for i := 0; i < len(la) || i < len(lb); i++ {
		var x, y string
		if i < len(la) {
			x = la[i]
		}
		if i < len(lb) {
			y = lb[i]
		}
		if x != y {
			return "line " + strconv.Itoa(i+1) + ":\n  single:   " + quote(x) + "\n  parallel: " + quote(y)
		}
	}
	return "(no line differs)"
}

func quote(s string) string { return "\"" + s + "\"" }
