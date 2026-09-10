package turns

import "testing"

func TestDefuseHTML(t *testing.T) {
	for _, c := range []struct{ in, want string }{
		{"<INSTRUCTIONS>\n# Heading\n", "\\<INSTRUCTIONS>\n# Heading\n"},
		{"</INSTRUCTIONS>", "\\</INSTRUCTIONS>"},
		{"<!-- a comment -->", "\\<!-- a comment -->"},
		{"text with <inline> tag", "text with \\<inline> tag"},
		// Autolinks stay links.
		{"see <https://example.com/x>", "see <https://example.com/x>"},
		{"mail <a@b.co>", "mail <a@b.co>"},
		// Comparisons are not markup.
		{"if a < b and c<d", "if a < b and c<d"},
		{"1 <2", "1 <2"},
		// Code spans and fenced blocks are verbatim already.
		{"use `<tag>` here", "use `<tag>` here"},
		{"```\n<tag>\n```", "```\n<tag>\n```"},
		{"``a `<b>` c``", "``a `<b>` c``"},
		// An escape already present keeps its meaning.
		{"\\<kept> <also>", "\\<kept> \\<also>"},
		{"", ""},
	} {
		if got := defuseHTML(c.in); got != c.want {
			t.Errorf("defuseHTML(%q) = %q, want %q", c.in, got, c.want)
		}
	}
}
