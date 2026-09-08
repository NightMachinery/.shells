package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// The label has to survive both scopes the picker uses: the profile roots
// themselves, and those roots scoped down to a single project, where every
// root ends in the same component.
func TestProfileLabels(t *testing.T) {
	cases := []struct {
		name  string
		roots []string
		want  map[string]string
	}{
		{
			name:  "single root is unlabelled",
			roots: []string{"/Users/e/.claude/projects"},
			want:  map[string]string{},
		},
		{
			name:  "profile roots",
			roots: []string{"/Users/e/.claude/projects", "/Users/e/.claude-work/projects"},
			want: map[string]string{
				"/Users/e/.claude/projects":      ".claude",
				"/Users/e/.claude-work/projects": ".claude-work",
			},
		},
		{
			name: "scoped to one project apiece",
			roots: []string{
				"/Users/e/.claude/projects/-Users-e-scripts",
				"/Users/e/.claude-work/projects/-Users-e-scripts",
			},
			want: map[string]string{
				"/Users/e/.claude/projects/-Users-e-scripts":      ".claude",
				"/Users/e/.claude-work/projects/-Users-e-scripts": ".claude-work",
			},
		},
		{
			name:  "trailing slashes do not shift the component",
			roots: []string{"/Users/e/.claude/projects/", "/Users/e/.claude-work/projects"},
			want: map[string]string{
				"/Users/e/.claude/projects/":     ".claude",
				"/Users/e/.claude-work/projects": ".claude-work",
			},
		},
		{
			name:  "one root a prefix of the other",
			roots: []string{"/a/b", "/a/b/c"},
			want: map[string]string{
				"/a/b":   "b",
				"/a/b/c": "c",
			},
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := profileLabels(tc.roots)
			if len(got) != len(tc.want) {
				t.Fatalf("got %d labels %v, want %d %v", len(got), got, len(tc.want), tc.want)
			}
			for k, want := range tc.want {
				if got[k] != want {
					t.Errorf("%s: got %q, want %q", k, got[k], want)
				}
			}
		})
	}
}

// The precedence is Claude Code's, not ours: `agent-name` is the name it
// resolved for itself, a `custom-title` is what `/rename` wrote, an `ai-title`
// is what it summarised the work as, and the slug is the generated fallback.
func TestNamePartsResolve(t *testing.T) {
	cases := []struct {
		name string
		in   nameParts
		want string
	}{
		{"nothing", nameParts{}, ""},
		{"slug alone", nameParts{slug: "snuggly-orbit"}, "snuggly-orbit"},
		{"ai title beats slug", nameParts{slug: "s", aiTitle: "a"}, "a"},
		{"custom title beats ai title", nameParts{slug: "s", aiTitle: "a", customTitle: "c"}, "c"},
		{"agent name beats all", nameParts{slug: "s", aiTitle: "a", customTitle: "c", agentName: "n"}, "n"},
		{"gaps are skipped", nameParts{slug: "s", customTitle: "c"}, "c"},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := tc.in.resolve(); got != tc.want {
				t.Errorf("got %q, want %q", got, tc.want)
			}
			if got := tc.in.hasTitle(); got != (tc.want != "" && tc.want != tc.in.slug) {
				t.Errorf("hasTitle() = %v for %+v", got, tc.in)
			}
		})
	}
}

// Reading forwards and backwards must agree on which occurrence of a revised
// title is the current one.
func TestNamePartsDirection(t *testing.T) {
	forwards := []nameFields{{AITitle: "first"}, {AITitle: "second"}, {AITitle: "third"}}

	var fwd nameParts
	for _, f := range forwards {
		fwd.observe(f, false)
	}
	if fwd.resolve() != "third" {
		t.Errorf("forwards: got %q, want %q", fwd.resolve(), "third")
	}

	var back nameParts
	for i := len(forwards) - 1; i >= 0; i-- {
		back.observe(forwards[i], true)
	}
	if back.resolve() != "third" {
		t.Errorf("backwards: got %q, want %q", back.resolve(), "third")
	}
}

// One `user` record, with `pad` bytes of filler so a test can build a
// transcript of a given size. `pad` is an unknown field to both scans, which
// is the point: it costs the walk what a real message body costs it.
func msgLine(slug, stamp string, pad int) string {
	return fmt.Sprintf(
		`{"type":"user","timestamp":%q,"slug":%q,"message":{"content":[{"type":"text","text":%q}]},"pad":%q}`,
		stamp, slug, "hi", strings.Repeat("x", pad))
}

func titleLine(kind, key, value string) string {
	return fmt.Sprintf(`{"type":%q,%q:%q}`, kind, key, value)
}

func writeTranscript(t *testing.T, lines []string) *os.File {
	t.Helper()

	p := filepath.Join(t.TempDir(), "session.jsonl")
	if err := os.WriteFile(p, []byte(strings.Join(lines, "\n")+"\n"), 0o600); err != nil {
		t.Fatalf("write: %v", err)
	}

	fh, err := os.Open(p)
	if err != nil {
		t.Fatalf("open: %v", err)
	}
	t.Cleanup(func() { fh.Close() })
	return fh
}

// `scanTail` reads only the end of the file, so it can only agree with the
// authoritative full scan when the name is written near it. These are the
// cases that decide whether the picker's column is trustworthy.
func TestScanTailName(t *testing.T) {
	const stamp = "2026-09-08T16:46:00.000Z"

	// Enough messages that the name walk has to continue past the point where
	// the timestamp walk stops (`tailRecords`).
	var deep []string
	deep = append(deep, titleLine("ai-title", "aiTitle", "buried title"))
	for i := 0; i < tailRecords*3; i++ {
		deep = append(deep, msgLine("snuggly-orbit", stamp, 32))
	}

	cases := []struct {
		name    string
		lines   []string
		want    string
		wantFul string // what the full scan says, when it differs
	}{
		{
			name:  "slug only",
			lines: []string{msgLine("snuggly-orbit", stamp, 8)},
			want:  "snuggly-orbit",
		},
		{
			name: "revised ai title takes the last",
			lines: []string{
				msgLine("snuggly-orbit", stamp, 8),
				titleLine("ai-title", "aiTitle", "early guess"),
				msgLine("snuggly-orbit", stamp, 8),
				titleLine("ai-title", "aiTitle", "what it turned into"),
			},
			want: "what it turned into",
		},
		{
			name: "a fork's appended agent-name wins over its ai-title",
			lines: []string{
				msgLine("snuggly-orbit", stamp, 8),
				titleLine("ai-title", "aiTitle", "pre-fork title"),
				titleLine("agent-name", "agentName", "pre-fork title ⑂ work"),
				titleLine("custom-title", "customTitle", "pre-fork title ⑂ work"),
			},
			want: "pre-fork title ⑂ work",
		},
		{
			name:  "a title further back than the timestamp walk goes",
			lines: deep,
			want:  "buried title",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			fh := writeTranscript(t, tc.lines)
			st, err := fh.Stat()
			if err != nil {
				t.Fatalf("stat: %v", err)
			}

			_, name := scanTail(fh, st.Size())
			if got := name.resolve(); got != tc.want {
				t.Errorf("scanTail: got %q, want %q", got, tc.want)
			}

			wantFull := tc.wantFul
			if wantFull == "" {
				wantFull = tc.want
			}
			if got := sessionName(fh); got != wantFull {
				t.Errorf("sessionName: got %q, want %q", got, wantFull)
			}
		})
	}
}

// A title older than `nameWindow` is out of reach by design, and the slug is
// what the column shows instead. The full scan still finds it, so this pins
// the one place the two are allowed to disagree.
func TestScanTailNameWindowCap(t *testing.T) {
	const stamp = "2026-09-08T16:46:00.000Z"

	lines := []string{titleLine("ai-title", "aiTitle", "out of reach")}
	for written := 0; written < nameWindow+(256<<10); written += 4096 {
		lines = append(lines, msgLine("snuggly-orbit", stamp, 3800))
	}

	fh := writeTranscript(t, lines)
	st, err := fh.Stat()
	if err != nil {
		t.Fatalf("stat: %v", err)
	}
	if st.Size() <= nameWindow {
		t.Fatalf("transcript is %d bytes, needs to exceed nameWindow (%d)", st.Size(), nameWindow)
	}

	last, name := scanTail(fh, st.Size())
	if got := name.resolve(); got != "snuggly-orbit" {
		t.Errorf("scanTail: got %q, want the slug fallback %q", got, "snuggly-orbit")
	}
	if last.IsZero() {
		t.Error("scanTail: no timestamp; the cap must not cost us the date")
	}
	if got := sessionName(fh); got != "out of reach" {
		t.Errorf("sessionName: got %q, want %q", got, "out of reach")
	}
}
