package main

import (
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"regexp"
	"strings"
	"testing"
)

func opts(machine string) []option {
	return []option{
		{"upstream", "Keep the remote's version"},
		{"local", "Keep " + machine + "'s version"},
		{"both", "Keep both, the remote's first"},
		{"both-local-first", "Keep both, " + machine + "'s first"},
	}
}

func testItems() []item {
	return []item{
		{File: "a.org", Hunk: 1, Total: 2, Options: opts("box")},
		{File: "a.org", Hunk: 2, Total: 2, Options: opts("box")},
		{File: "img.png", Whole: true, Options: opts("box")[:2]},
	}
}

func TestParseAnswer(t *testing.T) {
	ok := "1. both: Keep both, the remote's first\n2. local: Keep box's version\n3. upstream: Keep the remote's version"
	cases := []struct {
		name, text string
		want       []string
		doubt      string // a substring of the doubt; "" for none
	}{
		{"plain picks", ok, []string{"both", "local", "upstream"}, ""},
		{"trailing blank lines and CRLF", strings.ReplaceAll(ok, "\n", "\r\n") + "\r\n\r\n", []string{"both", "local", "upstream"}, ""},
		{"both-local-first", strings.Replace(ok, "1. both: Keep both, the remote's first", "1. both-local-first: Keep both, box's first", 1),
			[]string{"both-local-first", "local", "upstream"}, ""},
		{"no answer", strings.Replace(ok, "2. local: Keep box's version", "2. (no answer)", 1), nil, "question 2 (a.org, hunk 2 of 2) has no answer"},
		{"other", strings.Replace(ok, "1. both: Keep both, the remote's first", "1. Other: merge them by hand", 1), nil, "answered in your own words"},
		{"note", strings.Replace(ok, "box's version", "box's version. Note: careful", 1), nil, "has a note"},
		{"general message", ok + "\n\nAnd rerun tonight.", nil, "message after the picks"},
		{"missing line", "1. both: Keep both, the remote's first\n2. local: Keep box's version", nil, "2 line(s) for 3 question(s)"},
		{"lines out of order", "2. local: Keep box's version\n1. both: Keep both, the remote's first\n3. upstream: Keep the remote's version", nil, "line 1 of the answer"},
		{"wrong label", strings.Replace(ok, "Keep box's version", "Keep other's version", 1), nil, "not answered with one of its options"},
		{"both on a whole file", strings.Replace(ok, "3. upstream: Keep the remote's version", "3. both: Keep both, the remote's first", 1), nil, "question 3 (img.png (whole file))"},
		{"two keys", strings.Replace(ok, "1. both: Keep both, the remote's first", "1. both, local: Keep both, the remote's first; Keep box's version", 1), nil, "not answered with one of its options"},
		{"empty", "  \n", nil, "empty"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			got, d := parseAnswer(c.text, testItems())
			if c.doubt == "" {
				if d != nil {
					t.Fatalf("unexpected doubt: %v", d)
				}
				if !reflect.DeepEqual(got, c.want) {
					t.Fatalf("got %v, want %v", got, c.want)
				}
				return
			}
			if d == nil || !strings.Contains(d.msg, c.doubt) {
				t.Fatalf("doubt %v, want one containing %q", d, c.doubt)
			}
		})
	}
}

// listing is what `daily-sync conflict <machine> <repo>' prints, hint included.
const listing = `CONFLICT-OPEN notes/x: 2 file(s), replaying box's commit 1a2b3c4 edit onto the remote
### a.org, hunk 1 of 2, near line 3: BOTH-EDITED (suggested: 1=decide)
--- upstream (already on the remote):
  | remote line
--- base (before either side changed it):
  | base line
--- local (box):
  | local line
### a.org, hunk 2 of 2, near line 9: BOTH-ONLY-ADDED (suggested: 2=both)
--- upstream (already on the remote):
  | ### not a header, it is indented
--- base: nothing (both sides added text here)
--- local (box):
  | added here
### img.png: no conflict markers (binary, or deleted on one side); the only choices are all=upstream and all=local
### b.org, hunk 1 of 1, near line 1: SAME-IGNORING-WHITESPACE (suggested: 1=upstream)
--- upstream (already on the remote):
  | x
--- base: not shown
--- local (box):
  | x

NEXT: decide each hunk above by the conflict rules in your instructions. In short:
  zsh -c 'daily-sync resolve box notes/x "<file>" 1=<choice> 2=<choice> ...'
`

func TestParseListing(t *testing.T) {
	got := parseListing(listing)
	want := []item{
		{File: "a.org", Hunk: 1, Total: 2, Body: "--- upstream (already on the remote):\n  | remote line\n--- base (before either side changed it):\n  | base line\n--- local (box):\n  | local line"},
		{File: "a.org", Hunk: 2, Total: 2, Body: "--- upstream (already on the remote):\n  | ### not a header, it is indented\n--- base: nothing (both sides added text here)\n--- local (box):\n  | added here"},
		{File: "b.org", Hunk: 1, Total: 1, Body: "--- upstream (already on the remote):\n  | x\n--- base: not shown\n--- local (box):\n  | x"},
		{File: "img.png", Whole: true},
	}
	if !reflect.DeepEqual(got, want) {
		gb, _ := json.MarshalIndent(got, "", " ")
		t.Fatalf("got %s", gb)
	}
}

// TestListingParityWithPython runs the python in h-daily-sync-desk-ask on
// the same listing: the sidecar it writes must match parseListing.
func TestListingParityWithPython(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "..", "zshlang", "auto-load", "others", "daily-sync.zsh"))
	if err != nil {
		t.Skip("daily-sync.zsh not found:", err)
	}
	m := regexp.MustCompile(`(?s)<<'DS_PY'\n(.*?)\nDS_PY\n`).FindSubmatch(src)
	if m == nil {
		t.Fatal("no DS_PY block in daily-sync.zsh")
	}
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("no python3")
	}
	dir := t.TempDir()
	hunks := filepath.Join(dir, "hunks.txt")
	// The hunks file holds the listing without the hint, as the hint function writes it.
	if err := os.WriteFile(hunks, []byte(strings.Split(listing, "\n\nNEXT:")[0]+"\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	side := filepath.Join(dir, "side.json")
	cmd := exec.Command("python3", "-", hunks, filepath.Join(dir, "choices.json"), "box", "notes/x", side, dir)
	cmd.Stdin = strings.NewReader(string(m[1]))
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("python: %v\n%s", err, out)
	}
	b, err := os.ReadFile(side)
	if err != nil {
		t.Fatal(err)
	}
	var sc sidecar
	if err := json.Unmarshal(b, &sc); err != nil {
		t.Fatal(err)
	}
	if d := compareListing(sc.Items, parseListing(listing)); d != nil {
		t.Fatalf("python and Go disagree: %v", d)
	}
	if !reflect.DeepEqual(sc.Items[0].Options, opts("box")) || len(sc.Items[3].Options) != 2 {
		t.Fatalf("options: %+v", sc.Items)
	}
}

func TestCompareListing(t *testing.T) {
	asked := parseListing(listing)
	changed := func(f func([]item)) []item {
		c := append([]item(nil), asked...)
		f(c)
		return c
	}
	cases := []struct {
		name  string
		now   []item
		doubt string
	}{
		{"same", asked, ""},
		{"body changed", changed(func(c []item) { c[1].Body += "\n  | more" }), "a.org, hunk 2 of 2 changed since the question"},
		{"total changed", changed(func(c []item) { c[0].Total = 3 }), "a.org now has 3 hunks, not 2"},
		{"other file", changed(func(c []item) { c[2].File = "c.org" }), "question 3 was b.org, hunk 1 of 1, it is now c.org"},
		{"one more", append(append([]item(nil), asked...), item{File: "z", Whole: true}), "now has 5 question(s), the question had 4"},
		{"one fewer", asked[:3], "now has 3 question(s)"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			d := compareListing(asked, c.now)
			if c.doubt == "" {
				if d != nil {
					t.Fatalf("unexpected doubt: %v", d)
				}
				return
			}
			if d == nil || !strings.Contains(d.msg, c.doubt) {
				t.Fatalf("doubt %v, want one containing %q", d, c.doubt)
			}
		})
	}
}

func TestResolveCalls(t *testing.T) {
	got := resolveCalls(parseListing(listing), []string{"both", "upstream", "local", "local"})
	want := []resolveCall{
		{File: "a.org", Specs: []string{"1=both", "2=upstream"}},
		{File: "b.org", Specs: []string{"1=local"}},
		{File: "img.png", Specs: []string{"all=local"}},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("got %+v", got)
	}
}

func TestReport(t *testing.T) {
	if got := report("V", []string{"a", "b"}); got != "V\na\nb\n" {
		t.Fatalf("short: %q", got)
	}
	long := report("Verdict.", []string{strings.Repeat("x", 5000)})
	if len(long) > maxOutput+20 || !strings.HasPrefix(long, "Verdict.\n") || !strings.HasSuffix(long, "\nVerdict.\n") {
		t.Fatalf("long report: %d bytes, %q...%q", len(long), long[:20], long[len(long)-20:])
	}
}

func TestLoadSidecarMovedRun(t *testing.T) {
	root := t.TempDir()
	done := filepath.Join(root, "2026-01-02-done-101010")
	if err := os.MkdirAll(done, 0o700); err != nil {
		t.Fatal(err)
	}
	b, _ := json.Marshal(sidecar{Machine: "box", Repo: "notes/x", Run: filepath.Join(root, "2026-01-02"), Items: testItems()})
	if err := os.WriteFile(filepath.Join(done, "answer-box-notes_x-1.json"), b, 0o600); err != nil {
		t.Fatal(err)
	}
	sc, path, err := loadSidecar(filepath.Join(root, "2026-01-02", "answer-box-notes_x-1.json"))
	if err != nil || sc.Repo != "notes/x" || filepath.Dir(path) != done {
		t.Fatalf("got %v %q %v", sc, path, err)
	}
}
