package main

import (
	"reflect"
	"strings"
	"testing"
	"unicode/utf8"
)

func fixture(agent, prompt string, x int) Screen {
	m := "❯ "
	if agent == "codex" {
		m = "› "
	}
	return Screen{Agent: agent, Lines: []Line{{Text: "alphaFar"}, {Text: "────────────────"}, {Text: m + prompt}, {Text: "────────────────"}, {Text: "-- INSERT --"}}, X: x + 2, Y: 2}
}
func TestCandidates(t *testing.T) {
	got := candidates("al", "alphaFar alphaNear al", 22, []string{"alphaNear alphaOther", "alphaGit"})
	want := []string{"alphaNear", "alphaFar", "alphaOther", "alphaGit"}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("%v", got)
	}
}
func TestCycle(t *testing.T) {
	s := fixture("claude", "alphaNear alphaNext al", 22)
	s.X = 23 // caret after al
	e, err := expand(s, nil, Cycle{})
	if err != nil {
		t.Fatal(err)
	}
	s.Lines[2].Text += e.Text
	s.X += len(e.Text)
	next, err := expand(s, nil, e.State)
	if err != nil {
		t.Fatal(err)
	}
	if next.Backspaces != len(e.Text) || next.Text == e.Text {
		t.Fatalf("%+v", next)
	}
	s.X--
	in, _ := extract(s)
	if validCycle(e.State, in, s.Agent) {
		t.Fatal("moved caret accepted")
	}
	s.X++
	s.Lines[2].Text += "x"
	in, _ = extract(s)
	if validCycle(e.State, in, s.Agent) {
		t.Fatal("edited prompt accepted")
	}
}
func TestExtraction(t *testing.T) {
	for _, agent := range []string{"claude", "codex"} {
		s := fixture(agent, "review alpha suffix", 12)
		in, err := extract(s)
		if err != nil {
			t.Fatal(err)
		}
		if in.Prefix != "review alpha" || in.Suffix != " suffix" {
			t.Fatalf("%+v", in)
		}
		s.Lines[4].Text = "-- NORMAL --"
		_, err = extract(s)
		if (err != nil) != (agent == "claude") {
			t.Fatal("vim guard", err)
		}
	}
}
func TestWrapAndUnicode(t *testing.T) {
	s := Screen{Agent: "codex", Lines: []Line{{Text: "› hello", Wrapped: true}, {Text: "  world end"}, {Text: ""}}, Y: 1, X: 7}
	in, err := extract(s)
	if err != nil || in.Prefix != "helloworld" || in.Suffix != " end" {
		t.Fatalf("%+v %v", in, err)
	}
	for _, f := range []func(string, int) string{firstChars, lastChars} {
		got := f("aسلام😀e\u0301", 4)
		if !utf8.ValidString(got) || len([]rune(got)) != 4 {
			t.Fatal(got)
		}
	}
	if sanitise("x\r\n\x1b\x00\x7f\u0085y") != "xy" {
		t.Fatal("controls survive")
	}
	if fragment("open ~/a/file.na") != "~/a/file.na" {
		t.Fatal("path")
	}
}
func TestNonASCII(t *testing.T) {
	s := fixture("codex", "aسلام al alسلام", 2)
	e, err := expand(s, []string{"aسلام"}, Cycle{})
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(e.Text, "\n") {
		t.Fatal("newline")
	}
	if validCycle(Cycle{Agent: "codex", Prefix: "a", Inserted: "سلام", Candidates: []string{"aسلام"}, Fragment: "a"}, Input{Prefix: "aسلام"}, "codex") {
		t.Fatal("unicode cycle")
	}
}
