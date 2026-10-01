package main

import (
	"bytes"
	"encoding/json"
	"math/rand"
	"os/exec"
	"strings"
	"testing"
)

func TestJqTextCases(t *testing.T) {
	for in, want := range map[string]string{
		"":                 "",
		"plain é 日本":       "plain é 日本",
		"\xff\xfe":         "��",
		"a\xe2\x82Ab":      "a�Ab", // one lead and one continuation: one sequence
		"\xe0\x80\x80":     "�",    // overlong
		"\xed\xa0\x80x":    "�x",   // a surrogate
		"\xf4\x90\x80\x80": "�",    // past U+10FFFF
		"\xc0\xaf":         "��",
		"ab\xe2\x82":       "ab�", // cut short at the end
		"\x80\x80":         "��",
		"é\xff":            "é�",
	} {
		if got := jqText([]byte(in)); got != want {
			t.Errorf("jqText(%q) = %q, want %q", in, got, want)
		}
	}
}

// TestJqTextMatchesJq compares jqText with jq itself on random byte
// strings, through --arg (as $ARGS.positional) and through --raw-input, one
// string per line (so only for strings without a newline: see jqText).
// Skipped without jq.
func TestJqTextMatchesJq(t *testing.T) {
	jq, err := exec.LookPath("jq")
	if err != nil {
		t.Skip("jq not found")
	}
	rng := rand.New(rand.NewSource(4))
	interesting := []byte("a\x7f\x80\x8f\xbf\xc0\xc1\xc2\xdf\xe0\xed\xef\xf0\xf4\xf5\xff\xa0\x9f\x90")
	words := make([]string, 2000)
	for i := range words {
		b := make([]byte, rng.Intn(12))
		for j := range b {
			if rng.Intn(2) == 0 {
				b[j] = byte(1 + rng.Intn(255))
			} else {
				b[j] = interesting[rng.Intn(len(interesting))]
			}
		}
		words[i] = string(b)
	}

	out, err := exec.Command(jq, append([]string{"--null-input", "--compact-output", "$ARGS.positional", "--args"}, words...)...).Output()
	if err != nil {
		t.Fatalf("jq --args: %v", err)
	}
	var got []string
	if err := json.Unmarshal(out, &got); err != nil || len(got) != len(words) {
		t.Fatalf("jq --args gave %d strings: %v", len(got), err)
	}
	for i, w := range words {
		if want := jqText([]byte(w)); got[i] != want {
			t.Errorf("--arg %q: jq %q, jqText %q", w, got[i], want)
		}
	}

	var lines []string
	for _, w := range words {
		if !strings.Contains(w, "\n") {
			lines = append(lines, w)
		}
	}
	cmd := exec.Command(jq, "--raw-input", "--compact-output", ".")
	cmd.Stdin = strings.NewReader(strings.Join(lines, "\n") + "\n")
	out, err = cmd.Output()
	if err != nil {
		t.Fatalf("jq --raw-input: %v", err)
	}
	dec := json.NewDecoder(bytes.NewReader(out))
	for _, w := range lines {
		var s string
		if err := dec.Decode(&s); err != nil {
			t.Fatalf("jq --raw-input output: %v", err)
		}
		if want := jqText([]byte(w)); s != want {
			t.Errorf("--raw-input %q: jq %q, jqText %q", w, s, want)
		}
	}
}
