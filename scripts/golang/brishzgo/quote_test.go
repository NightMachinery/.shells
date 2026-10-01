package main

import (
	"bytes"
	"math/rand"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// The quoters are checked against zsh itself: random byte strings must come
// back exactly through `print -rn --`, and the text must match what zsh's
// own quoting prints. Skipped without zsh.

func needZsh(t *testing.T) string {
	t.Helper()
	p, err := exec.LookPath("zsh")
	if err != nil {
		t.Skip("zsh not found")
	}
	return p
}

// zshLocales are the locales the zsh checks run under: a UTF-8 one, where
// zsh decodes multibyte characters, and C.
var zshLocales = []string{"en_US.UTF-8", "C"}

// fuzzWords returns n random words without NUL (argv cannot carry one),
// biased towards the bytes that matter to quoting.
func fuzzWords(seed int64, n, maxLen int) []string {
	rng := rand.New(rand.NewSource(seed))
	interesting := []byte("'\"\\$`!~=%#^*?[](){}<>|&;, \t\n\r-+@:./_aZ9\x01\x1b\x7f\x80\xa0\xc3\xa9\xe2\x80\x8b\xff")
	words := make([]string, n)
	for i := range words {
		l := rng.Intn(maxLen + 1)
		b := make([]byte, l)
		for j := range b {
			if rng.Intn(3) == 0 {
				b[j] = byte(1 + rng.Intn(255))
			} else {
				b[j] = interesting[rng.Intn(len(interesting))]
			}
		}
		words[i] = string(b)
	}
	return words
}

// zshPrintEach runs a zsh script that prints each of the given zsh word
// texts with `print -rn --`, NUL-separated, and returns what came back.
func zshPrintEach(t *testing.T, zsh, locale string, texts []string, assign bool) []string {
	t.Helper()
	var script bytes.Buffer
	for _, text := range texts {
		if assign {
			// The text is a value as typeset -p prints it.
			script.WriteString("X=" + text + "\nprint -rn -- \"$X\"\n")
		} else {
			script.WriteString("print -rn -- " + text + "\n")
		}
		script.WriteString("print -rn -- $'\\0'\n")
	}
	f := filepath.Join(t.TempDir(), "s.zsh")
	if err := os.WriteFile(f, script.Bytes(), 0o600); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command(zsh, "-f", f)
	cmd.Env = append(os.Environ(), "LC_ALL="+locale)
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	out, err := cmd.Output()
	if err != nil {
		t.Fatalf("zsh: %v: %s", err, stderr.Bytes())
	}
	parts := strings.Split(string(out), "\x00")
	if len(parts) != len(texts)+1 {
		t.Fatalf("zsh printed %d words, want %d", len(parts)-1, len(texts))
	}
	return parts[:len(texts)]
}

func TestBareFirstWordSet(t *testing.T) {
	for c := 1; c < 256; c++ {
		w := "a" + string([]byte{byte(c)}) + "b"
		want := strings.ContainsRune("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_./,:@%+-", rune(c)) && c < 0x80
		if got := bareFirstWordP(w); got != want {
			t.Errorf("bareFirstWordP(%q) = %v, want %v", w, got, want)
		}
	}
	for _, w := range []string{"", "=x", "~x", "a b", "é", "!x", "x!", "a=b"} {
		if bareFirstWordP(w) {
			t.Errorf("bareFirstWordP(%q) = true", w)
		}
	}
	for _, w := range []string{"ec", "print", "-n", "a.b/c,d:e@f%g+h-i_j", "123"} {
		if !bareFirstWordP(w) {
			t.Errorf("bareFirstWordP(%q) = false", w)
		}
	}
}

func TestGquote(t *testing.T) {
	cases := []struct {
		in   []string
		want string
	}{
		{nil, "''"},
		{[]string{""}, "''"},
		{[]string{"print", "-r", "--", "ok"}, "print '-r' '--' 'ok'"},
		{[]string{"a b", "it's", ""}, "'a b' 'it'\\''s' ''"},
		{[]string{"~x", "a\nb"}, "'~x' 'a\nb'"},
		{[]string{"ec"}, "ec"},
	}
	for _, c := range cases {
		if got := gquote(c.in); got != c.want {
			t.Errorf("gquote(%q) = %q, want %q", c.in, got, c.want)
		}
	}
}

// TestQuoteSingleMatchesZsh compares quoteSingle with zsh's ${(qq)w}, byte
// for byte.
func TestQuoteSingleMatchesZsh(t *testing.T) {
	zsh := needZsh(t)
	words := fuzzWords(1, 400, 24)
	for _, locale := range zshLocales {
		args := append([]string{"-fc", `for w in "$@"; do print -rn -- "${(qq)w}"; print -rn -- $'\0'; done`, "zsh"}, words...)
		cmd := exec.Command(zsh, args...)
		cmd.Env = append(os.Environ(), "LC_ALL="+locale)
		out, err := cmd.Output()
		if err != nil {
			t.Fatalf("zsh: %v", err)
		}
		got := strings.Split(string(out), "\x00")
		for i, w := range words {
			if got[i] != quoteSingle(w) {
				t.Errorf("%s: quoteSingle(%q) = %q, zsh (qq) = %q", locale, w, quoteSingle(w), got[i])
			}
		}
	}
}

// TestQuoteRoundTrip is the fuzz: random byte strings, quoted as a first
// word and as a later word, must come back exactly through zsh.
func TestQuoteRoundTrip(t *testing.T) {
	zsh := needZsh(t)
	words := fuzzWords(2, 1500, 32)
	for _, locale := range zshLocales {
		for name, quote := range map[string]func(string) string{"first": quoteFirstWord, "single": quoteSingle} {
			texts := make([]string, len(words))
			for i, w := range words {
				texts[i] = quote(w)
			}
			got := zshPrintEach(t, zsh, locale, texts, false)
			for i, w := range words {
				if got[i] != w {
					t.Errorf("%s %s: %q quoted as %q came back as %q", locale, name, w, texts[i], got[i])
				}
			}
		}
	}
}

// typesetCorpus are values whose typeset -p text must match zsh's exactly.
var typesetCorpus = []string{
	"", "y", "a b", "it's", "'", "a'", "''", "a\\b", "!x", "%x", "a=b", "x~", ",",
	"/tmp/emacs501/server", "server name", "a\nb", "a\tb", "\r", "\x1b[", "\x01", "\x7f",
	"\xff", "\xc3", "é", "é x", "a'\x01\\b", "日本", "\U0001F600", "$HOME", "`x`", "\"q\"",
	"\x80", "a\xffb'c", "\xc3(",
}

// typesetDivergent are values where quoteTypesetValue knowingly prints
// other text than zsh, because zsh's own text does not give the value back
// (U+0085 comes back as one byte; \M-' and \C-\ escape what follows) or
// only does in a UTF-8 locale (\u200b). Only their round trip is checked.
var typesetDivergent = []string{"\u200b", "\xc2\x85", "\xa7", "\xdc", "\x1c", "a\x1cb"}

func zshTypesetValues(t *testing.T, zsh, locale string, values []string) []string {
	t.Helper()
	args := append([]string{"-fc", `for v in "$@"; do X=$v; export X; typeset -p X; print -rn -- $'\0'; done`, "zsh"}, values...)
	cmd := exec.Command(zsh, args...)
	cmd.Env = append(os.Environ(), "LC_ALL="+locale)
	out, err := cmd.Output()
	if err != nil {
		t.Fatalf("zsh: %v", err)
	}
	parts := strings.Split(string(out), "\x00")
	res := make([]string, len(values))
	for i := range values {
		res[i] = strings.TrimSuffix(strings.TrimPrefix(parts[i], "export X="), "\n")
	}
	return res
}

func TestTypesetValueMatchesZsh(t *testing.T) {
	zsh := needZsh(t)
	got := zshTypesetValues(t, zsh, "en_US.UTF-8", typesetCorpus)
	for i, v := range typesetCorpus {
		if q := quoteTypesetValue(v); q != got[i] {
			t.Errorf("quoteTypesetValue(%q) = %q, zsh typeset -p = %q", v, q, got[i])
		}
	}
}

// TestTypesetValueRoundTrip: whatever the text, the value must arrive.
func TestTypesetValueRoundTrip(t *testing.T) {
	zsh := needZsh(t)
	values := append(append(fuzzWords(3, 1500, 32), typesetCorpus...), typesetDivergent...)
	texts := make([]string, len(values))
	for i, v := range values {
		texts[i] = quoteTypesetValue(v)
	}
	for _, locale := range zshLocales {
		got := zshPrintEach(t, zsh, locale, texts, true)
		for i, v := range values {
			if got[i] != v {
				t.Errorf("%s: %q quoted as %q came back as %q", locale, v, texts[i], got[i])
			}
		}
	}
}
