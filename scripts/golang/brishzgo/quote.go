package main

import (
	"strings"
	"unicode"
	"unicode/utf8"
)

// The quoting that brishzq.zsh's gquote does, and the quoting of
// `typeset -p` that its variable forwarding relies on. Both are ports of
// zsh behaviour, and quote_test.go checks them against zsh itself.

// bareFirstWordP reports whether gquote leaves a first word unquoted: when
// it is made only of ASCII letters, digits and _ . / , : @ % + -. Such a
// word means the same bare as quoted, and leaving it bare keeps aliases,
// reserved words and functions working in command position. The set has
// no = or ~, so a word starting with one (an expansion there) is quoted.
func bareFirstWordP(w string) bool {
	if w == "" {
		return false
	}
	for i := 0; i < len(w); i++ {
		c := w[i]
		switch {
		case 'a' <= c && c <= 'z', 'A' <= c && c <= 'Z', '0' <= c && c <= '9':
		case strings.IndexByte("_./,:@%+-", c) >= 0:
		default:
			return false
		}
	}
	return true
}

// quoteSingle is zsh's ${(qq)w}: the word in single quotes, with each
// quote inside written as quote, backslash, quote, quote. Single quotes
// keep every other byte literal, newlines and invalid UTF-8 included.
func quoteSingle(w string) string {
	return "'" + strings.ReplaceAll(w, "'", `'\''`) + "'"
}

func quoteFirstWord(w string) string {
	if bareFirstWordP(w) {
		return w
	}
	return quoteSingle(w)
}

// gquote is brishzq.zsh's gquote, as `$(gq "$@")` sees it: the first word
// bare when it can be, the others single-quoted, all joined by spaces.
// With no words it is a quoted empty word, since "${(q+@)@[1]}" of an
// empty list still gives one empty word. `$(...)` strips trailing
// newlines, but the result never ends in one: a bare word has none and a
// quoted one ends in a quote.
func gquote(words []string) string {
	if len(words) == 0 {
		return quoteFirstWord("")
	}
	parts := make([]string, len(words))
	parts[0] = quoteFirstWord(words[0])
	for i, w := range words[1:] {
		parts[i+1] = quoteSingle(w)
	}
	return strings.Join(parts, " ")
}

// zshSpecialChars is zsh's SPECCHARS: a value holding any of them is quoted
// by `typeset -p`.
const zshSpecialChars = "#$^*()=|{}[]`<>?~;&\n\t \\'\""

// quoteTypesetValue quotes a value the way `typeset -p` prints it (zsh's
// quotedzputs): a quoted empty word when empty; $'...' with escapes when
// it has a character that is not printable, or a byte that is not valid
// UTF-8; bare when it has no special character; otherwise Bourne-style
// single quotes that avoid empty quoted strings, so a lone quote becomes a
// backslash and a quote.
//
// Printability follows Go's unicode tables, where zsh asks the C library,
// so an exotic character may be quoted another way than zsh would. Every
// form is still a correct quoting, so the value that arrives is the same.
func quoteTypesetValue(s string) string {
	if s == "" {
		return "''"
	}
	if niceformatP(s) {
		return "$'" + niceformat(s) + "'"
	}
	if !strings.ContainsAny(s, zshSpecialChars) {
		return s
	}
	var b strings.Builder
	inQuote := false
	for i := 0; i < len(s); i++ {
		if s[i] == '\'' {
			if inQuote {
				b.WriteByte('\'')
				inQuote = false
			}
			b.WriteString(`\'`)
			continue
		}
		if !inQuote {
			b.WriteByte('\'')
			inQuote = true
		}
		b.WriteByte(s[i])
	}
	if inQuote {
		b.WriteByte('\'')
	}
	return b.String()
}

func runeNiceP(r rune) bool {
	if r < 0x20 || r == 0x7f {
		return true
	}
	if r < 0x80 {
		return false
	}
	return !unicode.IsGraphic(r)
}

// niceformatP is zsh's is_mb_niceformat.
func niceformatP(s string) bool {
	for i := 0; i < len(s); {
		r, n := utf8.DecodeRuneInString(s[i:])
		if r == utf8.RuneError && n <= 1 {
			return true
		}
		if runeNiceP(r) {
			return true
		}
		i += n
	}
	return false
}

// niceformat is zsh's mb_niceformat with NICEFLAG_QUOTE, the body of a
// $'...' string, except for a character outside ASCII that is not
// printable: zsh writes U+0080 to U+00FF as one \M- byte, which does not
// round-trip, and the others as \uXXXX, which a shell in the C locale
// refuses. Here each byte of its UTF-8 encoding is written as a \M-
// escape instead, which gives back the same bytes in every locale.
func niceformat(s string) string {
	var b strings.Builder
	for i := 0; i < len(s); {
		r, n := utf8.DecodeRuneInString(s[i:])
		switch {
		case r == utf8.RuneError && n <= 1, r >= 0x80 && runeNiceP(r):
			for _, c := range []byte(s[i : i+n]) {
				b.WriteString(nicechar(c))
			}
		case r == '\\' || r == '\'':
			b.WriteByte('\\')
			b.WriteRune(r)
		case runeNiceP(r):
			b.WriteString(nicechar(byte(r)))
		default:
			b.WriteString(s[i : i+n])
		}
		i += n
	}
	return b.String()
}

// nicechar is zsh's nicechar_sel with quotable set, for one byte.
func nicechar(c byte) string {
	prefix := ""
	if c&0x80 != 0 {
		prefix = `\M-`
		c &= 0x7f
		if c >= 0x20 && c < 0x7f {
			// zsh writes 0xa7 as \M-' and 0xdc as \M-\, which do not
			// parse back; the escape here does, to the same byte.
			if c == '\'' || c == '\\' {
				prefix += `\`
			}
			return prefix + string(rune(c))
		}
	}
	switch {
	case c == 0x7f:
		return prefix + `\C-?`
	case c == '\n':
		return prefix + `\n`
	case c == '\t':
		return prefix + `\t`
	case c == 0x1c:
		// zsh writes \C-\, which escapes the character after it.
		return prefix + `\C-\\`
	case c < 0x20:
		return prefix + `\C-` + string(rune(c+0x40))
	}
	return prefix + string(rune(c))
}
