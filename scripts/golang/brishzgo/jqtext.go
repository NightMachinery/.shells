package main

import (
	"strings"
	"unicode/utf8"
)

// jqText is the string jq makes of the bytes b, as brishzq.zsh's JSON
// request gets its command (`jq --arg`) and its stdin (`jq --raw-input`):
// valid UTF-8 unchanged, and each invalid sequence replaced by one U+FFFD,
// with the sequences cut where jq's decoder (jvp_utf8_next) cuts them. So
// a lead byte and the continuation bytes that follow it are one sequence,
// even when it is overlong, a surrogate, past U+10FFFF or cut short, while
// Go's decoder would replace each of those bytes on its own.
//
// jq 1.6 reads raw input in pieces, lines of at most 4095 bytes, and
// decodes each on its own: a sequence cut short by a newline takes the
// newline with it, and a character that straddles a 4095-byte boundary
// becomes two U+FFFD. Those are bugs, which this does not copy.
func jqText(b []byte) string {
	if utf8.Valid(b) {
		return string(b)
	}
	var sb strings.Builder
	for i := 0; i < len(b); {
		c := b[i]
		if c < 0x80 {
			sb.WriteByte(c)
			i++
			continue
		}
		cp, n := jqDecode(b[i:])
		if cp < 0 {
			sb.WriteRune(utf8.RuneError)
		} else {
			sb.WriteRune(cp)
		}
		i += n
	}
	return sb.String()
}

// jqDecode is jvp_utf8_next for a sequence that starts with a byte of 0x80
// or more: the code point, or -1 for an invalid sequence, and its length.
func jqDecode(b []byte) (rune, int) {
	var length int
	var bits byte
	switch c := b[0]; {
	case 0xC2 <= c && c <= 0xDF:
		length, bits = 2, 0x1F
	case 0xE0 <= c && c <= 0xEF:
		length, bits = 3, 0x0F
	case 0xF0 <= c && c <= 0xF4:
		length, bits = 4, 0x07
	default:
		// A continuation byte out of place, or C0, C1 or F5 to FF.
		return -1, 1
	}
	if length > len(b) {
		// The input ends inside the sequence: all of it is one.
		return -1, len(b)
	}
	cp := rune(b[0] & bits)
	for k := 1; k < length; k++ {
		if b[k]&0xC0 != 0x80 {
			return -1, k
		}
		cp = cp<<6 | rune(b[k]&0x3F)
	}
	firstCodepoint := [...]rune{0, 0, 0x80, 0x800, 0x10000}
	if cp < firstCodepoint[length] || (0xD800 <= cp && cp <= 0xDFFF) || cp > 0x10FFFF {
		return -1, length
	}
	return cp, length
}
