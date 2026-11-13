package main

import (
	"bytes"
	"strings"
	"testing"
)

func TestQuoteAll(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{
			name:  "empty",
			input: "",
			want:  "\"\"\n",
		},
		{
			name:  "newlines only",
			input: "\n\n",
			want:  "\"\n\n\"\n",
		},
		{
			name:  "multiline with quotes and backslashes",
			input: "line1 \"q\"\nline2 \\b\r\nline3 \\\"both\\\"",
			want:  "\"line1 \\\"q\\\"\nline2 \\\\b\r\nline3 \\\\\\\"both\\\\\\\"\"\n",
		},
		{
			name:  "preserves tabs single quotes hashes unicode and control bytes",
			input: "a\tb\n'single' #hash\nhello 世界 🚀\na\x00b\x07c\x7fd",
			want:  "\"a\tb\n'single' #hash\nhello 世界 🚀\na\x00b\x07c\x7fd\"\n",
		},
		{
			name:  "large input across chunk boundaries",
			input: strings.Repeat("a\"b\\c\n", 25000),
			want:  "\"" + strings.Repeat("a\\\"b\\\\c\n", 25000) + "\"\n",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var out bytes.Buffer
			if err := quoteAll(strings.NewReader(tc.input), &out); err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if got := out.String(); got != tc.want {
				t.Fatalf("got %q, want %q", got, tc.want)
			}
		})
	}
}
