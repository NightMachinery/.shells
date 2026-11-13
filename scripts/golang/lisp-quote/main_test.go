package main

import (
	"bytes"
	"strings"
	"testing"
)

func TestQuoteLines(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{
			name:  "empty",
			input: "",
			want:  "",
		},
		{
			name:  "single blank line",
			input: "\n",
			want:  "\"\"\n",
		},
		{
			name:  "multiple blank lines",
			input: "\n\n",
			want:  "\"\"\n\"\"\n",
		},
		{
			name:  "no trailing newline",
			input: "hello",
			want:  "\"hello\"\n",
		},
		{
			name:  "multiple lines with and without trailing newline",
			input: "a\nb\r\nc\nd",
			want:  "\"a\"\n\"b\r\"\n\"c\"\n\"d\"\n",
		},
		{
			name:  "escapes quotes and backslashes only",
			input: "plain\n\"quoted\"\nback\\slash\nboth\\\"here\n\\\\\"\"\n",
			want:  "\"plain\"\n\"\\\"quoted\\\"\"\n\"back\\\\slash\"\n\"both\\\\\\\"here\"\n\"\\\\\\\\\\\"\\\"\"\n",
		},
		{
			name:  "preserves tabs single quotes hashes unicode and control bytes",
			input: "a\tb\n'single' #hash\nhello 世界 🚀\na\x00b\x07c\x7fd\n",
			want:  "\"a\tb\"\n\"'single' #hash\"\n\"hello 世界 🚀\"\n\"a\x00b\x07c\x7fd\"\n",
		},
		{
			name:  "long line exceeding 64KiB",
			input: strings.Repeat(`a"b\c`, 20000) + "\n",
			want:  `"` + strings.Repeat(`a\"b\\c`, 20000) + "\"\n",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var out bytes.Buffer
			if err := quoteLines(strings.NewReader(tc.input), &out); err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if got := out.String(); got != tc.want {
				t.Fatalf("got %q, want %q", got, tc.want)
			}
		})
	}
}
