// Renders Claude Code session `.jsonl` transcripts, and lists them for a
// fuzzy picker. Called from [agfi:h-claude-code-session-to-md],
// [agfi:h-claude-code-session-to-org-pandoc] and
// [agfi:h-claude-code-session-select-fz].
//
// Stdlib only, so `go build` needs no network and works on hosts without a
// module cache.

package main

import (
	"encoding/json"
	"fmt"
	"os"
)

// Session lines carry whole files inline, so the default 64KiB scanner
// buffer is nowhere near enough.
const maxLineBytes = 64 << 20

const (
	orgStamp  = "[2006-01-02 Mon 15:04]"
	listStamp = "2006-01-02 15:04"
)

type record struct {
	Type      string   `json:"type"`
	Slug      string   `json:"slug"`
	IsMeta    bool     `json:"isMeta"`
	Timestamp string   `json:"timestamp"`
	Message   *message `json:"message"`
}

type message struct {
	Content json.RawMessage `json:"content"`
}

type block struct {
	Type      string          `json:"type"`
	Text      string          `json:"text"`
	Thinking  string          `json:"thinking"`
	Name      string          `json:"name"`
	Input     json.RawMessage `json:"input"`
	Content   json.RawMessage `json:"content"`
	IsError   bool            `json:"is_error"`
	ID        string          `json:"id"`
	ToolUseID string          `json:"tool_use_id"`
}

func main() {
	if len(os.Args) < 2 {
		usage()
	}

	switch os.Args[1] {
	case "render":
		cmdRender(os.Args[2:])
	case "list":
		cmdList(os.Args[2:])
	case "slug":
		cmdSlug(os.Args[2:])
	case "-h", "--help", "help":
		usage()
	default:
		fmt.Fprintf(os.Stderr, "claude_session: unknown subcommand: %s\n", os.Args[1])
		usage()
	}
}

func usage() {
	fmt.Fprint(os.Stderr, `usage:
  claude_session render [flags] <session.jsonl>   #: transcript -> markdown/org on stdout
  claude_session list   [flags] <sessions-dir>    #: TSV of sessions, newest first
  claude_session slug           <session.jsonl>   #: session name, empty if unnamed

render flags:
  -format md|org|org-pandoc   output syntax (default md). org-pandoc pipes the
                              markdown through pandoc, in parallel chunks
  -max-block-lines N          elide code blocks longer than N lines (0 = never)
  -diff                       render Edit as a unified diff (default true)
  -jobs N                     worker count (default: CPU count)
  -pandoc PATH                pandoc binary for org-pandoc (default "pandoc")

list flags:
  -snippet-len N        max snippet width (default 120)
  -jobs N               worker count (default: CPU count)

list emits: epoch <TAB> path <TAB> local time <TAB> relative path <TAB> snippet
`)
	os.Exit(2)
}
