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
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"
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
	Subtype   string   `json:"subtype"`
	Slug      string   `json:"slug"`
	IsMeta    bool     `json:"isMeta"`
	Timestamp string   `json:"timestamp"`
	Message   *message `json:"message"`

	// `system` records keep their payload at the top level, unlike messages.
	// Raw, because other record types put an object here.
	Content json.RawMessage `json:"content"`

	AITitle     string `json:"aiTitle"`
	CustomTitle string `json:"customTitle"`

	Attachment      *attachment      `json:"attachment"`
	CompactMetadata *compactMetadata `json:"compactMetadata"`
	DurationMs      int64            `json:"durationMs"`

	PRNumber     int    `json:"prNumber"`
	PRUrl        string `json:"prUrl"`
	PRRepository string `json:"prRepository"`
}

type message struct {
	Content json.RawMessage `json:"content"`
	Model   string          `json:"model"`
}

// `system` record subtypes that carry something worth reading. The rest are
// bookkeeping: `stop_hook_summary` never has content (0 of 225 locally), and
// `turn_duration` is folded into the heading of the turn it measures.
const (
	subtypeRecap    = "away_summary"
	subtypeCompact  = "compact_boundary"
	subtypeCommand  = "local_command"
	subtypeInfo     = "informational"
	subtypeFallback = "model_consent_fallback"
	subtypeDuration = "turn_duration"
)

type attachment struct {
	Type        string `json:"type"`
	Filename    string `json:"filename"`
	DisplayPath string `json:"displayPath"`
	Snippet     string `json:"snippet"`

	// task_reminder: the outstanding task list Claude is reminded of. Every
	// one of the 485 locally is empty, because the task tools go unused here,
	// but a populated one is worth reading rather than dropping.
	Content   json.RawMessage `json:"content"`
	ItemCount int             `json:"itemCount"`
}

type compactMetadata struct {
	Trigger    string `json:"trigger"`
	PreTokens  int    `json:"preTokens"`
	PostTokens int    `json:"postTokens"`
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
	case "name":
		cmdName(os.Args[2:])
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
  claude_session name           <session.jsonl>   #: session name, empty if unnamed

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

// Every relative path inside ~/.claude/projects begins with a dash, because
// the project directories are named after the cwd they belong to
// (`-Users-evar-scripts`). The flag package would read those as flags, so an
// argument that is not a defined flag and does name an existing file is
// spelled `./…` before parsing. `--` still works for anything this misses.
func guardPathArgs(fs *flag.FlagSet, argv []string) []string {
	out := make([]string, len(argv))
	copy(out, argv)

	for i, a := range out {
		if !strings.HasPrefix(a, "-") || a == "-" || a == "--" {
			continue
		}

		name, _, _ := strings.Cut(strings.TrimLeft(a, "-"), "=")
		if fs.Lookup(name) != nil {
			continue
		}
		if _, err := os.Stat(a); err == nil {
			out[i] = "." + string(filepath.Separator) + a
		}
	}

	return out
}
