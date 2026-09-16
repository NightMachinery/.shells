package hold

import (
	"encoding/json"
	"fmt"
	"io"
	"strings"
	"time"
)

// The Claude Code PreToolUse guard: refuse a tool call that would touch a
// resource another session is holding.
//
// Fails open on every unexpected condition. A guard that bricks every agent in
// the house when something is missing is worse than the accident it prevents;
// it stops mistakes, not a determined process.

// Payload is the part of a PreToolUse hook payload this needs.
type Payload struct {
	SessionID string `json:"session_id"`
	CWD       string `json:"cwd"`
	ToolName  string `json:"tool_name"`
	ToolInput struct {
		FilePath     string `json:"file_path"`
		NotebookPath string `json:"notebook_path"`
		Command      string `json:"command"`
	} `json:"tool_input"`
}

// Decision is a guard verdict. Reason is empty when the call is allowed
// silently.
//
// Deny and Warn are the two strengths, and which signal earns which is the
// whole design. A structured `file_path' inside a held path is an unambiguous
// write to the resource, so it is denied. Every `Bash' signal is textual and
// therefore a guess -- the command may only be reading, or may merely quote the
// string in a heredoc -- so it warns and lets the call through. The agent is
// told to check `hold-status' and honour the hold itself.
type Decision struct {
	Deny   bool
	Warn   bool
	Reason string
}

// holdCommands are never blocked. Managing a hold necessarily names the
// resource, so without this the guard would deny the very command that clears
// one -- and that is not hypothetical: the holder id is the agent session id
// and that can change mid-task, leaving an agent locked out of a repository by
// its own hold with no way to release it.
var holdCommands = []string{
	"hold-acquire", "hold-release", "hold-renew", "hold-check", "hold-status",
	"night_hold",
}

// editTools carry a structured path, so they are tested exactly.
var editTools = map[string]bool{
	"Edit": true, "Write": true, "MultiEdit": true, "NotebookEdit": true,
}

// Guard reads a hook payload and decides.
func (s Store) Guard(r io.Reader, now time.Time) Decision {
	if now.IsZero() {
		now = time.Now()
	}

	raw, err := io.ReadAll(io.LimitReader(r, 8<<20))
	if err != nil || len(raw) == 0 {
		return Decision{}
	}
	var p Payload
	if err := json.Unmarshal(raw, &p); err != nil {
		return Decision{}
	}

	if p.ToolName == "Bash" {
		for _, c := range holdCommands {
			if strings.Contains(p.ToolInput.Command, c) {
				return Decision{}
			}
		}
	}

	// Never reap here: something on the hot path before every tool call has no
	// business deleting files.
	holds, err := s.All(now, false)
	if err != nil {
		return Decision{}
	}

	// The guard inherits the agent's environment, so it can use the pid
	// identity too -- which matters most here: once the session id changes the
	// payload carries one the hold has never seen, and without this the agent
	// is denied its own repository by its own hold.
	c := GuardCaller(sanitizeHolder(p.SessionID))
	var warn Decision
	for _, h := range holds {
		if h.Mine(c) {
			continue
		}
		d := h.blocks(p)
		if d.Deny {
			return d
		}
		if d.Warn && !warn.Warn {
			warn = d
		}
	}
	return warn
}

func (h Hold) blocks(p Payload) Decision {
	held := fmt.Sprintf("held by another session (%s), %s. Reason: %s.",
		h.Holder, h.blockWindow(), h.Reason)
	advice := "Run 'hold-status' to see it. Do not work in that resource until the holder releases it; " +
		"if you believe the hold is stale or wrong, ask the user rather than removing it."
	warned := "This is a WARNING, not a block -- the command ran. The guard cannot tell from the " +
		"text whether it writes anything, so honouring the hold is now your job: reading is fine, " +
		"but do not create, modify or delete anything in that resource, and do not re-run this " +
		"command if it did. " + advice

	pathPart := PathPart(h.Resource)

	switch {
	case editTools[p.ToolName]:
		target := p.ToolInput.FilePath
		if target == "" {
			target = p.ToolInput.NotebookPath
		}
		if pathPart != "" && underPath(target, pathPart) {
			return Decision{Deny: true, Reason: fmt.Sprintf(
				"Blocked by a hold: %s is inside %s, which is %s %s",
				target, h.Resource, held, advice)}
		}

	case p.ToolName == "Bash":
		if pathPart != "" && underPath(p.CWD, pathPart) {
			return Decision{Warn: true, Reason: fmt.Sprintf(
				"Hold warning: this shell's working directory is inside %s, which is %s %s",
				h.Resource, held, warned)}
		}
		// Explicit --match literals are plain substrings: the caller asked for
		// that exact text. A vcsh repository needs them -- `vcsh night.sh
		// commit` spells the path nowhere, so the path tests alone would let
		// the most dangerous command straight through.
		for _, m := range h.Matches {
			if m != "" && strings.Contains(p.ToolInput.Command, m) {
				return Decision{Warn: true, Reason: fmt.Sprintf(
					"Hold warning: the command names '%s', which belongs to %s — %s %s",
					m, h.Resource, held, warned)}
			}
		}
		for _, m := range h.PathMatches {
			if pathNamed(p.ToolInput.Command, m) {
				return Decision{Warn: true, Reason: fmt.Sprintf(
					"Hold warning: the command names '%s', which belongs to %s — %s %s",
					m, h.Resource, held, warned)}
			}
		}
	}
	return Decision{}
}

// blockWindow tells a denied agent how long this is likely to last, which is
// the difference between waiting and going to do something else.
func (h Hold) blockWindow() string {
	if !h.HasDeadline() {
		if h.Foreign() {
			return "for as long as its holder lives, which cannot be checked from this host"
		}
		return "for as long as its holder lives"
	}
	s := int(h.Until.Sub(time.Now()).Seconds())
	if s < 0 {
		s = 0
	}
	return fmt.Sprintf("for another ~%d min", (s+59)/60)
}

// underPath is the exact test: equal, or inside. `/x/tmp-backup` is not inside
// `/x/tmp`, which a prefix test alone would get wrong.
func underPath(candidate, dir string) bool {
	if candidate == "" || dir == "" {
		return false
	}
	return candidate == dir || strings.HasPrefix(candidate, dir+"/")
}

// pathNamed reports whether a command names a path, as opposed to merely
// containing its characters. A path occurs only where a boundary sits on each
// side of it; a bare substring test denied `ls ~/tmpfoo` under a hold on
// `path:~/tmp`, and a guard that cries wolf before every tool call teaches
// everyone to route around it.
func pathNamed(command, p string) bool {
	if command == "" || p == "" {
		return false
	}
	for i := 0; i+len(p) <= len(command); {
		j := strings.Index(command[i:], p)
		if j < 0 {
			return false
		}
		start, end := i+j, i+j+len(p)
		if boundaryBefore(command, start) && boundaryAfter(command, end) {
			return true
		}
		i = start + 1
	}
	return false
}

// Characters that can precede a path without being part of a longer word.
const beforeBoundary = " \t\n\"'(=:,|&;`<>"

// Characters that can follow one. `/` counts, so a file inside a held
// directory still names it.
const afterBoundary = " \t\n\"'):,|&;`<>"

func boundaryBefore(s string, i int) bool {
	return i == 0 || strings.ContainsRune(beforeBoundary, rune(s[i-1]))
}

func boundaryAfter(s string, i int) bool {
	if i >= len(s) {
		return true
	}
	return s[i] == '/' || strings.ContainsRune(afterBoundary, rune(s[i]))
}

// WarnJSON is what a warning prints on stdout. It deliberately sets no
// `permissionDecision': "allow" would not merely un-block the call, it would
// bypass the normal permission prompt for it, which is a far bigger grant than
// this wants to make. Leaving the field out lets the call take its usual route.
//
// The message goes out on three channels because they reach different readers
// and not all are honoured by every version: `additionalContext' is the one
// the agent is meant to read, `systemMessage' surfaces to the user, and
// cmdGuard also writes the reason to stderr. Extra fields are ignored, so an
// unsupported one costs nothing.
func WarnJSON(reason string) string {
	out := map[string]any{
		"systemMessage": reason,
		"hookSpecificOutput": map[string]any{
			"hookEventName":     "PreToolUse",
			"additionalContext": reason,
		},
	}
	b, err := json.MarshalIndent(out, "", "  ")
	if err != nil {
		return ""
	}
	return string(b)
}

// DenyJSON is what a denial prints on stdout.
func DenyJSON(reason string) string {
	out := map[string]any{
		"hookSpecificOutput": map[string]any{
			"hookEventName":            "PreToolUse",
			"permissionDecision":       "deny",
			"permissionDecisionReason": reason,
		},
	}
	b, err := json.MarshalIndent(out, "", "  ")
	if err != nil {
		return ""
	}
	return string(b)
}
