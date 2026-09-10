# Agent sessions: one hotkey, one picker, three agents

Claude Code, Codex and Antigravity (`agy`) all keep a transcript of every
session on disk, and all three are useless to you there: a directory of JSONL
under a name you did not choose. This is the family of helpers that turns any
of them into something you can read, resume and jump to -- the same key, the
same picker, the same org file, whichever agent wrote it.

Start here:

- `cmd+shift+o` in kitty opens the session showing in the focused window as an
  org file in emacs. It works out which agent that is by itself.
- `agent-view-session-fz` picks a session from disk and opens it; add `-all-fz`
  for every project rather than this one.
- `agent-session-live-fz` picks from the sessions running right now.
- `agent-session-resume-fz` resumes one, handing off to that agent's own resume
  command.

Everything below is why those four work, and what to change when they do not.

## The agent travels with the transcript path

Nothing in this family passes an agent token around. A transcript path is the
handle for a session, and [agfi:h-agent-session-agent-of] recovers the agent
from the path by asking each adapter's `owns-p` verb, which is a glob against
that agent's store.

This is not tidiness. The hotkey's real work happens in a `tmuxnewsh2` session
started by [agfi:h-agent-view-job], a different process, where zsh's dynamic
scope does not reach; the registry is a file whose contents are one line; fzf
rows cross into a `zsh -f` wrapper that cannot call functions at all. A token
would have to survive all three, and a path already does.

The one case a path cannot answer is a transcript copied out of its store, for
which [agfi:h-agent-session-sniff] reads the first record's shape and guesses.
It is the last resort, not the mechanism.

Only the functions that have no transcript yet take an agent argument: `roots`,
`list`, `live-list`, `current-*` and the hook bodies.

## Window to session, in order of how much each can promise

[agfi:h-agent-session-of-kitty-window] is what the hotkey leans on. It is given
kitty's `ls` JSON and a window id, and tries four things:

1. **The agent runs in the window.** A foreground pid of the window equals a
   live session's pid. Nothing to go stale here.
2. **The window shows a tmux client.** The autoname hooks record the identity
   on the tmux session as the `@agent_session` user option -- agent, id and
   transcript, tab separated -- so that option answers directly, for any agent.
   Failing that, the session name is matched against the live rows' tmux
   column, and only an unambiguous single hit counts: one tmux session can host
   several agents in several panes.
3. **The window shows an agent's own view** (`claude agents`, `claude attach`).
   Those set the window title to the attached session's name, sometimes behind
   a status glyph. That is observed rather than documented, so it counts only
   when a foreground process is one of the agents' binaries and the title
   matches exactly one live session.
4. **The registry.** A file per kitty window under
   `$XDG_STATE_HOME/agent-sessions`, written by the hooks, saying where the
   session was showing the last time it was prompted.

The first three read only what is true this instant. The fourth is a memory,
so it comes last. When all four come up empty the hotkey does not guess: it
opens the picker as a kitty overlay over the window
([agfi:h-agent-session-pick-overlay]), and `enter` there opens the choice in
the background under the same band.

## The adapter contract

An adapter is a set of zsh functions named `h-<agent>-session-<verb>`, and
[agfi:h-agent-session-call] dispatches to them. It returns 2 when a verb is not
defined, so "this agent cannot do that" stays distinguishable from "that
failed".

The verbs, all of them arguments in and lines out, tab separated where a line
has fields:

- `roots` -- one store root per line, and failure when the agent is not
  installed on this host, which is how such a host contributes no rows.
- `owns-p <path>` -- exit status only, a glob against the roots.
- `id-of <transcript>` -- the agent's own id for that session.
- `resolve <id-or-prefix>` -- matching transcripts, one per line. The core's
  [agfi:h-agent-session-resolve] takes a path as itself, else collects across
  agents and complains about none or several.
- `current-id` -- the id of the session this shell is inside, from the agent's
  environment.
- `live-list` -- seven columns: pid, id, name, cwd, transcript, tmux session or
  `-`, status.
- `resume <transcript> [args]` -- executes the agent's resume.
- `hook-transcript [payload]` -- the transcript a hook payload is about, with
  the payload in `$1` or on stdin.
- `name <transcript>` and `tmux-name <transcript>` are optional; an agent that
  omits `name` gets the Go binary's answer.

### Adding a fourth agent

1. A row in [agfi:h-agents-table]: token, label, glyph, the binary names its
   process runs under, and its launcher function.
2. A `<agent>-session.zsh` with the verbs above, plus the everyday aliases,
   which are one line each because they are the shared commands with
   `agent_session_agents=<token>` in front.
3. A Go package `internal/<agent>` satisfying `session.Adapter`, and one line
   each in `agentNames` and `adapters` in `main.go`. See
   `golang/agent_session/readme.org`.
4. A hook that calls `agent-session-register <token>`, if the agent has hooks.

Nothing else. The pickers, the previews, the band, the toggle, the reaper and
the resolver are already agent-neutral, and `go install` is the step people
forget: the zsh side calls the installed binary, so a new adapter that is not
installed shows up as an agent whose rows are simply missing.

## Where each agent keeps its sessions, and how liveness is decided

**Claude Code** writes one JSONL per session under
`~/.claude*/projects/<mangled-cwd>/<uuid>.jsonl`, one projects directory per
profile. Liveness is authoritative: `claude agents --json` answers it, and the
Go side asks every config home in parallel.

**Codex** writes a rollout per thread at
`~/.codex/sessions/YYYY/MM/DD/rollout-<local-timestamp>-<uuid>.jsonl`. Names
live in `~/.codex/session_index.jsonl`, which is append-only, so the last line
for an id wins and an empty name clears rather than sets. Liveness is exact
without being documented: a running Codex holds
`~/.codex/thread-writer-locks/<id>.lock` *open*, so one `lsof` says which
process owns which thread. The holder is the native binary, and the pid kitty
can see is the `node .../codex.js` launcher above it, so the adapter walks up
to the topmost Codex ancestor. The ChatGPT desktop app holds these locks too,
which is exactly why that walk is not optional.

**Antigravity** gives each conversation a directory,
`~/.gemini/antigravity-cli/brain/<id>/`, with the readable record at
`.system_generated/logs/transcript_full.jsonl` and a truncated
`transcript.jsonl` beside it. Titles live in `conversation_summaries.db`,
mirrored into `cache/conversation_metadata.json`. Liveness is the weakest of
the three: a running `agy` is paired to a conversation through its cwd and
`cache/last_conversations.json`, the same map agy itself uses to resume in a
directory. That is right whenever a workspace hosts one conversation at a time,
and when it does not, the hooks' record on the tmux session settles it.

The rule all three follow is that a live row names only a process that was
really traced. A session that cannot be paired to one gets no row rather than a
plausible pid, and a session with a pid but no tmux session carries `-` in that
column, which no window name can match. This is load-bearing: the resolver
matches windows against these rows, so an invented pid would not show up as a
miss, it would open the wrong transcript silently. A missing row opens the
picker instead.

The reading itself is all in one Go binary, run as
`agent_session <agent> <subcommand>`, whose `readme.org` covers the record
models, the org and markdown writer, the parallel pandoc path and the
performance work. The zsh side keeps the policy; Go does the work.

## Hooks, the registry, and trust

The registry is insurance, not the mechanism. [agfi:agent-session-register]
records the one fact a hook has that nothing else does: when a prompt hook
fires, Enter was just pressed, so the kitty window focused right now is the one
showing this session -- whatever the attach mechanism, including ones that do
not exist yet.

Two guards keep the record honest, plus a third for agy. No focused kitty
window means the prompt did not come from one, so nothing is written. A focused
window that resolves to a *different* session means the prompt was injected
while you sat elsewhere, so nothing is written. And a focused window running no
agent at all is not showing this session either, which matters because
Antigravity's `PreInvocation` fires once per model call rather than once per
prompt, by which time the focus may have moved to an ordinary shell.

The wiring, per agent:

- **Claude Code**: `configFiles/claude-code/settings.json`, `SessionStart` and
  `UserPromptSubmit`, still calling the `claude-code-session-register` shim.
- **Codex**: `configFiles/codex/hooks.json`, the same two events, as a second
  handler object beside the autoname one. Codex trusts handlers *by hash*, so
  every edit to that file makes its handlers untrusted until you accept them
  once in the TUI's startup review. Worse, a handler's non-JSON stdout on exit
  0 is injected into the model's context as extra context, which is why every
  command there ends in `>/dev/null 2>&1`.
- **Antigravity**: `configFiles/antigravity/hooks.json`, under its own
  top-level group `kitty-register`, on `PreInvocation` and `Stop`. That file is
  keyed by group name, and for a non-tool event the handler object goes
  *directly* in the list: wrapping it in a matcher group makes agy discard the
  whole file silently. Each command ends `; echo '{}'`.

All of them go through the brish garden with `brishz_async=y`, so no agent ever
waits on this, and `agents-md-doctor` checks that the symlinks are still
symlinks and not files an app has replaced.

## Knobs

Dynamically scoped, so they go in front of a call. Every one of them also
honours its old `claude_code_*` spelling, because these functions were the
Claude helpers first: `agent_session_max_block_lines`,
`agent_session_render_jobs`, `agent_session_render_jobs_max`,
`agent_session_diff_p`, `agent_session_subagents_p`, `agent_session_fz_scope`,
`agent_session_fz_subagents_p`, `agent_session_fz_header`,
`agent_session_fz_opts`, `agent_session_live_fz_opts`,
`agent_session_live_fz_no_multi_p`, `agent_session_live_fz_extra_rows`,
`agent_session_live_rows_scope`, `agent_session_live_list_cache`,
`agent_session_preview_bytes`, `agent_session_preview_color_p`,
`agent_session_registry_dir`, `agent_session_kitty_socket_glob`,
`agent_session_resume_scope` and `agent_view_session_prefix`.

Two are new and belong to this design rather than to Claude:

- `agent_session_agents` -- whitespace separated agent tokens, narrowing every
  picker and resolver. This is the whole implementation of the per-agent
  commands: `codex-resume-fz` is `agent_session_agents=codex
  agent-session-resume-fz`, and the Claude compat names are the same trick.
- `agent_launch_glyph` and `agent_launch_sync_p` -- the tab-title glyph and
  whether to sync the instruction files, for the shared launcher preamble
  [agfi:h-agent-launch] that the three launchers now share instead of each
  carrying its own copy.

## The names that were kept

The Claude-only spellings all still work, as `aliasfn`s that set
`agent_session_agents=claude` where the old name meant Claude alone:
`claude-code-view-session` and its `-fz`, `-all-fz`, `-md-fz`, `-md-all-fz`,
`-raw-fz`, `-raw-all-fz`, `-focused`, `-bg` and `-toggle` variants,
`claude-code-view-sessions`, `claude-code-view-reap`,
`claude-code-session-live-fz`, `claude-code-session-register`,
`claude-session-selftest`, the `h-claude-code-session-*` helpers, and
`claude_session` for `agent_session claude`. The profile machinery
(`claude-code-session-import`, `claude-code-session-resume`, the
`claude-resume*` family) stayed Claude-only on purpose: profiles are a Claude
Code idea, and Codex and Antigravity have nothing to map them onto.

`docs/claude_code_usage.md` covers the usage limit notifier, whose resume
target picker now spans all three agents, and `docs/tmux-session-rename.md`
covers the autoname hooks that write the `@agent_session` option this family
reads.

## Checking it by hand

The Go tests are synthetic and run themselves. What is worth pressing after a
change here, in rough order of how much it would hurt to lose:

- `cmd+shift+o` in a bare Claude Code window, behind a `tmux a`, in a Codex
  TUI, and in an agy window.
- The same key on a plain shell window, which should give the overlay picker,
  with `alt+enter` opening a row in the background and a second press
  cancelling it.
- A double-tap of `cmd+shift+o` on an agent window, which must cancel rather
  than start a second conversion.
- `codex-resume-fz` and `agy-resume-fz`, and `agent-session-live-fz` with more
  than one agent running, to see the glyph column and per-row previews.
- `agent-session-selftest`, which runs the Go tests and then the pandoc parity
  check over your real transcripts.
- `agents-md-doctor`, after any hook or settings change.

`brishz-restart` after every zsh edit: the garden holds persistent shells and
sees none of it otherwise, and every hook here runs inside the garden.

## When a converted transcript opens as octal escapes

If an org file shows `\302\267` where a `·` should be, and the prose around it
is mangled wherever it was not ASCII, the file is not broken: emacs decoded it
as binary. One NUL byte anywhere in a file is enough for that, and tool output
gets one whenever a command printed a binary file. Invalid UTF-8 has the same
effect one step down, giving `Â·` from a latin-1 fallback.

Two things now prevent it. The renderer drops the control bytes that are never
text and turns invalid UTF-8 into a replacement character, and the converter
writes a `-*- coding: utf-8 -*-` cookie as the file's first line, which settles
the encoding whatever emacs would have sniffed. Either one alone is enough.

For a file this pipeline did not write, `inhibit-null-byte-detection` set to
`t` makes emacs stop treating a NUL as a reason to open something as binary,
and `C-x RET r utf-8 RET` re-reads the buffer you already have.
