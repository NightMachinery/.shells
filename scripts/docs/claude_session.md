# claude_session.go

`golang/claude_session.go` renders Claude Code session transcripts
(`~/.claude/projects/**/*.jsonl`) into org-mode or markdown, and scans a
directory of them for the fuzzy session picker. The zsh wrappers live in
`zshlang/auto-load/others/claude-session.zsh`.

It is a scriptisto script, like `rust/*.rs`: the shebang is
`#!/usr/bin/env scriptisto`, and the build recipe sits in the
`scriptisto-begin` comment block at the top of the file. The first run
compiles it (about 7 seconds); after that scriptisto runs the cached binary
in roughly 30 milliseconds and rebuilds only when the source changes. The
program uses the Go standard library only, so `go build` needs no network
and no module cache — it works on a freshly bootstrapped, sudoless host.

## Commands

`claude_session.go render [flags] <session.jsonl>` writes the transcript to
stdout. Flags:

- `-format md|org` — output syntax. `md` (the default) is meant to be piped
  through pandoc; `org` emits org directly and is only a fallback, since
  message bodies stay markdown.
- `-max-block-lines N` — elide code blocks longer than N lines, leaving a
  `… [N lines elided]` marker. `0`, the default, never elides.
- `-diff` — render `Edit` tool calls as a unified diff. On by default.

`claude_session.go list [flags] <sessions-dir>` walks the directory for
`.jsonl` files and writes one TSV line per session, newest first:
`epoch`, absolute path, local time, path relative to the scanned directory,
and the first user message as a snippet. `-snippet-len N` caps the snippet
width (default 120).

## Why the session time is not the file mtime

The picker used to show each file's mtime, which drifts from the
conversation: Claude Code appends bookkeeping records (for instance
`{"type":"bridge-session"}`, which carries no timestamp) long after the last
message. In practice that put entries hours, and in one case two days, past
the last thing actually said. `list` therefore reports the newest timestamp
among the session's `user` and `assistant` records, and sorts by it. Files
with no timestamped message at all — empty or truncated ones — fall back to
mtime so they still appear in the picker rather than vanishing.

## How tool calls are rendered

The old jq renderer dumped every non-`Bash` tool input as compact JSON, so a
`Write` call arrived as one enormous line with every newline escaped as
`\n`. Now each input is taken apart:

- Scalars and short strings become bullets (`- **replace_all**: false`).
  Paths are abbreviated to `~`.
- Multi-line or long strings become their own fenced block, tagged with a
  language guessed from the input's `file_path` extension. `command` is
  always `zsh`.
- Nested arrays and objects become indented JSON.
- Anything already shown in the heading is not repeated as a bullet.

Tool-use headings carry a short summary so a folded outline stays scannable:
the path for `Read`/`Write`/`Edit` (with the line range for `Read`), the
description for `Bash` and `Agent`, the pattern for `Glob`/`Grep`, the URL
for `WebFetch`.

`Edit` calls render as a unified diff with three lines of context, produced
by a line-based LCS in the script itself — no `diff(1)` fork, no temporary
files. The output matches `diff -U3` byte for byte. Inputs large enough to
make the quadratic LCS table wasteful fall back to printing `old_string` and
`new_string` as two separate blocks.

## Prose fields and heading levels

`plan` and `prompt` are markdown, not code, so they are emitted as prose and
pandoc turns them into real org markup. To stop a plan's own `## Foo` from
escaping the transcript's outline, its headings are renumbered so the
shallowest one sits directly under the enclosing heading, keeping their
relative depths. This mirrors `[agfi:org-header-rm-shared-level]` followed
by `[agfi:org-header-indent]`, with one addition those cannot make: headings
inside fenced code blocks are skipped, because a shell snippet's
`# comment` lines are not headings. Assistant and user message text gets the
same treatment.

## pandoc

The org conversion runs `pandoc --from=gfm-gfm_auto_identifiers --to=org`.
Without `-gfm_auto_identifiers`, pandoc attaches a three-line
`:PROPERTIES:`/`:CUSTOM_ID:`/`:END:` drawer to every heading, which nothing
in these exports links to.

Avoid the em dash in generated headings: pandoc's org writer rewrites it as
`---`. The tool-use headings use a middle dot, which survives.

## zsh entry points

- `claude-code-view-session-fz` — pick a session, convert to org, open in
  emacs.
- `claude-code-view-session-md-fz` — same, as markdown.
- `claude-code-view-session-raw-fz` — open the original `.jsonl`.

Each has a `-all-fz` variant that selects from every project's sessions
instead of the current directory's.

Variables:

- `claude_code_view_session_fz_scope` — `project` (default) or `all`.
- `claude_code_view_session_fz_projects_dir` — defaults to
  `~/.claude/projects`.
- `claude_code_view_session_fz_fz_opts` — extra options for `fz`.
- `claude_code_session_max_block_lines` — passed to `-max-block-lines`.
- `claude_code_session_diff_p` — `n` disables diff rendering for `Edit`.
