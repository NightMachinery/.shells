# Claude Code's shell

How Claude Code runs a Bash-tool command (and a `!` line typed at its prompt),
what that does to our zsh library, and what we do about it. Read from
Claude Code 2.1.280.

## How a command runs

At session start Claude Code builds a **shell snapshot**: it runs
`zsh -c -l <script>` with `CLAUDECODE=1`, where the script sets
`SNAPSHOT_FILE=~/.claude/shell-snapshots/snapshot-zsh-<time>-<id>.sh`, sources
`~/.zshrc`, and writes into that file:

- `unalias -a`;
- the body of every function then defined (`typeset -f`), except names with a
  single leading underscore;
- every option that differs from zsh's defaults, as `setopt` lines;
- every alias (at most 1000), as `alias --` lines;
- Claude Code's own helpers: an `rg` fallback, and `grep`, `find` and `pkill`
  wrappers;
- `export PATH=` with Claude Code's own `PATH` plus plugin `bin/` directories.

Every command then runs as

    zsh -c 'source <snapshot> && setopt NO_EXTENDED_GLOB NO_BARE_GLOB_QUAL && ... && eval <command>'

`zsh -c` reads `.zshenv` first, which loads the whole zshlang library from
disk. The snapshot is sourced after it, so whatever the snapshot holds wins.
The file is deleted when the session exits.

## What went wrong

With a full `.zshrc` the snapshot held about 7,600 function bodies (4 MB). They
replaced the current definitions `.zshenv` had just loaded, so a function fixed
during a session kept its old body in that session's commands until a new
session started, and sourcing the file added about 0.15s to every command.
`2paseo` from a `!` line kept failing after its resolver had been fixed for
exactly this reason.

## What we do about it

The snapshot now carries nothing of ours, and `.zshenv` supplies everything
fresh for every command. Two pieces:

- **`.zshrc`.** When `CLAUDECODE` is set and `SNAPSHOT_FILE` names a snapshot,
  it drops the ZERR trap (its handler is about to go), removes every function
  and every alias, resets options with `emulate -R zsh`, and returns before its
  interactive setup. The snapshot then holds no functions, no aliases and no
  options except `login`. The interactive setup (completion, zle widgets,
  syntax highlighting, fzf-tab) never reached commands usefully anyway.
- **`unalias` (`zshlang/auto-load/others/claude-code-shell.zsh`).** The
  snapshot's opening `unalias -a` runs after `.zshenv` and would delete every
  alias it just defined. Under `CLAUDECODE`, `unalias` is a function that skips
  exactly that call, and only for a *lean* snapshot: one whose `# Functions`
  header is followed directly by the next header. Every other call, including
  the snapshot's own `unalias grep` before Claude Code's `grep` wrapper, goes
  to the builtin.

The lean check matters. A snapshot written before this change still holds
every function body, and those are parsed as it is sourced. With our aliases
still live, a definition such as `ls () {` expands `ls` and becomes a parse
error, which stops the file there: the remaining functions, Claude Code's
helpers and its `PATH` line are all lost. The first version of the wrapper did
exactly that to every running session for a few minutes on 2026-10-03. A
snapshot whose layout the check does not recognise gets the real `unalias -a`,
so the worst case is a command with no aliases, never a half-read snapshot.

So in a session started after the change, a command has the functions,
options and aliases `.zshenv` loads from disk. Global aliases (`@RET`, `...`)
stay global, so an unquoted `...` word in a command is expanded too. Claude
Code's own wrappers are unchanged: their bodies parse the same with our
aliases live (compared against a bare `zsh -f`).

Checked with a headless session (`claude -p --permission-mode default
--allowedTools Bash`, having it `source` a probe script, since a child `zsh`
loads `.zshenv` fresh and proves nothing): its snapshot had no function bodies
and no aliases, and its shell had 878 aliases, the 12 global ones, and the
resolver from disk. `zshlang/tests/claude-code-shell.zsh` covers the wrapper
against a lean and an old snapshot.

## What no setting changes

- The option preamble. `setopt NO_EXTENDED_GLOB NO_BARE_GLOB_QUAL` is added
  whenever the shell path contains `zsh` (and also when
  `CLAUDE_CODE_SHELL_PREFIX` is set). A function that uses a bare glob
  qualifier must set `bareglobqual` itself; see the rule in
  [Zsh.org](../PE/Zsh.org) and `zshlang/tests/agent-glob-qualifiers.zsh`.
  Turning `extendedglob` back on globally would also break commands such as
  `git show HEAD^`.
- The snapshot itself. Only an internal flag skips it; the Bash tool never
  sets it.
- A session that started before this change keeps its old snapshot, and its
  session-start functions and aliases, until it ends. There,
  `setopt bareglobqual ; <command>` gets past the glob bug, and a nested
  `zsh -c '<command>'` runs the current functions.
