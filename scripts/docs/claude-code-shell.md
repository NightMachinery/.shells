# Claude Code's shell

How Claude Code runs a Bash-tool command (and a `!` line typed at its prompt),
what that does to our zsh library, and what `.zshrc` does about it. Read from
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

## What `.zshrc` does

When `CLAUDECODE` is set and `SNAPSHOT_FILE` names a snapshot, `.zshrc`
removes every function, resets options with `emulate -R zsh`, and returns
before its interactive setup. The snapshot then holds no functions and no
options except `login`, so every command runs the functions and options
`.zshenv` loads from disk. The interactive setup (completion, zle widgets,
syntax highlighting, fzf-tab) never reached commands usefully anyway.

Aliases stay in the snapshot. Its `unalias -a` runs after `.zshenv`, so the
aliases `.zshenv` defines never survive into a command; the snapshot's copies
are the only aliases a command can have. They are session-start copies, and a
global alias such as `@RET` comes back as a plain one, so it only expands in
command position. Function bodies are unaffected: their aliases were expanded
when `.zshenv` defined them.

Checked with a headless session (`claude -p`): its snapshot had 0 function
bodies, one `setopt` and 891 aliases (78 KB), and its shell ran the resolver
from disk.

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
- A session that started before this change keeps its old snapshot until it
  ends. There, `setopt bareglobqual ; <command>` gets past the glob bug, and a
  nested `zsh -c '<command>'` runs the current functions.
