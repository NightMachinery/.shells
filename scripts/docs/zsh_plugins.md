# Zsh plugin layout

Selected directories under `scripts/zshlang/` are structured as installable Zsh plugins while remaining the single source used by the local loader.

## Conventions

- `*.plugin.zsh` files are loader entrypoints only.
- Implementation belongs in modular `.zsh` files in the same plugin directory or existing module directories.
- Public plugins should be idempotent and safe to source multiple times.
- Public plugin dependencies should be documented in each plugin README.
- Local loading should use an explicit list of desired plugins, not a broad glob over every public plugin.

## Bash compatibility

A few files under `zshlang/` are sourced by bash as well as zsh: `~/.bashrc`
bootstraps itself from `zshlang/basic/conditions.zsh`, so a bash shell gets the
same predicates ([agfi:isI], [agfi:isBash], [agfi:isDarwin]) as a zsh one. Such
a file declares the contract in its own header, as a `### BASH COMPATIBLE`
comment. `conditions.zsh` and `conditions-personal.zsh` carry it today.

Inside one, write only syntax both shells accept. Glob alternation is the easy
mistake: `[[ "$x" == (a|b)* ]]` is zsh-only and bash rejects it, where two
patterns joined with `||` work in both.

Getting this wrong fails far more quietly than it looks, which is why it needs
a test rather than care. A syntax error aborts `source` for the *remainder* of
the file, so bash keeps every function defined above the bad line and silently
loses every one below it. When [agfi:antigravity-p] picked up a zsh glob, bash
still had [agfi:isBash] from higher up and ran most of `~/.bashrc` normally,
but [agfi:isI] sits below and was gone: a parse error plus one "command not
found" on the stderr of every non-interactive `ssh`, and the arrow-key
history-search bindings guarded by `isI` stopped being set at all.

Regression check: `zsh -f zshlang/tests/bash-compatible.zsh`. It scans the tree
for the marker and runs `bash -n` on each file it finds, so adding the marker
to a new file enrols it without editing the test. It fails if the scan finds
nothing, since a typo in the marker would otherwise make it pass vacuously, and
it treats any `bash -n` output as a failure because some bash builds print a
`[[ ... ]]` parse error and still exit 0.

## Current basic entrypoints

- `scripts/zshlang/basic/basic.plugin.zsh` — minimal public helper layer.
- `scripts/zshlang/basic/basic-full.zsh` — full/opinionated local basic stack.

## Generic loading pattern

External users should load a plugin's documented dependencies before the plugin itself. With a monorepo layout, prefer plugin-manager features that select a subdirectory or an explicit `*.plugin.zsh` file.

Manual source pattern:

```zsh
source /path/to/repo/scripts/zshlang/basic/basic.plugin.zsh
source /path/to/repo/scripts/zshlang/plugins/<plugin>/<plugin>.plugin.zsh
```

The local loader sources `scripts/zshlang/basic/basic-full.zsh` from `scripts/zshlang/load-first.zsh`; full basic sets the minimal basic guard so later plugin loads do not replace local definitions.

## Authoring skill

Use the shared [zsh-plugin-authoring skill](../configFiles/agent-skills/zsh-plugin-authoring/SKILL.md)
to create or extract a plugin using these conventions. [agfi:agent-skills-link]
exposes it to the configured agents through the existing shared-skill mechanism.

## Agent session plugin

`zshlang/plugins/agent-session` is the portable exact-ID resume and tmux identity
layer shared with tmux-subagents. It loads without the personal basic stack;
local transcript discovery, profile lookup and pickers remain in auto-loaded
modules. Its README covers direct and subdirectory installation and private pane
state. [agfi:h-agent-session-resume-argv] is shared by the local provider adapters
and [agfi:agent-session-resume-exact].

## tmux-z plugin

`zshlang/plugins/tmux-z` holds the tmux session primitives
([agfi:tmux-session-id], [agfi:tmux-session-goto], [agfi:tmux-alive-p],
[agfi:tmux-ensure-attach]), the [agfi:tmuxnewsh2] launchers, and
[agfi:tmuxnewsh2-attach-z], which opens a session in whatever directory `z`
finds. It exists as a plugin because `setup/minimal_proxy/.shared.sh` used to
carry a second copy of those helpers, which had drifted. Its README covers
direct and plugin-manager installation; `docs/tmux-z-launchers.md` covers the
launchers themselves.
