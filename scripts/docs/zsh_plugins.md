# Zsh plugin layout

Selected directories under `scripts/zshlang/` are structured as installable Zsh plugins while remaining the single source used by the local loader.

## Conventions

- `*.plugin.zsh` files are loader entrypoints only.
- Implementation belongs in modular `.zsh` files in the same plugin directory or existing module directories.
- Public plugins should be idempotent and safe to source multiple times.
- Public plugin dependencies should be documented in each plugin README.
- Local loading should use an explicit list of desired plugins, not a broad glob over every public plugin.

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
