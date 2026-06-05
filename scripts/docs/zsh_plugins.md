# Zsh plugin layout

Selected directories under `zshlang/` are structured as installable Zsh plugins while remaining the single source used by the local loader.

## Conventions

- `*.plugin.zsh` files are loader entrypoints only.
- Implementation belongs in modular `.zsh` files in the same plugin directory or existing module directories.
- Public plugins should be idempotent and safe to source multiple times.
- Public plugin dependencies should be documented in each plugin README.
- Local loading should use an explicit list of desired plugins, not a broad glob over every public plugin.

## Current basic entrypoints

- `zshlang/basic/basic.plugin.zsh` — minimal public helper layer.
- `zshlang/basic/basic-full.zsh` — full/opinionated local basic stack.

## Generic loading pattern

External users should load a plugin's documented dependencies before the plugin itself. With a monorepo layout, prefer plugin-manager features that select a subdirectory or an explicit `*.plugin.zsh` file.

Manual source pattern:

```zsh
source /path/to/repo/zshlang/basic/basic.plugin.zsh
source /path/to/repo/path/to/plugin/plugin.plugin.zsh
```

The local loader sources `basic-full.zsh` from `load-first.zsh`; full basic sets the minimal basic guard so later plugin loads do not replace local definitions.
