# Zsh plugin layout

Selected directories under `zshlang/` are structured as installable Zsh plugins while remaining the single source used by the local loader.

## Conventions

- `*.plugin.zsh` files are loader entrypoints only.
- Implementation belongs in modular `.zsh` files in the same plugin directory or existing module directories.
- Public plugins should be idempotent and safe to source multiple times.
- Public plugin dependencies should be documented in each plugin README.

## Current entrypoints

- `zshlang/basic/basic.plugin.zsh` — minimal public helper layer.
- `zshlang/basic/basic-full.zsh` — full/opinionated local basic stack.
- `zshlang/plugins/psiphon/psiphon.plugin.zsh` — Psiphon wrapper; depends on `basic.plugin.zsh`.

## Loading order

External users should load the basic plugin before dependent plugins:

```zsh
source /path/to/.shells/zshlang/basic/basic.plugin.zsh
source /path/to/.shells/zshlang/plugins/psiphon/psiphon.plugin.zsh
```

The local loader sources `basic-full.zsh` from `load-first.zsh`; full basic sets the minimal basic guard so later plugin loads do not replace local definitions.
