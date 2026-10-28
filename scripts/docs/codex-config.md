# Codex config under version control

`~/.codex/config.toml` cannot be tracked whole. Codex rewrites it on its own:
every folder you trust, dismissed notices, hook trust hashes, desktop app
preferences, and plugin and MCP entries holding this machine's home path. A
symlinked copy would dirty the repository after every session and carry one
Mac's paths and project names to the others.

So only the stable settings are tracked, in `configFiles/codex/config.base.toml`:
model and reasoning effort, personality, approvals reviewer, web search, the
`notify` hook, project root markers, agent thread limit, feature flags and the
TUI status line. [agfi:codex-config-sync] merges that file into
`~/.codex/config.toml`:

- tracked values win, so a change made in Codex's own settings is reverted on
  the next sync; make lasting changes in the tracked file;
- every other key stays exactly as Codex wrote it, and nothing is deleted;
- it writes atomically, refuses to write a file that does not parse, and keeps
  the file's mode.

[agfi:codex-m] runs it before each launch, after the instruction sync, and
launches anyway if it fails. Run `codex-config-sync --check` to list the keys
that differ without changing anything.

The tracked file is public. Keep machine paths, project names and anything
secret out of it.

The merge needs `tomlkit` to preserve Codex's layout. `uv run --with tomlkit`
provides it without installing anything; the first run on a machine downloads
it into uv's cache.
