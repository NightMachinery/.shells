# codex_status.py

`python/codex_status.py` starts `codex app-server` and prints account rate-limit status in either human-readable or JSON form.

The status output includes identity context when available:

- plan type
- plan owner email
- workspace name

Workspace names are resolved from app-server account/rate-limit responses when present, then from non-`Personal` local Codex auth token claims. If Codex only exposes a workspace/account ID and the token organization title is `Personal`, the script does not present `Personal` as the active workspace name. As a local fallback, it can infer a workspace alias when `~/.codex/auth.json` is byte-identical to a sibling snapshot named `auth_<alias>.json`.

If no workspace name or alias can be resolved, the human-readable output prints `Workspace: n/a`.

## Swap

`python/codex_status.py swap` checks every `~/.codex/auth*.json` snapshot and
replaces `~/.codex/auth.json` with the best eligible auth.

Selection prioritizes the auth with the lowest weekly usage
(`secondary.usedPercent`). Auths with exhausted 5-hour usage
(`primary.usedPercent >= 100`) are not eligible. If no auth has 5-hour credit
remaining, `swap` exits nonzero and leaves `auth.json` unchanged.

Use `--dry-run` to inspect the selected auth without replacing `auth.json`.
Use `--json` to print the selected alias/path, previous active alias when
inferable, swap status, selected rate-limit summary, and the full checked auth
status list.
