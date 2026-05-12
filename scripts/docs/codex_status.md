# codex_status.py

`python/codex_status.py` starts `codex app-server` and prints account rate-limit status in either human-readable or JSON form.

Use `--retries N` to retry failed status checks `N` times after the first
attempt. Retries are per auth source, so `--all --retries 10` gives each auth
file up to 10 retry attempts before reporting its final error. The zsh
`codex-status` wrapper defaults to `--retries 10`; set `codex_status_retries`
to override it.

The status output includes identity context when available:

- plan type
- plan owner email
- workspace name

Workspace names are resolved from app-server account/rate-limit responses when present, then from non-`Personal` local Codex auth token claims. If Codex only exposes a workspace/account ID and the token organization title is `Personal`, the script does not present `Personal` as the active workspace name. As a local fallback, it can infer a workspace alias when `~/.codex/auth.json` is byte-identical to a sibling snapshot named `auth_<alias>.json`.

If no workspace name or alias can be resolved, the human-readable output prints `Workspace: n/a`.

If Codex reports that a refresh token has already been used for a checked auth
snapshot, `codex_status.py` looks in `~/tmp/.codex-auths` for a replacement auth
JSON with the same stable token identity. Matching uses token data such as
`tokens.account_id` and decoded `id_token` claims, not filenames. When a newer
matching auth is found, the status check is retried with that auth and
human-readable/JSON output records the recovery source. After a successful
retry, the recovered auth overwrites the failed snapshot so future checks use
the new refresh token.

When checking all auth files, the human-readable output uses each auth file path as
the section header, marks the snapshot matching `~/.codex/auth.json` with an
`[Active]` tag, and omits the redundant `Alias:` field. After the per-auth
details, it prints average primary and secondary usage across successful auths
that returned numeric usage values. The active auth is printed last in
human-readable status lists. JSON `--all` output includes the same aggregate
under `averageUsage`. When no checked auth has usable quota available, the
aggregate also includes `First Time to Reset`: the earliest 5-hour reset for
auths with weekly credit remaining, or the earliest weekly reset for auths
without weekly credit remaining. Human-readable output shows the auth alias
that will reset first, for example:

`First Time to Reset: some_alias in 2h 20m (2026-05-07 19:41:54 +0330)`

## Color

Human-readable output supports `--color {auto,always,never}`. When color is
enabled, `--true-color {on,off,auto}` controls RGB color output; `auto` detects
Kitty first, then `COLORTERM=truecolor|24bit`, then terminfo `RGB`/`Tc`.

True-color output has named themes. Use `--dark-mode {on,off,auto}` to select
dark or light theme mode; `auto` queries the terminal background color with OSC
11 when possible and falls back to environment heuristics. Dark themes are
`neon`, `ember`, and `ocean`; light themes are `day`, `paper`, and `mint`.
Select them with `--dark-theme NAME` and `--light-theme NAME`.

## Swap

`python/codex_status.py swap` checks every `~/.codex/auth*.json` snapshot and
replaces `~/.codex/auth.json` with the best eligible auth.

Selection prioritizes the auth with the lowest weekly usage
(`secondary.usedPercent`). Auths with exhausted 5-hour usage
(`primary.usedPercent >= 100`) or exhausted weekly usage
(`secondary.usedPercent >= 100`) are not eligible. If no auth has usable quota
remaining, `swap` exits nonzero and leaves `auth.json` unchanged.

Use `--dry-run` to inspect the selected auth without replacing `auth.json`.
Use `--json` to print the selected alias/path, previous active alias when
inferable, swap status, selected rate-limit summary, and the full checked auth
status list.

Human-readable swap output uses the same per-auth status blocks as status
output. The current active auth is tagged `[Active]`, the auth that was active
before swap selection is tagged `[Previously Active]`, and the selected block
adds `Previously active: <alias>` with the alias styled like the `Workspace:`
value. It also prints the same `Average usage` block as status output. If no
auth can be selected, the failure summary uses the heading `Swap Failed` after
the checked auth blocks and average usage.
