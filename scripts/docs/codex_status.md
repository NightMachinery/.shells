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
`[Active]` tag, and omits the redundant `Alias:` field. The bare
`~/.codex/auth.json` is checked as its own source (labeled `auth`) only when its
bytes do not already match one of the `auth_<alias>.json` snapshots; when it is
byte-identical to an alias snapshot, that alias source already represents it and
the bare file is skipped to avoid a duplicate check. After the per-auth
details, it prints average primary and secondary usage across successful auths
that returned numeric usage values. The active auth is printed last in
human-readable status lists. JSON `--all` output includes the same aggregate
under `averageUsage`. When no checked auth has usable quota available, the
aggregate also includes `First Time to Reset`: the earliest 5-hour reset for
auths with weekly credit remaining, or the earliest weekly reset for auths
without weekly credit remaining. Human-readable output shows the auth alias
that will reset first, for example:

`First Time to Reset: some_alias in 2h 20m (2026-05-07 19:41:54 +0330)`

## Progress

When more than one auth file is checked and stderr is a TTY, a single-line
progress bar is rendered to stderr while the parallel status checks run, then
cleared before the report prints. It uses [tqdm](https://github.com/tqdm/tqdm)
when importable and falls back to a small ASCII bar (`Checking auths
[####----] 3/8`) otherwise, so the script still works under a Python without
tqdm installed. This applies to both `status --all` and `swap`. The bar is
suppressed when `--color never` is in effect or when stderr is not a terminal,
so piped/redirected output stays clean.

## Color

The color/theme layer (and a few generic env/format helpers) lives in
`python/libs/common_sub_status.py`, shared with `claude_code_usage.py` (see
`docs/claude_code_usage.md`).

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

## Reset notifications

`codex-status` can also arm a one-shot background job for when the rate limit
resets, the same job the Claude Code notifier uses; `docs/agent-usage-armed.md`
covers the job itself, its idle gate, its knobs and how to cancel it. This
section is what is Codex's: the deadline, and the delivery.

- `codex-status-notify` prints the ordinary report and arms a notification.
- `codex-status-continue-fz` prints the report and arms a resume: an fzf
  picker over every live Codex thread, multi-select, and at reset time the
  job queues `Continue.` into each one you chose.
- `h-codex-status-arm-schedule` arms without printing a report, for when
  the report is already in front of you. The `h-` says the `-notify` forms are
  the intended way in, not that it is off limits.
- `codex-status-armed-cancel` and `codex-status-armed-status` are wrappers
  over the shared `h-agent-usage-armed-cancel` / `-status` for Codex's one
  session, `codex-status-armed`.
- `h-codex-status-arm-auth <session> <alias>` arms for *one* auth's reset,
  reading that auth's own primary and secondary windows and taking the latest
  reset among those at or above `codex_status_arm_full_pct`. It exists for the
  `/auto-continue` watcher (`docs/agent-auto-continue.md`): a running thread
  is signed in to one auth and cannot `swap`, so "every auth exhausted" is the
  wrong question for it. `h-codex-status-active-alias` names the auth the
  running Codex was started with, by the script's own byte-match rule.

The deadline is `averageUsage.firstTimeToReset` from `codex-status --json`,
read with ANSI stripped. That field is only present when *every* checked auth
file is exhausted, and its absence is treated as "usage possible, not
arming": if another auth still has room, the right move is `swap`, not
waiting, and arming a notifier would only tell you hours later what the report
is telling you now. The report's `First Time to Reset` line already names the
alias that frees up first, and the notification repeats it, so when the job
fires you know which auth to swap to. Under `deus` the job arms for the
earliest primary (5-hour) reset across the auth files instead, whether or not
anything is exhausted, which is how to exercise the mechanism without first
running every account dry.

Delivery is `codex queue --thread <id> --message`, and only that: Codex
accepts a message for a thread by name, so the resume needs no kitty window,
no tmux pane, no focus and no awake display, and it cannot land in the wrong
place. That is why there are no kitty or tmux variants on the Codex side, where
Claude has three; queueing dominates both. The picker lists every live thread,
not only those showing in a window, because a window is not needed to reach
one. The thread id is `agent_session codex id-of <transcript>`.

The caveat from the shared doc applies unchanged: the job trusts the reset
time it was armed with and does not re-run `codex-status` when it fires.
