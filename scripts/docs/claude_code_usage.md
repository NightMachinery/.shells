# claude_code_usage.py

`python/claude_code_usage.py` prints the usage stats of the current Claude Code
plan (what the in-app `/usage` shows: 5-hour session utilization, weekly
utilization, model-scoped weekly windows, reset times) in either human-readable
or JSON form. The zsh wrapper is `claude-code-usage` (alias `ccu`) in
`zshlang/auto-load/others/claude.zsh`.

## Data source

The script queries `GET https://api.anthropic.com/api/oauth/usage`, the same
undocumented endpoint Claude Code itself uses (also used by community tools
such as claude-powerline and ccstatusline). Because it is undocumented, its
schema and the required `anthropic-beta: oauth-2025-04-20` header may change
without notice.

The request must send a `User-Agent` that looks like Claude Code
(`claude-code/<version>`); other user agents land in an aggressively
rate-limited bucket that returns persistent 429s. If the pinned default
version string ever stops working, override it with `--user-agent` or
`CLAUDE_CODE_USAGE_USER_AGENT`.

Newer responses carry an authoritative `limits` array (integer percents,
severity, model-scoped weekly windows such as a per-model 7-day limit); the
script prefers it and falls back to the legacy `five_hour` / `seven_day` /
`seven_day_*` window objects when `limits` is absent. Unknown window keys are
rendered generically rather than dropped. An `extra_usage` line is shown only
when extra usage is enabled or credits have been spent.

## Profiles

Claude Code supports several logged-in accounts side by side, selected with
`CLAUDE_CONFIG_DIR`. Each keeps its own config file:

- the default profile has no `CLAUDE_CONFIG_DIR` and its config lives at
  `~/.claude.json` — home root, *not* inside `~/.claude/`, which has no
  `.claude.json` at all;
- any other profile sets `CLAUDE_CONFIG_DIR` and keeps its config at
  `<dir>/.claude.json`. `claude-work` in `zshlang/auto-load/others/claude.zsh`
  uses `~/.claude-work`.

Pass that directory as `--config-dir`, and omit it for the default profile. It
is deliberately *not* defaulted from `CLAUDE_CONFIG_DIR` itself, so running the
script from inside a Claude Code session cannot silently change which account
gets reported. `--profile-label` sets the name shown in the header.

Give each profile its own `--cache-dir`, or they overwrite each other's cached
response.

## Credentials

The OAuth access token is looked up in this order:

1. the `CLAUDE_CODE_OAUTH_TOKEN` environment variable,
2. the macOS Keychain generic password for this profile (read via
   `security find-generic-password`),
3. `<config dir>/.credentials.json`, then `~/.claude/.credentials.json` (Linux),
4. the profile's own usage cache — see Caching below.

The Keychain service name is derived exactly the way Claude Code derives it:

```
service = "Claude Code-credentials" + suffix
suffix  = ""                                     when no config dir is in play
        = "-" + sha256(configDir NFC).hex[0:8]   otherwise
account = $USER   (falling back to the login name; anything outside
                   [a-zA-Z0-9._-] becomes "claude-code-user")
```

The hash suffix goes at the **end**, after `-credentials`, and
`CLAUDE_SECURESTORAGE_CONFIG_DIR` takes the place of the config dir when it is
set. So a profile at `~/.claude-work` resolves to
`Claude Code-credentials-<8 hex>` while the default profile stays on the bare
`Claude Code-credentials`. The path is hashed verbatim, so a trailing slash
yields a different service name — it has to match what Claude Code itself was
given.

Deriving both service and account means the right item is picked
deterministically even with several accounts logged in. Previously the script
probed the account names Claude Code has used over time (no filter, the login
username, `unknown`) and took whichever token had the freshest expiry, which on
a machine with two profiles picked one of them arbitrarily and did not say
which. That probe survives as a fallback, but only for the default profile and
only when the derived account yields nothing, so an install from an older
Claude Code still resolves. `--keychain-service` and `--keychain-account`
override the derivation should a future build change it.

A Keychain item cannot be attributed to a profile from its contents: the usage
payload carries no account id, and the access tokens are opaque rather than
JWTs. There is also nothing else to cross-check against — in particular
`resets_at` is *not* a fingerprint, because it is recomputed on every response
(two fields of a single response differ in their microseconds) and the 5-hour
window re-anchors whenever a session starts after a gap. So instead of guessing,
`--json` reports the `keychain` service and account actually used, which makes a
wrong derivation visible by eye. The chosen account also appears in the
human-readable output source, e.g. `(keychain:evar)`.

Keychain items are ACL'd per item, and `security` is not the application that
created them, so the *first* read of each one can pop a one-time authorization
prompt. That is per profile: granting access for the default profile does
nothing for a newly added one.

A read that fails for any reason other than "no such item" — a locked keychain,
a denied or unanswered prompt — is reported as exactly that, rather than as an
absent credential. The distinction matters: treating them alike turns a locked
keychain into a silent fallback onto hours-old cached numbers that look
perfectly normal. `security` signals a genuinely missing item with exit 44;
anything else is surfaced as `could not read the Keychain credential ...`.

Because an authorization prompt has to be answered by a human, the Keychain read
gets its own `--keychain-timeout` (default 30s) rather than sharing the HTTP
`--timeout` (default 10s), which was short enough to expire while the dialog was
still on screen.

The Keychain/file credential also provides the plan name (`subscriptionType`)
and token expiry. An expired token only produces a warning — the request is
still attempted, since Claude Code may have refreshed the Keychain entry. The
script never refreshes the token itself; open `claude` (or run `/login` inside
it) to refresh.

## Caching

Successful responses are cached in `~/tmp/.claude-usage/<profile>/usage.json`
(`--cache-dir` to relocate; the profile name is always appended, since profiles
share the endpoint but not the account and would otherwise overwrite each
other's cached response). Cached data younger than `--cache-ttl` seconds
(default 300) is reused without hitting the network; `--refresh` skips the
cache read but still updates the cache afterwards. Do not disable caching in
tight loops — the endpoint rate-limits quickly and recovers slowly.

If a fetch fails (401, 429, network) and any cache exists — even an expired
one — the cached data is shown with a red `[stale cache: ...]` annotation and
the script exits 0.

If no token resolves at all, or a fetch fails and there is no cached response,
the script falls back to the profile's *own* cache. Claude Code stores the whole
usage payload under `cachedUsageUtilization` in each profile's `.claude.json`,
in exactly the shape the endpoint returns, so this path needs no credentials and
no network. It is annotated `[local cache: 56m ago]` together with the reason
live data was unavailable. It is only as fresh as the last Claude Code session
in that profile, so a window whose `resets_at` has already passed is annotated
`(rolled over)`; the recorded percentage is left alone rather than rewritten to
zero, which would invent a reading that was never taken. Only when that cache is
missing too is the error fatal (exit 1).

## Flags and environment variables

Each entry is the flag, then the environment variables it falls back to, then
the default.

- `--json` — no env fallback; off.
- `--timeout` — `claude_code_usage_timeout_s` / `CLAUDE_CODE_USAGE_TIMEOUT_S`; 10.
- `--keychain-timeout` — `claude_code_usage_keychain_timeout_s` /
  `CLAUDE_CODE_USAGE_KEYCHAIN_TIMEOUT_S`; 30. Separate from `--timeout` because
  a Keychain read can block on an authorization prompt.
- `--cache-ttl` — `claude_code_usage_cache_ttl_s` /
  `CLAUDE_CODE_USAGE_CACHE_TTL_S`; 300.
- `--refresh` — no env fallback; off.
- `--cache-dir` — `claude_code_usage_cache_dir` / `CLAUDE_CODE_USAGE_CACHE_DIR`;
  `~/tmp/.claude-usage`.
- `--user-agent` — `claude_code_usage_user_agent` /
  `CLAUDE_CODE_USAGE_USER_AGENT`; `claude-code/2.1.220`.
- `--config-dir` — `claude_code_usage_config_dir` /
  `CLAUDE_CODE_USAGE_CONFIG_DIR`; empty, meaning the default profile.
- `--all` — no env fallback; off. Reports every `--profile` given, fetching them
  concurrently.
- `--profile NAME=CONFIG_DIR` — no env fallback; repeatable, used with `--all`.
  An empty `CONFIG_DIR` means the default profile.
- `--workers` — no env fallback; 8. Maximum concurrent profile fetches.
- `--profile-label` — `claude_code_usage_profile_label` /
  `CLAUDE_CODE_USAGE_PROFILE_LABEL`; empty.
- `--keychain-service`, `--keychain-account` — no env fallback; both derived as
  described under Credentials.

`--json` output contains normalized `windows` (percent, epoch and ISO reset
times, severity, is_active) plus the `raw` payload for forward compatibility. It
also reports `profile`, `error`, the data `source` (`api`, `api-cache` or
`config-cache`), the `keychain` service and account used, and any `warnings`.
With `--all` it is an array of those objects, one per profile, in the order the
profiles were given; without it, a single bare object as before.

A profile that yields nothing at all renders as its own section carrying the
error, rather than aborting the run, and makes the exit status 1. So one dead
profile never costs the others their report.

## Zsh wrapper

`claude-code-usage` in `zshlang/auto-load/others/claude.zsh` reports one
profile; `claude-code-usage-all` reports every registered profile, and is what
the bare `ccu` / `ccs` / `claude-code-status` names run.

Profiles are registered in the `claude_code_profiles` associative array, which
maps a profile name to its `CLAUDE_CONFIG_DIR` (empty for the default profile),
and are ordered by `claude_code_profile_order`. Adding a profile is one line in
each: the config file path, the Keychain service and the cache dir all derive
from the config dir. Each registered profile gets a named command:
`claude-code-usage-default` (aliases `ccu-default`, `ccs-default`) and
`claude-code-usage-work` (aliases `ccu-work`, `ccs-work`), which are just
`claude_code_usage_profile=<name>`. The `-default` one is redundant with plain
`claude-code-usage` but says out loud which account you meant.

`claude-code-usage-all` passes the registry to the script as repeated
`--profile NAME=CONFIG_DIR` and lets it do the work.

The fan-out is inside the Python — one process running a `ThreadPoolExecutor`
over what is pure network wait, so the GIL is irrelevant — matching
`codex_status.py`, which checks several auth files the same way. Results are
stored by index, so the report order is `claude_code_profile_order` regardless
of which request finishes first.

Doing it in the shell instead was tried and is worse. Backgrounded subshells
cannot return anything, so each profile needs a temporary file, and then
reassembling those in order, keeping stderr attributable, and reducing the set
to one exit status is a lot of bookkeeping. Worse, every such scheme — a helper
process, GNU `parallel`, anything that captures output in order to reorder it —
gives each child a pipe for stdout instead of the terminal, so `--color auto`
resolves to "no colour" and the command you run most often silently loses its
colour. In-process keeps stdout the terminal.

Both entry points build their shared flags through
`h-claude-code-usage-argv-common`, so the single-profile and all-profile
invocations cannot drift apart.

Knobs (set as env vars): `claude_code_usage_profile` (default `default`),
`claude_code_usage_timeout_s`, `claude_code_usage_cache_ttl_s`, and the
booleans `claude_code_usage_refresh_p`, `claude_code_usage_json_p`,
`claude_code_usage_strip_ansi_p`. Each profile caches under
`~/tmp/.claude-usage/<profile>/`. Extra CLI args are passed through after the
derived ones, so explicit flags win.

## Reset notifications

A report can also arm a one-shot background job that fires `notif` once the
limits currently blocking that profile have reset, so you find out without
having to keep re-running the report.

This is **off by default** — checking your usage is not the same act as asking
to be told about it. Every report command has a `-notify` twin that turns it on,
or set `claude_code_usage_notif_p=y` on a plain one:

- `claude-code-usage-notify` (alias `ccun`) — one profile, report plus
  schedule.
- `claude-code-usage-default-notify` (aliases `ccu-default-notify`,
  `ccs-default-notify`) — the default profile, named explicitly.
- `claude-code-usage-work-notify` (aliases `ccu-work-notify`,
  `ccs-work-notify`) — the work profile.
- `claude-code-usage-all-notify` (aliases `ccu-notify`, `ccs-notify`,
  `claude-code-status-notify`) — every profile, like the bare short names.
- `claude-code-usage-fable-notify` — the default profile's report, but
  scheduling the weekly **Fable** watcher rather than the profile one. Fable is
  not a profile, only an extra window on the default profile, so it cannot be
  reached through `claude_code_usage_notif_p` and needs its own entry point. It
  gets its own tmux session and so can be scheduled alongside the profile
  watcher instead of replacing it.

Managing what is scheduled:

- `claude-code-usage-notif-cancel [session...]` — cancels, defaulting to all of
  them.
- `claude-code-usage-notif-status` — what is scheduled, and for when.

To schedule without printing a report, call
`h-claude-code-usage-notif-schedule`, `h-claude-code-usage-work-notif-schedule`
or `h-claude-code-usage-fable-notif-schedule` directly. They carry the `h-`
prefix because the `-notify` reports are the intended way in, not because they
are off limits — reach for one when you already have a report in front of you.

Each scheduled notifier lives in a tmux session named after the scheduling
function minus the `h-`: `claude-code-usage-<profile>-notif-schedule`, plus
`claude-code-usage-fable-notif-schedule`. So what `tmux ls` shows and the
function you called line up.

A window counts as blocking at or above `claude_code_usage_notif_full_pct`
(default 100). The deadline is the **latest** reset among the blocked windows,
because a 5-hour rollover buys nothing while the weekly limit is still spent.
When nothing is blocking, arming is skipped with a note; running it under `deus`
arms for the next 5-hour rollover anyway, which is how to exercise the whole
mechanism without having to be rate-limited first. A window the profile does not
have at all — a team seat has no weekly window — is skipped.

The job lives in a tmux session made by `tmuxnewsh2`, one per variant. `tmuxnew`
kills the previous session's processes before creating the replacement, so
re-arming *replaces* the pending notifier rather than stacking another one, and
needs no lock, marker or redis key of its own. The tmux server is also
independent of BrishGarden, so `brishz-restart` does not silently disarm
anything. A reboot does, and the next report re-arms.

The job polls the wall clock every `claude_code_usage_notif_poll_s` seconds
(default 30) instead of issuing one long `sleep`, so suspending the laptop
cannot skew a five-hour wait and it fires promptly on wake. It fires
`claude_code_usage_notif_grace_s` seconds (default 30) after the reset so the
endpoint has actually flipped by the time it says so, and notifies under a fixed
`notif_group` so repeats replace each other instead of piling up.

Since `remain-on-exit` is on here, a notifier that has fired leaves its tmux
session behind holding a dead pane. `claude-code-usage-notif-status` reports
that as already fired — which is also how to check whether a notification went
off — and the next arm, or a cancel, clears it away.

### Resuming instead of announcing

Instead of only telling you the limits have reset, the armed job can type
`Continue.` straight into the Claude Code session that was blocked. Each entry
point is the profile's ordinary report plus an arm whose action is to resume
rather than notify:

- `claude-code-usage-type-continue` (alias `cctc`) — the current profile.
- `claude-code-usage-default-type-continue` (alias `cctc-default`).
- `claude-code-usage-work-type-continue` (alias `cctc-work`).
- `h-claude-code-usage-type-continue-schedule` and
  `h-claude-code-usage-work-type-continue-schedule` arm without printing a
  report, matching the existing `h-…-notif-schedule` escape hatches.

There is deliberately no `-all` variant: the action needs an interactively
chosen target per profile, and prompting twice from one command is worse than
just running the two commands.

It is one job per profile: a resume reuses the profile's existing tmux
session, so arming one *replaces* a plain notifier for that profile, and the
reverse also holds — last arm wins, and exactly one thing happens per reset.
`claude-code-usage-notif-status` now prints the pending action and target
alongside the deadline, for example `[action: type-continue -> kitty:95]`, so
a downgrade from resume back to notify (or the other way around) is visible
rather than silent.

**Choosing the target, and why it is not automatic.** `hs-type-continue` types
through `hs.eventtap.keyStrokes`, a *global* synthetic keystroke with no
window targeting: it types wherever the keyboard focus happens to be, then
presses Return. In practice there are usually several Claude Code sessions
open at once, plus chat apps and a browser, so firing that blind risks sending
`Continue.` as a chat message, or into the wrong session entirely. All
sessions on a profile share one rate limit, so "the session that was blocked"
is ambiguous by construction, and the target has to be *chosen*, not guessed.

So arming opens an fzf picker, `h-claude-code-usage-type-continue-target-fz`,
built on `claude-code-session-live-fz`, over the Claude Code sessions
currently live in a kitty window, with a preview showing the session's title,
when it last moved, and the last prompt it was given. It is multi-select, so
several tabs can be resumed at once. Above the sessions sits one synthetic
choice, `frontmost`, which falls back to `hs-type-continue`. It stays in the
list because a session outside kitty cannot be reached any other way, but it
is never the default.

The picker runs only once the job is actually going to be armed, so a report
that changes nothing never puts a picker in your way. Presetting
`claude_code_usage_notif_targets` (space separated, e.g. `kitty:95
frontmost`) skips the picker entirely, which is what makes the whole thing
callable from a script or a test.

**Delivery.** A `kitty:<window-id>` target is delivered with `kitty @
send-text --match id:<n>`, straight into that one window: no focus stealing,
no global keystrokes, and it does not care which window is frontmost or
whether the display is asleep. `send-text` documents that it always succeeds
"even if no text was sent to any window", so its exit status proves nothing —
the window is checked for separately first, otherwise a tab closed during the
wait would swallow the resume while the job reported success. A vanished
window degrades to a notification saying so.

The `frontmost` target instead wakes the display via
`hs.caffeinate.declareUserActivity()` and pauses a beat before typing, because
`displaysleep` is ten minutes on this machine — the same as the idle
threshold below — so by the time the job fires the screen is asleep, and the
first synthetic keypress would otherwise be eaten waking it, typing
`ontinue.` instead.

**The idle gate, and failing safe.** It only types if the keyboard has been
untouched for at least `claude_code_usage_type_continue_idle_min_s` (default
600). If you are at the machine you get an ordinary notification instead and
can resume yourself. It also declines if the screen is locked, or if the idle
time cannot be read at all. The principle is the same in every case: the
notification goes out either way, so an unwanted resume is the worse of the
two errors, and anything that cannot be established counts against typing.
The idle time itself comes from `hs.host.idleTime()` through
`h-hammerspoon-eval`, and what gets checked is the returned string rather than
the exit status, because Hammerspoon exits 0 whether or not the Lua found
anything.

**Knobs:**

- `claude_code_usage_notif_action` — `notif` or `type-continue`.
- `claude_code_usage_type_continue_idle_min_s` (default `600`) — how long the
  keyboard must have been untouched before a resume is allowed.
- `claude_code_usage_type_continue_grace_s` (default `60`) — deliberately
  longer than the notifier's own `30`, because an early notification is
  harmless while an early resume is spent on a session that is still blocked.
- `claude_code_usage_type_continue_text` (default `Continue.`).
- `claude_code_usage_notif_targets` — preset targets, skipping the picker.
- `claude_code_usage_notif_log` (default `~/logs/claude-code-usage-notif.log`).

**The log.** Every fire writes one line saying which target it tried and
whether it typed or declined, and why. The tmux pane a fired job leaves
behind says the same thing, but only until the next reboot, and a job that
types into your session while you are away should stay answerable for it
afterwards.

One caveat worth stating plainly: the job trusts the reset timestamp plus the
grace period, and does not re-check usage when it actually fires. If the
endpoint lags behind its own `resets_at`, the resume is spent on a session
that is still blocked, and nothing remains armed afterwards.

## Color

Color handling (`--color`, `--true-color`, `--dark-mode`, `--dark-theme`,
`--light-theme`) is shared with `codex_status.py` via
`python/libs/common_sub_status.py`; see `docs/codex_status.md` for details.
