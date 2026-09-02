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
nothing for a newly added one. A denied or unanswered prompt makes the token
lookup fail, and the report then silently drops to the config-cache fallback —
so if a profile shows `[local cache: ...]` when you expected live data, read the
`[no live data: ...]` reason next to it before suspecting the derivation.

The Keychain/file credential also provides the plan name (`subscriptionType`)
and token expiry. An expired token only produces a warning — the request is
still attempted, since Claude Code may have refreshed the Keychain entry. The
script never refreshes the token itself; open `claude` (or run `/login` inside
it) to refresh.

## Caching

Successful responses are cached in `~/tmp/.claude-usage/usage.json`
(`--cache-dir` to relocate). Cached data younger than `--cache-ttl` seconds
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
- `--cache-ttl` — `claude_code_usage_cache_ttl_s` /
  `CLAUDE_CODE_USAGE_CACHE_TTL_S`; 300.
- `--refresh` — no env fallback; off.
- `--cache-dir` — `claude_code_usage_cache_dir` / `CLAUDE_CODE_USAGE_CACHE_DIR`;
  `~/tmp/.claude-usage`.
- `--user-agent` — `claude_code_usage_user_agent` /
  `CLAUDE_CODE_USAGE_USER_AGENT`; `claude-code/2.1.220`.
- `--config-dir` — `claude_code_usage_config_dir` /
  `CLAUDE_CODE_USAGE_CONFIG_DIR`; empty, meaning the default profile.
- `--profile-label` — `claude_code_usage_profile_label` /
  `CLAUDE_CODE_USAGE_PROFILE_LABEL`; empty.
- `--keychain-service`, `--keychain-account` — no env fallback; both derived as
  described under Credentials.

`--json` output contains normalized `windows` (percent, epoch and ISO reset
times, severity, is_active) plus the `raw` payload for forward compatibility. It
also reports `profile`, the data `source` (`api`, `api-cache` or
`config-cache`), the `keychain` service and account used, and any `warnings`.

## Zsh wrapper

`claude-code-usage` in `zshlang/auto-load/others/claude.zsh` reports one
profile; `claude-code-usage-all` reports every registered profile, and is what
the bare `ccu` / `ccs` / `claude-code-status` names run.

Profiles are registered in the `claude_code_profiles` associative array, which
maps a profile name to its `CLAUDE_CONFIG_DIR` (empty for the default profile),
and are ordered by `claude_code_profile_order`. Adding a profile is one line in
each: the config file path, the Keychain service and the cache dir all derive
from the config dir. `claude-code-usage-work` (aliases `ccu-work`, `ccs-work`)
is the work profile, i.e. `claude_code_usage_profile=work`.

`claude-code-usage-all` runs the profiles in parallel — separate accounts and
separate requests, so there is nothing to serialize — but prints them in
`claude_code_profile_order` so the output does not shuffle with whichever
request happened to finish first. A profile that fails has its error forwarded
to stderr, labelled with the profile name, and makes the function return
nonzero without costing the other profiles their reports. With `--json` (or
`claude_code_usage_json_p`) the per-profile objects are emitted as a JSON array,
since two bare objects in a row are not JSON.

The fan-out itself is `golang/parallel_sections`, not shell: backgrounded zsh
subshells cannot return anything, so each profile would need a temporary file,
and reassembling those in declared order while keeping stderr attributable and
reducing the set to one exit status is bookkeeping better done once in Go. Both
paths build their argv through the same `h-claude-code-usage-argv`, so the
single-profile and fan-out invocations cannot drift apart. See
`golang/parallel_sections/readme.org`.

Knobs (set as env vars): `claude_code_usage_profile` (default `default`),
`claude_code_usage_timeout_s`, `claude_code_usage_cache_ttl_s`, and the
booleans `claude_code_usage_refresh_p`, `claude_code_usage_json_p`,
`claude_code_usage_strip_ansi_p`. Each profile caches under
`~/tmp/.claude-usage/<profile>/`. Extra CLI args are passed through after the
derived ones, so explicit flags win.

## Reset notifications

Every `claude-code-usage` run also arms a one-shot background job that fires
`notif` once the limits currently blocking that profile have reset, so you find
out without having to keep re-running the report. Set
`claude_code_usage_notif_p=n` to skip arming.

- `claude-code-usage-notif` (alias `ccun`) — the default profile.
- `claude-code-usage-work-notif` — the work profile.
- `claude-code-usage-fable-notif` — the default profile, watching the weekly
  Fable window as well. It has its own session, so it can be armed alongside
  the plain one rather than replacing it.
- `claude-code-usage-notif-cancel [session...]` — cancels, defaulting to all of
  them.
- `claude-code-usage-notif-status` — what is armed, and for when.

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

## Color

Color handling (`--color`, `--true-color`, `--dark-mode`, `--dark-theme`,
`--light-theme`) is shared with `codex_status.py` via
`python/libs/common_sub_status.py`; see `docs/codex_status.md` for details.
