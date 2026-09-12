# codex_loop.py

`python/codex_loop.py` runs a Codex CLI prompt repeatedly, sleeping until the
account's usage limits reset when it needs to.

Unknown leading flags are passed through to `codex exec`; `-n` sets the
iteration count. `codex_loop.py --help` has the argument details.

## Waiting

Before each run it shells out to `--status-cmd` (default
`codex_status.py --json`), parses the report, and either proceeds or sleeps
until the reset.

The decision comes from `python/libs/codex_rate_limits.py`, shared with
`codex_status.py`, so the loop and the report cannot disagree about whether an
account is usable. `docs/codex_status.md` describes the rule under "Usage
windows"; in short, a window blocks only when it is present and at or above the
threshold, a plan reporting no window at all is unconstrained rather than
exhausted, the explicit signals (`rateLimitReachedType`,
`ordinaryUsageAllowed`, `spendControlReached`) are trusted over the
percentages, and the deadline is the latest reset among the blocking windows.

Only the default `codex` limit is consulted. A spent per-model family --
GPT-5.3-Codex-Spark, say -- does not block ordinary usage and must not park the
loop.

Erring long is deliberate: the loop re-polls after waking, so an over-long sleep
costs one extra check while an under-long one spins.

`credits.hasCredits: false` does not mean the account cannot run Codex, and is
not treated as a reason to wait.

## The status command's shape

`codex_status.py --json` defaults to `--all`, so its output is a
`{"authFiles": [...]}` envelope rather than a bare rate-limit object. The shared
`get_codex_limit` unwraps that envelope -- preferring the entry whose `alias`
matches, then the active one, then the first that succeeded -- so both the
default command and a user-supplied `--status-cmd` work, with or without
`--all`.

This is worth stating because it was silently wrong for a long time: the older
code looked for `rateLimits` only at the top level, found nothing in the
envelope its own default command produced, and concluded there was nothing to
wait for. The loop never waited on a rate limit, on any plan.
