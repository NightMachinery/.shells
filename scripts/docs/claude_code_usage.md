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

## Telling the profiles apart

Both seats symlink the same tracked `configFiles/claude-code/settings.json`, so
a work session and a personal one used to be indistinguishable once the TUI was
up. Five cues now name the seat from inside: the theme, the status line badge,
the pane tint, the pane border and the pane-border label. All five are on by
default.

Two of them apply to the work seat alone. The personal seat is the one the
terminal is already set up for, so it keeps the background it had and gets no
border, and is named by its emoji, its theme and its pane label. That is
expressed by leaving it out of `claude_code_profile_tints` and
`claude_code_profile_borders`: for both tables, a seat with no entry does not
get the cue, and the personal entries are commented out rather than deleted so
the convention stays visible.

Every cue is derived from the *effective* config dir, resolved once per launch
by [agfi:claude-code-profile-current] inside [agfi:claude], and never from the
launcher's name. `claude-m` typed inside a work session inherits
`CLAUDE_CONFIG_DIR` and is still a work session, cues included. The seat's
identity lives in tables beside `claude_code_profiles` in
`zshlang/auto-load/others/claude.zsh`: `claude_code_profile_markers` holds the
emoji and is the source of truth for it, `claude_code_profile_labels` the word
a human reads, `claude_code_profile_colors` an `R;G;B` triplet,
`claude_code_profile_tints` the pane wash, and `claude_code_profile_themes`
which tracked theme file the seat uses.

Personal is blue `rgb(90,150,240)` with 🦋; work is violet `rgb(108,113,196)`
with 🏫. These are the same values as `profileColors` in
`golang/agent_session/internal/claude/preview.go`, which paints the session
pickers, so a seat keeps one colour across the whole toolchain. Red and green
are avoided because the base theme is a daltonized one.

### The theme

On by default, `claude_theme_p`. Each config dir gets a `themes/profile.json`
symlinked to a tracked file, `configFiles/claude-code/themes/personal.json` or
`work.json`. Both start from `light-daltonized` and override `claude`,
`claudeShimmer`, `promptBorder`, `promptBorderShimmer`, `planMode` and
`briefLabelYou`, so the spinner, the assistant label, the input border, the
plan-mode accent and the `You` label all carry the seat's colour. `/theme`
lists them as "Personal 🦋" and "Work seat 🏫".

Only the work theme also sets `userMessageBackground`, the wash behind your own
messages, a shade deeper than the pane tint so a message still separates from
the background. The personal theme sets no background of any kind, so that seat
keeps whatever the terminal was already showing.

The slug is deliberately the same in both config dirs. That is what lets the
one shared settings file say `"theme": "custom:profile"` and still hand each
seat a different palette. Passing `--settings` per launch was rejected instead:
the tmux-subagents launcher already passes its own, and repeated flags are not
known to merge.

[agfi:claude-themes-link] writes the symlinks and is idempotent. [agfi:claude]
calls it before launching, behind a single stat, because Claude Code only
watches a themes directory that already existed when the session started — a
theme file added under a directory that was absent needs one restart, after
which edits apply to a running session. Both symlinks are listed in the
`agents_md_settings` table, so `agents-md-doctor` says so if editing a theme
through `/theme` ever replaces one with a plain file.

`claude_theme_p=n` passes `--settings '{"theme":"light-daltonized"}'` for that
session alone. A caller passing its own `--settings` may not merge with it.

### The status line badge

On by default, `claude_statusline_badge_p`. The seat's emoji is the first field
of the status line, ahead of the git branch. The script that prints it was
untracked at `~/.claude/hooks/statusline.sh`; it now lives at
`configFiles/claude-code/statusline.sh`, with a symlink left at the old path
and `statusLine.command` pointing at the tracked copy. It is MIT-licensed
third-party work.

Being bash, it cannot read a zsh table, so the emoji are mirrored there behind
a comment naming `claude_code_profile_markers` as the source of truth. An
unregistered config dir gets `❓` and the directory's basename, so a third seat
still says something true.

Two long-standing faults went with it. `USAGE_FILE`, `CREDENTIALS_FILE` and
`SETTINGS_FILE` all defaulted to `~/.claude/...` for both seats, so a work
session reported the personal seat's quota, credentials and settings; they now
follow the effective config dir. And the effort level now comes from the
`effort.level` field of the status line's stdin JSON, which tracks `/effort`
live, falling back to `effortLevel` in the seat's own settings file.

`claude_statusline_badge_p=n` reaches the script as
`CLAUDE_CODE_PROFILE_BADGE_P`. That is the only channel there is: the command
inherits the launcher's environment, and its stdin JSON has no profile field.

### The pane tint

On by default for the work seat under tmux, `claude_tint_p`. The launcher
writes OSC 11 before starting the session, washing the background to `#f7f5fd`,
a far paler version of the seat's violet, since it sits under a screenful of
text all day and should be noticed only when looked for,
and OSC 111 afterwards in an `always` block, so a normal quit and Ctrl-C both
undo it and neither can change the session's exit status. Cells the TUI paints
with a background of their own are unaffected, so it reads as a tint rather
than a repaint. A seat with no entry in `claude_code_profile_tints` is not
tinted, and is not reset on the way out either, so nothing is undone that was
never done.

Where it applies is `claude_tint_scope`, and it is not a boolean. Under tmux 3.0
and later the escape reaches only the pane that sent it, leaving a sibling pane
in the same window alone, which is why `tmux` is the default scope. A bare
terminal has no panes, so the same escape there recolours the whole window,
the user's terminal rather than the session's corner of it; `always` opts into
that and `never` disables the tint the way `claude_tint_p=n` does.

It is also skipped unless stdout is a terminal, so a piped or captured run is
never handed escape codes. A `kill -9` outruns the reset and leaves the pane
tinted until the next reset or a new pane.

A wash this faint cannot be judged from its hex, so the candidates live in
`color_background_palette` in `zshlang/basic/colors.zsh`. They are nothing to
do with Claude: setting a terminal's background is an ordinary terminal
operation, so they sit beside [agfi:color-cursor] under names that say what
they do. [agfi:color-background-palette] prints them as swatches with dark text
over each one, which is the question that actually matters, and marks a colour
you name. [agfi:color-background] takes a palette name or any hex and repaints
the pane you run it in straight away, so several can be compared side by side;
[agfi:color-background-reset] restores the terminal's configured background.
Whichever wins goes into `claude_code_profile_tints`.

The entries prefixed `solar-` are computed for Solarized Light. Each sits at
exactly the CIELAB lightness of that theme's background, `#fdf6e3`, and differs
only in hue, so the contrast of the text on top is left as Solarized tuned it.
Measured against Solarized's body text, the background itself scores 4.13 and
every one of those tints scores between 4.12 and 4.14, so none of them costs
any legibility. Violet is the most recognisable for the least colour, the
background being a warm yellow and violet its opposite, which is why the work
seat uses `solar-violet-faint`.

Both commands send their escape to the terminal rather than to stdout, because
run as `! color-background ...` from inside an agent session stdout is a pipe
the agent reads, and an escape sent there is printed as text instead of
reaching the terminal. They try `/dev/tty` first, then the tmux pane's own tty,
then stdout. The second step is what makes it work inside an agent at all: that
shell has no controlling terminal, so opening `/dev/tty` fails with "device not
configured". Worth knowing that `test -w /dev/tty` reports it writable anyway,
since access(2) does not check for a controlling terminal, so the guard has to
be an actual open.

### The pane border

On by default for the work seat, `claude_tmux_border_p`, and inside tmux only,
which is the only thing here with a pane border to colour. The launcher sets
both `pane-border-style` and `pane-active-border-style` as *pane* options, so
the border carries the seat's colour whether or not the pane is the active one.
The style is `fg=<colour>,bold`.

The colour is not written in `claude_code_profile_borders`. That table says
only *which* seats get a border, and its value is the extra tmux style
attributes to add, `bold` here. The colour itself comes from
`claude_code_profile_colors`, converted from the `R;G;B` triplet to the
`#rrggbb` that tmux styles want by [agfi:h-claude-profile-color-hex]. So a
seat's colour is written down once and the theme, the pickers and the border
cannot drift apart.

One cost worth knowing. A tmux window showing a single pane draws no borders at
all, which is the common case for an agent session, so the launcher turns
`pane-border-status` on for the window when it is off. That takes a line from
the window, exactly the price the label below pays, and it is left on
afterwards, since clearing it would blank the border of any other pane relying
on it. The per-pane styles themselves are removed when the session ends, unless
the pane already carried one.

### The tmux pane-border label

On by default, `claude_tmux_label_p`, for both seats. It sets the pane option
`@claude_profile` to `🏫 WORK` or `🦋 PERSONAL` and turns the window's border
row on. It was off while the border cue did not exist, because the row costs
the window a line; now that the border has already bought that line for the
work seat, the row may as well name the seat instead of showing tmux's own
format, which leads with the pane index and gives a bare `0` in front of the
pane title.

The format shows the pane index only when the window actually holds more than
one pane, since in a single-pane window it identifies nothing.

The label is a *pane* option, since one tmux session can host several seats in
several panes. The border row and its format are *window* properties, so they
are set only when the window has no `pane-border-format` of its own to lose,
and are left in place afterwards: clearing them would blank the label of any
other labelled pane. Panes with no label read `SHELL`. A pane that already
carried a label keeps it, since only a label this launch introduced is taken
away again.

### The flags, and a bare `command claude`

Every flag is read with [agfi:bool] and is dynamically scoped, so a one-off is
a prefix: `claude_tint_p=n claude-work`, `claude_theme_p=n claude-m`,
`claude_tmux_border_p=n claude-work`, `claude_tmux_label_p=n claude-work`.
`claude_tint_scope` is the exception, being an enum rather than a boolean.

Turning a cue on for a seat that has no entry in the tint or border table does
nothing: the flag says whether the cue may run at all, and the table says which
seats it describes. Give the seat an entry there to change that.

A bare `command claude` skips the launcher, so it gets neither the tint nor the
label. It still gets the theme and the badge, which live in the config dir and
the shared settings rather than in the launcher's environment. That asymmetry
was accepted when the design was chosen.

### The `/color` caveat

`/color blue|orange|default` sets a prompt-bar colour for one session, and it
is remembered when that session is resumed, so a resumed session can show a
prompt border that disagrees with its theme. This is documented behaviour
rather than a bug, and `/color default` clears it.

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

## Resuming across profiles

`claude --resume` only searches the active profile's config home, so a
session started on the work seat is invisible to the personal `claude`, and
the other way round. `claude-code-session-resume` in
`zshlang/auto-load/others/claude-session.zsh` closes that gap:

```
claude-code-session-resume-fz                 # pick a session of this project, any profile
claude-code-session-resume-all-fz             # the same, picking from every project
claude-code-session-resume <uuid>             # resume it under the profile that owns it
claude-code-session-resume <uuid> default     # fork it into the personal profile and resume there
claude-resume-personal <uuid>                 # the same
claude-resume-personal-fz                     # the same, picking the session with fzf
claude-resume-personal-all-fz                 # the same, picking from every project
claude-resume-work <uuid>                     # the reverse
claude-resume-work-fz                         # the reverse, picking with fzf
claude-resume-work-all-fz                     # the reverse, picking from every project
```

The first argument is a transcript path or a session uuid (a unique prefix is
enough); it is looked up under every profile. The second names the target
profile; anything after that goes to the launcher. The `-fz` forms take the
profile first and pick the session interactively. What they offer is set by
`claude_code_session_resume_scope`: `project`, the default, lists every
profile's sessions for the current directory, and `all` lists every profile's
sessions for every project. The `-all-fz` forms are the `-fz` forms with that
set to `all`: `claude-code-session-resume-all-fz`, `claude-resume-all-fz`,
`claude-resume-personal-all-fz` and `claude-resume-work-all-fz`.
`claude-resume`, `claude-resume-fz` and `claude-resume-all-fz` are the short
names for their `claude-code-session-resume` counterparts. Codex and
Antigravity have resume commands of their own now — `codex-resume-fz`,
`codex-resume`, `agy-resume-fz`, `agy-resume` — which pick from that agent's
sessions and hand the choice to `codex resume` or `agy --conversation`; they
know nothing about profiles, which are a Claude Code idea.
`docs/agent-sessions.md` covers the family. The launcher comes
from `claude_code_profile_launchers` next to `claude_code_profiles`, so the
work seat keeps its tty marker and every profile keeps the sync and watchdogs
of the `claude` wrapper.

The picker is `h-claude-code-session-select-fz`, the one the viewers use. Each
row shows the session's last-message time, its name, its profile-labelled path
and the first prompt. The preview pane shows the highlighted session's name,
coloured by profile (`.claude` blue, `.claude-work` violet), its uuid, profile
and Claude Code version, when it was last active and how long ago, the model
and effort that answered last, its permission and interaction modes, its
working directory and branch, and the last prompt. The pane is
`agent_session claude preview`, a single binary fzf runs directly on each
cursor move; it reads only the tail of the transcript rather than rendering it,
so it is instant even on a 26 MB file — the readme for `golang/agent_session`
records what the earlier shell version cost and where. The colour is there
because a work session and a personal one look identical otherwise, and
telling them apart is exactly what you want when the two are interchangeable.
The name is what makes choosing among a project's sessions workable — several
routinely share a directory and differ in nothing else visible. It is resolved
by the same code `agent_session claude name` uses, `agent-name` over
`custom-title` over `ai-title` over the slug, so a fork made by
`claude-code-session-import` shows under its ` ⑂ <profile>` name rather than
the title it had before the fork. The preview used to read `ai-title` alone and got exactly that case
wrong.

Enter resumes the highlighted session. `alt+enter` instead converts that
transcript to org and opens it in emacs in the background, without leaving the
picker, so a session can be read before deciding whether to resume it; pressing
it again on the same row cancels that conversion. The function behind the key is
`agent-view-session-toggle`, and the readme for `golang/agent_session` explains
why it is keyed on the transcript rather than on the kitty window, and
why it does not freeze the picker.

When the target differs from the owner, `claude-code-session-import` forks
the session there under a **new** uuid: it copies the transcript, the
`<uuid>/` directory holding subagent transcripts and tool results, and
`file-history/<uuid>/`, which `/rewind` uses, then rewrites the uuid inside
the copies. The same uuid in two profiles would make the kitty hotkey's title
match and Claude Code's own `--resume <name>` ambiguous, and resuming the
stale copy later would fork it silently. The fork is renamed to the source's
name plus ` ⑂ <profile>` (`claude_code_session_import_name_suffix`, a printf
format; a fork of a fork keeps one suffix, not a trail), written as both an
`agent-name` and a `custom-title` line so the `agent_session claude` resolver
and Claude Code's picker agree. The source is never modified;
`claude_code_session_import_remove_source_p=y` trashes it afterwards so the
session leaves the source profile's picker.

Not copied, because they are per profile rather than per session: plan files
(their names are not derivable from the transcript), the per-project auto
memory, and the prompt history.

Instruction files are not stored in a transcript. Claude Code injects them
from the active config dir at launch, so the fork runs under the *target*
profile's assembled CLAUDE.md. Going from work to personal this drops the
work overlay's privacy guardrails, which is right: they protect the work
store, and new turns now land in the personal one while the work original is
untouched. Going the other way puts the whole history into the work store
before those guardrails ever apply, so importing into any non-default profile
asks for confirmation first (`claude_code_session_import_yes_p=y` skips it).

A live source is refused, since the running process keeps appending to it
and the fork would be stale at once; quit that session first, or set
`claude_code_session_import_force_p=y` to fork whatever exists now.


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
built on `agent-session-live-fz`, over every agent session currently live in a
kitty window — Claude Code, Codex and Antigravity alike, each row carrying its
agent's glyph — with a preview showing the session's title, when it last moved,
and the last prompt it was given. It is multi-select, so several tabs can be
resumed at once. Above the sessions sits one synthetic
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

A Codex row becomes a `codex:<thread-id>` target instead, delivered with
`codex queue --thread <id> --message <text>`. Codex accepts a message for a
thread by name, so that path needs no window, no focus and no awake display,
and it cannot land in the wrong place; the picker prefers it whenever the
chosen session is a Codex one. Antigravity has no such command, so an agy
session is typed into its kitty window like a Claude one.

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
