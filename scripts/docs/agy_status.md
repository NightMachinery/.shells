# Antigravity accounts and quota

The built-in accounts are `m`, `cave`, and `fm2`. `m` keeps the existing
`~/.gemini` state; new accounts use `~/.agy-accounts/<name>`. The shell generates
`agy-m`, `agy-cave`, `agy-fm2` and their `-t` tmux variants. `agy` and
`antigravity` follow the current account inside an agy child shell, and select
`m` otherwise. Every interactive launcher uses [agfi:h-agent-launch].

## Account definitions and sign-in

Define additional accounts in the private shell/environment, then start a new
shell or run `agy-accounts-refresh`:

```zsh
export AGY_ACCOUNTS='m cave fm2 extra'
export AGY_ACCOUNT_LABEL_EXTRA='Extra account'       # optional display label
export AGY_ACCOUNT_DIR_EXTRA="$HOME/.agy-accounts/extra"  # optional Gemini root
```

Names use lowercase letters, digits and underscores, starting with a letter.
Reserved command names and duplicate roots/aliases are rejected. An optional
`AGY_ACCOUNT_NAME_<UPPERCASE_ID>` adds a friendly launcher alias. `HOME` is never
changed. The owner has explicitly approved the three built-in account names
for public code; email addresses and credentials remain outside the repository.

`agy-account-prepare <name>` prepares new configuration/history links. New
accounts are prepared on first launch; existing state is never rewritten by a
normal launch. Setup links the global rules (`config/rules`, written by
`agents-md-sync`), hooks, policies and skills (`config/skills`) when present,
drops a link it made to a shared file that no longer exists, and synchronizes
only the `statusLine` settings key. Run it again for an existing account after
a new shared path is added. Other settings
remain per account. Existing independent files are refused for manual
reconciliation, never moved or overwritten. Override the shared root with
`AGY_SHARED_GEMINI_DIR` if needed.

```zsh
agy-acc-login cave
agy-acc-login fm2
agy-acc-login --tmux cave  # or: agy-acc-login -t cave
```

The helper runs sign-in in the current terminal. Complete Google sign-in, then
exit agy; the helper runs `models` to verify the account and returns its status.
If the account is already signed in (a token file, or `m`'s keychain sign-in),
it verifies without signing out. No argument selects
the current account (or `m` outside an account shell).

Use `-t` or `--tmux` to keep sign-in in a detached session. The helper prints a
`tmux attach-session` command, and the pane waits for Enter after verification.
Re-running with `--tmux` prints
the attach command if any pane in the login session is alive. If all panes have
exited (and tmux retained them with `remain-on-exit`), it restarts a dead pane.
It refuses to replace a pane that another caller has already restarted. No
sign-in is performed by account setup or status reporting.

After updating login helpers, start a fresh shell before retrying. Existing
terminals retain their old function definitions.

## File authentication

All shell launchers, login verification and print-mode quota calls inject
`SSH_CONNECTION=agy-file-auth` only into the agy process (preserving a real SSH
value when present). This is the native CLI's verified file-storage trigger.
No global SSH environment is changed.

Programs that find agy through `PATH` get the same treatment from
`zshlang/wrappers/agy`, a shim that comes before `~/.local/bin` in `PATH`. It
sets the variable and execs the real binary (`AGY_REAL_BIN`, default
`~/.local/bin/agy`). That covers `command agy`, Paseo's built-in `antigravity`
provider (whose status check runs `agy --version` and `agy models`) and Paseo
test daemons. Only a caller that names `~/.local/bin/agy` itself bypasses
the shim. The `agy-acp` provider does this, but it sets the variable itself.

Without the shim, account `m` kept getting signed out. Any run without the
variable reads its token from the macOS keychain instead. When such a run has
to refresh an expired keychain token, the file token disappears moments later.
On one Mac, each of six sign-outs between 2026-11-13 and 2026-11-19 followed such
a refresh, by seconds to minutes. Most came from the Paseo daemon, whose
working directory was `~/tmp/.codex-status`, so its probes logged that path
as their workspace. Long-lived agy sessions used to rewrite the file on their
hourly refresh, which hid the loss. After they exited on 2026-11-18, every
loss lasted until the next sign-in. The refresh-then-loss link is inferred
from agy's logs, not read from its closed-source code. Its composite token
storage does contain functions named `healKeyringTokenFromFile` and
`keyringStateToMigrate`. To diagnose a new sign-out, list the
`antigravity-cli/log/cli-*.log` files that lack the line `Using file-based
token storage` and also contain `expired=true`. Each such file is a keychain
run that refreshed a token.

Installed agy 1.4.2 also accepts hidden `--gemini_dir=<absolute root>` and
`--app_data_dir=antigravity-cli` flags. The launchers supply both and reject
user overrides that could select another account's auth. The corresponding
`ANTIGRAVITY_GEMINI_DIR` environment variable serves plugin CLI children; it is
not the normal CLI's state-directory selector. Binary disassembly and an
unsigned scratch `models` run confirmed these behaviors, including the log
message selecting file token storage. These controls are not documented in
[Google's installation guide](https://www.antigravity.google/docs/cli/install/),
so recheck them after a native CLI update.

The token remains `<Gemini root>/antigravity-cli/antigravity-oauth-token`.
agy 1.5 still accepts a keychain sign-in when that file is missing: it tries
the file, then the keychain, even in file mode. The keychain item is one per
macOS user, so it counts only for the account in the default `~/.gemini`
(normally `m`).

[agfi:h-agy-stored-auth] decides whether an account is signed in, for
`agy-best`, `agy-acc-login` and the `agy-status` fallback. It asks
`agent-quotas agy-auth --dir <Gemini root>`, which prints `file-present`,
`keychain` or `missing`, so the rule lives only in agent-quotas. Where that
binary is missing, too old to know the command, or switched off with
`agent_quotas_p=n`, only the token file counts. Neither path opens a
credential. Cached quota cannot establish whether a sign-in still works:
output distinguishes stored credentials (`file-present`, `keychain`) from a
live verified sign-in (`verified`).

## API-billed profiles

`agy-api NAME` runs agy billed to a Google API key or a Google Cloud project
instead of a subscription. The shell generates `agy-api-personal`,
`agy-api-work`, their `-light` variants (below), `-t` tmux variants and
`-login` commands. For
other names, set `AGY_API_PROFILES` (default `personal work`) and run
`agy-api-refresh`.

The quickest start is Vertex AI with a browser sign-in:

```zsh
agy-api-work-login   # opens Google, then fzf over that account's projects
agy-api-work
```

`agy-api-login NAME [PROJECT]` runs `gcloud auth application-default login`
with a gcloud configuration directory of the profile's own,
`~/.config/gcloud/agy-api/NAME`. The browser asks the chosen Google account to
grant Google Cloud access (the `cloud-platform` scope, which covers Vertex
AI); the resulting `authorized_user` credentials and the project ID stay in
that directory, so each profile can sign in as a different account and none
touches the machine's default gcloud sign-in. Keeping it under
`~/.config/gcloud` puts it inside the paths the agy driver already denies its
children.

The project to bill is the argument, else `AGY_API_PROJECT_<NAME>`, else the
one a previous sign-in stored. With none of them, the command lists the active
projects the newly signed-in account can see (Cloud Resource Manager, every
page) and offers them in `fz`. A user ADC token lists them without a quota
project; checked on 2026-11-19. The chosen project becomes the credentials'
quota project, and the command then asks the Service Usage API whether Vertex
AI is enabled there, printing the console link to enable it if not.

Each profile is a hidden account, `api_NAME`, with its own root
`~/.agy-accounts/api_NAME`, prepared like any other account (shared rules,
skills and history). It is never in `AGY_ACCOUNTS`, so `agy-status` and
`agy-best` leave it out: an API profile has no quota buckets to report. The
conversations it starts are recorded as owned by `api_NAME`, and the resume
helpers bring them back through `agy-api-NAME`.

`agy-api-NAME-light` (`agy-api --light NAME`) is the same profile without the
global instructions or skills, for plain chat. It runs in a second hidden
account, `api_NAME_light`, which shares the credentials, hooks and history but
links none of `config/rules`, `config/skills`, `GEMINI.md`, `AGENTS.md`,
`rules` or `skills`. Every launch reconciles that root, removing such links if
it has them from before they were excluded. Its conversations resume through
`agy-api-NAME-light`, and profile names ending in `_light` are reserved for
it.

agy finds these only through those links. On 2026-11-19, Flash Low runs asked
about their instructions quoted the rules' first heading with `config/rules`
linked and answered "NO" without it; with `config/skills` linked they counted
38 skills (`agent-desk` among them), and without it they named only agy's own
built-in skills (`agy-customizations`, `antigravity-guide`, `run-workflow`),
which no link controls and no setting was found to turn off. Workspace
instructions and skills (an `AGENTS.md`, or skills, in the repository
agy runs in) are a separate mechanism and still apply; for a bare chat, start
elsewhere.

`agy-api-NAME-add-to-paseo` makes both variants Paseo providers on this
machine's daemon, `agy-api-NAME` and `agy-api-NAME-light` (underscores in NAME
become hyphens, as Paseo requires). Each runs `agy acp --sandbox` in the
profile's root with the environment `agy-api` gives it, so Auto mode and the OS
sandbox apply, and agy's permission requests appear in the Paseo app; there is
no permission hook. Rerun it after signing in with another project. It refuses
key-route profiles, because the key would have to sit in Paseo's
`config.json`; only the project ID and file paths go there.
`python/paseo_providers_set.py` edits `agents.providers` in place (other
providers and any options set by hand are kept, the old file stays as
`config.json.bak`), and `paseo daemon reload` applies it without a restart;
the command fails if the reload does not report `agents.providers` applied.
Paseo offers no per-provider setter: `paseo daemon config set` cannot address
one provider, only the whole map. Providers are per daemon, so run it on each
Mac whose Paseo should offer them, after signing in there.

Otherwise, credentials are variables named after the profile. Put them in
`~/.privateShell`. They need not be exported: the launcher reads them and
exports them only to the agy it starts, never on its command line. A variable
wins over the sign-in's stored value.

```zsh
AGY_API_KEY_PERSONAL='...'          # route 1: a Gemini API key
AGY_API_PROJECT_WORK='a-project'    # route 2: Vertex AI in this Cloud project
AGY_API_CREDENTIALS_WORK="$HOME/.config/gcloud/work-adc.json"  # optional
AGY_API_LOCATION_WORK=global        # optional; global is the default
```

A profile sets exactly one route:

- **Gemini API key** (`AGY_API_KEY_<NAME>`). The launcher writes
  `"modelProvider": "gemini"` into the profile's own `settings.json` and
  passes the key as `GEMINI_API_KEY`. It removes any ambient `GOOGLE_API_KEY`,
  which agy would otherwise prefer. Requests go to
  `generativelanguage.googleapis.com` and are billed to the key's Cloud
  project. Create the key in Google AI Studio, or in the Cloud console on a
  project with the Generative Language API enabled.
- **Vertex AI** (`AGY_API_PROJECT_<NAME>`, or the sign-in's project). The
  launcher removes `modelProvider` and sets `AGY_ADC_AUTH=1`,
  `GOOGLE_CLOUD_PROJECT` and `GOOGLE_CLOUD_LOCATION`. agy then signs in with
  Application Default Credentials: the file named by
  `AGY_API_CREDENTIALS_<NAME>` (a service-account key, or any `authorized_user`
  file), else the profile's sign-in, else the machine's default ADC.

agy 1.5.12 cannot use a Vertex AI express-mode API key. Its key route always
calls the Gemini API endpoint, even with `GOOGLE_GENAI_USE_VERTEXAI=true`, and
its Vertex route needs ADC plus a project and location.

Both routes were checked on 2026-11-19 with agy 1.5.12 and fake credentials. A
fake key reached the Gemini API and came back `API key not valid`. An empty
credentials file reached agy's `adcAuth`, which rejected it (`missing 'type'
field`) and fell back to sign-in. Which models a route offers is agy's choice
(its changelog lists Hydrogen and Carbon for API keys, Hydrogen for ADC), so
check `/model` in the profile. API-billed accounts count as pay-as-you-go,
which gives them Auto mode.

## Shared local history

New profiles link the `conversations`, `brain`, and `annotations` directories
to `m`'s existing copies when those directories exist. These contain local
trajectories, transcripts/artifacts and annotations. Directory links preserve
SQLite WAL/SHM siblings beside their databases. Each account keeps its own
`conversation_summaries.db` derived index and `cache/`, including account-linked
project selection and workspace last-conversation mappings.

The native trajectory store uses WAL and a five-second busy timeout; different
conversation IDs use different files. The first account recorded for each conversation is kept in an owner-only
`history-owners/` directory outside the repository. Existing main-account IDs
are seeded before the new profiles are prepared; hooks record new IDs with
exclusive creation so another account cannot overwrite ownership.

Concurrent turns on the **same** ID are
unsafe because annotation locking is per process. Fork a conversation before
running parallel workers against it. Existing independent history directories
must be reconciled manually before linking; setup refuses to replace them.

Local trajectory loading has no account ownership check in the installed
binary. Cross-account authenticated continuation still needs verification with
a second signed-in account; cloud-only history may be account-bound. Until that
check succeeds, `agy-best --conversation <id>` resumes with the account that
owns the local transcript. Explicit account launchers may select another
account for an explicit ID. `--continue` stays account/workspace-specific.

## Status and choosing an account

`agy-status` (`agys`) reports every configured account in a separate block.
The report is [agent-quotas](https://github.com/NightMachinery/agent-quotas)'
(`agent-quotas agy-status`): [agfi:agy-status] only turns the knobs below
into its flags, so the quota rules live in one place, and it needs that binary
installed. It asks the agent-quotas daemon, or reads in its own process
(`--direct`) when the daemon does not answer or `agent_quotas_p=n`. Either way
it serves the statusline cache, and `deus` or `agy_status_method=slow` make it
run the slow path. The accounts are those in agent-quotas' config, which
`agent-quotas config init` copied from `AGY_ACCOUNTS`; an account added to
`AGY_ACCOUNTS` later needs adding there too. The
header carries its name, sign-in state, known plan tier, cache age or live
provenance, and email (dim in a terminal). `auth stored` means file credentials
exist but this cached reading has not verified them. Each aligned row shows a
short model family, window, usage bar, percent **used**, and relative reset:

```text
m  auth stored  Pro  cache 2m  masked@example.test
  Gemini          5h      █░░░░░░░░░     6%  resets in 1h 25m
  Gemini          weekly  █░░░░░░░░░     6%  resets in 4d 7h
  Gemini 4 Argon  weekly  █████░░░░░    51%  resets in 2d 11h
  Claude/GPT      weekly  ████████░░    75%  resets in 12m
```

Reset durations use at most two units. Columns align across accounts using
display-cell widths, including wide and combining characters. Account header
fields also share columns, so names of different lengths do not shift the
sign-in state, tier, provenance or email. At 80 columns
only relative resets appear; at 100 or more, local absolute times appear dim
when space permits. `agy_status_columns=80` overrides the terminal width;
`agy_status_absolute_p=n` hides absolute times (`y` shows them when they fit).
Long names, emails and error messages are clipped to the available width.
Bars and percentages are green below 70%, yellow at 70 through 89%, and red at
90% or above. Piped output and any set `NO_COLOR` disable ANSI styling, including
with `agy_status_color=always`; `agy_status_color=never` disables it explicitly.
Each failed account keeps its own error and never hides other accounts. Exit
status is 1 when any account has an error; stdout is still a complete report.
An account with no stored sign-in is a normal `not signed in` status, with a login hint,
not a measurement error. It does not trigger quota-watcher error alerts.

`agy-status --json` emits an array of account objects with `account`, `label`,
`signed_in` (true/false/null), `auth_status`, `tier`, `email`, `error`, `message`, and
`buckets`. Buckets include `bucket_id`, `group`, `label`, `used_percent`,
`remaining_percent`, epoch `resets_at`, `resets_at_iso`, and `source` (cache/live).
`agy-status --account cave [--json]` selects one. Tier/email are shown only if
known from the statusline cache; token contents are never mined for identity.

The older `agy_status_json_p=y agy-status` interface (`--flat`) retains the
flat bucket array for the current account (`--account`, else
`AGY_ACCOUNT_ID`, else `m`), so existing reset notifiers keep their contract:
the account object's buckets without `used_percent`. An account with an error,
no sign-in or no buckets exits 1 instead. The notifier still arms only that
account and its existing delivery pickers; all-account resume scheduling is a
separate follow-up.

`agy_status_credits_p=y` (`--credits`) appends each signed-in account's
pay-as-you-go balance from `agy -p /credits`, one line per row. Nothing caches
it, so it costs an agy start-up per account (they run in parallel), though no
quota. It is not in the JSON.

For quota-watch, use `agy_status_method=statusline agy_status_cache_only_p=y
agy-status --json` (`--cache-only --max-age 3600`). It reads the caches in its
own process and never starts agy, requests authentication or contacts a
backend, even under `deus`. A missing cache, or one with nothing younger than
`agy_status_statusline_max_age_s` (default an hour), is that account's error.
The quota-management watcher consumes every account/bucket, preserves stale
readings with a marker, and uses the existing alert thresholds. Run agy with
its statusline configured to keep these cache readings fresh.

`agy-best --model '<model ID or full name from agy models>' [other arguments]`
first checks each account's model catalog. Accounts that do not offer the exact
model variant are excluded, even when they have more quota. Wildcard/family
names do not guess a variant. The selected canonical model ID is passed to agy.
Among eligible accounts, it chooses the highest minimum remaining percentage
across all known applicable five-hour and weekly windows: general `gemini-*`
or `3p-*` pools and any matching model-specific pools, such as
`gemini-4-argon-weekly` and `gemini-4-argon-5h`. Unrelated model pools do not
constrain that choice. Unknown/missing applicable quota and failed accounts are
excluded; ties follow configured account order. Weekly-only data cannot reveal
a missing five-hour restriction. Set
`agy_best_model` for a default model; that same model is passed to agy. Explicit
`--model` avoids selecting Gemini quota while launching a saved Claude model.
`agy-best --continue` requires an explicit account launcher instead.

Model catalogs live at each account's `antigravity-cli/models-cache.json`, mode
0600. They contain only account ID, capture time and model IDs/display names.
The default TTL is one hour (`AGY_MODELS_CACHE_TTL`, positive seconds). A missing
or expired catalog refreshes through the read-only, file-auth `agy models`
command, with auto-update disabled and a 20-second timeout
(`AGY_MODELS_TIMEOUT`). A failed refresh preserves the old file for inspection
but excludes stale availability. Signed-out accounts never start that command.
`agy-models-refresh cave` forces one refresh; `agy-models-refresh --all` (or no
argument) refreshes every account. Explicit model selection on an owner-bound
conversation is also checked against its owner's catalog before launch.

## Sandboxed unattended work

The exact artifact review setting is `artifactReviewPolicy: "agent-decides"`.
It is independent of the tool execution mode. The verified combination is:

```json
{
  "enableTerminalSandbox": true,
  "toolPermission": "proceed-in-sandbox",
  "artifactReviewPolicy": "agent-decides",
  "permissions": {
    "allow": ["write_file(/absolute/workspace/)"]
  }
}
```

Merge those keys into the selected account's `antigravity-cli/settings.json`,
preserving other settings and existing permission rules. Scope the write rule
to the actual absolute workspace, with a trailing slash. In the installed
1.4.2 build, relying on the implicit workspace grant denied even an in-workspace
terminal write; the explicit rule above fixed it. Configure every candidate
account for that workspace before using `agy-best`.

```zsh
cd /absolute/workspace
agy-m --model gemini-3.8-flash-low --sandbox --print-timeout 5m \
  --output-format json --print 'Create hello.txt containing hi.' \
  > result.json 2> agy.stderr
# Or use agy-cave / agy-fm2 after sign-in.
# Or agy-best --model gemini-3.8-flash-low with the same remaining flags.
```

The account wrapper supplies file auth and state-root flags. `--sandbox` forces
the terminal sandbox on; `--print-timeout` bounds the agent turn. Output goes
to the two redirected files. Conversation state goes to the shared local
history; each selected account keeps its own summary index/cache. Terminal
commands can write the explicitly allowed workspace. Do not add an
`unsandboxed(...)` rule for unattended workers. An action that still requires
review may fail headlessly; `agent-decides` does not guarantee approval for
every action. Verify requested artifacts as well as CLI exit status.

The scratch verification used this settings combination and `--sandbox`,
without a permission bypass. `printf hi > hello.txt` exited 0 and wrote exactly
`hi`; a write to a sentinel outside the workspace exited 1 with `operation not
permitted`, and the outside file did not exist. The overall JSON run exited 0,
so command/artifact verification is necessary. The native `--mode auto` is not a
portable substitute: it is gated by an account rollout and may warn and fall
back to the default mode.

See [Google's sandbox guide](https://antigravity.google/docs/sandbox/) and
[settings reference](https://www.antigravity.google/docs/cli/reference/).

## Paseo subagents that ask their parent for permission

Paseo's built-in `antigravity` provider runs agy with
`--dangerously-skip-permissions`. Agy subagents that ask their parent for
permission instead run through `agy acp` under the private `paseo-agy-driver`
skill, which also holds the measurements behind it.

Never register a `PreToolUse` hook in the shared
`configFiles/antigravity/hooks.json`. Every agy process loads it, including the
built-in provider, where a hook answer of `{}` refuses the tool call. A brief
global hook (2026-11-06) denied every tool call of that provider's children
until it was removed. Register such hooks under a separate `--gemini_dir`.

## Hook activation

The hook transport guards function existence before invocation and always
returns success. A stale garden or failed account lookup must not invoke the
command-not-found handler. Account/root/label travel explicitly into Brish;
`m` replaces the legacy `acc1` label. tmux names use `+Agy/<account> <title>`.

Commit the helper functions first, ask the owner to run `brishz-restart`, verify
all hook targets with `brishz whence -w`, then update the installed hooks. The
installed hooks file may be a symlink into this repository, so editing its
source is an immediate installed change. Never restart the garden automatically.

## Two implementations, one report

Both ways of reading are in agent-quotas' Antigravity reader
(`internal/agy`); this file and the hook's settings installer are the shell's
side. The knob `agy_status_method`, an enum with the values `statusline` and
`slow`, picks the way, and the two named forms `agy-status-statusline` and
`agy-status-slow` force one without the caller having to know the knob
(`slow` is `agent-quotas agy-status --refresh`). It is an enum rather than a
`_p` switch because "which way" is not a yes/no question.

`statusline` is the one that answers by default. It is built on something
Antigravity documents -- a hook it invites you to configure -- rather than on
anything we inferred about its backend. `slow` is what it falls back to, and
what to reach for when you need the number `agy` itself would print.

Whichever runs, the output is the same contract (see "The report"), so
everything built on top -- the notifier's deadline, `agent-status` -- neither
knows nor cares which path produced its input. The difference is in cost, in
what can go wrong, and in how many rows come back.

## The statusline path: letting agy push

Antigravity keeps its quota in the CLI's process memory and nowhere else.
There is no quota file under `~/.gemini/antigravity-cli/`, no `agy usage`
subcommand, no daemon to ask (`agy remote-control` only starts, stops and
reports the daemon), and the logs record percentages at most, never a reset
time.

What there *is* is a statusline hook. `agy` renders a status line on every
frame, and when `settings.json` names a command for it, agy pipes that
command a JSON snapshot of the session on stdin and shows whatever it prints.
The snapshot carries the quota table, including five-hour buckets that
`agy -p /usage` may omit -- along with the model, the plan tier, the
context window and a good deal else.

So the fast path is not a fetch at all. It is a cache that agy fills for us
while we are working anyway, and reading it is one small file read.

Our hook is `zshlang/wrappers/agy_statusline.dash`, and it does two things on
every render: merges the quota into the shared cache, and prints one short
line of its own -- the five-hour and weekly percentages for whichever family
the current model belongs to, each with its reset countdown, then the model's
display name.

Three properties of that file are load-bearing, and all three are there
because it runs on *every* frame of a TUI:

- It is `sh` and one `jq`, not Python. The whole job is a start-up cost, and
  an interpreter that takes tens of milliseconds to boot would be visible as
  lag in agy's own rendering.
- It cannot fail. Not "should not": agy counts consecutive failures and
  switches the custom statusline off once there have been a few, and it does
  so quietly. A malformed payload, a missing `jq`, a read-only cache
  directory -- each of those ends as `exit 0` with nothing printed. The write
  block redirects stderr as a *group* rather than per command, because a
  redirection that cannot be opened is reported by the shell before the
  command it belongs to exists to be silenced; without that, a read-only cache
  directory writes a permission error onto agy's stderr on every frame.
- It writes through a temp file and a rename. `agy-status` may be reading the
  cache at any instant, and a half-written file reads as a broken one rather
  than as an old one.

### The cache, and its two writers

The cache is one JSON file, `agy_status_cache_file`, holding one object per
bucket:

    {"buckets": {"gemini-5h": {"remaining_percent": 98.0,
                               "resets_at": 1789644453,
                               "captured_at": 1789130000,
                               "source": "statusline"}}}

`reset_in_seconds` in agy's payload is *relative* to the render that carried
it, so it is turned into an absolute `resets_at` at write time. Storing the
relative number would make every later read wrong by however long the cache
had been sitting there, and wrong silently.

Two writers share this file: the hook, and the slow path, which merges its own
rows in after every successful run so that what `agy` itself said is not
thrown away. Both merge **per bucket**, and that is the whole reason the merge
exists. The hook and `agy -p /usage` both see the general five-hour and
weekly pools, but `/usage` lists a model-specific pool (Gemini 4 Argon) only
some of the time. A writer that replaced the file wholesale would therefore
delete such a pool whenever a run left it out, and the report would lose rows
for reasons nobody could see from the outside. For the same reason a slow
read keeps, in its report, the cached pools its run did not print. Each writer stamps
its rows with a `source`, so a row's provenance is readable rather than
guessed at.

General bucket IDs remain `gemini-5h`, `gemini-weekly`, `3p-5h` and
`3p-weekly`. A separate model group gets its own normalized group slug plus
window, for example `Gemini 4 Argon / Gemini 4 Argon Weekly Limit` becomes
`gemini-4-argon-weekly`, and its five-hour bucket becomes
`gemini-4-argon-5h`. The native statusline alias `gemini-argon-wtu-weekly`
(and `gemini-argon-5h`) maps to those same canonical IDs. Existing alias entries
are deduplicated by capture time when read or merged. Argon must never merge
into `gemini-weekly`. Unknown windows
retain a stable ID derived from both group and label. Slow-path cache entries
also retain their original group and label; old entries reconstruct names from
the ID. The statusline hook retains arbitrary native quota keys, including
future model pools. The watcher uses `agy.<account>.<bucket_id>` as its gauge key,
so separate pools remain separate across repeated reads and reset checks.

### Staleness, and what falls back

agent-quotas' `statusline_max_age_s` is how old a cached bucket may be and
still be reported (`agy_status_statusline_max_age_s` for cache-only reads).
Anything older is dropped from the report; if *nothing* is left, the read
falls back to the slow path and says so. A **negative** value, agent-quotas'
default, means "never drop on age" -- serve the cache however old it is,
which is what you want when agy has not been open for a while and you only
need the reset times. A reading taken before a window's reset no longer
counts that window as blocking, and the account is marked stale.

agent-quotas also runs the slow path when the account was used after the
cache's newest capture (agy run without its TUI, such as Paseo's `agy acp`,
never renders the statusline), at most once per `active_refresh_s` (default
five minutes) per account. `/usage` itself touches the account's
conversations, so a slow run stamps its reading when it ends; otherwise every
run would look like use and start the next. A fallback runs at most once per
`slow_fallback_s` (default an hour) per account.

The daemon also keeps idle accounts current, which no command run by hand
does: it re-reads an account whose newest reading is older than
`stale_refresh_s` (default an hour; -1 turns it off), and re-reads once after
a pool's reset has passed since that pool was read, `blocked_grace_s` (90 s)
after the reset. That costs one CLI start-up per account per hour and no
quota. See agent-quotas' `docs/design.md`.

An absent or unreadable cache always falls back, whatever the knob says,
because that is a different question: there is nothing to serve at any age.
The note it prints names `agy-statusline-install`, since the usual cause is
that the hook has never been installed.

The fallback is loud on purpose. A fallback that fires silently hides a
breakage: the report keeps appearing, only slower, and the fast path could
have been dead for months. Here there is usually nothing broken to hide -- an
empty cache means the hook has not run, which is a fact about whether you have
been using agy, not about whether anything works -- but the reason is printed
either way, so the two cases are told apart by reading rather than by
guessing.

`deus` (agent-quotas reads its `deusvult` variable) forces the slow path and
does not open the cache at all, the same "force, bypass the memo" convention
the rest of zshlang uses. Asking under `deus` is asking to distrust what is
lying around.

### Provenance in the account header

The human report puts `cache 2m`, `live`, or both in each account header. Cache
age uses the oldest reported bucket, so a fresh row cannot conceal an older
one. Missing capture timestamps show `cache age unknown` (or `some undated`
when mixed with dated rows). Source diagnostics do not float above account
blocks; each failed account has one error row while the others stay visible.

The flat array (`--flat`) and public `--json` keep source diagnostics on
stderr and JSON on stdout. Their document shapes are unchanged. Human-only
cache paths used to calculate header ages are never added to public JSON.

### Installing the hook

`agy-statusline-install` writes the `statusLine` key into
`agy_statusline_settings_file`. `agy-statusline-uninstall` takes it back out,
and `agy-statusline-installed-p` is the quiet predicate.

The keys are agy's own: `type` (always `command`), `command`, `enabled`, and
`stack_with_default`, which keeps agy's built-in status line and stacks ours
underneath instead of replacing it. `agy_statusline_stack_with_default_p`
turns that off. A fifth key, `padding`, exists and is deliberately left unset
so agy's own default stands. These names were confirmed against
Antigravity's CLI documentation and against the changelog `agy changelog`
prints, which is where `stack_with_default` was announced; a strings pass over
the binary agrees.

The installer is idempotent in the strict sense: a second run leaves exactly
one correct configuration and *says* it changed nothing, rather than
rewriting the file invisibly. That distinction matters because of the backup.
The original is copied aside once, before the first modification; a backup
taken on every run would, by the second one, be a backup of our own output,
which is no backup at all.

It reads, modifies and writes with `jq`, so every other key in the file
survives untouched, and the write is atomic like the cache's. A settings file
that is not valid JSON is refused rather than overwritten -- it is agy's file,
and everything in it but our one key is the user's.

An existing `statusLine` that points somewhere else is refused too, with the
foreign command named and `deus` given as the way to override; the comparison
is on the first word of the command, since an installed renderer may carry
arguments (upstream's own documents a `--classic` flag that way) and is still
the same renderer. The uninstall has no such override, deliberately: removing
somebody else's statusline is not a decision this function should be able to
be talked into, and agy's `/statusline delete` is right there.

Two knobs exist chiefly so that all of this can be exercised against a copy
rather than against the real configuration: `agy_statusline_settings_file`
and `agy_status_cache_file`. The hook reads the latter out of its
*environment*, so pointing a test at another cache has to be an assignment
the hook itself can see.

In practice, installing is one command and checking it is two more:

    agy-statusline-install
    agy                       # start agy; the line appears under agy's own
    agy-status                # now answered from the cache, not from agy

`agy-status` showing `live` instead of a cache age can mean the hook has not run
yet: agy reads `settings.json` at start-up, so an agy that was already open
when the installer ran has to be restarted before it will.

## The slow path: asking agy

The slow path was the first of these, and for a while it was the only one.
`agy -p /usage` (alias `/quota`) answers the built-in slash command and
exits: no agent turn is started, no quota is spent, and no conversation is
left behind for `agy-resume-fz` to find. It prints one tab-separated row per
model group:

    <group>	<label>	<remaining>%	<reset, ISO-8601 UTC>

The general groups are `Gemini Models` and `Claude and GPT models`, each with
a `Weekly Limit Remaining` and a `Five Hour Limit Remaining` row, plus
model-specific pools such as `Gemini 4 Argon`, which appear only some of the
time. The percentage is what *remains*, the opposite sense from the
used-percent that Claude Code and Codex report, and the reset is a UTC
instant, not a duration. A window that has not started shows 100% with a
reset of `1970-01-01T00:00:00Z` (Claude and GPT) or one window length from
now (Gemini).

It is called the slow path for a reason. Every call is a full CLI start-up
followed by five sequential backend round trips, and measured end to end it
takes anywhere from a couple of seconds to well over ten -- a live run while
this was being written took thirteen. That is tolerable for a command you
type once and intolerable for anything that polls, which is what the
statusline cache is for.

The slow path lives in agent-quotas (`agy-status-slow` forces it), and the
reason it stays is that it guesses at nothing.
Asking the backend ourselves would mean guessing at agy's login, its headers
and its response shape, and then keeping pace with agy as those change; an
attempt at exactly that was made here once and never worked. `agy -p /usage`
makes none of those guesses, and when the two disagree agy is the one that is
right by definition. It is what the statusline path falls back to when its
cache has nothing to say.

It gained one duty with the cache: after a successful run it merges its rows
in with `source: "slow"`, per bucket, so that the numbers `agy` itself printed
are available to the fast path afterwards, and the pools a run did not print
survive. That merge is best-effort -- a cache we could not write must never
cost us a report we already have in hand.

It runs the command from a fresh temporary directory rather than wherever you
happen to be. Antigravity pairs conversations to their working directory, and
a status check should not be able to leave any trace against a real project,
nor pick up that project's configuration. It runs under a timeout,
agent-quotas' `timeout_s` (default 90 s), because the answer comes from a
backend and a status check that hangs is worse than one that fails and says
so. The timeout is
sized for a cold CLI start-up followed by five sequential round trips, which
makes it far more generous than a status check would otherwise want.

### No GUI dependency

The slow path uses the selected account's file credentials directly, never
BrishGarden, so tmux and background processes use the same auth path.

## The bucket engine

agent-quotas' `internal/agy` is the only engine: bucket ids, the cache merge,
staleness, fallbacks, sign-in and the documents all live there, with tests
whose golden files came from the zsh engine it replaced. Cache notes go to
stderr in JSON mode, including slow fallbacks, so a cache miss cannot corrupt
a JSON document with prose. Credits are an optional addendum to the human
report; they have no reset and are not quota buckets.

## Reset notifications

`agy-status` can arm the one-shot background job that the Claude Code and
Codex notifiers use, the job that waits in a tmux session for a reset time and
then either tells you or types `Continue.` into the sessions you picked;
`docs/agent-usage-armed.md` covers the job itself, its idle gate, its knobs
and how to cancel it. This section is what is Antigravity's: the deadline, and
the delivery.

- `agy-status-notify` prints the report and arms a plain notification.
- `agy-status-auto-continue-kitty-fz` (alias `agyk`), `agy-status-auto-continue-tmux-fz`
  (alias `agyt`) and `agy-status-auto-continue-frontmost` (alias `agyfront`) print
  the report and arm a resume, named by how the text is delivered, like their
  Claude counterparts `cck`, `cct` and `ccfront`.
- `h-agy-status-arm-schedule` arms without printing a report, for when the
  report is already in front of you. The `h-` says the `-notify` forms are the
  intended way in, not that it is off limits.
- `agy-status-armed-cancel` and `agy-status-armed-status` are wrappers over
  the shared `h-agent-usage-armed-cancel` / `-status` for Antigravity's one
  session, `agy-status-armed`. That session is also in
  `agent-usage-armed-sessions`, so a call over that list sees it alongside the
  Claude, Codex and manual jobs.

All of these go through the `agy-status` gateway, so they follow
`agy_status_method` like everything else -- including its fallbacks, so a
notifier armed while the cache is empty is armed off the slow path's numbers
rather than off nothing.

### The deadline

A group counts as exhausted when what remains is at or below
`100 - agy_status_arm_full_pct`. The knob is a *used* percentage, the same
sense as `claude_code_usage_arm_full_pct`, and the complement is taken here
rather than exposing a remaining-percent knob, so that the two mean the same
thing even though agy reports the other side of the fraction.

The deadline is the **earliest** reset among the exhausted groups. This is
the opposite of the Claude notifier, which takes the latest, and the
difference is in what the windows are. Claude's 5-hour and weekly windows are
nested over one pool of work: a 5-hour rollover buys nothing while the weekly
limit is still spent, so only the last reset frees anything. Antigravity's
model groups are independent quotas: when the Gemini group is spent and the
Claude-and-GPT group resets first, you can go back to work on that group
without waiting for the other, so the first reset is the one worth waking up
for. The notification names the group that reset, since with two independent
pools "your limit has reset" would leave you to guess which one.

When no group is exhausted, nothing is armed and the report says so: a
notifier that fires hours later to tell you what the report already told you
is noise. Under `deus` the job arms for the earliest reset of all groups
regardless, which is how to exercise the mechanism without first running a
quota dry.

### Delivery

Antigravity has no queue command, nothing like `codex queue --thread`, so the
resume has to be typed: into a kitty window through `kitty @ send-text`, or
into a tmux pane through `tmux-pane-send-text`, or wherever the focus is for
`frontmost`. That is why Antigravity gets the same three variants Claude has
where Codex needs only one.

Both pickers are narrowed to Antigravity sessions. An Antigravity reset frees
Antigravity conversations and nothing else, so a Claude tab or a Codex thread
in the list would only be something to scroll past; this is the same reasoning
that narrows Claude's tmux picker to the rate-limited profile. The kitty
picker offers agy sessions showing in a kitty window, the tmux picker the
panes running `agy` right now, both multi-select, so several conversations
resume on one reset.

The caveat from the shared doc applies unchanged: the job trusts the reset
time it was armed with and does not re-run `agy-status` when it fires. And
when the reset agy printed turns out to be wrong, `agent-usage-auto-continue-at`
is there to name the time yourself.
