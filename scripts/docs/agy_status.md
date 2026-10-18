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
normal launch. Setup links `GEMINI.md`, `AGENTS.md`, hooks, policies and skills
when present, and synchronizes only the `statusLine` settings key. Other settings
remain per account. Existing independent files are refused for manual
reconciliation, never moved or overwritten. Override the shared root with
`AGY_SHARED_GEMINI_DIR` if needed.

```zsh
agy-acc-login cave
agy-acc-login fm2
```

The helper prints a `tmux attach-session` command. Complete Google sign-in
there, exit agy, and the same pane runs `models` to verify the account. If a token
already exists it verifies without signing out. Re-running the helper attaches
to the existing login session rather than replacing it. No sign-in is performed
by account setup or status reporting.

## File authentication

All shell launchers, login verification and print-mode quota calls inject
`SSH_CONNECTION=agy-file-auth` only into the agy process (preserving a real SSH
value when present). This is the native CLI's verified file-storage trigger.
No global SSH environment is changed. A direct `command agy` deliberately
bypasses shell wrappers and therefore bypasses this guarantee.

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
Status tests file existence without opening token contents. Cached quota
cannot establish whether a token is still valid: output distinguishes stored
file credentials from a live verified sign-in.

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

`agy-status` (`agys`) reports every configured account in a separate block. The
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
An account without a token is a normal `not signed in` status, with a login hint,
not a measurement error. It does not trigger quota-watcher error alerts.

`agy-status --json` emits an array of account objects with `account`, `label`,
`signed_in` (true/false/null), `auth_status`, `tier`, `email`, `error`, `message`, and
`buckets`. Buckets include `bucket_id`, `group`, `label`, `used_percent`,
`remaining_percent`, epoch `resets_at`, `resets_at_iso`, and `source` (cache/live).
`agy-status --account cave [--json]` selects one. Tier/email are shown only if
known from the statusline cache; token contents are never mined for identity.

The older `agy_status_json_p=y agy-status` interface retains the flat bucket
array for the current account, so existing reset notifiers keep their contract.
The notifier still arms only that account and its existing delivery pickers;
all-account resume scheduling is a separate follow-up.

For quota-watch, use `agy_status_method=statusline agy_status_cache_only_p=y
agy-status --json`. Missing/stale caches then report errors without starting
agy, requesting authentication, or contacting a backend. The quota-management
watcher consumes every account/bucket, preserves stale readings with a marker,
and uses the existing alert thresholds. Run agy with its statusline configured
to keep these cache readings fresh.

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

`agy-status` (alias `agys`) is a gateway. The knob `agy_status_method`, an
enum with the values `statusline` and `slow`, picks which implementation
runs, and the two named forms `agy-status-statusline` and `agy-status-slow`
force one without the caller having to know the knob. It is an enum rather
than a `_p` switch because "which implementation" is not a yes/no question:
a third value lived here for a while and was removed again, and neither its
arrival nor its departure disturbed the shape.

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
while we are working anyway, and reading it costs a `jq` over one small file.

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
exists. The hook sees five-hour and weekly buckets; `agy -p /usage` can print
only weekly ones, including additional model-specific pools. A writer that
replaced the file wholesale would therefore delete the five-hour windows every time the slow path ran, and the report would
lose rows for reasons nobody could see from the outside. Each writer stamps
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

`agy_status_statusline_max_age_s` is how old a cached bucket may be and still
be reported. Anything older is dropped from the report; if *nothing* is left,
the read falls back to the slow path and says so. A **negative** value means
"never drop on age" -- serve the cache however old it is, which is what you
want when agy has not been open for a while and you only need the reset times.

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

`isDeus` forces the slow path and does not open the cache at all, the same
"force, bypass the memo" convention the rest of zshlang uses. Asking under
`deus` is asking to distrust what is lying around.

### Provenance in the account header

The human report puts `cache 2m`, `live`, or both in each account header. Cache
age uses the oldest reported bucket, so a fresh row cannot conceal an older
one. Missing capture timestamps show `cache age unknown` (or `some undated`
when mixed with dated rows). Source diagnostics do not float above account
blocks; each failed account has one error row while the others stay visible.

The legacy flat-array engine and public `--json` keep source diagnostics on
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

Two groups exist today, `Gemini Models` and `Claude and GPT models`, both
labelled `Weekly Limit Remaining`. The percentage is what *remains*, the
opposite sense from the used-percent that Claude Code and Codex report, and
the reset is a UTC instant, not a duration. Only the weekly buckets are
printed; the backend knows about five-hour windows too, but `/usage` does not
show them.

It is called the slow path for a reason. Every call is a full CLI start-up
followed by five sequential backend round trips, and measured end to end it
takes anywhere from a couple of seconds to well over ten -- a live run while
this was being written took thirteen. That is tolerable for a command you
type once and intolerable for anything that polls, which is what the
statusline cache is for.

The slow path is kept as `h-agy-status-slow` (reachable as
`agy-status-slow`), and the reason it stays is that it guesses at nothing.
Asking the backend ourselves would mean guessing at agy's login, its headers
and its response shape, and then keeping pace with agy as those change; an
attempt at exactly that was made here once and never worked. `agy -p /usage`
makes none of those guesses, and when the two disagree agy is the one that is
right by definition. It is what the statusline path falls back to when its
cache has nothing to say.

It gained one duty with the cache: after a successful run it merges its rows
in with `source: "slow"`, per bucket, so that the numbers `agy` itself printed
are available to the fast path afterwards and the hook's five-hour windows
survive. That merge is best-effort -- a cache we could not write must never
cost us a report we already have in hand.

It runs the command from a fresh temporary directory rather than wherever you
happen to be. Antigravity pairs conversations to their working directory, and
a status check should not be able to leave any trace against a real project,
nor pick up that project's configuration. It runs under a timeout,
`agy_status_timeout_s`, because the answer comes from a backend and a status
check that hangs is worse than one that fails and says so. The timeout is
sized for a cold CLI start-up followed by five sequential round trips, which
makes it far more generous than a status check would otherwise want.

### No GUI dependency

The slow path uses the selected account's file credentials directly. It no
longer delegates authentication or quota calls to BrishGarden, so tmux and
background processes use the same auth path. `agy-status-run-garden` remains a
compatibility entry point; the old GUI/keychain guard is disabled.

## The bucket engine

Both implementations feed the all-account report described above. Internally
the engine retains remaining percentages, the source note, and the legacy flat
bucket JSON. Cache notes go to stderr in JSON mode, including slow fallbacks,
so a cache miss cannot corrupt a JSON document with prose. Credits remain an
optional `agy_status_credits_p=y` per-account engine addendum; they have no reset
and are not quota buckets.

## Reset notifications

`agy-status` can arm the one-shot background job that the Claude Code and
Codex notifiers use, the job that waits in a tmux session for a reset time and
then either tells you or types `Continue.` into the sessions you picked;
`docs/agent-usage-armed.md` covers the job itself, its idle gate, its knobs
and how to cancel it. This section is what is Antigravity's: the deadline, and
the delivery.

- `agy-status-notify` prints the report and arms a plain notification.
- `agy-status-continue-kitty-fz` (alias `agyk`), `agy-status-continue-tmux-fz`
  (alias `agyt`) and `agy-status-continue-frontmost` (alias `agyfront`) print
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
when the reset agy printed turns out to be wrong, `agent-usage-continue-at`
is there to name the time yourself.
