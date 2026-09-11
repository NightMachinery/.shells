# agy-status

`zshlang/auto-load/others/agy-status.zsh` reports how much of each
Antigravity quota is left and when it comes back, and can arm the shared
reset notifier on that. It is the Antigravity counterpart of the Claude Code
report in `docs/claude_code_usage.md` and of `codex-status` in
`docs/codex_status.md`. It began a good deal smaller than either, because
Antigravity gave us less to work with; it has since grown two more
implementations, and most of this document is about the three of them and why
all three are kept.

## Three implementations, one report

`agy-status` (alias `agys`) is a gateway. The knob `agy_status_method`, an
enum with the values `statusline`, `direct` and `slow`, picks which
implementation runs, and the three named forms `agy-status-statusline`,
`agy-status-direct` and `agy-status-slow` force one without the caller having
to know the knob. It is an enum rather than a `_p` switch because there is
nothing boolean about "which of these three": the third way in arrived as a
third value, exactly as the shape predicted, rather than as a second switch.

`statusline` is the one that answers by default. It is the only fast path
that is built on something Antigravity documents -- a hook it invites you to
configure -- rather than on something we inferred, and unlike `direct` it has
been seen to work. `slow` is what it falls back to and what to reach for when
you need the number `agy` itself would print; `direct` remains unproven
against the live backend, so it answers only when asked for by name.

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
The snapshot carries the whole quota table -- all four buckets, not just the
two that `agy -p /usage` prints -- along with the model, the plan tier, the
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
exists. The hook knows all four buckets; `agy -p /usage` prints only the two
weekly ones. A writer that replaced the file wholesale would therefore delete
the two five-hour windows every time the slow path ran, and the report would
lose rows for reasons nobody could see from the outside. Each writer stamps
its rows with a `source`, so a row's provenance is readable rather than
guessed at.

Bucket ids are agy's own -- `gemini-5h`, `gemini-weekly`, `3p-5h`,
`3p-weekly` -- and the slow path's rows are mapped onto them rather than the
other way round, by `h-agy-status-bucket-id`; `h-agy-status-bucket-names` is
the inverse, and is what gives the cache-backed report the same group and
label text the other two paths print. A row that maps to no id we know gets a
stable id derived from its own group and label instead of being dropped: a
group upstream invents should show up in the report looking odd, not vanish
between two writers.

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

This is a fallback where the direct path deliberately has none, and the
difference is worth stating. The direct path refuses to fall back because a
silent fallback would hide a *breakage* -- a fast path that has been dead for
months while the report kept appearing. Here there is nothing broken to hide:
an empty cache means the hook has not run, which is a fact about whether you
have been using agy, not about whether anything works. And it is not silent
either way -- the reason is always printed.

`isDeus` forces the slow path and does not open the cache at all, the same
"force, bypass the memo" convention the rest of zshlang uses. Asking under
`deus` is asking to distrust what is lying around.

The human report says, once, how old the oldest row it is serving is and
which writers produced the set. In JSON mode that note goes to stderr, so
stdout stays the bare array the notifier parses. A bucket carrying no
`captured_at` at all is reported as such rather than folded into the age:
"unknown" and "captured at the epoch" are different claims, and treating them
alike renders an age of half a million hours.

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

`agy-status` printing a note about falling back means the hook has not run
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
type once and intolerable for anything that polls, which is what prompted the
direct path.

The slow path is kept as `h-agy-status-slow` (reachable as
`agy-status-slow`), and the reason it stays is the reason that once argued
against having a direct path at all. The backend wants the OAuth token agy
holds in the Keychain, and calling it ourselves means guessing at agy's login,
its headers and its response shape, and then keeping pace with agy as those
change. `agy -p /usage` makes none of those guesses. When the two disagree,
agy is the one that is right by definition. It is what a failing direct
report tells you to run, and what the statusline path falls back to when its
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
check that hangs is worse than one that fails and says so. That timeout is
the slow path's alone: it is sized for a cold CLI start-up, which would be
far too generous for a single HTTP request.

## The direct path: asking the backend

`h-agy-status-direct` (reachable as `agy-status-direct`) hands the work to
`python/agy_status.py`, which asks the one endpoint behind agy's numbers,
`v1internal:retrieveUserQuotaSummary` on the internal Code Assist API, and
renders the answer itself. One HTTP round trip in the common case, two at
most, and no CLI start-up.

The credential is the OAuth token agy keeps in the macOS login Keychain, as a
generic password under service `gemini` and account `antigravity`. The CLI
stores it through go-keyring, which base64-encodes any value that is not
plain ASCII and marks it with a `go-keyring-base64:` prefix; the Keychain
hands the marker back too, so the script strips it and decodes before parsing
the JSON underneath. The script only ever *reads* this item. The token is
agy's to manage, and the section on refreshing below is about how we stay out
of that.

The endpoint wants a project id. When the credential names one, that is used;
when it does not, the script asks `v1internal:loadCodeAssist` for it first.
That is a second round trip, so it is made only when needed -- the whole
point of this path is to make as few as possible.

Two hosts answer the API, and the order they are tried is a constant in the
script: `cloudcode-pa.googleapis.com` first, then
`daily-cloudcode-pa.googleapis.com`. agy is reported to talk to the `daily-`
one, but the plain host has answered the same calls, so it leads; `--host`
forces one when you need to know which is misbehaving. A 401 stops the walk
rather than moving on to the next host, since that is the credential being
refused, and asking a second host with the same token only turns one clear
error into two.

The request carries the same headers the CLI sends, including a User-Agent
with a pinned Antigravity version string. Pinned rather than read from
`agy --version`, because shelling out to `agy` is the very cost this path
exists to avoid; if the backend ever starts caring about the version, the
constant is bumped in the script. Each request runs under
`agy_status_direct_timeout_s`, the direct path's own per-request timeout,
kept separate from the slow path's for the reason given above.

### Refreshing the token

The script never refreshes the token itself. When the credential's own expiry
says it is stale, or the backend answers 401 -- the expiry is only what the
item claims -- and relogin is enabled (`--relogin` / `--no-relogin`), it runs
`agy -p /usage` once, from a fresh temporary directory, under a timeout.
Print mode `/usage` is a built-in agy answers without a model turn, but agy
still does the OAuth refresh every start-up does and rewrites the Keychain
item, so one such run costs nothing, leaves no conversation behind, and hands
the next request a live credential. The script re-reads the item and retries
the request once. At most one attempt, ever: the retry cannot become a loop.

What it deliberately does *not* do is a refresh-token grant against
`oauth2.googleapis.com`. That would need the client id and secret the CLI
registers with, which we do not know, and a wrong one buys an
`invalid_client` error rather than a token. Letting agy do the refresh also
means we never see the refresh token at all, only the access token it leaves
behind. This mirrors the relogin in `python/claude_code_usage.py`, for the
same reasons.

### No silent fallback

The direct path never quietly becomes the slow one. When anything breaks --
no Keychain item, a credential with no token in it, an unreachable host, a
response with no buckets -- it prints what broke to stderr, names
`agy-status-slow` as the way round it, and exits non-zero.

This is deliberate and worth defending, because the reflex is to fall back. A
fallback that fires silently is a breakage nobody notices for months: the
report keeps appearing, only slower, and the fast path has been dead the
whole time. The whole point of having a fast path is that you can tell
whether it is working, and a failure that says so is how you tell.
`agy-status-slow` is one word to type when you need the number now.

### More rows than agy prints

The direct path can return more rows than the slow one, because the backend
reports every bucket and `/usage` prints only the weekly ones. Bucket ids
seen upstream are `gemini-5h`, `gemini-weekly`, `3p-5h` and `3p-weekly`, so a
direct report shows a five-hour window beside each weekly one. Buckets the
backend marks `disabled` are skipped. During upstream's migration the
response has carried buckets both grouped under a display name and as a
legacy flat list, sometimes both at once, so the script reads the groups
first and takes from the flat list only what the groups did not already
carry: a bucket id shown twice is worse than a field read from the older
shape.

For the notifier this is a change in inputs, not in behaviour. The deadline
is the earliest reset among the exhausted buckets (see below), and a spent
five-hour window that rolls over before the weekly one is exactly the reset
worth waking up for.

### An absent remaining is a full bucket

The API is proto3, and proto3 omits default values from the wire. A bucket at
100% remaining can therefore arrive carrying neither `remainingFraction` nor
`remainingAmount`. The script reads an absent remaining as "nothing spent",
not as zero. Reading it as zero would report a full quota as exhausted and
arm the reset notifier for a reset that changes nothing -- the worst kind of
false alarm, one that wakes you up to tell you nothing happened.

Where a bucket reports a `remainingAmount`, a count, and no fraction, the
human report shows the count rather than inventing a percentage. No total
comes with it to divide by, and a number the report cannot back up is worse
than a number in a different unit. The JSON still carries a
`remaining_percent` for such a row, at the assumed full value, because the
contract requires one; the count sits beside it as `remaining_amount`.

### The brish garden guard

Reading the login Keychain only works from a process attached to the GUI
session. Over ssh the keychain search list collapses to the System keychain
alone, so the lookup finds nothing and `security` reports the credential as
simply absent -- not locked, not forbidden, absent, which is the wrong
diagnosis and would send you off to log in again for nothing.

So `h-agy-status-run-direct`, the one place the script is invoked, delegates
to the brish garden when `h-agy-status-garden-p` says so and the garden is
alive. The garden's worker shells are attached to the GUI session and can
read the login Keychain. It is proactive rather than a retry, because a
detached local read *cannot* succeed; attempting it first would only buy a
round trip and a misleading error.

The slow path is delegated too, through `agy-status-run-garden`, which forces
the guard off so a worker can never bounce the command on to another worker.
It needs the guard for a reason that is easy to miss: `agy` reads the
Keychain itself whenever the access token it holds has expired and has to be
refreshed. Detached, that refresh cannot happen, and rather than failing
`agy` falls back to its interactive browser login and sits there printing an
OAuth URL until its own timeout expires. A report that hangs for a minute
and then asks you to log in is a worse answer than a slow one.

The knob is `agy_status_garden_p`, an enum `auto|y|n`. Under `auto` the
question asked is whether `launchctl managername` answers `Aqua`, which it
does only in a session attached to the GUI login -- the very attachment the
Keychain search list depends on. That names the real condition where an
ssh test only guesses at it: an agent-spawned shell on this machine is
`Background` and cannot read the Keychain either, despite no ssh being
involved. Where there is no `launchctl`, the ssh test stands in.

The *whole* invocation is delegated, not just the Keychain read. The token
never leaves the GUI-attached process: only the rendered report or the JSON
array comes back over the garden's transport. Fetching the token into the ssh
session and making the HTTP request from there would work, and would put the
credential in one more place than it needs to be. This is the same shape as
the Claude Code usage report's garden guard, and `docs/claude_code_usage.md`
has the longer version of the reasoning.

One consequence: inside the garden, stdout is a pipe whatever you are looking
at, so `agy_status_color` is resolved on our side before the delegation and
passed to the script as an explicit `--color`, rather than left to the
script's own `auto`, which would decide "no colour" for the command run most
often.

### What has been verified

The direct path's mapping from response to rows, its JSON shape, its error
handling and its rendering are covered by fixture tests that need neither a
network nor a Keychain. What has *not* been done is an end-to-end run against
the live backend. Reading the credential was blocked in the environment where
the path was written, and the first live attempt got as far as reading the
Keychain item and then reported that it carried no field named
`access_token`. The extraction was made tolerant in response: it accepts
several spellings of each field (snake_case and camelCase) and a credential
nested one level inside a wrapper object, and when it still finds no token it
names the fields that *were* present -- names only, never values -- so the
shape can be corrected from the error message alone.

So, plainly: until someone runs `agy-status-direct` and sees numbers, the
direct path is unproven against the live backend. If it fails, the error says
why, and `agy-status-slow` is there in the meantime.

## The report

All three paths emit the same thing. The human-readable report prints one
line per bucket: the group name, the remaining percentage, and the reset as
local time with how far off it is, since "resets at 03:00 UTC" is not a number
anyone wants to convert in their head at the moment they have just been cut
off.
Colour follows `agy_status_color`: `auto` colours only when stdout is a
terminal, while `always` and `never` force it either way, which is what a
wrapper that captures the output and prints it later needs.

`agy_status_json_p=y` prints JSON instead: an array with one object per
bucket, carrying `group`, `label`, `remaining_percent`, `resets_at` as epoch
seconds and `resets_at_iso` as the string upstream sent. The epoch is what
the notifier and anything else scripted on top of this want, so the
conversion is done once here rather than by every consumer; the original
string is kept beside it so a disagreement between the two is something you
can see. Every path now carries `bucket_id` beside those, and the direct one
adds `remaining_amount`; existing consumers ignore both. That the three
paths' JSON is interchangeable was checked the same way each time, by feeding
the output through the jq in `h-agy-status-arm-deadline`.

Credits are the one number with no endpoint of ours. `agy -p /credits` prints
`Remaining credits` and a count on one row, then an upgrade URL. `agy-status`
leaves credits out of the default report and out of the JSON, since they are
a purchased pool with no reset to wait for; `agy_status_credits_p=y` appends
them to the human report for when the question is whether to buy more. On
the fast paths that addendum is still an `agy` start-up, so asking for it is
asking for that cost.

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
