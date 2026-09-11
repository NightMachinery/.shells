# agy-status

`zshlang/auto-load/others/agy-status.zsh` reports how much of each
Antigravity quota is left and when it comes back, and can arm the shared
reset notifier on that. It is the Antigravity counterpart of the Claude Code
report in `docs/claude_code_usage.md` and of `codex-status` in
`docs/codex_status.md`. It began a good deal smaller than either, because
Antigravity gave us less to work with; it has since grown a second
implementation that reads the backend directly, and most of this document is
about the two of them and why both are kept.

## Two implementations, one report

`agy-status` (alias `agys`) is a gateway. The knob `agy_status_method`, an
enum with the values `direct` and `slow`, picks which implementation runs,
and the two named forms `agy-status-direct` and `agy-status-slow` force one
without the caller having to know the knob. It is an enum rather than a `_p`
switch because there is nothing boolean about "which of these two": a third
way in would be a third value, not a second switch.

The slow path is the one that answers by default for now: the direct path's
request is built from a credential whose exact shape was inferred rather
than read, and the backend has so far rejected the token it extracts. Until
that is settled, `agy-status-direct` is the way to exercise it, and the
default costs nothing but time.

Whichever runs, the output is the same contract (see "The report"), so
everything built on top -- the notifier's deadline, `agent-status` -- neither
knows nor cares which path produced its input. The difference is in cost, in
what can go wrong, and in how many rows come back.

## The slow path: asking agy

Antigravity keeps its quota in the CLI's process memory and nowhere else.
There is no quota file under `~/.gemini/antigravity-cli/`, no `agy usage`
subcommand, no daemon to ask (`agy remote-control` only starts, stops and
reports the daemon), and the logs record percentages at most, never a reset
time. That leaves two sources: agy's own print mode, and the backend call
behind agy's numbers.

The slow path is the first of these, and for a while it was the only one.
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

The slow path is kept, unchanged, as `h-agy-status-slow` (reachable as
`agy-status-slow`), and the reason it stays is the reason that once argued
against having a direct path at all. The backend wants the OAuth token agy
holds in the Keychain, and calling it ourselves means guessing at agy's login,
its headers and its response shape, and then keeping pace with agy as those
change. `agy -p /usage` makes none of those guesses. When the two disagree,
agy is the one that is right by definition, and when the direct path breaks,
this is what it tells you to run.

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
read the login Keychain. The knob is `agy_status_garden_p`, an enum
`auto|y|n`, where `auto` means "when we are in an ssh session". It is
proactive rather than a retry, because over ssh the local read *cannot*
succeed; attempting it first would only buy a round trip and a misleading
error.

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

Both paths emit the same thing. The human-readable report prints one line per
bucket: the group name, the remaining percentage, and the reset as local time
with how far off it is, since "resets at 03:00 UTC" is not a number anyone
wants to convert in their head at the moment they have just been cut off.
Colour follows `agy_status_color`: `auto` colours only when stdout is a
terminal, while `always` and `never` force it either way, which is what a
wrapper that captures the output and prints it later needs.

`agy_status_json_p=y` prints JSON instead: an array with one object per
bucket, carrying `group`, `label`, `remaining_percent`, `resets_at` as epoch
seconds and `resets_at_iso` as the string upstream sent. The epoch is what
the notifier and anything else scripted on top of this want, so the
conversion is done once here rather than by every consumer; the original
string is kept beside it so a disagreement between the two is something you
can see. The direct path adds `bucket_id` and `remaining_amount`, which
existing consumers ignore; that the two paths' JSON is interchangeable was
checked by feeding the direct path's output through the jq in
`h-agy-status-arm-deadline`.

Credits are the one number with no endpoint of ours. `agy -p /credits` prints
`Remaining credits` and a count on one row, then an upgrade URL. `agy-status`
leaves credits out of the default report and out of the JSON, since they are
a purchased pool with no reset to wait for; `agy_status_credits_p=y` appends
them to the human report for when the question is whether to buy more. On
the direct path that addendum is still an `agy` start-up, so asking for it is
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
`agy_status_method` like everything else.

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
