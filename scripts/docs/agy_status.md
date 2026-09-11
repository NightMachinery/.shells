# agy-status

`zshlang/auto-load/others/agy-status.zsh` reports how much of each
Antigravity quota is left and when it comes back, and can arm the shared
reset notifier on that. It is the Antigravity counterpart of the Claude Code
report in `docs/claude_code_usage.md` and of `codex-status` in
`docs/codex_status.md`, and it is a good deal smaller than either, because
Antigravity gives us less to work with.

## Data source

Antigravity keeps its quota in the CLI's process memory and nowhere else.
There is no quota file under `~/.gemini/antigravity-cli/`, no `agy usage`
subcommand, no daemon to ask (`agy remote-control` only starts, stops and
reports the daemon), and the logs record percentages at most, never a reset
time. The backend call behind the numbers,
`cloudcode-pa.googleapis.com/v1internal:retrieveUserQuotaSummary`, wants the
OAuth token that agy holds in the Keychain, so calling it ourselves would mean
reimplementing agy's login and then keeping pace with it, for a number agy
will print on request anyway.

So the source is agy's own print mode. `agy -p /usage` (alias `/quota`)
answers the built-in slash command and exits: no agent turn is started, no
quota is spent, and no conversation is left behind for `agy-resume-fz` to
find. It prints one tab-separated row per model group:

    <group>	<label>	<remaining>%	<reset, ISO-8601 UTC>

Two groups exist today, `Gemini Models` and `Claude and GPT models`, both
labelled `Weekly Limit Remaining`. The percentage is what *remains*, the
opposite sense from the used-percent that Claude Code and Codex report, and
the reset is a UTC instant, not a duration. `agy -p /credits` prints
`Remaining credits` and a count on one row, then an upgrade URL. `agy-status`
leaves credits out of the default report and out of the JSON, since they are
a purchased pool with no reset to wait for; `agy_status_credits_p=y` appends
them to the human report for when the question is whether to buy more.

`agy-status` runs the command from a fresh temporary directory rather than
wherever you happen to be. Antigravity pairs conversations to their working
directory, and a status check should not be able to leave any trace against a
real project, nor pick up that project's configuration. It runs under a
timeout, `agy_status_timeout_s`, because the answer comes from a backend and
a status check that hangs is worse than one that fails and says so.

## The report

The human-readable report prints one line per group: the group name, the
remaining percentage, and the reset as local time with how far off it is,
since "resets at 03:00 UTC" is not a number anyone wants to convert in their
head at the moment they have just been cut off. Colour follows
`agy_status_color`: `auto` colours only when stdout is a terminal, while
`always` and `never` force it either way, which is what a wrapper that
captures the output and prints it later needs.

`agy_status_json_p=y` prints JSON instead: an array with one object per
group, carrying `group`, `label`, `remaining_percent`, `resets_at` as epoch
seconds and `resets_at_iso` as the string agy printed. The epoch is what the
notifier and anything else scripted on top of this want, so the conversion is
done once here rather than by every consumer; the original string is kept
beside it so a disagreement between the two is something you can see.

## Reset notifications

`agy-status` can arm the one-shot background job that the Claude Code and
Codex notifiers use, the job that waits in a tmux session for a reset time and
then either tells you or types `Continue.` into the sessions you picked;
`docs/agent-usage-notif.md` covers the job itself, its idle gate, its knobs
and how to cancel it. This section is what is Antigravity's: the deadline, and
the delivery.

- `agy-status-notify` prints the report and arms a plain notification.
- `agy-status-continue-kitty-fz` (alias `agyk`), `agy-status-continue-tmux-fz`
  (alias `agyt`) and `agy-status-continue-frontmost` (alias `agyfront`) print
  the report and arm a resume, named by how the text is delivered, like their
  Claude counterparts `cck`, `cct` and `ccfront`.
- `h-agy-status-notif-schedule` arms without printing a report, for when the
  report is already in front of you. The `h-` says the `-notify` forms are the
  intended way in, not that it is off limits.
- `agy-status-notif-cancel` and `agy-status-notif-status` are wrappers over
  the shared `h-agent-usage-notif-cancel` / `-status` for Antigravity's one
  session, `agy-status-notif-schedule`. That session is also in
  `agent-usage-notif-sessions`, so a call over that list sees it alongside the
  Claude, Codex and manual jobs.

`agys` is the short alias for `agy-status` itself.

### The deadline

A group counts as exhausted when what remains is at or below
`100 - agy_status_notif_full_pct`. The knob is a *used* percentage, the same
sense as `claude_code_usage_notif_full_pct`, and the complement is taken here
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
