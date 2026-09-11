# Resuming an agent when its usage limit resets

`zshlang/auto-load/others/agent-usage.zsh` is the half of the rate-limit
handling that does not care which agent hit the limit: a one-shot background
job, armed for the reset, that waits for the time to come and then either
tells you or types `Continue.` into the sessions you picked when you armed it. What it does *not* know is
where the reset time comes from. Each agent supplies that and hands the
deadline over:

- Claude Code reads it from the usage endpoint, per profile;
  `docs/claude_code_usage.md` covers the windows, the roles and the entry
  points (`cck`, `cct`, `ccfront` and their profile variants).
- Codex reads it from `codex-status --json`; `docs/codex_status.md` covers
  `codex-status-notify` and `codex-status-continue-fz`.
- Antigravity reads it from `agy-status`, which asks `agy -p /usage`;
  `docs/agy_status.md` covers `agy-status-notify` and the
  `agy-status-continue-*` variants.
- Anything else, and any agent whose own reset time you do not trust, goes
  through `agent-usage-continue-at <when>`, where you name the time yourself.

That list is where the per-agent deadline sources are enumerated; the linked
docs say how each one is read and reduced to a single time.

The shared entry is `h-agent-usage-arm <session> <reset-epoch> <msg>`.
Everything below is what happens once it has been called.

## Arming

The job is a tmux session made by `tmuxnewsh2`, named after the function that
scheduled it minus the `h-`, so what `tmux ls` shows and what you called line
up. That name is also the lock. `tmuxnew` kills the previous session's
processes before creating the replacement, so re-arming *replaces* the pending
job rather than stacking a second one, and exactly one thing happens per
reset. No marker file, redis key or pidfile is needed, and the bookkeeping
cannot drift from whether the job exists: the deadline, the action and the
targets are recorded as options on the session itself,
`@agent_usage_arm_deadline`, `@agent_usage_arm_action` and
`@agent_usage_arm_targets`, and vanish with it. The autoname hooks are
switched off on that session, because the name is how the job is found and
re-armed, and the agent it resumes must not rename it out from under the next
arm.

The tmux server is independent of BrishGarden, so `brishz-restart` does not
silently disarm anything. A reboot does, and the next report re-arms.

Arming adds the grace period to the reset time and refuses a deadline that is
already past, which is what stale usage data looks like. The target picker runs
only once the job is actually going to be armed, so a report that changes
nothing never puts a picker in your way; presetting `agent_usage_arm_targets`
(space separated, `kitty:95 tmux:%3 frontmost`) skips the picker altogether,
which is what makes the whole thing callable from a script or a test.

Since `~/.tmux.conf` sets `remain-on-exit` globally, a job that has fired
leaves its session behind holding a dead pane with its report on screen. The
status helper reports that as already fired -- which is also how to check
whether a notification went off while you were away -- and the next arm, or a
cancel, clears it.

## Waiting

The job polls the wall clock every `agent_usage_arm_poll_s` seconds instead
of issuing one long `sleep`. A suspend skews a single five-hour sleep by however
long the lid was closed; a poll notices on wake that the deadline has passed
and fires at once.

It fires a little after the reset rather than on it, so the endpoint has
actually flipped by the time the job claims it has. The margin is
`agent_usage_arm_grace_s` when the action is a notification and
`agent_usage_continue_grace_s` when it is a resume, and the second is
deliberately the longer of the two: an early notification is harmless, an
early resume is spent on a session that is still blocked. The manual
`agent-usage-continue-at` adds no grace at all, since there you named the
time.

Notifications go out through `notif` under one fixed `notif_group`, so a
repeat replaces the previous one instead of piling up in Notification Center;
`docs/bell-auto.md` has the mechanics.

## What it does when it fires

`agent_usage_arm_action` is `notif`, a notification saying which limits
reset, or `continue`, which additionally types `agent_usage_continue_text`
into each recorded target and then notifies with what it managed to reach.

### Targets

A target is one of four kinds, and the kind decides how the text is delivered:

- `kitty:<window-id>` -- typed into that one kitty window with `kitty @
  send-text --match id:<n>`. No focus stealing, no global keystrokes, and it
  does not care which window is frontmost or whether the display is asleep.
  `send-text` documents that it "always succeeds, even if no text was sent to
  any window", so its exit status proves nothing; the window is looked up in
  `kitty @ ls` first, otherwise a tab closed during the wait would swallow the
  resume while the job reported success.
- `tmux:<pane-id>` -- typed into that pane through `tmux-pane-send-text`,
  which sends the text with `send-keys -l` and then a separate `Enter`, so the
  text can never be parsed as key names. It fails when the pane is gone or
  `#{pane_dead}`, and that check has to be explicit because `remain-on-exit`
  is global here: a pane whose process has exited is still a perfectly valid
  `send-keys` target, it just types into nothing. On top of that the fire path
  requires the pane to still host a *live agent* -- it recomputes
  `h-agent-session-tmux-panes` and insists on membership -- so a session that
  finished during the wait never gets `Continue.` typed into the shell that
  replaced it.
- `codex:<thread-id>` -- queued with `codex queue --thread <id> --message`.
  Codex accepts a message for a thread by name, so this path needs no window,
  no focus and no awake display, and it cannot land in the wrong place. Both
  pickers prefer it whenever the chosen session is a Codex one.
- `frontmost` -- typed wherever the keyboard focus happens to be, through
  `hs-type-continue`, a global synthetic keystroke with no window targeting.
  It first wakes the display via `hs.caffeinate.declareUserActivity()` and
  pauses a beat: `displaysleep` on this machine is the same length as the idle
  threshold below, so by the time the job fires the screen is asleep, and the
  first synthetic keypress would otherwise be eaten waking it, typing
  `ontinue.` instead. It exists because a session outside kitty and tmux
  cannot be reached any other way, and it is never the default, since with
  several agents, a browser and a chat app open, blind typing can send
  `Continue.` as a chat message.

Every unreachable target degrades to a notification naming it. What
`Continue.` cannot do is pick its own target: all of an agent's sessions share
one rate limit, so "the session that was blocked" is ambiguous by
construction, and the target has to be chosen, not guessed.

### Choosing targets at arm time

`agent_usage_continue_via` names the mechanism, and the mechanism decides the
picker. The split is by *how the text gets there* rather than by agent,
because that is what actually differs: a Claude session in a kitty tab and a
Claude session in a tmux pane are the same conversation reached two ways, and
which way is right depends on how you are working, not on who is answering.

- `kitty` -- `h-agent-usage-continue-targets-kitty-fz`, built on
  `agent-session-live-fz`: every agent session showing in a kitty window,
  Claude Code, Codex and Antigravity alike, each row with its agent's glyph
  and a preview of the session's title, when it last moved and its last
  prompt. Multi-select, so several tabs resume at once. One synthetic row,
  `frontmost`, sits above the sessions.
- `tmux` -- `h-agent-usage-continue-targets-tmux-fz`: the panes running an
  agent right now, one row each, from `h-agent-session-tmux-panes`, which
  walks every live agent pid up its parents to the pane holding it over one
  `ps` and one `tmux list-panes -a`. The pane *id* is what gets recorded, not
  the session name, because the autoname hooks rename sessions on every
  prompt and a pane does not get renamed. Rows are filtered to
  `agent_session_agents` when the caller sets it, and, when the caller sets
  `agent_usage_continue_profile`, to panes whose Claude config home matches
  that profile: a Claude reset frees one profile's sessions, so the other
  profile's panes would only be noise. Also multi-select.
- `frontmost` -- no picker; the single target is `frontmost`.

Both pickers share one row-to-target mapper: a Codex row becomes
`codex:<thread-id>` whichever picker it came from, since queueing beats
typing; anything else becomes `kitty:<id>` or `tmux:<pane>` according to the
picker. Antigravity has a reset source but no queue command, so an agy row
becomes a window or pane target and is typed into like a Claude one; the
pickers `agy-status` runs are narrowed to Antigravity sessions, for the same
reason Claude's tmux picker is narrowed to a profile.

## The idle gate, and failing safe

A resume only types if the keyboard and mouse have been untouched for at
least `agent_usage_continue_idle_min_s`. If you are at the machine you get an
ordinary notification instead and can resume yourself. It also declines when
the screen is locked and when the idle time cannot be read at all. The
principle is the same in every case: the notification goes out either way, so
an unwanted resume is the worse of the two errors, and anything that cannot be
established counts against typing.

The idle time is `hs.host.idleTime()` through `h-hammerspoon-eval`, and what
gets checked is the returned string rather than the exit status, because
Hammerspoon exits 0 whether or not the Lua found anything; a non-number means
"cannot tell", and the job declines on that.

## Knobs

All are `typeset -g` in `agent-usage.zsh`, which is the one place their
defaults live.

- `agent_usage_arm_action` -- `notif` or `continue`.
- `agent_usage_continue_via` -- `kitty`, `tmux` or `frontmost`; which picker
  runs at arm time and, for `frontmost`, that none does.
- `agent_usage_arm_poll_s` -- how often the waiting job re-reads the clock.
- `agent_usage_arm_grace_s` -- how long after the reset a notification
  fires.
- `agent_usage_continue_grace_s` -- the same for a resume; keep it the longer
  of the two.
- `agent_usage_continue_idle_min_s` -- how long the keyboard must have been
  untouched before typing is allowed.
- `agent_usage_continue_text` -- what gets typed; the newline that submits it
  is added per mechanism.
- `agent_usage_arm_targets` -- preset targets, skipping the picker.
- `agent_usage_arm_log` -- where fires are logged.

## The log

Every fire appends one line to `agent_usage_arm_log`: which session fired,
which targets it tried, whether it typed or declined, and why. The dead tmux
pane a fired job leaves behind says the same thing, but only until the next
reboot, and a job that types into your sessions while you are away should stay
answerable for it afterwards.

## Cancel and status

`h-agent-usage-armed-cancel <session…>` kills the named jobs, reaping the
already-fired ones too, since clearing those out is what someone running a
cancel actually wants. `h-agent-usage-armed-status <session…>` prints, per
session, whether it is armed, for when and how long from now, and the pending
action and targets, for example `[action: continue -> tmux:%3 codex:0199…]`.
The action is shown because arming a resume replaces a plain notifier in the
same session and the reverse, and a downgrade should be visible rather than
silent.

Each agent wraps these with its own session list --
`claude-code-usage-armed-cancel` / `-status`, `codex-status-armed-cancel` /
`-status`, `agy-status-armed-cancel` / `-status` -- and
`agent-usage-armed-sessions` prints every session any of them can live in,
Claude's, Codex's, Antigravity's and the manual one, so a single call over
that list sees them all.

## Naming the time yourself

`agent-usage-continue-at <when>` arms a resume for a time you give in natural
language -- `2h`, `tomorrow 9am`, `in 45 minutes` -- parsed by `datenat-unix`.
A phrase that resolves to the past is refused, but by the arm rather than the
parser: `h-agent-usage-arm` rejects any deadline that has already gone,
whoever computed it. The parser's own future-only mode is deliberately not
used, because it rounds down to midnight before it checks and so would refuse
`in 45 minutes` on any day that still has one. No grace is added, because you
named the time rather than an endpoint predicting it. The pickers are the
shared ones with no agent or profile filter, so every live session is
offered, an Antigravity pane included. It is the fallback for every agent,
not only for those without a status command: when an endpoint's reset time
is wrong, or you know something it does not, this is how to override it. It
lives in its own session,
`agent-usage-continue-at`, so it coexists with the Claude and Codex jobs
instead of replacing one, and shows up in their status through
`agent-usage-armed-sessions`.

`agent-usage-continue-at-tmux-fz`, `agent-usage-continue-at-kitty-fz` and
`agent-usage-continue-at-frontmost` are presets over
`agent_usage_continue_via`, with `acat`, `acak` and `acafront` as their short
aliases.

## All three reports at once

`agent-status` (alias `agst`) prints the Claude Code, Codex and Antigravity
reports one after another, each under a header carrying the agent's glyph
and name. The three commands run concurrently through the repo's GNU
parallel wrapper, `parallelm`, so the whole thing takes about as long as the
slowest of them rather than the sum, and `--keep-order` keeps the sections
in the configured order however the jobs happen to finish. The output goes
through `pager-if-overflow`, so a report longer than the screen pages and a
short one simply prints.

The jobs run inside BrishGarden shells and therefore see a pipe, not your
terminal, so colour is decided once, up front, by `agent_status_color`
(`auto`, `always` or `never`, where `auto` means "when stdout is a
terminal") and passed to each command explicitly rather than left to their
own tty checks. The list of agents is written one per line inside the
function, so dropping one is a matter of commenting out a line; the array
knob `agent_status_agents` overrides it without editing. A report that
fails prints its error inside its own section, and the others still appear.

`agent-usage-armed-cancel-fz` is the middle ground between the two bulk
commands, for when several agents have jobs armed and only one or two of
them should go -- `agent-usage-armed-status` prints every job but cancels
nothing, and `agent-usage-armed-cancel` with no arguments takes them all.
It walks the same `agent-usage-armed-sessions` list and opens an fzf picker
with one row per session that still exists, armed ones first and soonest
at the top, each showing the session name, when it fires (RFC-3339) and how
long from now, plus the pending action and targets, with sessions whose job
has already fired listed last as `fired` alongside the same action and
targets so they can be reaped from the picker too. It is multi-select, and
enter cancels everything selected through `h-agent-usage-armed-cancel`;
if nothing is armed it says so and does nothing, and selecting nothing
cancels nothing.

## The caveat

The job trusts the reset timestamp it was armed with, plus the grace, and does
not re-check usage when it fires. If the endpoint lags behind its own reset
time, or the limit is hit again in the meantime, the resume is spent on a
session that is still blocked, and nothing remains armed afterwards. The
notification and the log line still go out, so you can tell that it happened.
