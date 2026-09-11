# `/auto-continue`: a session arms itself to resume after its usage limit resets

`/auto-continue` is one skill, shared by Claude Code (both seats), Codex and
Antigravity, the same way `/done` is. Typed in a running session it registers
that session to be resumed when its account's usage limit resets, and makes
sure a watcher is running for that account. When the limit is hit, the watcher
arms the one-shot job from `docs/agent-usage-armed.md` with this session as its
target, and at the reset the job types `Continue.` into the session — unless
you are at the keyboard, in which case you get a notification instead.

The difference from `cct`, `cck`, `csc` and `agyt` is who decides *when* and
*what*. Those are run by a person from an outside shell after the limit has
been hit, and open a picker for what to resume. Here the session itself says
"resume me" ahead of time, and a watcher does the waiting; there is nothing to
pick, because the session knows how it can be reached. Everything downstream is
the same engine: grace, the idle gate, fail-safe notifications, the log,
`agent-usage-armed-status` and `agent-usage-armed-cancel`.

Code: `zshlang/auto-load/others/agent-auto-continue.zsh`, the skill file
`configFiles/agent-skills/auto-continue/SKILL.md`, and
[agfi:h-codex-status-arm-auth] in `codex.zsh`.

## Using it

At an agent's prompt:

- `/auto-continue` — register this session and start its scope's watcher.
- `/auto-continue off` — forget this session; with nothing left in the scope,
  cancel its armed job and stop the watcher.
- `/auto-continue status` — this session, its watcher, its scope's armed job.
- `/auto-continue --frontmost` — register with the `frontmost` target, i.e.
  blind typing into whatever holds the keyboard at reset time. Never chosen on
  its own; see "Targets" in `agent-usage-armed.md` for why.

From any shell: [agfi:agent-auto-continue-list] shows every registration,
marked `live` or `dead`, with each scope's watcher and armed job;
`agent-auto-continue-list --prune` forgets the dead ones (the watcher does
this itself on every tick). The armed jobs also appear in
[agfi:agent-usage-armed-status] and go away with
[agfi:agent-usage-armed-cancel], like every other armed job.

The skill runs [agfi:agent-auto-continue-on] (or `-off`, `-status`) through
`zsh -ic` from the agent's own shell tool, for the same reason `/done` does: that
shell inherits the environment that names the session — `CLAUDE_CODE_SESSION_ID`
and `CLAUDE_CONFIG_DIR`, `CODEX_THREAD_ID`, `ANTIGRAVITY_CONVERSATION_ID`, and
`TMUX_PANE` — which a garden shell does not have.

## Scopes

A registration lives under a *scope*: the account whose limit the session
shares, since that is what resets. [agfi:h-agent-auto-continue-scope] reads it
from the environment:

- Claude Code: `claude-<profile>` — `claude-default` or `claude-work`, from
  [agfi:claude-code-profile-current]. A work reset must not resume a personal
  session, and the other way round.
- Codex: `codex-<alias>`, the alias of the auth the running Codex was started
  with: the `auth_<alias>.json` snapshot whose bytes match `auth.json`, else
  `auth`, which is what `codex_status.py` calls the bare file. This is
  [agfi:h-codex-status-active-alias], the same rule as the script's own
  `workspace_name_from_matching_auth_alias`.
- Antigravity: `agy`. One account.

One watcher runs per scope, whatever the number of sessions registered in it,
and one usage check per tick serves all of them.

## The registry

One file per session, `<dir>/<scope>/<agent>-<id>`, tab separated: agent, id,
transcript, targets (space separated, sorted), registered-at epoch. The
directory is beside the kitty-window registry ([agfi:h-agent-session-registry-dir]),
not under `~/tmp`, which gets swept; `agent_auto_continue_dir` overrides.

Files, not redis, deliberately: no daemon to depend on, `ls` shows the state,
and it matches how the session layer already keeps its registry.

A session that is resumed keeps its id, so a registration follows the
conversation across a restart. What ends a registration is the session
ending: the watcher prunes registrations whose session is not in
[agfi:h-agent-session-live-list] (matched on id or transcript) at every tick,
and so does `--prune`. Once a scope has no registration left, its watcher
exits.

## Targets

[agfi:h-agent-auto-continue-targets] resolves how a resume reaches *this*
session, in the target syntax [agfi:h-agent-usage-continue-send] takes, in
order of how little each needs:

- Codex: `codex:<thread-id>`, always. `codex queue --thread` delivers by name,
  so it needs no terminal, no focus, no awake display, and cannot land in the
  wrong place.
- A tmux pane: `tmux:<pane-id>`, when `TMUX_PANE` is set and
  [agfi:h-agent-session-tmux-panes] lists that pane as holding a live agent
  session. The pane *id*, not the session name, because the autoname hooks
  rename sessions and a pane is not renamed. Needs no window manager.
- A kitty window: `kitty:<window-id>`, the row in
  [agfi:h-agent-session-live-pairs] whose transcript is this session's. Needs
  kitty's remote control socket.
- Otherwise it fails and says what it tried; `--frontmost` is the way past
  that, and it is a deliberate choice, not a fallback.

The target is fixed at registration. The engine re-checks liveness when it
fires — a pane must still hold a live agent, a kitty window must still exist —
so a session that moved or ended gets a notification naming the unreachable
target rather than `Continue.` typed into whatever replaced it.

## The watcher

[agfi:h-agent-auto-continue-watch] runs in a tmux session named
`agent-auto-continue-watch-<scope>`, created by
[agfi:h-agent-auto-continue-watch-ensure] through [agfi:tmuxnewsh2] — the same
construction as the armed jobs, and for the same reasons: the tmux server
outlives the agent, the shell and the brish garden, and the session name is the
lock ([agfi:tmuxnew] kills a previous, fired one before creating the
replacement). The autoname option is switched off on it so the agent it serves
cannot rename it. Its knobs travel as `VAR=value` arguments, because the
watcher runs in its own interactive shell hours later and a value scoped to the
call that started it would otherwise be lost.

Each tick: prune the scope's registrations against one live listing, run
[agfi:agent-auto-continue-check], and exit if nothing is registered. Then sleep
`agent_auto_continue_poll_s`.

## The check

[agfi:agent-auto-continue-check] is idempotent, and the watcher, the hooks,
`on` and `off` all call it. With nothing registered it cancels the scope's
armed jobs and returns. Otherwise it gathers every registration's targets and
hands them, preset in `agent_usage_arm_targets` with
`agent_usage_arm_action=continue`, to the scope's own deadline source:

- Claude Code: [agfi:h-claude-code-usage-arm], with roles
  `agent_auto_continue_claude_roles` (`session` and `weekly_all` by default)
  plus the session's model-scoped weekly window. The model family — `fable`,
  `opus`, `sonnet` — is read off the model id on the transcript's most recent
  assistant record ([agfi:h-agent-auto-continue-claude-family]; nothing else is
  read out of the transcript), and registrations are grouped by it, one job per
  family: `agent-auto-continue-claude-work-fable`, say. A blocked `session` or
  `weekly_all` window arms every group; a blocked Fable window arms only the
  Fable sessions. A family with no such window is skipped by the role lookup
  and costs a gray line.
- Codex: [agfi:h-codex-status-arm-auth], which reads *that auth's* primary
  and secondary windows from `codex-status --json` and takes the latest reset
  among those at or above `codex_status_arm_full_pct`. This is not
  [agfi:h-codex-status-arm], which waits for *every* auth to be exhausted
  because a person can `swap`; a running thread cannot, and is blocked until
  the auth it holds resets.
- Antigravity: [agfi:h-agy-status-arm].

Each of these already refuses to arm while usage is possible and honours
`deus`, so the check adds no policy of its own about *when*.

The job session is per scope (and per family for Claude), so several scopes
armed at once each get their own job, and [agfi:agent-usage-armed-sessions]
lists them through [agfi:agent-auto-continue-armed-sessions], read from
`tmux ls` rather than from a table since the set depends on what is registered.

[agfi:h-agent-auto-continue-arm-group] wraps the call with one guard: if the
job is already armed for exactly this target set, do nothing. Without it a
blocked scope would kill and recreate its job on every tick. A changed set — a
session registered or pruned while blocked — re-arms, which is what keeps the
job's targets in step with the registrations.

## Hooks: sooner, not instead

Detection is the poll. Where an agent has a hook that sees the failed turn, the
hook makes the check run now rather than at the next tick, through
[agfi:agent-auto-continue-hook], and revives the scope's watcher if a reboot
killed it. For a session that is not registered it is one glob and an exit.

- Claude Code: `StopFailure` with matcher `rate_limit`, in
  `configFiles/claude-code/settings.json`. The event is documented as firing
  "when the turn ends due to an API error"; whether Pro/Max limit exhaustion
  arrives under `rate_limit` is not spelled out anywhere, which is fine, since
  the hook only triggers a check and the check reads the usage endpoint.
- Antigravity: `Stop`, as its own group `auto-continue` in
  `configFiles/antigravity/hooks.json`. `Stop` runs on error turns as well.
- Codex: nothing. Codex's `Stop` hook and its `notify` command fire only after
  a *successful* turn; a usage-limit turn takes the error path and fires no
  hook at all. So a Codex session is detected by the poll alone, within one
  interval. (Its `hooks.json` is also not watched for edits, and every edit
  needs re-trusting in the TUI, which is one more reason not to touch it.)

Kicks are debounced per scope on a stamp file, `agent_auto_continue_kick_min_s`,
so a burst of failed turns costs one usage call.

## Knobs

All `typeset -g` in `agent-auto-continue.zsh`, defaults in the code:

- `agent_auto_continue_poll_s` — how often a watcher re-reads its scope's usage.
- `agent_auto_continue_kick_min_s` — least time between two hook-triggered checks.
- `agent_auto_continue_claude_roles` — the Claude windows every registration is
  checked against, before the per-session model window is added.
- `agent_auto_continue_dir` — where registrations live.
- `agent_auto_continue_session_prefix` — the tmux session prefix for watchers
  and jobs.
- `agent_auto_continue_frontmost_p` — what `--frontmost` sets.
- `codex_status_arm_full_pct` — in `codex.zsh`; the utilization at which one
  auth's window counts as blocking.

The engine's own knobs — grace, idle threshold, the text typed, the log — are
in `agent-usage-armed.md` and apply unchanged.

## Caveats

- Latency is the poll interval, except where a hook kicks it.
- The engine trusts the reset time it was armed with and does not re-check
  usage when it fires (`agent-usage-armed.md`, "The caveat"). The watcher keeps
  running after a fire, so the next limit cycle re-arms on its own.
- A reboot kills watchers. The next `/auto-continue` in any session of the
  scope, or the next hook kick from a registered one, revives it; until then
  nothing polls. Registrations survive, being files.
- Codex's scope is the auth at registration time. Swapping auths under a
  running thread is not modelled.

## Checking it by hand

    zsh -ic 'agent-auto-continue-on'          # from an agent's shell tool
    agent-auto-continue-list
    deus agent-auto-continue-check claude-work  # arm for the next rollover, no limit needed
    agent-usage-armed-status
    agent-auto-continue-check claude-work       # "already armed for these targets"
    printf '{"session_id":"<id>"}' | agent-auto-continue-hook claude "%3"
    zsh -ic 'agent-auto-continue-off'

The watcher's own pane (`agent-auto-continue-watch-<scope>`) shows what each
tick decided, and every fire is a line in `agent_usage_arm_log`.
