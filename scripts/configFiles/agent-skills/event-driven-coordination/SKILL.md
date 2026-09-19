---
name: event-driven-coordination
description: Coordinate long-running workers (subagents, GPU lanes, remote jobs) by events instead of polling. Workers hand back only on state changes and keep a checkpoint current; the coordinator arms one-shot timers for expected milestones and one cheap hourly recovery ping that catches missed events. Use when babysitting a fleet, a multi-hour job or several subagents for hours, and whenever a coordinator notices it is polling for progress.
---

# Event-driven coordination

Terms used below:

- An **event** is a state transition somebody must act on: a job drained, a
  node published and verified, a preemption, a stall, a failure, a decision
  that needs the coordinator or the user. Progress ("7 of 12 done") is not an
  event.
- A **hand-back** is the message a worker sends its coordinator. It is sent on
  events only.
- A **watch** is a zero-token waiter whose exit, or whose emitted line, is the
  event: a background `until` loop, a Monitor tool call, a `tail` that ends at
  a done marker.
- A **one-shot timer** is a scheduled wakeup for a milestone that is expected
  at a known time (CronCreate with `recurring: false`).
- The **recovery ping** is one recurring hourly wakeup whose only job is to
  notice an event that was missed.
- A **checkpoint** is the worker's file on disk with the current facts, the
  running commands, the projected times and a resume procedure a stranger can
  follow.

## Why polling fails twice

Polling burns tokens on every check and still misses what happens between two
checks. Coordinators that poll also drift into relaying: each worker reports
every completion, the coordinator reads every report, and a night of a few
hundred cells becomes a few hundred model turns that change nothing. In one
run this style reached the quota checkpoint threshold within hours and then
starved the actual decisions.

Events fail too, silently. The process holding a watch can die: a session
compaction, a quota stop, an unrelated process-wide kill on the laptop. A
one-shot fires while the session is mid-query and is dropped. A hand-back
lands while the coordinator is paused and is never read. In every one of
these cases the coordinator sees silence, and silence looks exactly like
"nothing happened yet". A drained fleet then idles for hours at full price.
That is what the recovery ping is for.

## Three layers, all armed at launch

1. **Watches** for the events you already know are coming, armed in the same
   turn as the launch. "I will check later" does not survive a compaction.
2. **One-shot timers** for milestones with a known time: a quota reset, a
   projected drain, a deadline. The timer's prompt carries the check and the
   action, so it is self-contained after a compaction.
3. **One recovery ping**, hourly, at an off-minute. Never more than one, and
   never a second poller next to it: when the ping misses a class of event,
   widen its checklist.

## Rules for a worker

- Hand back only on events: resource drained and verified, stall, preemption
  or failure, decision needed, all done. Never a progress count.
- Keep the checkpoint current instead of reporting. It holds: what is running
  where, the measured rates and projected times, the exact NEXT COMMANDS, the
  ids of any timers or watches you armed, and a RESUME block that says what to
  do first after a pause. The coordinator reads the checkpoint, not your
  transcript, so a stale checkpoint is a lie.
- One lean watch, as a script file. Test it once in single-pass mode before
  arming it, use absolute paths, filter to the lines you would act on, and
  make it emit on every terminal state, not only success. Watches expire: on
  expiry re-arm and note the time in the checkpoint.
- Query remote state with the cheapest call that answers the question (a
  plain batch-mode ssh, a single list command), on an interval measured in
  tens of minutes. A heavy interactive shell wrapper per poll multiplied by
  several workers is what got waiters killed for memory.
- When you are told to pause, finish the in-flight verify or publish, write
  the checkpoint, and stop. Autonomous jobs keep running on their own.

## Rules for the coordinator

- When you launch or resume a worker, in the same turn arm a one-shot for the
  time by which it should have handed back. Resuming by message counts as a
  launch.
- Keep exactly one recovery ping. Its prompt is one facts call (the resource
  list, the checkpoints' modification times and last lines), a comparison
  against the latest dated line of your own state file, and a fixed list of
  discrepancies to act on. Otherwise it appends nothing and replies in one
  line. The template is below.
- Act on verified state, not on the line that predicts it. A log line saying
  complete is not the event; the artifacts counted in durable storage are.
  Deleting a resource on the prediction is the one mistake this style cannot
  undo.
- After a compaction or a restart: scheduled jobs are session-only and gone.
  Read the checkpoints, re-arm the ping and the fallback one-shots first, then
  continue. Record the ids in the state file so the next reader can tell what
  is armed.
- Do not idle autonomous work to protect quota. Jobs that run on their own
  once started should be started; a brief pause at the reset boundary is
  cheaper than an idle hour of machines.
- A missed event is a bug in the checklist, not bad luck. When the ping or a
  human finds one, add its signature to the ping prompt.

## The recovery ping prompt, as a template

Adapt the names; keep the shape.

```
Hourly recovery ping. This is the missed-event catch, not a progress poll.
Do exactly this: one facts call (list the live resources; stat the worker
checkpoints and read their last lines), then compare with the latest dated
line in STATE.md. Act only on a discrepancy:
- a resource that its checkpoint says is published and verified but still
  exists: re-verify the durable copy, then release it;
- a checkpoint untouched for more than 90 minutes while its resources run:
  message that worker once;
- a dead watcher, loop or pane: restart it with its recorded command;
- a worker silent past the milestone it was given: read its checkpoint and
  act on its NEXT COMMANDS.
Otherwise append nothing and reply in one line.
```

## Anti-patterns

- Per-cell progress hand-backs and coordinator relays of them.
- A watch typed inline and armed untested, or armed with relative paths.
- Two pollers covering the same fleet, one of them "just to be safe".
- Treating a quiet notification stream as proof that nothing changed.
- Releasing a resource on a completion log line without counting the durable
  artifacts.
- Pausing everything, including autonomous jobs, because a quota window is
  nearly used up.

## What this caught, and what it would have caught

- 2026-09-19: an unrelated process on the laptop sent SIGKILL to every user
  process. Every worker watcher and both publication loops died in one
  second, and the coordinator's scheduled one-shots for that evening never
  acted. Eighteen drained GPU nodes then sat idle for about three hours before
  a human noticed. With an hourly recovery ping the gap would have been under
  an hour, and the ping's checklist (dead loop panes, checkpoints untouched
  while nodes run) names exactly what it would have found.
- 2026-09-19, earlier: three operators polling nodes every few minutes and
  handing back on every completion took the coordinator to the quota
  threshold in a few hours. Switching them to checkpoint-plus-events cut the
  traffic to a handful of hand-backs per night.

Related skills: `long-run-handoff` for the state file, the result-file
protocol and zero-token waiters; `quota-management` for the quota watcher and
the pause-and-resume mechanics.
