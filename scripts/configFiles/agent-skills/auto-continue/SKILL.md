---
name: auto-continue
description: Arm this session to resume itself when its usage limit resets. Registers the current session with a watcher that polls the account's usage and, once it is blocked, types Continue. into this session after the reset. Use when the user types /auto-continue, /auto-continue off or /auto-continue status.
argument-hint: "[off|status|--frontmost]"
---

# Auto-continue

One job: run the command below from the shell tool, and relay what it prints.
The command has to run from *your* shell tool, not from a helper or another
shell, because that shell inherits the environment naming this session (its id,
its tmux pane, its seat), and that is how the session is found.

## Which command

Pick by the argument the user gave:

- no argument → `zsh -ic 'agent-auto-continue-on'`
- `--frontmost` → `zsh -ic 'agent-auto-continue-on --frontmost'` (only if the
  user asked for it: it means typing wherever the keyboard focus is at the time)
- `off` → `zsh -ic 'agent-auto-continue-off'`
- `status` → `zsh -ic 'agent-auto-continue-status'`

Run exactly one of them, once. Do not retry a failure with a different
variant; report it.

## What to say

Relay the command's output verbatim — the scope, the target (something like
`tmux:%3`, `kitty:12` or `codex:<thread>`) and the poll interval are the facts
the user wants, so do not paraphrase them. A gray line saying usage is already
possible is expected: it is the first check, and it means the account's usage
could be read.

Then, once, in one or two sentences: a watcher now polls this account's usage;
when it is blocked, a job is armed for the reset that types `Continue.` into
this session, but only if the keyboard has been idle for a while — otherwise a
notification is sent instead. `/auto-continue off` undoes it.

Do nothing else. Do not edit hooks or settings, do not start anything yourself,
and do not continue with other work unless the user asked for that separately.
