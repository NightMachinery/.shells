---
name: done
description: Wrap up the session. Verify the work really is finished, write an executive summary of the final state, and then end the session, leaving the summary on screen. Use when the user types /done or asks to close the session down.
argument-hint: "[anything extra to put in the summary]"
---

# Done

Two jobs, in this order: decide whether the work is actually finished, and only
if it is, end the session in a way that leaves a readable account behind.

Anything the user passed as an argument is a note for the summary, not
permission to skip the check.

## 1. Check that it is done

Go through this and say what you find. Be concrete: name files, commands and
their results, not "everything looks good".

- **Committed.** Nothing you changed is left uncommitted. Only files *you*
  edited in this session count as yours; other modified files belong to a
  parallel session and are not yours to commit, stash or revert. Say which
  files you left alone and why.
- **Pushed**, if the user asked for that or the project's rules do.
- **Verified.** The tests, build or linter for what you touched were actually
  run, and passed. Quote the command. If you could not run them, say so
  plainly rather than implying they passed.
- **Documented.** Any docs the project requires alongside a change are
  updated, including the readme when there is one.
- **Nothing of yours still running.** No background jobs, watchers or agents
  you started are still going. Any hold or banner you took is released.
  Restart or re-sync anything that caches your changes, so the next session
  does not read a stale copy.
- **Nothing dropped.** Everything the user asked for in this session either
  happened, or is named here as not having happened, with the reason.

**If anything is outstanding, stop here.** Report what is left and do not run
the exit command. If what remains is small and clearly inside what the user
already asked for, finish it now and re-check instead of handing it back.

## 2. Write the summary and exit

The summary is the last thing the user will see from this session, and it is
the whole reason the session gets to end noisily rather than just stopping.
Write for someone who was not watching:

- what state things are in **now**, in a line or two;
- what changed, by path, and why it was worth doing;
- how it was verified;
- what is deliberately left, and any risk worth knowing about;
- the obvious next step, if there is one.

Five to fifteen lines. Plain text, no markdown headings — it is displayed by
`cat` in a terminal pane. Leave out the session id, name, transcript path and
timestamp: the exit command fills those in.

Then end the session:

```sh
zsh -ic 'agent-done' <<'SUMMARY'
<your summary here>
SUMMARY
```

That saves the report, asks this agent to exit, and then shows the report where
the session was: inside tmux it replaces the pane with it and lets the pane die
with the text still on screen, and outside tmux it prints it to the terminal
once the agent is gone. The saved reports are listed by `agent-done-reports`
and the newest is printed by `agent-done-report-last`, so nothing is lost even
if the terminal is closed.

To see what it would do without ending anything: `zsh -ic 'agent-done
--dry-run'`, with the summary on stdin the same way.

Expect no reply from that command. It ends the session, so its output has
nowhere to go — that is success, not a failure to investigate.
