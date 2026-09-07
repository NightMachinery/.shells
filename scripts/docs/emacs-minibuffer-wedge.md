# Emacs daemon wedges into an unexitable minibuffer stack

A handout for independent review. Everything below is measured, not inferred;
where something is a hypothesis it says so. Network addresses and the phone's
hostname are redacted; paths are otherwise verbatim.

## Summary

A long-lived Emacs 29.2 daemon serving several terminal frames reaches a state
where a newly created frame opens inside a stack of active minibuffers,
`recursion-depth` only ever grows, and nothing short of restarting the daemon
clears it. Two occurrences in one day, roughly two hours apart. The second
required a restart, which cost 21 open buffers and 6 registers.

The question for review is whether the diagnosis is right, whether anything
could have recovered the wedged daemon short of a restart, and whether the
chosen mitigation is the correct one.

## Environment

Emacs 29.2 from emacs-plus, macOS (Darwin 23.3.0), Doom Emacs with ivy, evil.
Two daemons run: a tty daemon on socket `~/tmp/.emacs-servers/server` and a GUI
one on `server_gui`. `enable-recursive-minibuffers` is `t` (Doom's default;
ivy relies on it).

Frames on the tty daemon come from three sources: `emacsclient -t` from kitty
on the laptop; `emacsclient -t` from tmux panes; and `emc-mobile`, which is
`TERM=xterm-emacs emc-gateway --frame-parameters '((night/mobile . t))'`, run
over ssh from Termux on an Android phone. The phone sessions are the ones that
drop out from under Emacs when mobile connectivity does.

Relevant shell code lives in `~/scripts/zshlang/auto-load/others/emacs/emacs.zsh`;
Emacs config in `~/doom.d`.

## Symptom as the user experiences it

A terminal frame opens and focus is in the minibuffer. Most keys do nothing.
Reported as "space is not recognized" and "focus goes to minibuffer where most
commands aren't recognized". In Doom with evil, `SPC` is the leader key, so a
minibuffer keymap swallowing it makes the whole frame feel dead.

## Incident one

`*Messages*` showed a repeating prompt:

```
Save file /Users/fixture/code/x.py (buffer  *temp*)? (y, n, !, ., q, C-r, C-f, d or C-h) ESC
Type C-h for help.
Save file /Users/fixture/code/x.py (buffer  *temp*)? (y, n, !, ., q, C-r, C-f, d or C-h)
Quit
```

State at the time:

```
recursion-depth 6, minibuffer-depth 6
frames: F16 (night/mobile t), F1
*Backtrace* absent, debug-on-error nil
 *Minibuf-1* .. *Minibuf-7* all empty strings
```

Cause found: a buffer named ` *temp*` was modified while visiting
`/Users/fixture/code/x.py`, a path that does not exist. Its content was
`"def add(a, b):\n    \n"`, a fixture documented in `docs/fim.md` from a session
testing fill-in-middle completion. Something set `buffer-file-name` on a temp
buffer by hand; `with-temp-buffer` ends in `kill-buffer`, which does not kill a
modified file-visiting buffer but prompts about it, so the buffer outlived the
test. Every subsequent `save-some-buffers` prompted for it again, through
`map-y-or-n-p`, whose keymap answers anything outside
`(y n ! . q C-r C-f d C-h)` with "Type C-h for help." -- ESC included.

Killing that buffer removed the prompt. `recursion-depth` stayed at 6.

## Incident two

About two hours later, with the phone reconnecting several times in between.

```
recursion-depth 8, minibuffer-depth 8, server-clients 4
frames: F22 (night/mobile t, minibuffer active), F21 (mobile), F16 (mobile), F1
modified file-visiting buffers: none
```

All three mobile frames were on distinct, still-live ttys, each an ssh login
from the phone. `*Messages*` tail:

```
evil-ex-search-exit: No catch for tag: exit, nil [2 times]
y-or-n-p: Terminal 0 is locked, cannot read from it [2 times]
Back to top level
When done with this frame, type SPC q f
Quit
j is undefined
k is undefined
exit-minibuffer: Not in most nested command loop
SPC is undefined
q is undefined [2 times]
ESC <escape> is undefined [7 times]
```

## Recovery attempts, all measured, none effective

Each was run from `emacsclient -e` against the wedged daemon, with
`recursion-depth` re-read afterwards.

- `(top-level)` scheduled via `run-with-timer 0 nil`: printed "Back to top
  level" in `*Messages*`, so it did execute. Depth stayed at 8. Tried twice
  across the two incidents, same outcome both times.
- `(abort-recursive-edit)` via timer, ten times in a row: depth stayed at 8.
- `delete-frame` on the two older mobile frames F16 and F21: the frames
  disappeared, depth stayed at 8.
- `delete-frame` on F22, the frame holding the active minibuffer: F22
  disappeared, depth stayed at 8, and `active-minibuffer-window` then reported
  its frame as F1 -- the daemon's own initial frame, whose `tty` parameter is
  nil, so there is no terminal at which to type anything.

Restarting the daemon was the only thing that worked. The replacement daemon
reports `recursion-depth 0`.

## Mechanism as currently understood

Stated as the current hypothesis, which is what most needs checking.

1. Something prompts on a tty frame. In incident one this was
   `save-some-buffers`; in incident two the trigger was not captured before
   `*Messages*` rolled.
2. Several tty frames coexist in one daemon. Three concurrent mobile frames
   were present in incident two.
3. Minibuffer reads belonging to different terminals interleave. An older
   frame's minibuffer stops being the innermost one, and Emacs then refuses to
   exit it: `exit-minibuffer: Not in most nested command loop`.
4. A terminal dies while one of its minibuffer reads is live. Emacs reports
   `Terminal 0 is locked, cannot read from it`. That level now sits on a
   command loop that no input can reach, because the terminal is gone, and that
   no external eval can unwind, because a `throw` evaluated in an
   `emacsclient -e` context returns to the top of *its* command loop while the
   stranded read remains nested on another terminal's stack.
5. Depth therefore only grows. Every later frame opens inside the pile.

Step 4 is the part believed to be unrecoverable, and the measurements above are
the evidence for it.

## Explicitly ruled out

Considerable time was lost to a terminal-layer theory before `*Messages*` was
read, so it is worth stating what is not involved.

- Not TERM or terminfo. Concurrent work had changed the phone's TERM from a
  false `xterm-kitty` to an honest `xterm-256color`, and this was initially
  blamed. But `emc-mobile` sets `TERM=xterm-emacs` as a command prefix, so
  Emacs receives the same TERM before and after; verified by reading the
  computed value.
- Not the tmux layer. The affected sessions run `emc-mobile` in a plain ssh
  session with no tmux involved.
- Not a keymap problem. `<escape>` is bound to `abort-recursive-edit` in
  `minibuffer-local-map` and `ivy-minibuffer-map`; `C-]` likewise;
  `ESC ESC ESC` is `keyboard-escape-quit`. ESC does escape -- one level per
  press, which at depth 8 with no depth indicator looks like doing nothing.
- Not the debugger. No `*Backtrace*` buffer, `debug-on-error` nil.
- Not unsaved work piling up. Zero modified file-visiting buffers at the time
  of the second wedge.

## Mitigations already committed, in ~/doom.d/autoload/night-minibuffer.el

- `minibuffer-depth-indicate-mode` enabled, so nesting is visible. Emacs only
  draws the indicator above depth 1, so ordinary single prompts are unchanged.
- `save-some-buffers-default-predicate` set to skip buffers whose name begins
  with a space. Internal buffers are never the user's to save, and one only
  acquires a `buffer-file-name` because some code set it by hand. This removes
  incident one's trigger specifically, not the class.
- `night/minibuffer-diagnose`, a manual report of depth, clients, orphaned
  clients and phantom buffers, which offers to kill the phantoms. It
  deliberately does not call `top-level`, because in a daemon each waiting
  `emacsclient -t` legitimately holds a recursive edit of its own and
  unwinding would end live sessions.
- A `server-after-make-frame-hook` warning that fires when a new frame opens
  inside existing minibuffer levels, distinguishing an answerable live prompt
  (owning frame has a tty) from a wedged daemon (owning frame has none).

Two heavier options were considered and deferred: allowing only one
`night/mobile` frame at a time, and giving `emc-mobile` its own daemon so a
wedged phone session cannot take down the laptop's Emacs.

## Questions

1. Is the step-4 mechanism correct? Specifically, is a minibuffer read whose
   terminal has died genuinely unrecoverable in Emacs 29, or is there a way to
   unwind it that was not tried?
2. Why did `top-level` report "Back to top level" while `recursion-depth`
   stayed at 8? Is the multi-terminal command-loop explanation right, and does
   it imply anything about where a repair would have to run from?
3. After `delete-frame` on the frame owning the active minibuffer, ownership
   moved to the daemon's initial frame F1 rather than the level being
   released. Is that expected, and does it offer any handle for recovery?
4. Is there a supported way to make a prompt on one tty frame not block or
   nest with other frames' command loops -- something equivalent to per-frame
   minibuffer isolation?
5. Is the mitigation set proportionate, or is the separate-daemon option the
   only design that actually contains this?
6. Anything above that looks like a misreading of the evidence.
