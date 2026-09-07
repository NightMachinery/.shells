# Emacs daemon wedges into an unexitable minibuffer stack

Written as a handout for independent review, then revised after that review.
The measurements are unchanged; the mechanism section is not, because the
review found a better explanation and refuted several claims. Corrections are
marked as such rather than quietly edited away. Network addresses and the phone's
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

## Mechanism, as revised after review

The first version of this document blamed stranded per-terminal command loops.
That was wrong. The better explanation is a cleanup defect in Emacs 29.2.

`read_minibuf` in `minibuf.c` runs in this order: `minibuf_level++` (line 664),
then `temporarily_switch_to_single_kboard` (line 715), which can signal an
error, and only then does it register `read_minibuf_unwind` (line 740) and
enter `recursive_edit_1` (line 905). The decrement lives in
`read_minibuf_unwind`. So when that call signals, the increment has already
happened and nothing is registered to undo it. The level is leaked.

The error it signals is "Terminal N is locked, cannot read from it". That does
not mean a terminal died. `temporarily_switch_to_single_kboard` raises it when
Emacs is already restricting input to one keyboard and code tries to read from
another, which is exactly what happens when a second terminal frame tries to
prompt while a first one is prompting.

`recursion-depth` returns `command_loop_level + minibuf_level`. A reading of
8 with `minibuffer-depth` also 8 therefore means `command_loop_level` was zero,
not that eight client recursive edits existed.

This accounts for every measurement above: empty minibuffer buffers, a
`top-level` that genuinely ran and reported success while the number did not
move, `No catch for tag: exit`, and frame deletion changing nothing. There is
no stranded stack to unwind, only a counter that no cleanup record owns.

## Corrections to the first version

- `with-temp-buffer` killing a modified file-visiting buffer: the reviewer
  tested stock 29.2 and it *is* killed, because the save confirmation in
  `kill-buffer` is conditional on an interactive call. The phantom buffer was
  real, but how it survived is unexplained.
- `save-some-buffers` explaining the depth: `map-y-or-n-p` shows an echo-area
  message and calls `read-event`; it never enters `read_minibuf`. Its response
  map also accepts space as "act". So it explains the rejected keys, not the
  depth.
- "Not in most nested command loop" was read as "another terminal's minibuffer
  is newer". `exit-minibuffer` checks command-loop membership and innermost
  minibuffer identity separately, with distinct errors.
- Frame F1 being named as the minibuffer owner after the other frames were
  deleted proves less than assumed. `active-minibuffer-window` falls back to
  `minibuf_window` when it cannot find the level's buffer displayed, so with
  leaked levels it can name a frame that owns no prompt.
- The claim that each waiting `emacsclient -t` must keep its own recursive
  edit is contradicted by `server-goto-toplevel` in `server.el`, which calls
  `top-level` when a minibuffer is active.
- The two incidents are probably not independent. Depth stayed at 6 after the
  first and was 8 at the second; one corruption worsening fits better than a
  recovery followed by a fresh failure.

## Mitigations

In `~/doom.d/autoload/night-minibuffer.el`:

- `minibuffer-depth-indicate-mode`, so nesting is visible. Emacs draws the
  indicator only above depth 1, so ordinary prompts are unchanged. A leaked
  count is not a count of answerable prompts, which is worth remembering when
  reading it.
- `save-some-buffers-default-predicate` skipping leading-space internal
  buffers. This removes a prompt that should never have existed, though it is
  broader than the one buffer that caused it and does not address the depth.
- `night/minibuffer-diagnose`, a manual inventory. It does not unwind anything,
  because leaked C state cannot be repaired from Lisp.
- A `server-after-make-frame-hook` report when a frame opens inside existing
  levels. It states depth and ownership and stops short of declaring the daemon
  recoverable or not, since neither a nil nor a non-nil tty establishes that.

In `zshlang/auto-load/others/emacs/emacs.zsh`:

- `emc-mobile` now runs against its own daemon, `server_mobile`. This does not
  prevent the leak. It bounds the cost: mobile ssh sessions are the ones that
  drop, reconnect and pile up frames, and a wedged mobile daemon is a restart
  of nothing, where wedging the shared daemon cost 21 open buffers and 6
  registers.

Rejected: limiting the daemon to one `night/mobile` frame. Several concurrent
mobile frames are a workflow in use here, not an accident.

## Still open

- Whether the leak is fixed in a later Emacs. That would be the actual repair;
  everything above is containment.
- What actually kept the phantom ` *temp*' buffer alive, now that
  `with-temp-buffer` is ruled out.
- A reproducer: two tty frames, one prompting, the other made to prompt, with
  `minibuffer-depth` sampled before and after. That would confirm the leak
  directly rather than by inference.
