# Termux mouse: DECSET 1003 and the agent launchers

Touch and scroll do nothing inside an agent TUI on the phone. Antigravity, run
from Termux through mosh into a tmux session on the laptop, ignores every tap:
no selection, no scrolling, no clicking a menu entry. The same agent in the
same tmux session is fully mouse-driven from a kitty window on the laptop, and
the same phone client drives tmux's own mouse features (pane select, scroll)
fine until the agent starts.

## The chain

Three programs sit between the agent and the finger, and each one handles
mouse-tracking modes slightly differently. Individually all three are
defensible; composed, they lose the mouse.

**tmux 3.7b** re-emits a pane's mouse modes to the outer terminal as a set. For
a pane that has asked for any-event tracking, `tty_update_mode` in `tty.c`
writes, in order:

```
ESC[?1006l ESC[?1000l ESC[?1002l ESC[?1003l
ESC[?1006h
ESC[?1000h ESC[?1002h ESC[?1003h
```

It clears everything, turns SGR extended coordinates back on, then asks for
1000 (button press), 1002 (button + drag) and 1003 (any event) together. A
terminal that understands only some of those keeps the highest one it knows.

**mosh 1.4.0** does not keep a set. `terminaldisplay.cc` carries a single
`mouse_reporting_mode` field in the frame state, so the last mode the
application asked for overwrites the others, and the client is told:

```
ESC[?1003l ESC[?1002l ESC[?1001l ESC[?1000l
ESC[?1003h
```

Only one mode survives the crossing, and it is whichever came last in tmux's
run -- 1003.

**Termux 0.118** does not implement 1003 at all. Its emulator drops the
sequence silently, and its touch forwarding is gated on *some* mouse mode being
active. With 1003 as the only mode requested, no mode is active, and the
terminal sends nothing at all. termux-app PR 5281 fixes this by aliasing 1003
to 1002: any-event tracking becomes button-event tracking, which is all a
touchscreen can report anyway, since there is no hover.

So the mouse dies only where all three meet. Plain ssh plus tmux works, because
tmux hands Termux the whole set `?1000h ?1002h ?1003h`, Termux keeps 1002 and
discards 1003, and button events flow. mosh narrows that set to its last
element before Termux ever sees it, and the one element it keeps is the one
element Termux cannot use.

## Why one pane poisons the whole client

`~/.tmux.conf` has `mouse on`, and with that set tmux treats button-event
tracking as a floor for the attached client: it wants drag reporting for pane
resizing whatever the panes are doing. `server-client.c` then raises the client
to any-event mode as soon as *any* visible pane is in it. The mode is recomputed
per client, so a single agent pane in 1003 takes the mouse away from every other
pane of that tmux client on the phone, while the kitty clients attached to the
same server are unaffected -- they are recomputed separately and their terminal
handles 1003.

## What the installed agents ask for

Measured as string atoms in the installed binaries (`strings` plus `grep` for
`\[?`), not by running them:

- Antigravity `agy` 1.2.1 sets `?1002h`, `?1006h` and `?1003h` on entry. 1003
  comes last, so it is exactly the failing case.
- Codex 0.153.4 has no mouse-tracking modes at all. It is unaffected; wrapping
  it is insurance rather than a fix.
- Claude Code 2.1.268 shows `?1000h`, `?1006h` and `?1007h`, and no `1003`
  atom. That is weaker evidence than it looks: the binary is a bun bundle that
  builds many sequences from templates with numeric parameters, and it carries
  42 bare `[?` atoms with the number supplied at runtime, so 1003 cannot be
  ruled out statically. The author of PR 5281 reports that Claude's fullscreen
  mode requests 1003. The trace recipe below is how to settle it from the
  phone rather than from the binary.

## The fix

The agent's own output is rewritten on the way out, before tmux, mosh or Termux
ever see it: `ESC [ ? 1003 h` and its `l` become the 1002 form. This is what
PR 5281 does inside the emulator, done one layer earlier, so it needs nothing
installed on the phone and works with or without tmux and with or without mosh.

It lives in [agfi:h-agent-launch], the preamble every agent launcher shares
(`zshlang/auto-load/others/agents.zsh`), which runs the agent behind a pty proxy
built from `golang/decset-rewrite/` -- see `golang/decset-rewrite/readme.org`
for the tool itself. [agfi:h-decset-rewrite-dep] builds it on first use. A
launch is never blocked on this: if the proxy cannot be built, the launcher
says so once and starts the agent unwrapped.

Wrapping is skipped when stdout is not a terminal, and the tool additionally
execs the command directly when stdin is not one, so `claude -p` in a pipe and
any scripted run see no proxy at all.

## Knobs

All of them are read by [agfi:h-agent-launch]; the code holds the values.

- `agent_launch_decset_rewrite_p` -- the global switch.
- `<agent>_decset_rewrite_p` -- per agent, overriding the global:
  `claude_decset_rewrite_p`, `codex_decset_rewrite_p`, `agy_decset_rewrite_p`.
  Set it in front of the launcher, as in `agy_decset_rewrite_p=n antigravity`.
- `agent_launch_decset_map` -- the rewrites, as `FROM=TO` entries.
- `agent_launch_decset_trace` -- a path turns tracing on; empty is off.
- `agent_launch_echo_p` -- prints the final command line, wrapper included,
  before running it. Antigravity's launcher sets it.

## Verifying on the phone

First the terminal alone, with no agent involved, in a tmux pane on the phone:

```
printf '\033[?1002h' ; cat -v
```

Tap the screen. Button-event tracking should produce SGR reports:

```
^[[<0;17;9M
```

Then the same with 1003:

```
printf '\033[?1003h' ; cat -v
```

A tap should produce nothing at all. That is the Termux limitation itself,
independent of everything above; once PR 5281 ships, this probe starts
reporting and the workaround can go.

Then the real thing:

```
antigravity
```

Touch, drag and scroll should work inside the TUI.

## Tracing what an agent sends

With the proxy:

```
agent_launch_decset_trace=~/tmp/claude-decset.log claude
```

One line per private-mode sequence seen in the agent's output -- timestamp,
set or reset, the modes, and the rewritten modes where they differ. Mode
numbers only; no content is written.

Without it, or to capture an agent that is not launched through the preamble:

```
script -q ~/tmp/claude-trace.log claude
command grep -a -o $'\e\\[?[0-9;]*[hl]' ~/tmp/claude-trace.log | sort | uniq -c
```

## Process identity

The proxy becomes the foreground process of the outer tty and the agent runs in
its own session on the inner pty, so anything that identifies an agent by the
terminal's foreground process now sees `decset-rewrite -map ... -- claude ...`.
Checked, one by one:

- The kitty-window mapping in `agent-session.zsh` looks through the wrapper.
  [agfi:h-agent-session-cmd-unwrap] strips it off a command line, and
  [agfi:h-agent-session-cmds-agent-p] runs every line through that before its
  existing tests. Strategy 1 of [agfi:h-agent-session-of-kitty-window] compares
  kitty's foreground pids against live rows, and the agent is now a *child* of
  the pid kitty reports, so the children of a wrapper pid are added as
  candidates -- only for wrapper pids, so the ordinary case costs nothing.
  Inside tmux none of this matters: strategies 0 and 2 match the tmux client.
- [agfi:h-agent-done-pid] walks *ancestors* from the agent's own shell, and the
  agent is still an ancestor of everything it spawns. Unaffected.
- `codexCmdRe` in `golang/agent_session/internal/codex/live.go` also matches the
  wrapper's command line, so `codexPids` picks up the wrapper's pid alongside
  the real one. Harmless -- the rows are built from thread-lock holders, and the
  wrapper holds none -- and left alone deliberately, with a comment there.
- `#{pane_current_command}` would report the wrapper, but nothing in `~/scripts`
  or `~/.tmux.conf` reads it. Noted so the next person does not go looking.

## Dead ends and alternatives

- **A wrapper at the session level, under `mosh-server`.** One process instead
  of one per agent, but it needs mosh detection to avoid rewriting for every
  other client, and it fixes only the mosh chain. The launcher placement fixes
  every chain and needs no detection.
- **A wrapper on the phone, around `mosh-client`.** Correct in principle -- the
  rewrite belongs next to the terminal that cannot parse the mode -- but it
  means installing and maintaining a binary inside Termux, which is the thing
  this was meant to avoid.
- **Per-agent symlinks to the wrapper**, so the command line keeps the agent's
  name. This would make every agent matcher see two agents on one line, and
  hide the proxy from `ps` where it is worth seeing. Teaching the consumers to
  look one level down was cheaper.
- **DECRQM (`ESC [ ? 1003 $ p`) is deliberately untouched**, request and reply
  both. An application that asks the terminal what it supports gets Termux's
  honest answer; lying there would trade a mouse bug for a harder one.
- **Hover is lost wherever the flag is on**, which is the whole point of 1003
  and the one thing 1002 cannot do. None of the three agents uses hover, and on
  a touchscreen there is nothing to hover with; on the laptop the flag costs
  kitty the same capability for no gain, which is the argument for turning it
  off once it is no longer needed.

## Turning it off

Once Termux ships PR 5281 and the phone is updated, set the global knob
`agent_launch_decset_rewrite_p` to `n`. The emulator then does the aliasing
itself, hover comes back on the laptop, and nothing else needs unwinding.

See also `./tmux-termux-truecolor.md`: same client, same class of problem --
a capability mismatch that only shows up at the end of a chain of terminals.
