# Hammerspoon without BrishGarden

Many Hammerspoon hotkeys and watchers run zsh code through BrishGarden, the
long-lived zsh server on `127.0.0.1:7230`. On 2026-09-29 a tmux crash took
BrishGarden down for 15 hours, and every one of those hotkeys failed with
nothing but a console line. This note describes what Hammerspoon does now when
BrishGarden is down, and how to write a hotkey that does not need it.

The code is the "BrishGarden calls that say when it is down" section of
`hammerspoon/core/helpers.lua`.

## The helpers

- `brishz_eval_hs(cmd, label, opts)`: runs the string `cmd` in BrishGarden
  through `/usr/local/bin/brishz2.dash`, without blocking.
- `brishz_eval_q_hs(argv, label, opts)`: the argument-list form, through
  `/usr/local/bin/brishzq.zsh`, which quotes every word. Use it whenever a
  value is interpolated.
- `brishz_eval_out_hs(cmd, callback, label, opts)`: like `brishz_eval_hs`,
  but hands the trimmed stdout to `callback`, or `nil` on failure. The
  callback is called exactly once on every path.
- `brishz_eval_q_out_hs(argv, callback, label, opts)`: the argument-list form
  of that, through `brishzq.zsh`; `nil` also when the command itself failed.
  `ntagFinder` (hyper+cmd+N) asks it on every keystroke.

`label` names the caller in console lines and bands. `opts` is optional:

- `quiet`: `true` drops the console line for ordinary failures.
- `timeout`: seconds before a client call is killed (default 30).
- `onFail`: called as `onFail(code, notSent)` once when the call failed, after
  the band, for a caller with a native way to do the job or state to take back.
  `notSent` is true when the call provably never reached BrishGarden, and false
  when it may have run. The blackout chords use it: a blackout that never
  started gives the keyboard back, and one that cannot be ended through
  BrishGarden is ended in Lua (see "Ending a blackout without BrishGarden" in
  `hammerspoon/docs/hammerspoon.md`).

## When a call fails

A failed call is "provably not sent" when the client's exit status proves the
request never reached BrishGarden, which then means BrishGarden is down and the
command did not run.

- `brishz2.dash` exits with curl's own status. These codes mean curl never
  sent the request: 2 (initialisation failed), 3 (malformed URL), 6 (could
  not resolve the host), 7 (could not connect) and 26 (could not read the
  request body).
- `brishzq.zsh` exits with the remote command's status once it has an
  answer, so a 7 from it may be the command's own. It counts as not sent only
  when a TCP connect to BrishGarden's port is refused as well
  (`gardenProbe`).
- A client killed by a signal reports 128 plus the signal number, as a shell
  would. `hs.task` hands back the bare signal number, and a SIGINT (2) would
  otherwise read as curl's 2.
- Every other failure may have run the command: 22 (an HTTP error), 28 (a
  timeout), 52 (an empty reply), 18, 55 and 56 (transfer errors).

Nothing is run anywhere else. An earlier version re-ran a not-sent command in
a fresh `zsh -c`, but the hotkeys that matter now have garden-free code (the
kitty panel is one), and a slow local zsh for the rest was not worth its
complications: commands that detach (`awaysh-fast`) or call the garden again
from inside could not run that way.

## What you see

- The "BrishGarden-down band": a warn band with id `garden-down`, so a new
  message replaces the one on screen instead of stacking.
  - For a not-sent call: "BrishGarden down: <label> did not run; run ivy",
    for 30 s.
  - For another failure of `brishz2.dash`, which fails only when the call
    itself did: "BrishGarden call failed: <label> (exit N)". A failing
    `brishzq.zsh` is usually the command's own failure, so it only gets a
    console line.
- A liveness probe, `gardenLivenessCheck`, connects to BrishGarden's port
  every 60 s and 5 s after each config load. While BrishGarden is down, every
  probe re-issues "BrishGarden is down: hotkeys that need it do nothing; run
  ivy" with a 90 s lifetime, so the band stays up until BrishGarden is back
  (an unchanged message on the same id only extends it). Then it says
  "BrishGarden is back". The probe keeps its own state, so a call that
  succeeded in between cannot swallow the "back" band. The global `gardenUp`
  holds the latest answer from a call or a probe.
- Console lines per failure, prefixed with the caller's label, such as
  `hyper+d: BrishGarden down; not run`.

## Timeouts

Every client call is killed, with its whole process tree, if it is still
running after `opts.timeout` seconds. `hs.task:terminate()` alone would kill
only the client, and its curl would stay behind, blocked on a BrishGarden that
never answers. The timeout also covers a reply larger than 64 KiB. `hs.task`
collects a child's stdout only once the child has exited, so a child that
writes more than the pipe buffer blocks forever (measured with Hammerspoon
1.1.1).

## Testing without touching the live BrishGarden

`garden_port_override` points the helpers, their clients and the probe at
another port. It sets both `GARDEN_PORT` and `bshEndpoint` for the client,
because `brishzq.zsh` reads `bshEndpoint` first and the files it sources may
set one; with `GARDEN_PORT` alone, a test call went to the live BrishGarden.
Set it to a closed port such as 7231 to play a dead BrishGarden, test with
inert commands (`print -r -- sentinel`), and swap `alert_gateway` for a
recorder while testing, so no band reaches the screen.

## What no longer needs BrishGarden

Each of these has a counterpart in zsh, and both copies carry the same
`@duplicateCode/<id>` tag, so grepping the tag finds the other one.

- hyper+z, the kitty panel: kitty's remote-control socket
  (`core/kitty-panel.lua`).
- hyper+cmd+L, display off: `pmset displaysleepnow` (`display_off` in
  `core/reload.lua`).
- hyper+F6, Do Not Disturb: the 'Get Focus', 'Focus Off' and 'Focus Set: Do
  Not Disturb' Shortcuts (`focusDndToggle` in `core/window-media-bindings.lua`).
- hyper+d, dismiss notifications: `osascript` on `notif-dismiss-v2.jxa`
  (`core/app-hotkeys.lua`).
- hyper+g, anycomplete: Google's and DuckDuckGo's suggestions over `hs.http`
  (`anycompleteSuggest` in `core/choosers.lua`).
- FIM completion: `hammerspoon/bin/fim-get.zsh`, which sources `fim.zsh`
  itself (`core/fim.lua`).
- The load bell: `hs.sound` (`core/reload.lua`).
- Ending a blackout, and rung three's panel-off and lock: in Lua when the
  garden call fails (`core/blackout-lock.lua`).

## Writing a hotkey that does not need BrishGarden

Most hotkeys do not need anything BrishGarden holds. Prefer, in order:

1. Native Lua: `hs.task` on a binary, `hs.socket`, `hs.http`, or a
   Hammerspoon API. The hyper+z kitty panel (`core/kitty-panel.lua`) is the
   worked example, speaking kitty's remote-control socket directly.
2. A standalone script with `#!/usr/bin/env -S zsh -f` that sources
   `zshlang/basic/basic.plugin.zsh` and the one plugin it needs, run through
   `hs.task`. It starts in a few ms and needs neither BrishGarden nor
   night.sh.
3. The helpers above, which at least say when BrishGarden is down.

Remember that `hs.task` runs with launchd's bare `PATH`; `taskWithPath` in
`core/helpers.lua` adds `/opt/homebrew/bin`, `/usr/local/bin` and `~/go/bin`.

## Callback objects: pin them, do not capture them

An `hs.timer`, `hs.task` or `hs.socket` that only a local refers to is
collected, and its callback then never fires. The opposite mistake is quieter.
Hammerspoon keeps each such callback in the Lua registry until its object is
collected, so a callback that can still reach its own object, as an upvalue or
through a table it captures, keeps both alive forever. Measured 2026-09-30
with 3000 `doAfter` timers, 200 tasks and 200 sockets: every self-referencing
one survived a full collection, and every plain one was collected.

`core/helpers.lua` has the fix, used by the helpers above and by the kitty
panel:

- `hsPin(obj)` pins an object and returns its key, a fresh empty table;
  `hsPinned(key)` returns the object, and `hsUnpin(key)` releases and returns
  it. Callbacks capture the key, never the object.
- `hsAfter(seconds, fn)` is a pinned `hs.timer.doAfter` that releases itself
  when it fires; `hsCancel(key)` stops one early.
