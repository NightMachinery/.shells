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
  through `brishzgo --` with `brishz_noquote=y`, without blocking. This keeps
  shell code and named-session state intact.
- `brishz_eval_q_hs(argv, label, opts)`: the argument-list form, through
  `brishzgo --`, which quotes every word. Use it whenever a
  value is interpolated.
- `brishz_eval_out_hs(cmd, callback, label, opts)`: like `brishz_eval_hs`,
  but hands the trimmed stdout to `callback`, or `nil` on failure. The
  callback is called exactly once on every path.
- `brishz_eval_q_out_hs(argv, callback, label, opts)`: the argument-list form
  of that; `nil` also when the command itself failed.
  `ntagFinder` (hyper+cmd+N) asks it on every keystroke.

All four use `${BRISHZGO_BIN:-$HOME/go/bin/brishzgo}`, clear inherited
async/copy/stdin controls, and report the remote command's status. They keep
`hs.task` asynchronous until completion; Go's detached mode would lose that
callback. The STT garden backend uses the same client and output collector.

`label` names the caller in console lines and bands. `opts` is optional:

- `quiet`: `true` drops the console line for ordinary failures.
- `timeout`: seconds before a client call is killed (default 30).
- `onFail`: called as `onFail(code, notSent)` once when the call failed, after
  the band, for a caller with a native way to do the job or state to take back.
  `notSent` is true when a connection/setup status coincides with a failed
  TCP probe. This is a best-effort outage check: a garden dying after the
  command returned that same status can also satisfy it. The blackout chords
  use it: a blackout that never started gives the keyboard back, and one that cannot be ended through
  BrishGarden is ended in Lua (see "Ending a blackout without BrishGarden" in
  `hammerspoon/docs/hammerspoon.md`).

## When a call fails

The Go client returns the remote command's status once it has an answer.
Connection/setup codes 2, 3, 6, 7 and 26 therefore need a TCP probe as well:
a command can return those exact numbers. If the probe succeeds, the helper
reports an ordinary command failure. If it fails, the helper reports the
garden as down. This cannot prove non-execution if the garden dies between
the command returning and the probe.

A client killed by a signal reports 128 plus the signal number, as a shell
would. `hs.task` hands back the bare signal number, so the helper normalizes
it before classifying failures. HTTP, timeout, empty-reply and transfer errors
may have run the command and never trigger an automatic retry.

Nothing is run anywhere else. An earlier version re-ran a not-sent command in
a fresh `zsh -c`, but the hotkeys that matter now have garden-free code (the
kitty panel is one), and a slow local zsh for the rest was not worth its
complications: commands that detach (`awaysh-fast`) or call the garden again
from inside could not run that way.

## What you see

- The "BrishGarden-down band": a warn band with id `garden-down`, so a new
  message replaces the one on screen instead of stacking.
  - For a failed connection/setup status with a failed probe: "BrishGarden
    down: <label> did not run; run ivy", for 30 s.
  - For a local start or timeout failure: "BrishGarden call failed: <label>
    (exit N)". Ordinary remote failures get a console line and `onFail`,
    without a garden-down band.
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

Every client call is killed, with its whole process tree, after
`opts.timeout` seconds (default 30). With the streaming garden API, terminating
the Go client closes the connection and cancels the remote command. Older
raw/JSON fallback APIs can continue running the remote command.

`taskCollectWithPath` captures stdout and stderr directly into separate 0600
temporary files. A small shell launcher uses `exec`, retaining the client's
PID and signal status. Completion reads both files as Lua byte strings and
removes them. Failure to create/start, timeouts and garbage collection also
remove them. This uses disk I/O and keeps the full reply in memory when the
callback runs, as the existing callback contract requires.

This avoids two `hs.task` problems: without a streaming callback, a child can
fill an output pipe before exit; with one, Hammerspoon decodes each read as
UTF-8 separately and drops chunks that split a character. The
[hs.task implementation](https://github.com/Hammerspoon/hammerspoon/blob/master/extensions/task/libtask.m)
shows both paths. A live test writing the three bytes of `€` in separate
flushed writes reproduced the Unicode loss. File capture preserves the exact
bytes and allows replies larger than the pipe buffer. Go still streams its
network reply directly into the files.

## Testing without touching the live BrishGarden

`garden_port_override` points the helpers, their clients and the probe at
another port. It sets both `GARDEN_PORT` and `bshEndpoint`, because the client
reads `bshEndpoint` first. Go does not source shell startup files.
Set it to a closed port such as 7231 to play a dead BrishGarden, test with
inert commands (`print -r -- sentinel`), and swap `alert_gateway` for a
recorder while testing, so no band reaches the screen.

`lua lua/tests/garden-task-test.lua` mocks tasks, probes, timers and bands.
It checks large binary/Unicode stdout and stderr, argv and raw shell mode,
remote status 7 with a live probe, a closed-garden failure, start failures,
timeout cleanup, signal status, exactly-once callbacks, object collection and
STT output/start-failure handling.
Live `gardenTask` checks should use inert producers, compare exact output,
and avoid clipboard or hotkey actions.

## What no longer needs BrishGarden

Each of these has a counterpart in zsh, and both copies carry the same
`@duplicateCode/<id>` tag, so grepping the tag finds the other one.

- hyper+z, the kitty panel: kitty's remote-control socket
  (`core/kitty-panel.lua`).
- hyper+cmd+L, display off: `pmset displaysleepnow` (`display_off` in
  `core/reload.lua`).
- hyper+F6, Do Not Disturb: the 'Get Focus', 'Focus Off' and 'Focus Set: Do
  Not Disturb' Shortcuts (`focusDndToggle` in `core/window-media-bindings.lua`).
- hyper+F5, mic mute, for a mic with its own mute control and no soft mute in
  effect (`inputMuteToggle` in `core/window-media-bindings.lua`). The iPhone
  mic's soft mute is a redis state machine that stays in zsh.
- hyper+d, dismiss notifications: `osascript` on `notif-dismiss-v2.jxa`
  (`core/app-hotkeys/main.lua`).
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

## Plain Lua callers

`lua/pipe.lua` uses `${BRISHZGO_BIN:-$HOME/go/bin/brishzgo}` for normal
`brishz_eval`, `_q`, `_bsh` and `_bg` calls. The string form sets
`brishz_noquote=y`, preserving named-session shell state; the argv form quotes
values. Both synchronous forms return trimmed stdout, stderr and the remote
command's status. Background forms use `brishz_async=y`, returning after local
launch and discarding the garden reply. The worker receives any direct stdin
before the parent exits.

`pipe_simple` multiplexes nonblocking stdin, stdout and stderr with
[luaposix poll](https://luaposix.github.io/luaposix/modules/posix.poll.html).
It handles partial writes, a child closing stdin early, EOF on each output and
interrupted system calls, closes all descriptors and reaps the child. Signals
are reported as 128 plus the signal number. Binary input is piped instead of
put into environment variables.

The explicit `evalFile` and `outFile` options keep `brishzq.zsh` for their
file-based contract. No current normal caller requires those flags.
`lua lua/tests/pipe-test.lua` exercises a child which fills stdout and stderr
before reading more than a pipe buffer of binary input, early input closure,
signal status, exec failure and the Go argument/option boundary. Run it under
an external timeout when checking the deadlock regression.
