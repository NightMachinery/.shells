# Hammerspoon

`~/.hammerspoon/init.lua` is a symlink to `~/scripts/hammerspoon/boot.lua`.
The boot file only sets up Lua/Hammerspoon dependencies and then loads an
ordered module list, mostly from `~/scripts/hammerspoon/core/`.

`core/ipc-fix.lua` is loaded before all of them, by an explicit `dofile` right
after `require "hs.ipc"` — see the ipc print recursion section below for why it
has to sit exactly there.

The explicit core load order is:

- `helpers.lua`
- `modal-mode.lua`
- `alert/state.lua`, `alert/colors.lua`, `alert/markup.lua`,
  `alert/layout.lua`, `alert/render.lua`, `alert/api.lua`
- `agent-banner.lua`
- `redis.lua`
- `wifi-watcher.lua`
- `hyper-mode.lua`
- `blackout-lock.lua`
- `purple-mode.lua`
- `mouse.lua`
- `input-language.lua`
- `popclick.lua`
- `system-keys.lua`
- `choosers.lua`
- `app-hotkeys.lua`
- `window-media-bindings.lua`
- `stt.lua`
- `fim.lua`
- `reload.lua`

`reload.lua` loads every Lua file in `~/scripts/hammerspoon/auto-load/` in
alphabetical order after the core modules are ready.

Put core features in `core/` and add them to the explicit list in `boot.lua`.
Put app-specific add-ons that can run after all core modules in `auto-load/`.

## The ipc print recursion fix

`hs -c` used to wedge whenever anything printed to the Hammerspoon console
while the command was being handled. It spun until the client's receive
timeout: one measured run produced 39,984 warning lines and 4.6MB of output in
four seconds, and Hammerspoon has been seen to restart afterwards. The usual
trigger was lazy extension loading, since `-- Loading extension: pasteboard` is
a console print like any other, so the *first* `hs -c` that touched an
extension wedged and every later one was fine — which is exactly the shape that
makes it look like an intermittent fault rather than a bug.

The cause, in the installed `hs/ipc.lua` (1.1.1, identical to upstream master):

- `hs.ipc` replaces the global `print` (lines 65-87) with one that prints to
  the console and then mirrors the text to every registered CLI instance. It
  guards against re-entering itself with `module.print_inside(id)`, and when
  the guard trips it *reports* that with `log.w(...)`.
- `hs.logger` formats through `hs.printf`, which is `print(string.format(...))`
  (`_coresetup.lua:26`) — a global `print` lookup, so the warning re-enters the
  replacement while the guard counter is still raised, trips the guard, and
  warns again, without bound. The guard is what floods.
- A client's default console mode is the string `"none"` (line 373), which is
  truthy, so the mirroring branch runs for every `hs -c`, not only for `hs -C`.
  Only `-q` (line 384) skips it, and it must come before `-c`, since flag
  parsing stops at the first argument that is `--` or looks like a path.

Upstream issue: https://github.com/Hammerspoon/hammerspoon/issues/3872

`core/ipc-fix.lua` replaces the global `print` again, with a correct guard: a
depth counter, and a nested print goes straight to the console and stops there.
Nested output is still visible, it just never reaches the mirror, so it cannot
recurse — and the warning that was the fuel is never emitted at all. The
console write is a local `rawPrint` rebuilt on `hs._logmessage`, the same
primitive `_coresetup.lua` uses, so it never touches the global `print`.

It is loaded by an explicit `dofile` in `boot.lua` immediately after
`ipc = require "hs.ipc"`: it has to run after `hs.ipc` has replaced `print` and
before anything else in the config can print.

The whole patch is guarded. It applies only if `hs.ipc` really is loaded and
really has replaced `print`, and otherwise writes one line to the console
saying why it did not. `ipcFixApplied` says which happened:

```sh
hs -q -c 'return tostring(ipcFixApplied)'
```

The `"none"` default is deliberately left alone. That is upstream's bug, and
flipping it here would also change what `hs -i` shows in this config; the guard
fixes the hang without changing what anyone asked to see.

Scripted callers should still pass `-q` — [agfi:hammerspoon] does it for you —
since a mirrored console line lands in the middle of the output a caller is
parsing. It is no longer a hazard, just noise.

## Running zsh in the garden

`lua/pipe.lua` holds the clients, named `brishz_eval[_q][_bg]`. `_q` takes an
argument list instead of a command line, `_bg` does not wait. `brishz_eval_hs`
lives in `core/helpers.lua` instead, because it uses `hs.task`, and `pipe.lua`
is plain Lua over posix.

Which to use, with warm measurements from this machine:

- `brishz_eval(cmd, opts)` — 53ms, waits, returns output, stderr and exit status
- `brishz_eval_q(argv, opts)` — 78ms, same but the client quotes each element,
  and the status is the command's own rather than the client's
- `brishz_eval_bg(cmd, opts)` — 20ms, forks twice and forgets
- `brishz_eval_q_bg(argv, opts)` — 22ms, the same with an argument list
- `brishz_eval_bsh(cmd)` — a session that keeps its state between calls
- `brishz_eval_hs(cmd, label)` — 7.5ms, and the only one that reports a failure

Inside Hammerspoon prefer `brishz_eval_hs` for anything whose output you do not
need: it is the cheapest of them and it logs a non-zero exit. `brishz_eval_bg`
exists for Lua without Hammerspoon. Anything synchronous blocks the main thread,
which is also the hotkey and event thread, so treat 53ms as 53ms of frozen
keyboard.

The `_q` distinction is about cost. Quoting means going through `brishzq.zsh`
rather than the small dash client, which is about 25ms of zsh startup — worth it
when a value is interpolated, wasted when the command is a constant. In the
`_bg` forms it is free, since nothing waits, so prefer `_q` there whenever a
value is involved.

Nothing here builds a shell string. Every call execs a client with an argument
list, so there is no quoting step that can turn a value into code. `opts` covers
`session`, `stdin`, `evalFile` and `outFile`; passing data on `stdin` is the way
to feed a pipeline something arbitrary, as `system-keys.lua` does with the
clipboard.

## Auto-reload

`reload.lua` watches `~/.hammerspoon/` and `~/scripts/hammerspoon/` recursively
and reloads the whole config when a `.lua` under either changes. Hyper+Cmd+R and
`hs-reload` call `hs.reload` directly, so a manual reload is never affected by
anything below.

A save is rarely a single write — editors write a temp file and rename it, and
anything touching several files fires the watcher repeatedly — so the reload is
coalesced: each event restarts a timer, and the reload happens
`hammerspoonReloadCoalesceSeconds` (0.2) after the last one. Measured, five
saves 100ms apart become one reload, while three saves a second apart still
produce three, which is right: a burst is one edit, a pause means you meant it.
Set `hammerspoonReloadCoalesce` to false to reload on every event instead.

### Holds

Any file in `~/.hs-no-reload/` whose mtime is in the **future** is a live claim
on the reloader, and while one exists nothing reloads by itself. That is what
`hs-reload-hold` writes and what `hammerspoonReloadHeldBy()` reads; `hs -c
'return hammerspoonReloadHeldBy()'` answers "why did my save not do anything".

It exists for agentic editing. Reloading mid-edit loads a half-written module,
and worse, leaves the previous code's canvases and timers behind — a state that
reads exactly like a real bug and is not one, which has already cost one
session a detour to disprove.

A directory rather than a single flag file, because several agents edit this
repo at once. One shared flag would let whoever finished first re-enable
reloading under someone still typing; a counter would be worse still, since the
first agent to be killed would leave it stuck above zero and auto-reload dead
for good, silently. One file per holder has no shared mutable state to race on.

The deadline lives in the mtime rather than the contents so the check is one
`hs.fs.attributes` stat with no parsing and no file reads — it runs on
Hammerspoon's main thread, where blocking freezes every keystroke on the
machine. Redis would have fit the house style for flags, but reading it there
means a blocking socket round-trip on that same thread, and an outage would
force a choice between suppression silently failing and auto-reload never
running again. A missing directory simply means nothing is holding.

Holds expire on their own, and that is the point rather than a limitation: an
agent that crashes, is killed, or just forgets must not be able to leave
auto-reload off permanently. A hold that ends early is a far smaller problem
than one that never ends.

Setting `hammerspoonReloadHeldAlert` to true puts a grey band on screen naming
the holder whenever a reload is suppressed. It is off by default because that is
a band on every save; it uses a fixed alert id, so a burst refreshes one band
rather than stacking a wall of them. Note that all of these knobs are ordinary
globals, so changing one from the console lasts only until the next reload —
edit `reload.lua` to change one for good.

App-scoped modes should use the shared `ModalMode` helpers. qView is defined in
`auto-load/qview.lua`, exposes `qview_bind_v2` and `qview_bind_v3`, and enters
while qView is the frontmost app. Its overlay is positioned in the top-left
corner. App-scoped modes are temporarily suspended while global modes such as
Hyper or Purple are active, then re-sync with the frontmost app after the
global mode stack exits.

Mode overlays are indicator groups (`ModalMode.createIndicatorGroup`): one
canvas per target screen, cached per screen. `ModalMode.screenWatcher`
invalidates the cached canvases whenever displays are added, removed, or
rearranged, so overlay positions self-heal without an `hs.reload()`. Other
modules can hook the same watcher with `ModalMode.onScreenChange(fn)` (the STT
recording indicator does this).

Which screens an overlay appears on is controlled by the `overlayScreens`
style key, resolved by `ModalMode.targetScreens`. Accepted values: `all` (every
screen), `primary` (menu-bar screen), `internal` (built-in display),
`all_external` (alias `external`), `active` (alias `main`; the screen with
keyboard focus), and `mouse` (the screen containing the pointer). Specs that
match no screen (e.g. `all_external` with no external attached, or `internal`
in clamshell mode) fall back to the primary screen so the overlay is never
invisible. App-scoped modes default to `primary`; pass `overlayScreens` in the
mode's `overlay` table to change it. The Hyper banner defaults to `all` via the
`hyper_overlay_screens` global in `core/hyper-mode.lua`, and the Purple banner
via `purple_overlay_screens` in `purple-mode.lua`.

Both global modes draw their banner this way. Hyper and Purple each used to
build one `hs.alert` per screen instead, rebuilt on every entry from
`hs.screen.allScreens()`; neither could draw over a fullscreen space
([issue 3586](https://github.com/Hammerspoon/hammerspoon/issues/3586)) and
neither followed a display being plugged in or unplugged. Hyper moved to a
canvas group behind a `hyper_alert_canvas_p` flag, which is gone: the alert
path it selected between was dead and has been removed.

`bind_v3` defines modal key chords. Key arrays use Hammerspoon key names plus
aliases such as `SPC`, `RET`, and `ESC`. While a chord is pending, the mode
overlay shows the pressed prefix. Valid next keys are consumed and advance the
chord; invalid next keys are consumed and cancel it. `Escape` cancels a pending
chord. Chords have no timeout, so prefix-overlapping chords are rejected.

Purple Mode is defined in `purple-mode.lua` and loaded before the mouse
bindings that use `purple_bind_v2`. Enter it with `Hyper+Cmd+P`. Current
built-in Purple bindings include:

- `Shift+Escape`: exit Purple Mode.
- `q`: send `Cmd+Z`.
- `a`: send `Cmd+Ctrl+H`.
- `d`: send `Cmd+Ctrl+M`.
- `s`: send `Cmd+Shift+M`.
- Arrow keys: move the mouse cursor using the keyboard-mouse helpers in
  `boot.lua`.

The avy grids in `core/mouse.lua` (`screenPositionAvy` and its click, drag,
text-select, and screenshot wrappers) label the screen with two-key
combinations. The label alphabet adapts to the screen: when a screen needs
more labels than the base list provides (bigger monitors), the second-char
alphabet grows minimally with comfort-ordered extension keys
(`avySecondCharExtension`), so small laptop screens always render the classic
grid unchanged. Beyond the two-key ceiling (~7.7k labels, e.g. non-HiDPI 4K),
cells enlarge just enough for full coverage. Extended lists are memoized per
alphabet size in `avyCombinationsFor`.

## Alerts

`alert/` draws alerts as coloured bands across the screen, one
per live alert, stacked. It replaces `hs.alert`'s single centred box for
everything that goes through `hs-alert` in zshlang: several alerts can be up at
once without hiding each other, long text wraps and the band grows rather than
being cut off, and an optional fullscreen flash makes one impossible to miss.

It is the engine for the Lua config too, which is why alerts raised from a
hotkey and alerts raised from the shell now look and stack the same way.

It is one module in six files: `state.lua` (the namespace, the live state, every
tunable), `colors.lua` (both palettes and how a name resolves), `markup.lua`,
`layout.lua` (measuring, wrapping, where each band goes), `render.lua` (canvases,
the flood, the countdown ticker, animation) and `api.lua`. `boot.lua` lists them
in that order rather than globbing the directory, so the order is visible where
the loading happens. Since it loads files with `dofile`, each file is its own
chunk and a file-local is invisible to the next one, so everything shared across
the cut hangs off an `AlertEngine` table — the same shape `modal-mode.lua` uses
for `ModalMode`. Only the public `alertV2*`/`alert_gateway*` functions and the
tunable `alertV2*` knobs are globals in their own right.

Callers do not name it, though. Everything goes through `alert_gateway`, with
`alert_gateway_dismiss` and `alert_gateway_exists` beside it, and only those
three functions know that the engine is `alertV2`. Changing which engine draws
an alert, or routing a subset of them somewhere else, is an edit at the bottom
of `alert/api.lua` rather than a sweep over every caller. `opts` is
passed through untouched, so callers still use the option names below. The zsh
side has the same shape: `hs-alert` is a gateway over `hs-alert-v2`, with
`hs-alert-v1` still beside it.

For the same reason callers ask for colours by name (`color = "agent"`) rather
than by referencing an `alertV2*Color` table.

Stock `hs.alert` is the `alert_v1` global in `boot.lua` — named for what it is
so nothing reaches for it by accident, and kept only so the old engine stays
reachable by hand from the console.

Anything fired repeatedly passes a stable `id`, which is what keeps a held
volume key from stacking one band per repeat: re-showing an id rewrites that
band in place. The ids in use are `volume`, `input-language`,
`stt-input-device`, `stt-recorder-mode`, `emoji-chooser`, `wifi-chooser`,
`wifi-watcher`, `hyper-secure-input`, `purple-secure-input` and `nop`. A
message that can be superseded rather than repeated wants one too: the Wi-Fi
chooser shares a single id across the whole connect flow, so `Connecting` is
replaced by its own outcome rather than leaving two bands up.

Callers that used to hold an alert handle and close it now use a fixed id and
`alert_gateway_dismiss`, and pass `seconds = math.huge` where they mean "until
I clear it" — the engine clamps that to its own ceiling, so a caller that dies
cannot leave a band on screen.

It loads after `modal-mode.lua` because it reuses `ModalMode.targetScreens` and
`ModalMode.onScreenChange` rather than running a second screen watcher. No
canvas mouse events are registered, so every canvas is inert to the pointer and
clicks pass through to whatever is underneath.

```sh
hs -c 'alert_gateway("hello", { seconds = 5 })'
hs -c 'alert_gateway_dismiss("some-id")'
hs -c 'alertV2FromFile("/tmp/message.txt", { position = "bottom" })'
hs -c 'alertV2DismissAll()'
```

The first two are the gateway. The last two are engine entry points with no
gateway of their own: `alertV2FromFile` is what the shell wrapper calls, and
`alertV2DismissAll` has no caller in the Lua config, so neither was given an
indirection that nothing would use.

Options: `id` (re-showing the same id updates that alert in place), `seconds`,
`color`, `position`, `flashSeconds`, `floodFade`, `countdown`, `pinned`, and
`screens` (a `ModalMode.targetScreens` spec). Everything expires on its own, so
a caller that crashes cannot leave the screen branded.

`alertV2FromFile` is the entry point the shell uses: it reads the message from
the file and deletes it. `hammerspoon -c` hangs on payloads of a few hundred
characters and takes the ipc port down with it until the stuck client is
killed, so the text must not travel in the command string — escaping it or
base64-encoding it makes no difference. `alertV2FromBase64` exists for short
messages typed by hand, where quoting is the only problem.

Payload size is not the only way to wedge `hs -c` — printing to the console
during a command used to do it too. That one is fixed now, by
`core/ipc-fix.lua`; see "The ipc print recursion fix" above. So the old advice
here, to warm every extension with a call of its own before driving anything
interesting from the shell, is no longer needed. Passing a large payload in a
file still is: that is a separate wedge and the patch does nothing for it.

### Stacking and positions

Positions are `top` (the default), `center` and `bottom`, each an independent
stack. Two positions can overlap on a small screen; that is accepted rather
than prevented.

A stack grows away from its anchor edge with the oldest band at the anchor, so
a new alert never shifts the words someone is part-way through reading. For a
top stack that puts the newest at the bottom; a bottom stack anchors at the
bottom edge, which puts the newest on top.

### Height, wrapping and the cap

Text wraps against the actual pixel width and the band grows to fit. The font
is never shrunk. Menlo is used because it is monospaced, which makes wrapping
arithmetic rather than a measuring call per word, and makes command output line
up.

No position may use more than `alertV2MaxStackFraction` (0.45) of a screen's
usable height. Within that budget every alert gets one line before any alert
gets two, and the surplus then goes to the pinned alert and the newest one. So
a sixty-line command output does not push everything else off the screen: the
older alerts collapse to a single line reading `first line ... (+59 more
lines)`. An alert only disappears entirely when there is not room for one line,
and then a band at the anchor edge says how many are hidden. Nothing is
silently dropped.

`pinned` alerts claim their space before everyone else. The agent banner uses
it, so a wall of text elsewhere cannot push it off.

### The fullscreen flash

`flashSeconds` washes every screen in the alert's colour before the alert
settles into its band. During the flash all bands are drawn at exactly the
geometry they will keep, so when the wash drains away the words do not move,
resize or reflow — a flash that re-centred its own text would yank it out from
under whoever started reading it. Two flashes at once: last one wins.

The wash is see-through, `alertV2FloodAlpha` (0.33). It has to be impossible to
miss, but it covers every screen and should not black out what you were looking
at to do it. The bands keep their own opacity and stay readable on top of it.

The wash fades in and back out rather than snapping, which otherwise reads as a
glitch instead of as something arriving. Both ramps live *inside*
`flashSeconds`: the flood's total life is exactly what the caller asked for, and
adding the animation moved nobody's timing. The alternative — `flashSeconds`
meaning "time at full opacity", with the ramps added around it — would have
stretched every existing caller silently, and would have broken the banner's
release flash, which is built so that its alert and its flash end together.
Neither ramp may take more than 40% of the window, so even a very short flash
still reaches full colour in the middle. `alertV2FloodFadeInSeconds` (0.10) is
shorter than `alertV2FloodFadeOutSeconds` (0.20) because arriving is an alarm
and wants to be abrupt, while leaving is a release and wants to drain. The ramp
is a smoothstep, redrawn `alertV2AnimationFps` (30) times a second; a linear
ramp reads as stopping abruptly at both ends. That knob now paces every animated
thing, not just the fade — see Colours below.

`floodFade = false` restores the hard cut, and a number sets both ramps to that
many seconds. Only the wash rectangle is animated, never the canvas: fading the
canvas would fade the band copies drawn on it and then pop them back to full
opacity the moment the flood died. The fade is derived from the clock rather
than stored on the canvas, because the flood is torn down and rebuilt whenever
an alert arrives or a countdown ticks, and a fade held on the canvas would be
wiped by the first of those.

### Colours

All of it lives in `alert/colors.lua`, which is the point of that file existing:
the palette should be readable at a glance rather than reconstructed from the
engine around it.

Callers name a colour as a string rather than passing a table, because the shell
is a first-class caller here and a table literal would have to survive
`hammerspoon -c` quoting. A name resolves in this order, first match winning:

- the five originals — `default`/`warn`/`amber`/`crit`/`agent`/`free`/`notice`,
  backed by `alertV2DefaultColor` (dark slate) for ordinary alerts,
  `alertV2WarnColor` (amber) for something that wants attention without being on
  fire, `alertV2AgentColor` (crimson) for the agent banner, `alertV2FreeColor`
  (blue) for its release flash and `alertV2NoticeColor` (grey) for the
  hidden-alerts band. These stay separate globals, read live, so overriding one
  in a console takes effect immediately;
- the curated palette, `AlertEngine.bandColors`: `success`/`green`, `forest`,
  `ocean`/`info`, `teal`, `sky`, `violet`, `plum`, `rose`, `blood`, `rust`,
  `gold`, `olive`, `slate`, `graphite`, `midnight`, `ink`;
- the animated colours, below;
- any of the ~140 `hs.drawing.color.x11` names, with brightness capped at 0.85
  so `white` or `yellow` becomes a band rather than a highlighter.

Ours resolve before x11 on purpose: several names (`green`, `gold`, `violet`,
`rose`, `teal`, `slate`) exist in both, and a name that reads as a mood should
render as a band. An unknown name is not an error — it falls back to the default
band, because no alert at all is a worse outcome than a plain one.

Everything curated takes its opacity from `alertV2BandAlpha` (0.8), applied by a
constructor so a palette author cannot forget it, and stays dark enough that
white text sits on it. A colour passed in per alert keeps whatever alpha it
carries.

Text colour is not fixed. `AlertEngine.textColorFor` picks black or white from
the band's relative luminance, so an x11 `khaki` band is legible without the
caller thinking about it. The threshold sits above the midpoint, biasing toward
white, because bands are translucent: whatever is behind one bleeds through and
drags its effective brightness down, so a band that measures as borderline light
usually looks darker than it measures.

#### Animated colours

`rainbow-1` walks the hue circle in ten seconds, `silver-pulse-1` breathes a
pale metal band, `wolf-eye-1` swells from near-black to amber-gold and sinks
back. The `-1` is a version, not a count: a variant that spins faster becomes
`rainbow-2` rather than replacing the original.

A colour may be a descriptor instead of a table:

```lua
{ animated = true, period = 10, textColor = { white = 1 },
  at = function(now) return <colour table> end }
```

`at` must be a pure function of the wall clock, because that is the only kind of
animation this engine can keep — canvases are torn down and rebuilt on every new
alert, every dismissal and every countdown tick, so animation state stored
anywhere would be wiped. Phase from `now % period` survives all of it, needs no
per-alert bookkeeping, and makes two bands wearing the same colour move in step.
Adding one is a few lines in `colors.lua`; `wave` and `lerp` are there for it.

`textColor` is fixed for the whole cycle rather than recomputed per frame, since
text flipping between black and white mid-cycle would strobe. That is why each
animation's brightness is capped rather than swinging the full range: it has to
stay inside one contrast regime. Anything an `at` returns must carry its own
alpha — assigning an HSB table without one resets the element to fully opaque.

The fullscreen flash follows the cycle too, unless a colour sets
`animate_flood_color_p = false`. Then it paints one fixed shade instead:
`floodColor` if the descriptor names one, otherwise the brightest point of the
cycle, found by sampling once.

That opt-out exists for a colour whose cycle goes dark, and `wolf-eye-1` is the
only one here that does. A flash is a moment whose whole job is to be impossible
to miss, and its phase comes from the wall clock rather than from when the alert
fires — so a colour dipping near black flashes at whatever brightness the clock
happens to be at. Measured, `wolf-eye-1` spans 0.079 to 0.244 in luminance, and
the same command flashed anywhere across that threefold range from run to run,
including a near-black that read as no flash at all; at `flashSeconds = 5` it
sat near the dark end for seconds. Pinned, it flashes at 0.244 every time.
`silver-pulse-1` never drops below 0.577 and `rainbow-1`'s floor of 0.182 is
about that of the static crimson band, so both animate their flash safely.

The band animates in every case. It is on screen long enough for a cycle to read
as a slow glow rather than as a light going out.

One timer paints all of it, at `alertV2AnimationFps`, shared with the flash's
fade ramps because they are the same job: writing a colour onto an element that
already exists. It runs while a flood is fading, a flood is animated, or a band
that layout actually placed is animated, and stops when none of that holds — so
an ordinary static alert starts no timer at all.

#### Markup spans

Markup spans have their own palette, `alertV2MarkupColors`, because that text
sits *on* a band rather than being one. `grey` and `dim` there are translucent
white rather than fixed greys: dimness is a relation to the background, so a
dimmed span recedes into whichever band happens to carry it instead of assuming
a dark one. A fixed grey looked right on the slate default and was unreadable on
amber.

That palette assumes a dark band, which every curated name is. On a light band —
an x11 name, or `silver-pulse-1` — spans wash out, and `grey`/`dim` disappear
into it entirely. Left alone rather than second-guessed: someone who asked for a
light band with coloured spans on it gets what they asked for, and silently
dropping the colour they named would be the worse answer.

### Peek: holding hyper fades the bands

Bands live at the top of the screen, which is also where tab bars, title bars
and status lines live. So while the hyper modality is held, every band drops to
`alertV2PeekOpacity` (0.08) and comes back the moment hyper is released —
`alertV2PeekBegin` and `alertV2PeekEnd` in `alert/api.lua`, called from
`hyper_modality:entered`/`:exited`, with `alertV2PeekActive` to query it and
`alertV2PeekEnabled = false` to turn the whole thing off. Both knobs are read at
the moment of the peek rather than captured, so either can be changed from the
console without a reload.

What moves is `hs.canvas:alpha()` on the live canvases, not the palette: one
setter per canvas, instantly reversible, no colour recomputed, and with no
alerts up it is two walks over empty tables — which matters, because hyper is
entered and left constantly. The alpha is applied in `newCanvas` rather than
only at the moment hyper is entered, so a band raised *during* a peek comes up
already faint, and so does a canvas rebuilt mid-peek by a new alert, a
rewrapping countdown tick or a screen change. Nothing fades: `alpha()` and
`show()` with no fade time are both immediate, so it reads as a card being
lifted rather than as an animation.

It is visual only. No timer is touched, so an alert whose lifetime runs out
mid-peek dies on schedule instead of reappearing stale when you let go.

The state is a flag rather than a counter, because that is the shape of the
hooks: a modality entered while already entered fires `entered()` again, and a
stray `exited()` has to land on "not peeking" rather than on -1. Two begins and
one end therefore leave the peek off, which is the safe direction — the worst an
uneven pair can do is show alerts that could have stayed hidden.

Three things are deliberately excluded:

Purple. It is a sticky mode you can sit in for minutes at a time, and hiding
alerts for minutes is not peeking, it is hiding them.

The hyper indicator. The star at the top of the screen is a `hs.canvas` from
`ModalMode.createIndicatorGroup`, not an alert, and it stays fully opaque: it is
the mode indicator itself, and its red variant is the Secure Input warning. The
peek only ever walks `alertEngineState.canvases` and `floodCanvases`, so the
indicator groups are never reachable from it.

`hs.alert` (v1). Its canvases live in a module-local table with no public
accessor, so fading them would mean a `debug.getupvalue` reach into the
extension's internals — fragile, and for nothing: v1 is down to a `require` in
`boot.lua` and one call inside an `if false then` block. Not worth retrying.

#### Bands that must not fade

An alert raised with `peek = false` is exempt: `AlertEngine.peekAlpha` returns 1
for as long as any such band is up. It exists for the hyper+F1/F2 brightness
band, a case the peek could not have anticipated — those keys deliberately leave
hyper *entered* so the level can be stepped repeatedly, so the peek was fading
the one band the keypress exists to show.

The exemption is coarse on purpose: one exempt band holds the peek off for every
band on screen, not only for itself. Exempting a single band would mean
splitting a stack across a canvas per group, because a canvas carries the whole
stack for one screen and position, and `hs.canvas` has no per-element alpha —
`c[1].alpha = 0.2` is rejected outright, only `fillColor` carries an alpha
channel, and rewriting colours would collide with the animator that repaints
them. Worth doing if it ever grates; in the case it was written for, the exempt
band is normally the only one up.

It re-dims on its own. `alertV2Dismiss` calls `AlertEngine.render`, which
rebuilds every canvas through `peekAlpha`, so the moment the exempt band expires
the rest go faint again if hyper is still being held.

## Agent focus banner

`core/agent-banner.lua` shows a banner while a coding agent is driving the GUI
and needs the focus left alone, so a human and an agent can share the machine
without either guessing about the other. It is a thin wrapper over the alert
engine: a crimson, pinned, counting-down alert with a fixed id. It is driven
from the shell:

```sh
hs -c 'agentBannerOn("what it is doing", 900)'   # seconds; default 30 min
hs -c 'agentBannerOff()'
hs -c 'return agentBannerActive()'
```

It washes each screen for `agentBannerFlashSeconds` (0.35 by default; 0 skips
it, and a third argument to `agentBannerOn` sets it) before settling into its
strip — see the flash notes above. `agentBannerOff` flashes
`agentBannerReleaseFlashSeconds` (0.5) of blue the same way; the moment the
machine is free again is the one worth noticing, so it lingers longer than the
raise. Both are long enough to pay for the flash's fade in and out, which the
old hard-cut values were not.

Re-calling `agentBannerOn` with the same message refreshes the countdown
without re-flashing, so a long task can heartbeat without strobing. A changed
message does flash again. The banner always expires on its own, so an agent
that crashes or forgets cannot leave the screen branded.

## Blackout keyboard lock

`core/blackout-lock.lua` makes a blacked-out screen deaf as well as dark.
hyper+shift+F1 blanks every display, but on its own that changes nothing about
input: whatever had focus still has it, so a brushed key types into a window
you cannot see, a hardware brightness key raises the level you just cut, and
any other hyper chord fires blind. While the blackout is up, the lock swallows
all of that, and the only input that does anything is the one that ends it.

It is an `hs.eventtap`, which runs before Carbon hotkeys and before any app, so
returning `true` from its callback drops an event for everyone at once — every
other hyper binding and every app shortcut included. It taps `keyDown`, `keyUp`
and `systemDefined` (the hardware brightness and media keys), and with
`blackoutLockMouse` on, mouse buttons and scroll. Movement and `flagsChanged`
are left alone: a lone modifier is harmless, and the escape chord's modifiers
are read off the F2 event itself.

Exactly two things pass through. F18, the physical hyper key, so the hyper
modal can still be entered; and F2 with shift only while hyper mode is entered
— cmd, alt or ctrl on the same event block it. The chord goes through
`blackoutRestore`, which releases the lock synchronously before asking the
garden to run `brightness-on-all-loop`, so the keyboard is back at once.

Two taps are in play during a blackout, and they do different jobs. The lock
tap above only ever decides what to *drop*. The chord tap is separate: it runs
whenever hyper mode is entered, black screen or not, and it is what actually
dispatches hyper+shift+F1, hyper+cmd+shift+F1, hyper+shift+F2 and the bare
hyper+F1/F2 brightness keys, because Carbon drops those presses — see "When a
hyper chord does nothing" below. Since a tap that deletes an event hides it
from every tap after it, the order macOS calls them in must not change the
outcome, so the chord tap repeats the lock's own rule rather than relying on
it: while the lock is up, the only chord that does anything is the one that
ends it.

What F2 leaves on screen is decided when the blackout starts, not when it
ends. After hyper+shift+F1, F2 within the grace period restores straight to
the desktop; past it, the session is locked with `hs.caffeinate.lockScreen()`
first and the display restored a moment later — a blackout up for an hour is
one nobody is watching, so whoever ends it meets the login screen.
hyper+shift+cmd+F1 starts a blackout marked lock-first, via
`blackoutBegin(true)`, and F2 then locks first at any age. The mark belongs to
whoever *starts* the black, because the person who presses F2 might be an
adversary; that is why there is no cmd chord on F2. A reload used to lose the
start time, so F2 restored without locking; now it does not, while redis is up.

Start time and mark are kept in redis under `blackout_lock` — epoch seconds, a
space, `0` or `1` — whenever redis is available. On load the module reads the
key back, but trusts it only if the zsh side's `display_black_saved`, the "is
anything blanked" flag, is present too; otherwise the blackout ended while
Hammerspoon was not running to hear it, and the stale key is deleted. A
recovered blackout gets its tap re-installed with the expiry that remains, so
a reload no longer drops the keyboard lock either. `redisGet`, `redisSet` and
`redisDel` in `core/redis.lua` return `(value, ok)`, `ok` false meaning redis
unreachable rather than key absent; if it is down at load, recovery retries.

The knobs are globals in the usual `x = x or default` style:

- `blackoutLockEnabled`, default true. When false, the F1 chords black the
  screen as they always did and install no tap; they still call
  `blackoutBegin`, so the age rule above holds regardless.
- `blackoutLockMouse`, default true. Whether clicks and scroll are swallowed.
- `blackoutLockScreenAfterSeconds`, default one hour. A blackout older than
  this locks the session before the display is restored. 0 locks first every
  time; false never does. A lock-first blackout ignores it and always locks.
- `blackoutLockMaxSeconds`, default a week. An expiry backstop: a blackout
  older than this is forgotten and ends by way of `blackoutRestore(true)`, the
  session locked, then the screen back. A week, because the chord, a wake and
  the shell are the real ways out and a blackout over a holiday must not end
  by itself; it exists so the lock always ends, on a login screen rather than
  an unlocked desktop behind black.

It is driven from the shell too:

```sh
hs -c 'blackoutLockOn(seconds)'       # seconds optional; a short value is for testing
hs -c 'blackoutLockOff()'
hs -c 'return blackoutLockActive()'
hs -c 'blackoutRestore()'             # what hyper+shift+F2 does
hs -c 'blackoutRestore(true)'         # lock the session first, always; shell only
```

There are four ways out, and every path that ends a blackout takes one of them.
F2, through `blackoutRestore`. The wake watcher in `core/power-watcher.lua`,
which calls `blackoutLockOff` on `systemDidWake` and `screensDidWake`, since a
wake lands on a login screen anyway. The zsh `display-black-off`, which calls
`blackoutLockOff` over `hammerspoon -c` right after its unconditional gamma
restore — the one point every unblack path reaches, whether F2, `h-hook-wake`,
`h-hook-unlock` from the Swift lock-watcher or the function run bare from
another machine, so the lock can never outlive the black. And the expiry.

Only the Hammerspoon side locks the session; a shell restore releases the lock
and brings the display back, nothing more, because it is the owner acting from
ssh, not a hand at the keyboard. The invariant that falls out of all this: the
keyboard lock never releases into an unlocked session on its own. The expiry
locks first, a wake is a login screen already, and the only path onto a live
desktop is F2 inside the grace period of an unmarked blackout — a person's act.

The tap is installed only for the life of a blackout, for the privacy and
latency reasons given in `core/fim.lua`. Its limits all fail toward "the keys
come back", never toward a locked machine. Secure Input — a focused password
field, the login screen — hides keystrokes from event taps, so the lock cannot
block typing there; engaging while it is on shows a warning alert saying so. A
Hammerspoon crash drops the tap silently while the screen stays black, until
the next load recovers it from redis, and macOS disables a tap whose callback
stalls. The engage alert shows before the screen goes black, since black-on is
asynchronous through the garden and the alert is not. There are two of them: a
plain blackout says "Input locked." in the amber `warn` band, and a lock-first
one says "Input locked. Ending the blackout locks the screen." in `blood`, a
far darker red. The colour carries that difference because nothing else can —
both chords leave a screen equally black, and the mark cannot be revoked once
it is set, so this band is the only moment it is ever confirmed, and it has to
be legible at a glance rather than by reading. `blood` is darker than the
crimson `crit` of the Secure Input warning above as well, which can land in the
very same instant.

## When a hyper chord does nothing

A hyper chord fails silently perhaps one press in five, and pressing again
works. Nothing is logged and nothing visibly happens, yet the keypress is still
delivered to the focused app — so a latched Sticky Keys shift is consumed,
which makes the chord look as though it registered. Seen on hyper+shift+F1 and
hyper+shift+F2, and reported independently on the bare hyper+F1/F2 brightness
keys.

What was measured, over fourteen presses: at every failure hyper mode was
entered, the chord's hotkey was listed by `hs.hotkey.getHotkeys()`, the event
carried exactly the right modifiers, Secure Input was off, and the press was
not an autorepeat. Every one of those was identical on the presses that worked,
and the timings overlap completely — 402.2 ms after the modal entered
succeeded, 403.6 ms failed. The single asymmetry: an eventtap watching the same
two keycodes saw all fourteen, including every failure, while Carbon delivered
about four in five. The loss is inside `RegisterEventHotKey`, underneath
anything Lua can observe. That is why these chords are dispatched from a tap in
`core/blackout-lock.lua`, and why a chord that must not fail has no business on
`hs.hotkey`.

Dead ends, recorded so nobody walks them twice:

- The modal's own state. `mode.down` in `modal-mode.lua` genuinely does toggle,
  so a mode that is already entered gets turned *off* by the next F18 press —
  a trap worth knowing, but not this one: the failures all logged
  `entered=true`.
- Sticky Keys. It is on (`stickyKey = 1`), and both the event flags and the
  live hardware modifier state showed shift on every failure.
- Autorepeat. Carbon does not fire a hotkey for an autorepeat keyDown, but
  every captured press had autorepeat 0.
- The `fn` flag. Every F-key press here carries it, successes included, so
  Carbon evidently ignores it.
- Chord bindings. There are none; `chordRoots` is empty, so the chord eventtap
  cannot have swallowed anything.
- A registration race after modal entry. Ruled out by the overlapping timings,
  and by a failure and a success in the same hyper session with no F18 between
  them.
- The `hs -c` wedge that `core/ipc-fix.lua` documents. IPC calls into
  Hammerspoon did land 0.5–1.1 s before all eight early failures, and
  `hs.hotkey` printed a conflict warning on every modal transition because
  STT's global bare F1/F2 collided with the modal's own. Silencing those prints
  with `hs.hotkey.setLogLevel("error")` changed nothing at all.

How to investigate the next one. Instrument at runtime through `hs -c` rather
than by editing files: nothing reloads, so no state is lost and the
auto-reloader stays out of it. Log to a file, because `hs.console.getConsole()`
does not update from inside an `hs -c` call. Filter the tap to the keycodes
under investigation and log nothing else — anything wider is a keylogger. Above
all, wrap the *handler* as well as the key, so that "the hotkey never fired"
can be told apart from "the handler ran and did nothing"; those two have
nothing in common and confusing them wastes the afternoon. Such instrumentation
is runtime-only, so any `hs-reload` clears it — the right default, but it means
a reload mid-investigation silently disarms you.

Synthetic events cannot test a Carbon problem. `hs.eventtap.keyStroke` never
triggers an `hs.hotkey` binding at all, so a posted-keystroke harness reports
failure at every delay and proves nothing. It does reach an eventtap, which is
what makes the tap dispatch testable end to end without blanking the screen.

## FIM completion

`core/fim.lua` puts the fill-in-the-middle completion of `docs/fim.md` on two
hyper chords, inserting one line at the cursor of whatever text field is
focused:

- `Hyper+Shift+Right`: complete with the default provider (`codestral`, ~0.3s).
- `Hyper+Ctrl+Right`: complete with `deepseek` (~1.4s, better output).

It reads the text around the cursor through `hs.axuielement` where the app
exposes `AXValue` and `AXSelectedTextRange`, and otherwise through a
`shift+cmd+up` / `cmd+c` / `right` / `shift+cmd+down` / `cmd+c` / `left` dance
over the clipboard — Purple Telegram's Qt draft box is not in the accessibility
tree at all, and kitty's text area is always empty. The two arrow keys collapse
each selection back to the original cursor: the anchor does *not* stay put
across the two extensions, and without them the second copy returns the whole
document and the cursor ends up at offset 0. See `docs/fim.md`.

The completion appears as a ghost in a violet alert band with the id `fim`: any
key inserts it, `Escape` discards it, and `⇧Escape`, a `cmd`/`ctrl` chord or a
change of app copies it to the clipboard. Hyper and purple keys pass straight
through and do neither — they are held modalities carrying no modifier flags, so
until this they read as ordinary typing and accepted the ghost. A keyDown
eventtap exists only for the duration of a run, never permanently.

The band also shows the context that was sent, dimmed, above the completion —
60 characters of prefix, a `‸` cursor mark, 40 of suffix, with the two newlines
nearest the cursor kept as real line breaks and the rest collapsed to `⏎`.
That is a deliberate exposure of whatever field you were typing in, at readable
size on your own screen; `fimContextInBand = false` turns it off. Because the
band is now `md` markup, every value interpolated into it is escaped first.

Accepting swallows the key you pressed and posts it once the paste has landed —
watched through `AXNumberOfCharacters` where the app exposes it, on a fixed 60ms
delay where it does not. Returning the key from the tap alongside `cmd+v`, which
is what this used to do, has it processed against the pre-paste buffer in any
app that pastes asynchronously (Qt, Electron, WebKit), so an arrow navigated the
old text. Keys typed while the paste is in flight queue behind it and are posted
in order. Auto-repeat across an accept is lost. See `docs/fim.md`.

`fimAppPolicy` decides what the chord does per app, keyed by bundle ID. kitty
(`net.kovidgoyal.kitty`) and Emacs (`org.gnu.Emacs`) are mapped to
`{ hotkey = { mods = {"alt"}, key = "." } }`, so the chord posts *their* FIM
binding instead of running this one — both know their own buffer exactly, and
kitty's text area is not readable through Accessibility anyway. Other values are
`"default"` (run FIM, and what an unlisted app gets via `fimAppPolicyDefault`),
`"ignore"`, and `"ignore_and_notify"`.

`Hyper+Shift+Right` used to move the mouse pointer. Those four
`hyper_bind_v2{mods={"shift"}, key=<arrow>}` bindings in `core/mouse.lua` are
retired — wrapped in `if false then` rather than deleted — since purple mode's
bare arrows already cover keyboard mouse movement. The `purple_bind_v2` arrow
bindings beside them are untouched.

`~/.hammerspoon/init.lua` includes a `hyper+w` Wi-Fi chooser.

The chooser shows cached network names immediately when available, refreshes
nearby Wi-Fi networks in the background, ignores transient scan errors while
keeping the cache visible, marks the currently connected SSID with `*`, and
toggles the selected network. Selecting the current network disconnects from
Wi-Fi. Selecting another network runs:

```sh
networksetup -setairportnetwork <interface> <ssid>
```

This works best for open or previously remembered networks. New protected
networks may still need credentials added through macOS first.

## kitty: hyper+z

`kittyHandler` in `core/window-media-bindings.lua` is the hyper+z toggle for
kitty, and hyper+z is the only way kitty is ever reached. A press shows every
tab of the one kitty instance (bundle `net.kovidgoyal.kitty`; nothing else in
this config knows any other) and the next press hides it again and hands focus
back to the app you came from. There is no hyper+shift+z any more. How the
tabs are shown is decided by the global `kitty_hotkey_mode`: `"panel"`, the
default, or `"window"`. It follows the usual `x = x or default` pattern, so a
file that loads earlier can set it before this one runs.

In panel mode every tab lives in a kitty *panel* OS window, which floats over
whatever is in front, fullscreen apps included. The main kitty creates it for
itself over remote control, with `kitty @ launch --type=os-panel --os-panel
edge=center --os-panel layer=top --os-panel focus-policy=on-demand
--os-window-class kitty-panel --dont-take-focus`; kitty recognises it by
`wm_class` `kitty-panel` in `kitty @ ls`, and Hammerspoon as kitty's only
non-standard window. The zsh side is `kitty-panel-ensure`, `kitty-panel-show`
and `kitty-panel-hide` in `zshlang/auto-load/others/terminal
emulators/kitty.zsh`, run in the garden so nothing in Hammerspoon blocks.
`kitty-panel-ensure` launches kitty when it is not running (`open -b
net.kovidgoyal.kitty --args --start-as minimized`, then polls the socket for
up to 20 s), creates the panel if there is none, and hides it once: a fresh
panel counts as shown for kitty while macOS has put it on the desktop space
only, so without that its first show would be a no-op there. When it has just
created the panel, which is every kitty launch, since kitty cannot start as a
panel and the startup session always opens in a normal window first, it moves
the tabs of every other OS window in with `detach-tab`, in order, and closes
the shell tab the panel was born with, which was only scaffolding once real
tabs have arrived. Windows that scripts open later are left alone unless the
zsh variable `kitty_panel_fold_strays` is set to `y` (default `n`, settable in
a private startup file), in which case every show folds them in. It never
focuses anything: focusing a hidden panel activates kitty on the desktop space
before `show` has joined the current one. `kitty-panel-show` calls it, shows
the panel, and only then runs `focus-window` on the active window.
`kitty-panel-hide` matches a window inside the panel by id, so it hides only
the panel and a normal window is left alone. On the Hammerspoon side `kittyPanelToggle` decides show
or hide by whether kitty is frontmost, nothing more, and calls the two through
`brishz_eval_hs`; remember `brishz-restart` after editing the zsh, since the
garden does not see edits on its own. The window level is
`macos_ns_window_layer NSFloatingWindowLevel + 1` in
`configFiles/kitty/kitty.conf`, level 4: above every window in a space, below
Spotlight at 23 and Handy at 25. 3 never came up over a fullscreen space. It
applies only to panels created after the config is loaded.

In window mode `kittyWindowToggle` shows kitty's normal window maximized on
the screen the mouse is on, and hides it on the next press. From a fullscreen
app a press switches to kitty's desktop and a second press switches back,
which is exactly what macOS itself would do with any normal window: a normal
window cannot be put over a fullscreen space, and Hammerspoon cannot move one
there, since a forced `hs.spaces.moveWindowToSpace` into a fullscreen space
returns true and does nothing (measured 2026-09-07, macOS 14.3.1, Hammerspoon
1.1.1). What it can do is evict. A kitty window can be born inside another
app's fullscreen space, because macOS parks a new window in whatever space is
active, and after that every activation of kitty jumps to that space; that was
the original "hyper+z keeps opening Telegram" bug. So before every show and
every hide `kittyEvictFromFullscreen` checks whether the window is in any user
space and, if not, moves it to `kittyHomeSpace`, the first user space of its
screen, with `force=true`, which leaving a fullscreen space requires. Between
user spaces a plain move does work and saves a space switch, so on show, when
the mouse screen's active space is a user space the window is not already in,
it is moved there.

Both modes share two things. The first is the launch: kitty quits when its
last window closes (`macos_quit_when_last_window_closed`), so "not running" is
a normal state and the key has to start it. The second is the return of focus
after a hide, which macOS will not do for you. `kittyReturnTo` holds the last
activated app other than kitty and the transient apps in
`kittyTransientBundles`: Hammerspoon itself, for choosers and the Secure Input
webview; Maccy, whose popup is the hyper+v passthrough key; and Handy, whose
dictation overlay is cmd+'. It is kept by `kittyFocusWatcher`, an
`hs.application.watcher`, so the memory is refreshed by every switch you make
and is never stale. The handler reads it at press time, before the hide,
because the activation the hide causes would overwrite it; then
`kittyFocusAfterHide` focuses that app's focused window, falling back to
`activate()`, inside `pcall` in case the app has quit since. Nothing in the
kitty path enumerates windows: `hs.window.orderedWindows` asks every process
through Accessibility, and the "Handy Web Content" processes take 1.5 s each
to answer (see `axLatencyReport` in `core/app-hotkeys.lua`). In panel mode the
same watcher also hides the panel when any non-transient app is activated, by
an app hotkey, Cmd-Tab or a click, because an overlay cannot go behind the app
you switch to.

Every press logs one line to the Hammerspoon console, `kittyHandler: press
(<mode>); kitty <frontmost|running|not running>; frontmost=<app>; ->
show|hide`, so a press that "did nothing" can be traced to which way it went
and what was in front. In panel mode, `kitty-remote ls` piped through `jq -c
'.[] | {id, wm_class, ntabs: (.tabs|length)}'` should show one OS window of
`wm_class` `kitty-panel`; with the default `kitty_panel_fold_strays`, a second
OS window of `wm_class` `kitty` is a script's window and is expected to stay.
In window mode, run
`hs.inspect(hs.spaces.windowSpaces(<kitty window id>))` after a hide and check
the result with `hs.spaces.spaceType`: it must name a user space.

The panel design was reverted once today, so the day is worth recording. The
panel drew black frames for about 0.35 s on most Cmd+arrow presses, and no
setting changed that. Later the same day the normal window drew the same
frames, in the same kitty process, which by then had been through many config
reloads, panel creations and tab moves, and a kitty restart cured them; the
long-running process was to blame, not the panel, and the panel came back as
the default. The full account, with measurements, is in the notes under
`~/notes/public/subjects/tools/CLI/terminal emulators/Kitty/hotkey window.org`,
heading "Trying to Use Kitty Panel". kitty's own quick-access kitten was
rejected for a different reason: it runs as a second app bundle,
`net.kovidgoyal.kitty-quick-access`, and every per-app rule in this config
assumes only `net.kovidgoyal.kitty` exists.
