# Hammerspoon

`~/.hammerspoon/init.lua` is a symlink to `~/scripts/hammerspoon/boot.lua`.
The boot file only sets up Lua/Hammerspoon dependencies and then loads an
ordered module list, mostly from `~/scripts/hammerspoon/core/`.

The explicit core load order is:

- `helpers.lua`
- `modal-mode.lua`
- `alert/state.lua`, `alert/colors.lua`, `alert/markup.lua`,
  `alert/layout.lua`, `alert/render.lua`, `alert/api.lua`
- `agent-banner.lua`
- `redis.lua`
- `wifi-watcher.lua`
- `hyper-mode.lua`
- `purple-mode.lua`
- `mouse.lua`
- `input-language.lua`
- `popclick.lua`
- `system-keys.lua`
- `choosers.lua`
- `app-hotkeys.lua`
- `window-media-bindings.lua`
- `stt.lua`
- `reload.lua`

`reload.lua` loads every Lua file in `~/scripts/hammerspoon/auto-load/` in
alphabetical order after the core modules are ready.

Put core features in `core/` and add them to the explicit list in `boot.lua`.
Put app-specific add-ons that can run after all core modules in `auto-load/`.

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
