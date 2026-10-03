# Multiple monitors

How this Hammerspoon config decides which screen something happens on, why it
used to get that wrong, and what is still unmeasured. The per-feature details
live next to each feature in `docs/hammerspoon.md`; this file is the map.

## Terms

These are used throughout, here and in `core/screens.lua`:

- **Active screen**: the screen of the focused window,
  `hs.screen.mainScreen()`.
- **Primary screen**: the screen with the menu bar,
  `hs.screen.primaryScreen()`. With the lid open this is usually the laptop
  panel.
- **Pointer screen**: the screen under the mouse,
  `hs.mouse.getCurrentScreen()`.
- **Typing screen**: where keyboard input lands. Today that is the active
  screen; it is a separate name so that typing-side overlays say what they
  mean.
- **Working screen**: where a user-directed action that is tied to neither
  typing nor the pointer lands (avy, `cursorHide`, the kitty panel). It is the
  active or the pointer screen, chosen by the enum `screens_working_policy`.
- **Spec**: a string that names a set of screens, resolved by
  `Screens.target(spec)`. Most specs name an *intent* (`typing`, `working`)
  rather than a mechanism (`mouse`), so the policy can change in one place.
- **Display UUID**: `hs.screen:getUUID()`, upper-cased. It is the same value
  m1ddc prints as "System UUID" (checked on both displays here), so Lua and
  zsh can key state on it. It is the only identity that survives a hotplug.
- **CG id**: the CGDirectDisplayID, `hs.screen:id()`. zsh's `id:<n>` selector
  takes it. It is not guaranteed stable across a hotplug, and the blackout's
  pending rows here named CG ids 34 and 36, which no attached display had. So
  it is only ever carried for the length of one call.

## The `main` clash

The word `main` means two different screens on the two sides:

- In Hammerspoon, `hs.screen.mainScreen()` is the **active** screen.
- In zsh's display commands (`brightness-get main`, the default selector of
  [agfi:h-brightness-select]), `main` is the **primary** screen.

Before this work, the level keys sent no selector, so zsh used its `main`: the
laptop panel, while you were working on the monitor. That is half of why the
contrast keys looked broken (see "Which display a press steps" in
`docs/hammerspoon.md`).

Neither name was renamed, since both are load-bearing in existing code. Instead
both sides gained an unambiguous spelling, and new code uses it:

- Lua specs: `active` and `primary`. `main` stays as an alias of `active`.
- zsh selectors: `primary` (an alias of zsh's `main`), `id:<n>` and
  `uuid:<U>`.

## Inventory: who picked which screen, before

Every module picked its own screen, through `hs.screen` directly:

- **App-mode overlays** (qView's) defaulted to `primary`, so the overlay
  appeared on the laptop while qView was on the monitor.
- **Hyper and Purple banners** drew on `all`. That was already right.
- **Alerts** took a `screens` spec per alert, but the fullscreen flash always
  washed every screen.
- **The level keys** (hyper+F1/F2, hyper+ctrl+F1/F2) sent no selector, so
  they stepped zsh's `main`, the primary screen.
- **`cursorHide`** took the active screen's width and height but not its
  origin, so on any screen but the primary the pointer landed on the primary.
- **The avy grid** covered the active screen, with no way to reach another.
- **The STT indicator** sat on the primary screen.
- **The emoji tally** sat on the primary screen, next to a chooser that opens
  on the active one.
- **The kitty panel** appeared wherever kitty first created it.
- **Window-mode kitty** followed the pointer screen. It still does.
- **Blackout rows** were keyed by CG id, so a display that came back under a
  new id lost its saved levels.

Nothing agreed with anything else, and a fix in one module taught the next
one nothing.

## Three designs

### Design 1: fix each module where it stands

Each module keeps calling `hs.screen` itself; each wrong choice is corrected
in place.

- For: the smallest change, with no new module and no load-order dependency.
- Against: there is still no shared vocabulary, so the next module makes its
  own choice again. Each module that must react to a display change carries
  its own watcher. The Lua and zsh sides still share no identity, so the
  blackout bug stays.

### Design 2: intent-named specs

One resolver, `Screens.target(spec)`, and modules name what they want
(`typing`, `working`) instead of how to find it.

- For: one place decides what "the screen I am working on" means, and one knob
  (`screens_working_policy`) changes it everywhere. It costs only a function.
- Against: it is stateless. It cannot say that a screen was added or that
  focus moved to another one, so an overlay can resolve its screen only at the
  moment it is drawn and then stays put. It has no identity across a hotplug.

### Design 3: a screen registry

A module that owns the list of screens, as records with a stable identity
(display UUID), a CG id for the current layout, a role, and events for
changes.

- For: identity is shared with zsh through the UUID, which is what fixes the
  blackout restore. Events let caches invalidate and let overlays follow focus.
  Roles give a screen a name that does not depend on its position or its
  model.
- Against: global state, more code, and a watcher that wakes on every
  active-screen change. It also adds a load-order coupling: the registry loads
  before `core/redis.lua`, and its first build silently ignored the
  per-screen prefs until that was fixed.

### What was chosen

All three, layered: the registry (Design 3) underneath, specs (Design 2) as
its only interface, and the per-module fixes (Design 1) as the migrations
onto it. The registry without specs would have left every module to interpret
the records itself. Specs without the registry could not have fixed the
blackout restore or let overlays follow focus.

## What was built

### The registry: `core/screens.lua`

- `Screens.list()` returns one record per screen, left to right (then top to
  bottom): `screen`, `uuid`, `cgid`, `name`, `internal`, `frame`,
  `fullFrame`, `role`.
- Roles: `laptop` for the built-in panel, `external-1` to `external-N` for
  the rest, numbered left to right. A role can be overridden per screen in
  redis under `screen_prefs`, a JSON object keyed by display UUID, for example
  `{"<UUID>": {"role": "desk"}}`. It is kept in redis, not in this repository,
  because a UUID identifies one particular monitor. Nothing watches the key, so
  run `Screens.invalidate()` after editing it.
- Events, through `Screens.on(event, fn)`: `layout` (screens added, removed,
  moved or resized), `added` (with the record), `removed` (with the UUID) and
  `active` (the active screen changed). They come from
  `hs.screen.watcher.newWithActiveScreen`. `ModalMode.onScreenChange(fn)` is
  kept as the old name for `Screens.on("layout", fn)`.
- `Screens.target(spec)` resolves every spec. The list, with its aliases, is
  the comment above that function. A spec that matches nothing falls back to
  the primary screen, and an unknown one to every screen, so nothing is ever
  drawn nowhere. `Screens.specMoves(spec)` says whether a spec's answer can
  change without a display change (`active`, `typing`, `pointer`, `working`
  and their aliases); only overlays with such a spec are redrawn on `active`.

### The migrations

Each is documented with its feature in `docs/hammerspoon.md`:

- **Level keys** step the active screen's display, sent to zsh as `id:<n>`.
  Contrast on the laptop, which has none, goes to the external displays.
  Every target has its own flight state. ("Which display a press steps".)
- **Blackout restore** keys saved rows by display UUID and falls back to the
  display's last-good level, then to a fixed level, and never restores below a
  floor. (`docs/external-display-brightness.md` in the repository's top-level
  `docs/`.)
- **App-mode overlays** default to `active`. The STT indicator and the emoji
  tally use `typing`.
- **Alerts**: the fullscreen flash covers the alert's own screens, and zsh's
  `hs-alert-v2` takes `alert_screens`.
- **Mouse**: `cursorHide` and the avy grid use `working`, and space in the
  grid moves it to the next screen.
- **Focus keys**: hyper+; focuses the frontmost window on the next screen,
  and hyper+shift+; moves the focused window there. Both bring the pointer
  along. ("Moving between screens".)
- **kitty panel** shows on the screen named by `kitty_panel_screens`. Every
  show asks kitty for a fresh layout on that screen, and fits the panel when
  it is still off; hyper+shift+; moves the shown panel through kitty, not
  over Accessibility. ("kitty: hyper+z".)
- **Return after a hide**: the second press of an app hotkey, and hiding
  kitty, return to the previous app on the same screen
  (`screenReturnTarget`), not the newest app anywhere. ("App hotkeys".)
- **Floating windows**: after any activation this config causes (an app
  hotkey, a hide's return, kitty's return), a focused window above layer 0,
  such as a Picture-in-Picture video, hands focus to the app's front normal
  window (`appFocusOffFloating`). ("App hotkeys".)
- **Maccy's hyper+v popup** opens on the screen named by
  `maccy_popup_screens`: its `popupScreen` setting is rewritten when that
  screen changes, one write at a time. ("App hotkeys".)

### Windows on a screen

The registry knows screens; several fixes also need the windows on one. That
comes from CoreGraphics' window list (`hs.window.list`), wrapped as
`Screens.windowStack()`: every on-screen window front to back, with owner pid,
bounds and layer, in 19 to 40 ms, asking no app anything. Accessibility's
`hs.window.orderedWindows()` asks every running app instead, and some take
1.5 s to answer. On top of it:

- `Screens.isNormalEntry(e)`: layer 0, visible, and not smaller than a
  helper strip (`kMinNormalW`, `kMinNormalH`). Floating
  windows sit above layer 0 (the kitty panel at 4, Brave's 24 px strip at
  26).
- `Screens.normalWindowsOn(screen, skip, stack)`: those on one screen, front
  to back.
- `Screens.entryWindow(e)`: the `hs.window` for an entry, asking only its
  owning app.
- `Screens.layerOf(id)`: a window's layer at the latest read that listed it,
  kept across reads so a hidden app's windows are still known; it reads
  nothing itself. The floating-window check trusts only its 0 and reads the
  list afresh otherwise.
- `Screens.moveHandlers`: windows that hyper+shift+; must move some other
  way than Accessibility. The kitty panel registers one.
- `Screens.onTargetChange(spec, fn)`: calls `fn(screen)` now, then rechecks
  the spec on every display change and, for a moving spec
  (`Screens.specMoves`), on every active-screen change, calling `fn` again
  when the answer is a different screen. Nothing watches the pointer, so a
  pointer spec follows the mouse only at the next focus or display change.
  For state outside Hammerspoon that has to follow a screen (Maccy's
  setting).

### Checking it from the console

```lua
hs.inspect(Screens.list())
Screens.target("working")[1]:name()
Screens.target("role:laptop")[1]:name()
```

## Follow-ups and open questions

Measured after the reload (2026-10-02; macOS 14.3.1, Hammerspoon 1.1.1, the
laptop panel plus one monitor):

- **The registry and specs.** `Screens.list()` gave the laptop as `laptop`
  and the monitor as `external-1`, both with UUIDs. Every spec resolved to the
  screen it should, with focus on the monitor and the pointer on the laptop,
  and an unknown spec fell back to both screens.
- **The contrast keys.** One `hyperContrastStep("inc")` and one `"dec"` from
  the console, with focus on the monitor, moved its contrast 51, 52, 51, and
  `ddc.log` recorded both `set` calls with their callers.
- **`newWithActiveScreen` reports focus moving between screens.** A
  subscriber on `active` fired once when focus went from the monitor to the
  laptop, with no display change. So overlays with a moving spec follow focus.
- **kitty accepts the move.** kitty 0.48.2 answered ok to the incremental
  `os-panel` call on a live panel, in about 20 ms.

Bugs found in real use the same night, each pinned down with a runtime probe
(installed through `hs -c`, logging to a file, gone at the next reload), as
"When a hyper chord does nothing" in `docs/hammerspoon.md` recommends:

- **The kitty panel came out at the wrong size.** Shown from the laptop after
  living on the monitor, it came out 1920×1056, the monitor's size, at the
  laptop's origin. kitty takes the size from the screen it lays the panel out
  on, so either it used outdated screen data or no move was sent: the move
  was skipped whenever a record said the panel was on that screen already,
  and the record went stale whenever anything else moved the panel. Now
  every show sends the move, and the panel is fitted to kitty's own layout
  frame when it is still off.
- **hyper+shift+; did nothing.** The key arrived with shift and hyper
  entered, and the handler ran without error, but the window was Thunderbird
  in native fullscreen, whose `AXPosition` is not settable. Fixed by leaving
  fullscreen, moving, and going fullscreen again, and by checking every move.
- **hyper+/ focused Brave's Picture-in-Picture window.** Fixed by the
  floating-window check above. Its first version skipped any window that
  called itself standard, and a Chromium PiP window does (AeroSpace's
  recorded Accessibility dumps, upstream), so it would never have fired; it
  now goes by the layer alone.
- **A hide returned to an app on the other monitor.** Fixed by the per-screen
  lists.
- **Maccy's popup opened on the laptop.** Fixed by keeping `popupScreen` on
  the active screen. The first write made macOS 14 ask whether Hammerspoon may
  access data from other apps, because Maccy's settings live in its sandbox
  container.

Still unmeasured:

- **Do the second-round fixes work on two screens?** They loaded cleanly and
  their read-only parts were exercised from the console (the window list in
  20 ms, a return target chosen in 12 ms), but the laptop panel was off by
  then, so the panel fit, a fullscreen move, a per-screen return across two
  screens, Maccy following focus, and a Picture-in-Picture redirect all wait
  for a real press with both screens on.
- **Does the panel take a fitted frame, and keep it?** It is a borderless
  window, which may refuse a new size over Accessibility; the console line
  says `could not fit` then. If kitty fights the fit, the panel has to be
  recreated on the new screen instead.
- **Brave's Picture-in-Picture window, measured here.** The layer-3 figure
  comes from Chromium's source and AeroSpace's dumps. No PiP video has been
  open since the probes were armed.
- **The new blackout restore.** The blackout that ended before the reload was
  restored by the old code, so the UUID rows, the last-good fallback and the
  floors (see `docs/external-display-brightness.md`) have been tested only
  against fake m1ddc and brightness binaries. One F1/F2 cycle would settle
  it.
- **The keys that move focus and the pointer**: hyper+; and hyper+shift+;,
  `cursorHide` and avy on the monitor, an app-mode overlay following focus.
  They were left for a real press, because driving them from a script takes
  over the screen.

Known gaps:

- **`screenshotAll`** (hyper+3 and hyper+shift+s, `core/mouse.lua`) runs
  `screencapture -c`.
  What that puts on the clipboard with two displays is unmeasured: measuring
  it overwrites the clipboard.
- **The avy grid overhangs its screen** by one cell on every side, so next to
  a second monitor the edge cells are drawn on the neighbour.
- **Window-mode kitty** still follows the pointer screen rather than
  `working`. Its hide also still hides kitty first and focuses the return
  target afterwards, so macOS picks an app for a moment in between; the app
  hotkeys focus the target first and hide once it has activated.
- **A summon from the other screen.** From B on the laptop, an app hotkey
  that brings up A on the monitor and a second press return to the monitor's
  previous app, not to B. That is the per-screen rule as asked for. Should it
  turn out wrong in practice, the shape for a choice is an enum knob (screen,
  summoner, global), not a boolean.
- **Duplicate names.** Two identical monitors share an `hs.screen:name()`.
  The registry does not care (it keys on UUID), but band labels would read the
  same, and kitty's `output-name` cannot tell them apart.
- **`screen_prefs`** carries only `role` so far. Per-screen defaults for the
  level keys, or a preferred screen per app, would go there too.
- **`modal-mode.lua`** still falls back to the primary screen when an
  indicator is drawn without one.
