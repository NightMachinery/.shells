# Multiple monitors

How this Hammerspoon config decides which screen something happens on, why it
used to get that wrong, and what is still unmeasured. The per-feature details
live next to each feature in `docs/hammerspoon.md`; this file is the map.

## Terms

These are used throughout, here and in `core/screens.lua`:

- **Active screen**: the screen of the focused window,
  `Screens.focusedScreen()`, read from CoreGraphics' window list. Not
  `hs.screen.mainScreen()`, which falls behind focus (see "Where focus is").
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

- In Hammerspoon, `hs.screen.mainScreen()` is meant to be the **active**
  screen (though it lags; see "Where focus is").
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
  moved or resized), `added` (with the record), `removed` (with the UUID),
  `active` (the active screen changed) and `focus` (with the frontmost app
  and its screen, once focus has settled after any switch). Layout changes
  come from `hs.screen.watcher`; the other two from the focus check in
  "Where focus is". `ModalMode.onScreenChange(fn)` is kept as the old name
  for `Screens.on("layout", fn)`.
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
  grid moves it to the next screen. `cursorHide` also passes over the top of
  every other screen first, since a menu bar stuck down is put away only by
  the pointer passing over its own screen's top.
- **Focus keys**: hyper+; focuses the frontmost window on the next screen,
  and hyper+shift+; moves the focused window there. Both bring the pointer
  along. ("Moving between screens".)
- **kitty panel** shows on the screen named by `kitty_panel_screens`. Every
  show asks kitty for a fresh layout on that screen, and fits the panel when
  it is still off; hyper+shift+; moves the shown panel through kitty, not
  over Accessibility. ("kitty: hyper+z".)
- **Return after a hide**: the second press of an app hotkey, and hiding
  kitty, return to the previous app on the same screen
  (`screenReturnTarget`), not the newest app anywhere. The enum
  `hide_return_policy` picks the rule: `screen` (that), or `summoner`, the
  app you were in just before, on whichever screen. ("App hotkeys".)
- **Floating windows**: after any activation this config causes (an app
  hotkey, a hide's return, kitty's return), a focused window above layer 0,
  such as a Picture-in-Picture video, hands focus to the app's front normal
  window (`appFocusOffFloating`). ("App hotkeys".)
- **Maccy's hyper+v popup** opens on the screen named by
  `maccy_popup_screens`: its `popupScreen` setting is rewritten when that
  screen changes, one write at a time. ("App hotkeys".)
- **Screenshots**: hyper+3 copies the screen named by `screenshot_screens`
  (the active one by default) and hyper+shift+3 every screen, through
  `screencapture -R` on the screens' frames. A picture of one screen comes
  out at its full resolution (the monitor gave 3840×2160 for its 1920×1080
  points). One across both screens comes out at one pixel per point
  (3390×1080), with an empty strip under the shorter laptop screen: one
  clipboard image has to span them. (Measured 2026-10-03, to a file.)

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

### Where focus is

`hs.screen.mainScreen()` is meant to answer the active screen, and everything
that followed focus used to ask it. It falls behind. Measured 2026-10-03,
with the load average near 46: after hyper+/ brought Brave's fullscreen
window forward on the monitor, it still answered the laptop 0.15, 0.5 and
1.2 s later, while CoreGraphics' window list and Accessibility (Brave's
focused window) both had the monitor at 0.15 s. Once focus went back to
kitty on the laptop, it answered the monitor, so it was one change behind
rather than slow, and waiting did not help: hyper+; kept picking the same
screen however long you waited. After the reload, with kitty focused on the
laptop, it still answered the monitor. Whether it lags on an idle machine is
unmeasured.

That one cause was behind four bugs reported the same day:

- hyper+z opened the kitty panel on the screen focus had just left;
- Maccy's popup stayed on the laptop, because `popupScreen` was written for
  the screen `mainScreen` named;
- hyper+; kept going to the monitor;
- a hide returned to the wrong app. Brave, brought forward on the monitor,
  was filed in the laptop's list, so hyper+x, hyper+/, hyper+l, hyper+l
  (Emacs and Telegram on the laptop) returned to Brave instead of Emacs.

So the active screen is now `Screens.focusedScreen()`: the screen of the
frontmost app's front normal window in the window list, else of its front
window floating below the menu bar (the kitty panel), else `mainScreen` as a
last resort. The frontmost app comes from NSWorkspace, which was right in
the same measurement. A hide takes the screen of the hidden app's own
window, and kitty's the screen of its panel or window.

The trade-offs:

- For: it agrees with what is on screen, and it asks no app anything.
- Against: a window-list read costs 19 to 40 ms where `mainScreen` cost
  nothing. So the answer is kept while the same app stays frontmost, for at
  most `kFocusTrustSeconds`, and read afresh `kFocusSettleSeconds` after
  every activation, every active-screen change `hs.screen.watcher` reports,
  and every focus change hyper+; and hyper+shift+; make. That read emits
  `focus` and, when the screen changed, `active`; the per-screen app lists
  are filed from `focus`.
- Against: a click that moves focus between two windows of one app on two
  screens activates nothing. It is seen when `hs.screen.watcher` reports it,
  which may be late for the same reason as `mainScreen` (unmeasured), or when
  the kept answer expires.
- Rejected: asking Accessibility for the frontmost app's focused window. It
  is exact, but it is a query to that app on every read, and a hung app
  holds Hammerspoon's one Lua thread, and so every hyper key, until the
  query times out.

### Checking it from the console

```lua
hs.inspect(Screens.list())
Screens.target("working")[1]:name()
Screens.target("role:laptop")[1]:name()
Screens.focusedScreen():name(), hs.screen.mainScreen():name()
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

Bugs found in real use the same night. The first two were pinned down with a
runtime probe (installed through `hs -c`, logging to a file, gone at the next
reload), as "When a hyper chord does nothing" in `docs/hammerspoon.md`
recommends; the other three were diagnosed from the symptom, and Maccy's from
its upstream source. Each is addressed, but no fix has had a real press with
both screens on yet (see "Still unmeasured" below):

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
  in native fullscreen, whose `AXPosition` is not settable. Addressed by
  leaving fullscreen, moving, and going fullscreen again (where it was, when
  the move fails), and by checking every move.
- **hyper+/ focused Brave's Picture-in-Picture window.** Addressed by the
  floating-window check above. Its first version skipped any window that
  called itself standard, and a Chromium PiP window does (AeroSpace's
  recorded Accessibility dumps, upstream), so it would never have fired; it
  now goes by the layer alone.
- **A hide returned to an app on the other monitor.** Addressed by the
  per-screen lists.
- **Maccy's popup opened on the laptop.** Addressed by keeping `popupScreen`
  on the screen named by `maccy_popup_screens`. Why Maccy's own choice was the
  laptop is unmeasured. The first write made macOS 14 ask whether Hammerspoon may
  access data from other apps, because Maccy's settings live in its sandbox
  container. In real use since the focus fix below, the popup follows focus
  between the screens.

Found in real use on 2026-10-03, after the round above:

- **The `mainScreen` lag** behind hyper+z, Maccy, hyper+; and the hide
  return; see "Where focus is".
- **hyper+shift+; left a still picture of the window behind.** After a
  fullscreen Brave window was moved, a Brave window the exact size of the
  screen it had left stayed there, showing the page as it had been. It was
  reproduced with a scratch Brave window and read through `hs.window.list`
  every few tenths of a second. While macOS animates a window out of or
  into fullscreen, the window itself is not in the on-screen list, and the
  app shows a stand-in, a full-screen-sized layer-0 window, on each screen
  involved. The old wait (fullscreen state and frame steady for one poll)
  ended while both stand-ins were still up, the move cut the animation
  short, and the stand-in on the screen left behind leaked. It outlived the
  scratch window, and the two seen were still up 5 and 20 minutes later. It
  is an `AXUnknown` window with no close button and no action but
  `AXRaise`, and macOS lets only the owning app close a window, so nothing
  outside Brave can destroy it; quitting Brave does. Its position can be
  set, though, and a position outside every screen hides it. Now the wait
  also needs the window back in the list and no stand-in newer than the
  move left (window ids grow over time, so "newer" means a higher id than
  any listed before the move; a list of the app's own windows would miss a
  leftover in a Space not showing). Any leftover that still appears is
  moved out of sight a second after the move, and
  `Screens.parkStandIns("com.brave.Browser")` does the same from the
  console for the Spaces showing. Re-run on the new code, the wait ended
  1.9 s after the press and nothing leaked. Every move's console line now
  ends with the milliseconds from the press to each step.
- **hyper+shift+; on a fullscreen window is slow.** It is two macOS
  animations, out of fullscreen and back in. A research pass found no
  setting that shortens them on 14.3.1: Reduce Motion is on already, and the
  Dock keys people quote (`expose-animation-duration`,
  `workspaces-swoosh-animation-off`) are not in this build's binaries at
  all. The only way around both animations is to move the fullscreen Space
  itself to the other display, as a Mission Control drag does, which needs
  the Dock's private calls (yabai's scripting addition, with SIP partly
  off). Untested; no newer macOS adds a public way either.
- **hyper+z showed nothing, however often it was pressed.** The kitty panel
  had been lost in a Space that was not showing; see "Which Space the panel
  joins" in `docs/hammerspoon.md` for the mechanism, measured with scripted
  moves between the laptop and Brave fullscreen on the monitor, and the
  fix. With the fix, a show from fullscreen Brave, a move onto the
  fullscreen monitor and a recovery from a lost panel all ended with the
  panel on screen; two of them needed the repair show.
- **hyper+x, hyper+l, hyper+l returned to kitty, not Emacs.** Emacs was
  filed under the laptop correctly, but it was fullscreen in its own Space,
  so once Telegram's Space showed, its window was not in the on-screen list,
  and an app without a window there was taken on its record only when the
  app being hidden was fullscreen. Now the app's own focused window, asked
  over Accessibility (1.4 ms for Emacs), counts too; the same sequence,
  scripted, returned to Emacs.
- **Focus moving to the laptop took kitty off the monitor.** hyper+/, hyper+z,
  hyper+x left the panel hidden, because the panel was hidden on every app
  activation, wherever that app was. Now only an activation on the panel's
  own screen hides it, and hyper+; can focus the panel. Scripted: the panel
  stayed up on the monitor while Emacs was activated on the laptop, hyper+;
  then landed in kitty, and activating Brave on the monitor hid it.

Still unmeasured:

- **What is still untested on two screens.** Since the second round, real
  presses and scripted runs have covered the panel fit (the panel takes the
  new frame, read back afterwards), hyper+shift+; on windowed and fullscreen
  windows, per-screen returns across both screens, hyper+; and Maccy
  following focus. A fullscreen move that fails and a Picture-in-Picture
  redirect still wait for one.
- **Does kitty keep a fitted frame?** The panel takes one, but whether kitty
  puts it back at its next re-layout is unobserved.
- **Brave's Picture-in-Picture window, measured here.** The layer-3 figure
  comes from Chromium's source and AeroSpace's dumps. No PiP video has been
  open since the probes were armed.
- **What layer an app-modal dialog reads at.** The floating-window check
  leaves alone windows at or above the modal-panel level, and any window whose
  subrole says it is a dialog, on the strength of AppKit's constants. No real
  dialog's layer has been read right after its app was activated.
- **The new blackout restore.** The blackout that ended before the reload was
  restored by the old code, so the UUID rows, the last-good fallback and the
  floors (see `docs/external-display-brightness.md`) have been tested only
  against fake m1ddc and brightness binaries. One F1/F2 cycle would settle
  it.
- **The pointer keys**: `cursorHide` and avy on the monitor, and an
  app-mode overlay following focus, have not been pressed for real.

Known gaps:

- **The avy grid overhangs its screen** by one cell on every side, so next to
  a second monitor the edge cells are drawn on the neighbour.
- **Window-mode kitty** still follows the pointer screen rather than
  `working`. Its hide also still hides kitty first and focuses the return
  target afterwards, so macOS picks an app for a moment in between; the app
  hotkeys focus the target first and hide once it has activated.
- **A summon from the other screen.** From B on the laptop, an app hotkey
  that brings up A on the monitor and a second press return to the monitor's
  previous app, not to B. That is the per-screen rule, the default.
  `hide_return_policy = "summoner"` returns to B instead, for every hide.
  Choosing per press (by whether the hidden app was summoned from another
  screen, say) is not built.
- **Duplicate names.** Two identical monitors share an `hs.screen:name()`.
  The registry does not care (it keys on UUID), but band labels would read the
  same, and kitty's `output-name` cannot tell them apart.
- **`screen_prefs`** carries only `role` so far. Per-screen defaults for the
  level keys, or a preferred screen per app, would go there too.
- **`modal-mode.lua`** still falls back to the primary screen when an
  indicator is drawn without one.
