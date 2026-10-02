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
- **kitty panel** shows on the screen named by `kitty_panel_screens`, default
  `working`. ("kitty: hyper+z".)

### Checking it from the console

```lua
hs.inspect(Screens.list())
Screens.target("working")[1]:name()
Screens.target("role:laptop")[1]:name()
```

## Follow-ups and open questions

Unmeasured, because the work was written while a blackout was running and
nothing could be reloaded or shown:

- **Does `newWithActiveScreen` report focus moving between screens?** Its
  documentation says it reports active-screen changes, but whether a click or
  hyper+; from one screen to the other fires it has not been checked. If it
  does not, overlays with a moving spec move only on their next show.
- **Does kitty move a live panel on macOS?** kitty's help says that on Wayland
  a panel's output is fixed at creation. If macOS behaves the same, the panel
  has to be recreated on the new screen, which means moving its tabs out and
  back.
- **Everything else in the live checklist**: each level key with focus on each
  screen, an F1/F2 blackout cycle, cursorHide and avy on the monitor, an
  app-mode overlay following focus.

Known gaps:

- **`screenshotAll`** (hyper+3 and hyper+shift+s, `core/mouse.lua`) runs
  `screencapture -c`.
  What that puts on the clipboard with two displays is unmeasured: measuring
  it overwrites the clipboard.
- **The avy grid overhangs its screen** by one cell on every side, so next to
  a second monitor the edge cells are drawn on the neighbour.
- **Window-mode kitty** still follows the pointer screen rather than
  `working`.
- **Duplicate names.** Two identical monitors share an `hs.screen:name()`.
  The registry does not care (it keys on UUID), but band labels would read the
  same, and kitty's `output-name` cannot tell them apart.
- **`screen_prefs`** carries only `role` so far. Per-screen defaults for the
  level keys, or a preferred screen per app, would go there too.
- **`modal-mode.lua`** still falls back to the primary screen when an
  indicator is drawn without one.
