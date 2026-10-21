# Brave shortcuts

`hammerspoon/auto-load/brave-shortcuts.lua` installs two hotkeys while Brave
(`com.brave.Browser`) is frontmost:

- **Cmd+.** opens Brave's native Tab Search.
- **Cmd+/** opens a new tab through File > New Tab.

The existing auto-load scan loads the module after the core configuration.
Other apps retain their normal shortcuts. Extra modifiers are not bound, and
holding a key does not repeat the action.
Pressing Cmd+. while its native search input is already focused leaves it open.

These hotkeys consume the original chord and activate native browser controls,
without forwarding Cmd+Shift+A or Cmd+T. Webpage keyboard handlers therefore
cannot cancel either action. Each action rechecks that Brave is still in front
after leaving the hotkey callback.

Tab Search uses an accessibility `AXPress` on the native "Tab search" popup
button. The lookup skips webpage accessibility trees and tab contents, caches
the button for its native window, and limits traversal to 250 elements, 14
levels, and a 0.5-second search budget. Accessibility calls have a 0.15-second
timeout. Chromium focus proxies are resolved through their `AXWindow`.

If a control is unavailable, the module shows a warning instead of sending a
fallback keystroke. The current labels and File > New Tab path expect an
English Brave UI. Tab Search needs its native button to be exposed, such as in
a regular browser window with the tab strip visible. An unresponsive browser,
unavailable native control, or failed OS hotkey registration can still prevent
the action; bypassing page handlers does not make those controls always exist.

Console helpers: `BraveShortcuts.newTab()`, `BraveShortcuts.tabSearch()`,
`BraveShortcuts.sync()`, and `BraveShortcuts.stop()`. The actions return false
outside Brave. `BraveShortcuts.lastError` records the latest failure.
Re-running the file stops its previous watcher, hotkeys, and pending timer
before replacing them.

## Verification

Version 1.0.2 passed Lua syntax validation and live Hammerspoon checks in a
temporary Brave window. A page with canceling keyboard handlers received
neither shortcut: Cmd+. focused the native Tab Search combo box, and Cmd+/
created a new tab with the page textarea focused. Repeated Cmd+. kept the
search open without a warning. Re-running the module replaced its previous
bindings and watcher. App scoping and loading through the full configuration
were checked after restoring the previous app.
