# Brave shortcuts

`hammerspoon/auto-load/brave-shortcuts.lua` installs browser hotkeys while Brave
(`com.brave.Browser`) is frontmost:

- **Cmd+.** opens Brave's native Tab Search.
- **Cmd+/** opens a new tab through File > New Tab.
- **Cmd+N / Cmd+M** move older/newer through tab recency in the current window.
- **Cmd+Up / Cmd+Down** select the previous/next tab in tab-strip order.
- **Cmd+Left / Cmd+Right** navigate page history through History > Back/Forward.
- **Cmd+,** opens the titled-bookmark chooser, replacing Brave Preferences.
- **Cmd+B** presses Bitwarden’s native extension action.

The existing auto-load scan loads the module after the core configuration.
Other apps retain their normal shortcuts. Shift/Option/Ctrl variants are not bound, and
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

## Tab navigation

Enable **Cycle through the most recently used tabs with Ctrl-Tab** in
`brave://settings/braveContent`. Cmd+N calls Tab > Select Next Tab (older in
recency); Cmd+M calls Select Previous Tab (newer). Brave owns the actual MRU
history, including visits before Hammerspoon loaded. Hold Cmd to traverse the
list; release it to commit. Separate Cmd+N presses toggle the last two tabs.
These bindings replace New Window and Minimize while Brave is frontmost.

Brave ends its MRU cycle on Ctrl release. A small adapter posts only that
modifier-release event when Cmd is released, preserving physical flags and
never releasing a physically held Ctrl. Switching apps also ends Brave's cycle.
Turning the browser's recency setting off makes these native menu commands
follow tab-strip order instead.

Cmd+Up/Down select native tab indices directly, wrapping at each end, so their
strip order is independent of the MRU setting. `brave/tabs.lua` serializes
asynchronous `osascript` calls to `brave/tabs.js`, with a four-second timeout
and no automatic retry. This requires Automation permission for Hammerspoon
to control Brave, but does not require JavaScript from Apple Events.

The menu path is from Brave 1.97.56. The relevant native behavior is in
[BraveTabStripModel::SelectRelativeTab](https://github.com/brave/brave-core/blob/v1.97.56/browser/ui/tabs/brave_tab_strip_model.cc)
and the Ctrl-release handling in
[TabCyclingEventHandler](https://github.com/brave/brave-core/blob/v1.97.56/browser/ui/views/frame/brave_browser_view.cc).

Page history bindings invoke native menu actions; they do not send navigation
keys to page handlers. An unavailable Back/Forward at a history boundary is a
normal no-op. Like macOS navigation shortcuts, these replace text-field
start/end-of-line behavior while Brave is frontmost.

Arrow chords include the Fn flag because macOS marks arrow events as function
keys. Registering the same arrow twice with/without Fn interfered with Carbon
hotkey registration in live tests, so each arrow has one binding.

Version 1.1.4 passed Lua/JavaScript syntax checks and live tests in an isolated
three-tab Brave window: native MRU differed from strip order, holding Cmd
traversed and reversed the list, separate completed Cmd+N presses toggled the
last two tabs, Up/Down wrapped in strip order, and native Back/Forward moved
between controlled history entries. The asynchronous strip task was checked
after completion. App scoping remained disabled outside Brave.

## Bookmark chooser

Bookmarks live in `$nightNotesPrivate/browser/bookmarks.org`, falling back to
`$HOME/notes/private/browser/bookmarks.org`. Use one titled Org link per line:

```org
- [[https://example.org/][Example]]
# [[https://example.org/paused][Commented out]]
```

The file is re-read whenever Cmd+, opens the chooser. Any line whose first
non-whitespace character is `#` is ignored, as are `#+BEGIN_COMMENT` blocks.
Only titled HTTP(S) links appear. Empty titles are ignored, surrounding title
whitespace is trimmed, and identical URL/title pairs are deduplicated. Search
matches both the displayed title and URL.

Selecting an entry scans native tabs across Brave windows. A matching tab is
selected and its window raised; otherwise a new tab opens in the front Brave
window (or a new window if none exists). Matching normalizes the scheme,
host case, default port and empty root path. Other paths, trailing path slashes,
queries and fragments stay distinct. This avoids substring matches activating
an unrelated page. Bookmark data stays in the private notes file; URLs travel
to the scripting bridge as JSON on stdin, with no shell interpolation.

Version 1.2.0 passed parser cases for commented lines, indented comments,
comment blocks, duplicate entries and non-HTTP links. Live tests checked the
seeded chooser entry, selecting a fixture in another window without adding a
tab, opening one missing fixture, and reselecting it without adding another tab.
The private bookmark itself was not opened during testing.

## Native Bitwarden action

Cmd+B presses the native Bitwarden toolbar button. If the button is not exposed,
it opens the native Extensions menu and selects Bitwarden there. Pin/Unpin
controls and webpage accessibility trees are excluded from the search. An
already-open extension popup or side panel stays open. The action never sends
Cmd+B to the browser. If no native action is available, it shows a warning.

Bitwarden must be enabled and available in the current browser window. Pinning
its action avoids the menu step. `BraveShortcuts.bitwarden.open()` runs the
lookup, and `.lastResult` records toolbar/menu/already-open success.

The selector tests cover native toolbar and menu actions, page imitation
rejection, Pin/Unpin rejection, and already-open extension URL detection.
