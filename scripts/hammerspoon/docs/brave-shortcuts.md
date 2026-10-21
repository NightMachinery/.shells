# Brave shortcuts

`hammerspoon/auto-load/brave-shortcuts.lua` installs browser hotkeys while Brave
(`com.brave.Browser`) is frontmost:

- **Cmd+.** opens a new tab through File > New Tab.
- **Cmd+/** opens Omni Prompt, combining open tabs and bookmarks.
- **Cmd+Shift+/** opens Brave's native Tab Search.
- **Cmd+N / Cmd+M** move older/newer through tab recency in the current window.
- **Cmd+Up / Cmd+Down** select the previous/next tab in tab-strip order.
- **Cmd+Left / Cmd+Right** navigate page history through History > Back/Forward.
- **Cmd+,** opens the titled-bookmark chooser, replacing Brave Preferences.
- **Cmd+B** presses Bitwarden’s native extension action.

The existing auto-load scan loads the module after the core configuration.
Other apps retain their normal shortcuts. Other Shift/Option/Ctrl variants are not bound, and
holding a key does not repeat the action.
Pressing Cmd+Shift+/ while its native search input is already focused leaves it open.

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
neither shortcut under the original mapping: Cmd+. focused the native Tab Search
combo box, and Cmd+/ created a new tab with the page textarea focused. Repeated Cmd+. kept the
search open without a warning. Re-running the module replaced its previous
bindings and watcher. App scoping and loading through the full configuration
were checked after restoring the previous app.

Version 1.4.0 passed live checks of the swapped New Tab and Tab Search chords,
and reuse of redirected Gmail account tabs. Gmail matching checks cover encoded
email addresses, different accounts, malformed parameters and lookalike origins.

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

### Gmail accounts

Gmail account bookmarks can use
`https://mail.google.com/mail/?authuser=ADDRESS#inbox`, with the email address
URL-encoded in place of `ADDRESS`. This preserves the account identity when
Google changes session numbers. Gmail redirects to `/mail/u/N/`; the chooser
recognizes that account from Gmail's own tab title and reuses it across windows.
Only the exact HTTPS Gmail origin and mail path participate in this matching.
An existing tab for that account is selected even if it shows another folder.
Other websites retain exact URL matching.

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

Version 1.3.2 swaps the two browser actions: Cmd+. creates a tab and Cmd+/
opens native Tab Search. The action implementations stay the same.

## Omni Prompt and local matching

Omni Prompt snapshots tabs across all Brave windows when opened, then appends
bookmarks from the same private file as Cmd+,. Each row identifies its source.
Selecting a tab uses its stable ID, even if it moved to another window. A closed
tab reports an error instead of opening a replacement. Selecting a bookmark
uses the existing switch-or-open behavior. Reopen the chooser to refresh tabs.
An unavailable source reports a warning while the other source remains usable.

Both choosers use case-insensitive literal substring matching: split the query
on whitespace and require every term, in any order, across the title and URL.
For example, `inbox example.org` requires both terms. Punctuation such as `.`,
`+`, `[` and `%` is literal. There is no fuzzy matching, regex syntax, quoting,
negation, or OR syntax.

`core/chooser-search.lua` supplies the default constructor for local lists:
`ChooserSearch.new(completion)` and `ChooserSearch.setChoices(chooser, rows)`.
It also handles emoji and Wi-Fi selection. History, tag lookup, and autocomplete
retain their query-provider callbacks. New local choosers should use this
constructor; the native Hammerspoon API is left intact for custom providers.

Version 1.5.0 passed matching and native bridge regression tests, Lua/JavaScript
syntax checks, and live hotkey checks. Typed queries matched terms across
fields, absent terms cleared rows, choosing a tab selected its stable ID without
creating a tab, and Cmd+Shift+/ focused native Tab Search. The tab bridge test
also covers a tab moved between windows and a tab closed after listing.

## Browser chooser query actions

Both Omni Prompt and Cmd+, search the title, displayed subtitle, URL, and hidden
hostname keywords using the shared literal AND matcher.

- Enter selects the highlighted result. With no matching rows, it searches
  Google for the entire query.
- Shift+Enter always searches Google, even when a result matches.
- Cmd+Enter opens the query in a new ChatGPT tab and submits it automatically.

Query actions always create a fresh Brave tab. Empty or whitespace-only queries
perform no navigation. Query text is encoded as UTF-8 URI-component bytes, so
spaces, `&`, `+`, and non-ASCII text stay part of the query. Modified Return is
intercepted only while its chooser is visible, including keypad Enter. Other
choosers retain their own Return actions. Query text is never sent as a shell
command. The ChatGPT browser query link depends on the site's current behavior;
it is separate from the documented desktop app deep links.

Exact-host aliases live in `$nightNotesPrivate/browser/search-aliases.json`
(with the same private-notes fallback as bookmarks). The JSON object maps
lowercase hostnames to keyword lists, for example:

```json
{"chat.example.org": ["Team chat"]}
```

A query such as `team channel` can match a channel URL on that host even when
its title does not name the service. Ports and host case are normalized; paths,
lookalike domains, and unrelated subdomains do not acquire the alias. Keywords
remain hidden in the chooser. Reopen either chooser to reload the file. A
missing file means no aliases; malformed JSON shows a warning.

Version 1.6.0 adds query actions and exact-host aliases. Focused tests cover
explicit URL matching, URI encoding, Enter variants, callback cleanup, fresh
query tabs, alias AND matching, and lookalike-domain rejection.

Live checks captured outgoing navigation locally: no-match Enter requested
Google, Shift+Enter did so with matching rows, and Cmd+Enter requested ChatGPT
with the complete query. These checks did not send test queries to either site.
The configured private hostname alias also passed an exact-host fixture check.

### Automatic ChatGPT submission

Version 1.6.1 fixes the draft-only ChatGPT query link. After creating the fresh
tab, `brave/chatgpt-submit.lua` waits for its composer to contain the exact
query and an enabled Send control. It focuses that composer and posts Return
once. Native tab identity, the ChatGPT new-chat URL, and Brave focus are checked
before sending. Changing the active tab or app cancels submission. A query that
does not become ready within 45 seconds reports a warning. A delivered Return
is never retried.

This uses Brave's native tab scripting and Accessibility, so no userscript
update is required. `BraveShortcuts.chatgpt.lastResult` reports waiting,
submitted, cancelled, or timeout. `lua tests/chatgpt-submit.lua` covers delayed
readiness, exact text, disabled Send, changed focus/tab, wrong origin/path,
reload cancellation, and one-send behavior. A live test verified that the
prompt left the composer and became an actual ChatGPT conversation.
