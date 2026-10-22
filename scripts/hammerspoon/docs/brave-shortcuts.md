# Brave shortcuts

`hammerspoon/auto-load/brave-shortcuts.lua` installs browser hotkeys while Brave
(`com.brave.Browser`) is frontmost:

- **Cmd+.** opens Omni Prompt, combining open tabs, bookmarks, and browser history.
- **Cmd+/** opens a new tab through File > New Tab.
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

The parsed file is cached until its modification time, size, change time, or inode
changes. Metadata is checked whenever a variant using bookmarks opens. Any line whose first
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
bookmarks from the same private file as Cmd+, and recent browser history. Each
row identifies its source. Cmd+, is a bookmarks-only instance of the same chooser.
Tab subtitles show the position within their window, the window number, and URL
(for example, `Tab 3 · Window 1 · https://example.org/`).
Selecting a tab uses its stable ID, even if it moved to another window. A closed
tab reports an error instead of opening a replacement. Selecting a bookmark
uses the existing switch-or-open behavior, as does selecting history. Reopen
the chooser to refresh its sources. An unavailable source reports a warning
while other sources remain usable.

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

## Configurable Omni variants and history

Edit `$nightNotesPrivate/browser/omni-variants.json` (or
`$HOME/notes/private/browser/omni-variants.json`) and reload Hammerspoon. The JSON
array replaces the default variant list. Defaults preserve these two bindings:

```json
[
  {
    "id": "omni", "name": "Omni", "mods": ["cmd"], "key": ".",
    "sources": {"tabs": true, "bookmarks": true, "history": true},
    "magic": true, "queryActions": true
  },
  {
    "id": "bookmarks", "name": "Bookmarks", "mods": ["cmd"], "key": ",",
    "sources": {"tabs": false, "bookmarks": true, "history": false},
    "magic": false, "queryActions": true
  }
]
```

Add another object to bind another variant. For example, use `id: "history"`,
`name: "History"`, `mods: ["cmd", "alt"]`, `key: "h"`, and
`sources: {"history": true}` for history only on Cmd+Alt+H. Set both `magic` and
`queryActions` explicitly. Supported modifiers are `cmd`, `alt`, `ctrl`, `shift`,
and `fn`; keys are single printable ASCII characters available on the current
keyboard layout. IDs and chords must be
unique. Native browser action chords are reserved. Invalid configuration warns
and falls back to the entire default list.

Omitted sources are off. Disabled sources perform no reads. `magic` enables
prefix routing and `.help`; `queryActions` enables modified Enter and no-match
Google search. With both off, Enter only selects a result. Magics require query
actions. Turning all sources off with query actions on creates a query launcher.
All variants share literal AND matching, aliases, and switch-or-open behavior.
`BraveShortcuts.showOmni("history")` opens an installed variant by ID, and
`BraveShortcuts.omniVariants` contains the live chooser instances.

History uses the most recently used Brave profile from `Local State`, falling
back to `Default`. This can differ from the front window's profile when using
multiple profiles. It does not aggregate profiles or expose incognito visits.
Brave holds an exclusive SQLite lock, so direct read-only queries can fail.
The managed Go helper in `golang/omni-history` copies `History` and any rollback
or WAL journal into a private temporary directory, requiring unchanged source
file identities, nanosecond timestamps and sizes during copying (up to three
attempts). System SQLite recovers and integrity-checks the copy. The source is
never opened for writing, and the temporary snapshot is deleted after reading.
The 5,000 most recently visited non-hidden HTTP(S) URLs are indexed in memory,
newest first, including untitled entries under their URL. Older entries remain
available through Brave's full history search.

`omni-history-boot`, called from `launchers/various-darwin.zsh`, builds the local
Go tool with system SQLite and supervises it in a named tmux session. Run it once
when installing this feature without rebooting. The shared service survives
Hammerspoon reloads; stopping a chooser/client does not discard the index.
It checks database/journal metadata every two seconds and rebuilds a changed
index at most once per five seconds. Unchanged snapshots are skipped, with a
one-minute fallback refresh. Failed refreshes return an error rather than
serving another profile's old data. No history index is persisted.

The service listens on a random loopback port, authenticates each request with
Basic authentication with a mandatory password, rejects browser-origin requests, and does not log history or
queries. A random password is generated per service run unless
`OMNI_HISTORY_PASSWORD` is explicitly configured. Empty passwords are refused.
The endpoint/password are atomically published in
`~/.local/state/omni-history/server.json` (0600, parent 0700), outside the repo.
A singleton lock prevents duplicate owners. HS validates and rereads private
discovery metadata after replacement. HS sends literal AND queries, the cached host aliases, and URLs already
represented by tabs/bookmarks. Go uses precomputed searchable strings and returns
at most 200 unique matching rows. History never crosses the process stdout pipe.
Typing is debounced by 40 ms; generations and query serials discard stale replies.

Tabs and bookmarks appear immediately after the native tab snapshot. History
matches append asynchronously, preserving a highlighted local result. The chooser
transfers at most 200 matching rows to its native UI; all source entries remain
searchable by refining the query. Plain no-match Enter waits for an outstanding
history query before deciding between a result and Google search. Modified
Enter and magic actions remain immediate. A failed source leaves others usable,
and a three-second history-request timeout prevents indefinite waiting.

`core/chooser-search.lua` precomputes lowercase search strings at `setChoices()`.
All local chooser callers benefit; call `setChoices()` again after mutating rows.
Omni uses its optional `{limit=200}` constructor setting to bound native rows.
Parsed bookmarks and aliases are cached by file metadata and checked on opening.

Version 1.11.1 passes tests for source isolation, asynchronous Enter behavior,
stale-response cancellation, configuration, cache invalidation, indexed matching,
and bounded rows. Go tests cover actual exclusive locks, pending rollback/WAL
transactions, matching/aliases, duplicate suppression, authorization, and request
bounds. Race checks and `go vet` pass. On this host, a live 5,000-URL index loaded
in about 60 ms, local HTTP queries took about 1.2 ms median, and resident memory
was about 18 MiB. Ten warm Hammerspoon queries averaged 3 ms end to end
(maximum 8.5 ms), separate from the 40 ms typing debounce. The pure indexed
worst-case fixture search measured 0.13 ms.
Completed requests clear timer/task references so callbacks can be collected.
Timings depend on the machine and database. Existing native tab and query
submission checks remain passing.

## Browser chooser query actions

Tab toggles **New tab** and **REPLACE CURRENT TAB**. Replacement mode colors
chooser text orange. It targets the tab captured when Omni opened, even if the
active tab later changes; a closed target is refused. Existing matching tabs are
still reused. For combined magic destinations, only the first replaces the
captured tab; subsequent destinations open new background tabs. All destinations open up front
in one native request and their DOM readiness checks run concurrently. Opening
query destinations selects the last new tab inside Brave without activating the
application or raising its window over another app.

All dot magics can be combined with dots: `.g.l.c query` submits to Gemini,
Claude and ChatGPT; `.gs.s.gh query` opens Google Scholar, Semantic Scholar and
GitHub. Order is preserved, duplicate destinations open once, and any unknown
component leaves the whole input as ordinary text. Prefixes still must begin at
character one and be followed by an ASCII space.


Both Omni Prompt and Cmd+, search the title, displayed subtitle, URL, and hidden
hostname keywords using the shared literal AND matcher.

- Enter selects the highlighted result. With no matching rows, it searches
  Google for the entire query.
- Shift+Enter always searches Google, even when a result matches.
- Cmd+Enter opens the query in a new ChatGPT tab and submits it automatically.
- Alt+Enter opens the query in a new Gemini tab with Argon High and submits it.
- Ctrl+Enter opens the query in a new Claude tab and submits it.

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
remain hidden in the chooser. Alias parsing is cached by the same file metadata
as bookmarks. Reopening a chooser checks for edits, replacements, deletion, or
recreation. A missing file means no aliases; malformed JSON shows a warning.

Version 1.6.0 adds query actions and exact-host aliases. Focused tests cover
explicit URL matching, URI encoding, Enter variants, callback cleanup, fresh
query tabs, alias AND matching, and lookalike-domain rejection.

Live checks captured outgoing navigation locally: no-match Enter requested
Google, Shift+Enter did so with matching rows, and Cmd+Enter requested ChatGPT
with the complete query. These checks did not send test queries to either site.
The configured private hostname alias also passed an exact-host fixture check.

### Omni magic prefixes

Plain Enter supports these destinations, including when a local result is
highlighted:

- `? query`: Google.
- `.a query`: Amazon Germany search (`amazon.de`, added in 1.7.4).
- `.k query`: Keepa search on Amazon Germany (domain 3).
- `.c query`: a fresh ChatGPT conversation with automatic submission.
- `.g query`: the Gemini consumer app at `https://gemini.google.com/app`,
  selecting 4 Argon and High thinking before submitting.
- `.l query`: a fresh Claude conversation with automatic submission.
- `.s query`: Semantic Scholar.
- `.gs query`: Google Scholar.
- `.gh query`: GitHub repository search.
- `.y query`: YouTube.
- `.w query`: English Wikipedia.
- `.r query`: Google restricted to `site:reddit.com`.
- `.ni query`: Google restricted to `site:ninisite.com`.

Type exactly `.help` and press Enter to open a searchable magic cheatsheet.
Filter by prefix, destination name, or description using the usual literal AND
matching. Selecting a row reopens Omni with that prefix and a trailing space,
with the caret after the space, so typing appends the query; it does not
navigate or submit anything. Escape closes the
cheatsheet. The list and routing share one registry, so new entries appear in
help automatically. `.help` is an exact command and needs no trailing space;
leading whitespace or extra text keeps it a normal local query. Modified Enter
retains its Google/ChatGPT/Gemini/Claude behavior. Run `lua tests/omni-prompt.lua` for the
cheatsheet selection, filtering, and cancellation checks.

The token must begin at character one and be followed by one literal ASCII
space. `.g question` is magic; ` .g question`, bare `.g`, `.gquestion`, and a tab
immediately after `.g` are ordinary local searches. Tokens are case-sensitive.
Only the token and its first separator space are removed; remaining whitespace
and line breaks are preserved. An empty magic payload keeps the chooser open.
These prefixes apply only to Omni, not the bookmarks chooser. Shift+Enter,
Cmd+Enter, Alt+Enter, and Ctrl+Enter retain their explicit destinations and use
the full typed query, including any prefix. Additional mixed modifiers keep their native behavior.

Gemini ignores a plain prompt query parameter in the tested browser, so
`brave/gemini-submit.lua` fills its accessible composer directly. Version 1.9.0
uses the native picker label as a fast path when it already confirms Argon High,
skipping both menus. Otherwise, it verifies model and thinking selections through
the dependent menus. Focusing can open a web menu itself, so each dependency must
be observed before a Return event that could toggle the menu closed.

Fill and focus are issued together. A subsequent scan verifies the exact query,
focused composer, and enabled Send control before one Return, with no retry.
There is one native tab-identity check immediately after each read-only AX scan,
instead of two AppleScript launches per step. Pure state transitions run together;
newly revealed menus still yield to the event loop. Each queried AX reference
has a 0.1-second timeout. The WebArea must belong to the new Gemini `/app` page;
LuaSkin's structured AX URL values are handled explicitly.

An absent composer value means not ready, not an existing draft. Empty Unicode
formatting and exposed placeholders are allowed; real draft text is preserved.
Changing tabs or apps, unavailable Argon, or unconfirmed High cancels or times out
without sending. Readiness expires after 45 seconds. `lastResult` includes elapsed
time, check count, and whether the confirmed-picker fast path was used. This
uses native scripting and Accessibility, without page JavaScript or clipboard
changes.

No supported consumer-app model or thinking-level URL parameters were found.
[Google's model instructions](https://support.google.com/gemini/answer/18648722?hl=en)
describe selection through the picker. API thinking settings are not a documented
web-app deep-link contract, so the shortcut does not guess URL parameters.

Run `lua tests/browser-query-actions.lua` and `lua tests/gemini-submit.lua` for
prefix and modifier precedence, immediate and delayed readiness, model selection,
missing values, draft preservation, AX page identity, focus/tab cancellation, and
one-send checks. A live ready-page fast-path check completed submission in about
0.4 seconds with two native identity checks, and the owned tab became a Gemini
conversation. Cold page loading and model changes can take longer.

### Automatic Claude submission

Version 1.10.0 makes `.l query` submit the URL-prefilled prompt.
`brave/claude-submit.lua` waits for the new Claude `/new` page to expose the exact
query and an enabled native Send message button. It checks the active native tab
ID after each read-only AX scan. If the button supports `AXPress`, that action is
requested once. Chromium currently exposes only menu/scroll actions for it, so
the submitter focuses the Send button, confirms that focus and query again, and
activates the button with Space. It does not send Return to the composer.

Changing tabs/apps, a different query, missing controls, or unconfirmed button
focus prevents submission. Readiness expires after 45 seconds. Stop/reload
cancels timers and pending callbacks. A Send action is never retried, including
an ambiguous AX failure. No existing text is overwritten, no clipboard is used,
and page JavaScript is not required. `BraveShortcuts.claude.lastResult` reports
waiting, submitted, cancelled, timeout, or an unconfirmed action.

Run `lua tests/claude-submit.lua` for delayed prefill, disabled Send, delayed
focus, changed tab/app/query, wrong origin, reload, and one-send checks. A native
live test confirmed that activating the button created a new Claude conversation.

### Native ChatGPT fallback

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

### Community evidence for URL submission (2026-10-21)

URL auto-submit existed historically. [Tenable's July 2025 disclosure](https://www.tenable.com/security/research/tra-2025-22)
records automatic submission through `q=` and OpenAI reporting protections based
on `Sec-Fetch-Site` on July 11, 2025. This establishes a security-related change,
not a complete specification of current behavior for every launch context.

More recent community tools report draft-only behavior for both `q=` and
`prompt=` and add their own Send-button automation:
[January 2026 userscript](https://greasyfork.org/en/scripts/564702-chatgpt-auto-submit-query)
and [September 2026 userscript](https://greasyfork.org/en/scripts/592577-chatgpt-url-prompt-auto-submit).
[Community reports](https://www.reddit.com/r/ClaudeAI/comments/1kvuz7u/any_way_to_autosubmit_queries_via_url_parameters/)
also describe previously working auto-submit becoming prefill-only. URL options
such as `#autoSubmit=1` in [ChatGPT-URL-Extension](https://github.com/GrayXu/ChatGPT-URL-Extension)
are implemented by that extension, not evidence of a native ChatGPT switch.
No dependable current URL-only submission parameter was established by this search.

Brave's installed scripting dictionary exposes tab URL navigation but no request
header argument, so the current Hammerspoon/JXA route cannot override fetch
metadata. [Browser-generated Sec-Fetch-Site](https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Sec-Fetch-Site)
is forbidden to ordinary page JavaScript; `none` represents user-originated
navigation. Privileged extensions can modify request headers through
[declarativeNetRequest](https://developer.chrome.com/docs/extensions/reference/api/declarativeNetRequest),
and [Requestly documents this specific override](https://requestly.com/use-case/header/sec-fetch-site/).
An extension override is technically feasible, but neither the required current
ChatGPT value nor whether that change alone restores auto-submit was verified.
The working Hammerspoon send step remains in place.

The installed `chrome-cli` likewise has no custom-header navigation option.
Its [upstream documentation](https://github.com/prasmussen/chrome-cli)
identifies Scripting Bridge as its transport. `execute` can run page JavaScript
when Allow JavaScript from Apple Events is enabled, permitting DOM-based
submission, but page JavaScript still cannot override `Sec-` headers.
The optional DOM submitter described below now uses that existing JXA execution
command. It needs no additional chrome-cli subprocess. Background-tab suspension
and changing site selectors remain limits of this route.
A separate
[chrome-devtools-cli](https://github.com/aeroxy/chrome-devtools-cli) uses CDP
and documents `--extra-headers` for navigation; whether a specific fetch metadata
override restores ChatGPT auto-submit still needs verification.

## Adaptive layout and recent selections

Browser Omni, bookmarks and the magic cheatsheet use 75% of the current display's
usable width, capped at 1800 logical pixels, recalculated on each opening.
Ctrl+Shift+Enter searches the full query using Google Web (`udm=14`), excluding
AI Overviews through Google's Web filter. Shift+Enter retains regular Google,
and Ctrl+Enter retains Claude.

Hyper+Shift+Z opens [OmniPrompter](omni-prompter.md), with cached prompt-function
discovery, ordered pipelines and guarded copy/paste. Hyper+A emoji choices now
put recent insertions first. Both use the shared in-memory recency helper backed
by Hammerspoon preferences, with one load per namespace and saves on selection.

## Optional background DOM submission

With the owner's JavaScript-from-AppleScript setting enabled, chat submissions
first use a stable-tab-ID DOM route. Its first real execution detects support;
there is no separate probe or subprocess. Concurrent initial checks are coalesced. An explicit disabled result is cached
for 72 hours in memory, cleared immediately on Hammerspoon reload.
`BraveShortcuts.tabs.dom.lastCheckMs` reports the most recent execution latency.
Other errors and timeouts stop instead of falling back after an uncertain send.

The DOM route validates origin, supported route, exact composer text and an
enabled Send button. ChatGPT conversation routes are allowed only for an exact
prefilled draft; an empty conversation never receives inserted text. A matching
latest user message plus an empty composer is recognized as already submitted. It never replaces a different draft. It records its nonce before
clicking, so a repeated readiness call cannot click twice. It can work when the
active tab or application changes and never activates the browser application. Closing
or navigating the target away cancels it.

Gemini's DOM route clicks the observed model/effort menu items and verifies that
the picker reports Argon High before filling. Already-correct tabs skip those
menus. A model-menu action is issued once and allowed three seconds to settle;
unconfirmed selection yields to the guarded native fallback before composer
insertion. Native fallbacks run sequentially and require the original browser/active-tab ownership
chain. Changing focus cancels native submission; key events and AX traversal do
not provide a reliable background replacement for the DOM route. Native paths
remain available when JavaScript execution is disabled.

Version 1.14.0 adds concurrent native background opening and tab-targeted DOM
submission. Focused tests cover generic prefix composition, replacement identity,
coalesced support detection, negative-cache expiry/reload, model/effort transitions,
no duplicate Send, navigation, and native fallback ownership. Live individual
DOM sends succeeded for ChatGPT, Claude and Gemini without changing the frontmost
app; Gemini selected Argon High from Flash/Low in about 2.4 seconds. A short native
multi-open check preserved both NSWorkspace frontmost PID and selected tab.
The longer concurrent live test was interrupted by the owner closing fixture
tabs; it is not evidence about focus preservation across user activity.

Version 1.14.1 clarifies tab focus: query opening selects the last destination
inside Brave while leaving the foreground application alone. ChatGPT's DOM route
accepts an exact prefilled draft on an allocated conversation URL, and recognizes
an already-sent latest message when the composer is empty. A historical matching
message cannot suppress sending a new exact draft. Generic failure reasons are
retained in submission diagnostics without storing prompt text.
