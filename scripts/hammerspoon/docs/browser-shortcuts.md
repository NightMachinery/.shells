# Shared browser shortcuts

The auto-loaded browser controller applies the same Omni query actions to Brave,
Chrome, Edge, Safari, Arc, Dia and Firefox. Chords are enabled only while a
registered browser is frontmost. Other applications keep their own bindings.
`BraveShortcuts` remains the Brave console alias; `BrowserShortcuts` owns the
per-browser controllers. Existing private Omni variant settings apply to every
browser. See [Omni configuration and magics](brave-shortcuts.md).

## Chords and chooser behavior

- Cmd+. opens Omni, Cmd+, opens the bookmark variant, Cmd+/ creates a native tab.
- Cmd+Shift+/ opens native tab search where exposed; Safari uses Tab Overview,
  Firefox uses the native tab-list button. Missing native controls report an
  error; Omni remains available. Arc/Dia depend on their native menu/toolbar UI.
- Cmd+N/Cmd+M cycle older/newer tabs by recency. Brave retains its native
  controller. Others learn selections from Accessibility notifications and keep
  a frozen cycle until Cmd is released. Their MRU is in memory for this HS run;
  tabs not yet observed fall back to strip order. Cycles stay within one window.
- Cmd+Up/Down move through the tab strip; Cmd+Left/Right use native page history.
- Cmd+B presses Bitwarden's native toolbar/extension control if installed and
  exposed. It never forwards the conflicting browser/page chord.
- Alt+P/Alt+N recall accepted chooser queries. Query history is separate from
  browsing history and result recency. Moving past the newest entry restores
  unfinished input. Recalled Unicode text has a caret at its end.
- Every fresh Omni opening resets Tab's mode to New tab. Tab toggles this session
  to the short `↺ Replace` label. Help-prefix continuation retains its session.
  Only the input turns orange; result titles, subtitles and shortcut hints keep
  their normal colors. An orange badge or border remains an alternative, not a
  selected replacement for the current indicator.

Modified Enter, strict leading magics, combined destinations, literal whitespace
AND matching, aliases, recent bookmarks and adaptive 75% width are shared.
Shift+Enter uses Google, Cmd+Enter ChatGPT, Alt+Enter Gemini Argon High,
Ctrl+Enter Claude, and Ctrl+Shift+Enter Google Web. Tab replacement options are
captured before deferred submission.

## Native adapters and safety limits

Chrome/Edge share Chromium's scripting dictionary and stable tab IDs with Brave.
Arc has read-only `activeTab` plus `select()`; Dia uses `focus()`. Their selection
commands are refused after the browser loses foreground focus. Have an existing
Arc/Dia window open before using scripted query destinations. Browser settings
and extension availability remain browser/profile-specific.

Safari exposes no documented tab ID. Its adapter resolves a window-plus-URL
identity again for every action, follows reordering, and refuses duplicate URLs
or navigation that makes a saved identity ambiguous/stale. This can cancel a
pending query after an initial redirect, or prevent selection/replacement of
identical-URL tabs; reopening Omni refreshes the list. It avoids blindly acting
on a saved tab number. Native tab groups are those exposed by Safari's current
scripting window, not a separate inventory of inactive tab groups.

Firefox has no native tab/DOM scripting dictionary. Its adapter retains native
Accessibility tab elements, skips webpage content while listing tabs, and uses
Launch Services for background new-tab URLs. It uses accessible windows, with a
focused-window fallback if macOS exposes an empty application window list.
Only selected tabs expose verified URLs; other open tabs can be searched by
title. Bookmark reuse therefore covers verified selected URLs, not guessed URLs
for every background tab. Safe native replacement is unavailable in Firefox:
choose New tab instead. The adapter refuses uncertain new-tab identity and never
sends a prompt in that case. It waits for initial navigation before native chat
submission and cancels if tab/application ownership changes.

macOS Automation permission is required for each scripted browser. The owner
must approve its system prompt. The code never grants that permission or enables
JavaScript from Apple Events. Where already enabled, the first real DOM request
detects support, with independent per-browser 72-hour negative caches reset by
HS reload. Native fallback requires the browser to remain focused. Unsupported
or ambiguous send outcomes are not retried. Gemini submits only after confirming
Argon High; a different account may lack that model.

## History and performance

The existing authenticated helper keeps separate browser indexes; see
[browser history sources](browser-history.md). Disabled chooser sources do no
I/O. Bookmark/alias metadata caches and query-result generations are preserved.
MRU observation is event driven: no continuous AppleScript polling. Chooser query
history is cached in memory and persisted locally on acceptance, bounded to 100
entries per chooser purpose. Browser instances share a purpose's input history;
`historyId=false` opts a chooser out. No queries, clipboard values or history
records are written to logs or source control.

## Verification

Focused Lua/Node fixtures cover native dictionary differences, Safari URL
identity and ambiguity, Firefox AX identity and webpage boundaries, browser
scope, MRU cycling/release, asynchronous source isolation and existing DOM/native
submission guards. Native background-open checks preserve the foreground app and
track disposable fixture identities for cleanup. See the test scripts under
`hammerspoon/tests/`; the history helper also has Go race and schema tests.
