# Hammerspoon

See also the Hammerspoon section of `../AGENTS.md` (reload holds, calling `hs`
from a shell) and `docs/hammerspoon.md`.

## Keyboard Event Taps

An **always-on tap** is an `hs.eventtap` on key events (`keyDown`, `keyUp`,
`flagsChanged`) that stays enabled while the user works in other apps. It puts
every keystroke in every app behind Hammerspoon's main Lua thread:

- Anything that blocks that thread (a slow Accessibility call, a synchronous
  shell command, a reload) stalls typing system-wide, and macOS disables a
  tap that answers too slowly.
- A callback that throws drops the key. ChooserSearch's always-on tap
  swallowed every ⌥Up/⌥Down in every app until 1.7.1, which silently broke
  `org-metaup` in Emacs.

Rules:

- Do not add an always-on tap, and do not turn a scoped tap into one, without
  the user's direct approval. Before asking, warn them plainly that it will
  slow down their whole system, and say why the alternatives below do not
  work.
- Prefer an `hs.hotkey` bound only while the UI that needs it is open: the Tab
  bindings in `core/choosers.lua`, or ChooserSearch's ⌥ keys since 1.8.0.
  macOS routes only that key combination to Hammerspoon.
- A tap scoped to a short Hammerspoon interaction (started when it opens,
  stopped when it closes) is acceptable. Stop it on every exit path, keep the
  callback cheap, and wrap it in `pcall` so an error passes the key through.
