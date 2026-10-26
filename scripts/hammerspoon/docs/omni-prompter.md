# OmniPrompter

Hyper+Shift+Z opens a global chooser for loaded Zsh functions starting with
`fragment-`, `prompt-`, `metaprompt-` or `snippet-`. Clipboard text is captured
before opening. Enter runs the highlighted function with that text on stdin.

- Shift+Enter appends the highlighted function to an ordered chain and clears
  the filter. Repeated stages are allowed.
- Shift+Backspace removes the last stage.
- Enter runs the chain when it contains stages; the current highlight is then
  ignored. Escape cancels.
- Cmd+R refreshes the loaded function catalog while the chooser is visible.

One quoted pipeline runs through the shared Brish helper with `pipefail`, so a
failed stage prevents copying the final stdout. Output whitespace is preserved.
Successful output replaces the clipboard and uses the existing `doPaste()`
helper if the original app, native window and focused element still match. A
changed caret/selection, or keyboard/mouse interaction while running, suppresses
pasting and leaves the result on the clipboard. Unverifiable targets also copy
without pasting. Clipboard data and output are not included in diagnostics.
Functions themselves can still have side effects; this is a function launcher,
not a sandbox.

The catalog comes from `omni-prompter-functions`, which inspects the running
BrishGarden shell's function table and does not execute listed functions. This
includes generated functions. It accepts optional case-insensitive literal AND
search terms and emits shortest names first, then alphabetical. Hammerspoon
caches the complete list in memory until Cmd+R or reload; typing and reopening
do not invoke the shell. After editing Zsh definitions, restart BrishGarden
before refreshing the chooser.

Ranking is most recently used first, then shortest name, then alphabetical.
Successful pipelines record their stages in selection order. Usage names,
counts and ordering counters persist through `hs.settings` under
`chooser.recent.omni-prompter.v1`. Emoji selection uses its own
`chooser.recent.emoji.v1` namespace, with alphabetical fallback. Each namespace
loads preferences once per reload and sorts in memory; saves happen only after
selection. Neither clipboard content nor generated output is stored there.

OmniPrompter and browser Omni use 75% of the active display's usable width,
capped at 1800 logical pixels. Layout is recalculated on every opening.

`brishz_eval_pipeline_out_hs(stages, callback, label, opts)` is the common
pipeline API: each stage is a quoted argv array. It shares `opts.stdin`,
`opts.trim=false`, timeout handling and private file-backed I/O with the existing
Brish helpers. Other callers keep their previous trimming/stdin defaults.

Catalog version 1.1.0 explicitly excludes internal instruction/file helpers,
chain runners/copy wrappers, the argument-taking preamble generator and fragment
definition generator. The exclusion list lives in the Zsh catalog, so discovery
and manual catalog queries agree. Public builders such as IMDb recommendations
now use the standard stdin helper. See the linked prompt-family corrections for
pipeline, mode, code-fence and JSON fixes in the
[prompt-family corrections](../../docs/prompt-family-review.md).

Emoji picker controls: Tab alternates recent-usage order and newest Unicode
introduction first. Shift+Enter adds the highlighted emoji; Shift+Backspace
removes one per key-down, including repeated presses while Shift stays held.
Shift+Tab and backslash also remove the last selection. The scoped key listener
stops when the picker closes.

For emoji that Apple's font does not yet draw, run
`python3 hammerspoon/bin/emoji-images.py` after installing the emoji data repo.
It caches official Noto previews for Unicode 16 and later, including Unicode 18,
under `~/Library/Caches/night-emoji/noto-18.0`. The chooser uses the image and
name when available; copying and recency still use the original Unicode emoji.
Missing cached previews fall back to the normal system text rendering. The
cache includes source/release metadata and the artwork licenses. This avoids
relying on Noto's CBDT/CBLC color font support in macOS AppKit.
