# Prompt-family review and corrections

@OpenAI, 2026-10-22. The stdin and composition fixes below are implemented in
`zshlang/auto-load/others/prompt/` (prompt library/snippet helpers 1.1.0).

`snippet-chain first second` streams stdin through the named functions and
writes final stdout. No stages means pass-through. It preserves trailing
whitespace and propagates failures with `pipefail`. It does not copy anything.
`snippet-chain-copy` is the interactive compatibility boundary: read clipboard
when stdin is a terminal, buffer the final output, then copy and print once on
success. A failed stage leaves the clipboard untouched. `review-diff-do` uses
this wrapper to preserve its existing interactive clipboard behavior.

`prompt-instruction-input` accepts only empty mode, `none` and `n` as the plain
modes (case-insensitive). It rejects unrelated values such as `python` and
`none-junk`. Block input uses at least three backticks and a fence longer than
any backtick run in the input. `prompt-define-long` supplies a valid JSON example
with a real array of example strings.

IMDb recommendations/adaptations and URL-based definition prompts use the common
`prompt-instruction-input` path, accepting either arguments or stdin. URL
summarization, COCO label mapping and playlist-theme input also accept stdin.
The image prompt helper forwards `"$@"` so an omitted text argument can reach
stdin instead of being replaced by an explicit empty string.

The `omni-prompter-functions` catalog has an explicit helper exclusion list:
`prompt-input-file`, `prompt-instruction-input` and its variants,
`prompt-blockify`, `snippet-input`, `snippet-input-file`, `snippet-chain`,
`snippet-chain-copy`, `snippet-preamble-gen1`, and `fragment-from-snippet`.
Public prompt, fragment, snippet and metaprompt builders remain discoverable.
After Zsh edits, restart BrishGarden, then refresh OmniPrompter with Cmd+R or
reload Hammerspoon.

Run `zsh zshlang/tests/prompt-family.zsh` from the scripts root using the
configured Zsh. The test stubs clipboard/network boundaries and checks stdin
parity, pipeline failures, successful-only copying, fence safety, parsable JSON
and catalog exclusions. On this macOS setup the configured Homebrew Zsh supplies
the PCRE module used by the existing shell library; the system Zsh does not.

Remaining improvement suggestions:

- `audio.zsh` asks for word-for-word transcription while also asking to remove
  fillers and false starts. Separate verbatim and edited transcript modes.
- `def-prompts-from-file` defines `_define_one` as a function despite declaring
  a scalar of that name local. Use a scoped anonymous helper or explicit
  cleanup to avoid leaving implementation functions in the global table.
- A small optional registry could label public transformations and declare
  required inputs, but adds maintenance. Automatic name discovery plus the
  explicit helper exclusion list is sufficient for the current chooser.
