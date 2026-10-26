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

## Emoji picker

Hyper+A opens the emoji picker. Every row uses a large Noto image and the
Unicode name, including older joined sequences such as 🙂‍↔️.

- Tab alternates recent usage and newest Unicode introduction first.
- Shift+Enter adds the highlighted emoji. Shift+Backspace removes one selection
  per key-down, including repeated presses with Shift held. Shift+Tab and
  backslash also remove the last selection.
- Enter pastes the selected Unicode text, or the highlighted emoji when empty.
- Cmd+Enter builds a lossless transparent WebP from native 512px artwork, with
  its longest side at 512px,
  and pastes the actual file. Telegram needs the file URL to recognize a sticker.
  The chooser selects its English “Send as a document” checkbox when available;
  otherwise choose that option manually. It never presses Send.
- Cmd+Shift+Enter copies and pastes a transparent PNG at 512px per emoji.
  Telegram's photo mode flattens PNG transparency; send PNG as a document to
  retain its original bytes.
- Both image shortcuts use the multi-selection, or the highlighted emoji when
  empty. Up to 128 emoji fit in one horizontal export.

Install the emoji data repo, then run:

```sh
python3 hammerspoon/bin/emoji-images.py
```

This caches all 1,923 official Noto previews, source metadata and licenses under
`~/Library/Caches/night-emoji/noto-18.0` (about 12 MB). Country/subdivision flags
use Noto's regional artwork, with aliases resolved and large originals resized.
Downloads resume from valid cached files. No font installation is needed.
WebP export needs `brew install webp` (`cwebp`). It runs asynchronously, preserves
alpha losslessly and downloads native 512px Noto artwork on first use for the
selected emoji into `~/Library/Caches/night-emoji/noto-18.0-512`. The 128px chooser
previews are never enlarged for export. It keeps reusable files in `~/Library/Caches/night-emoji/exports`
with content hashes, so an open Telegram attachment never changes underneath it.
Temporary encoder inputs are deleted. Other apps also get PNG image data on the
clipboard. Closing the chooser during encoding cancels the eventual paste.
WebP clipboard data includes `org.nightmachinery.emoji-export` with UTF-8 JSON
`{"version":1,"format":"webp","intent":"sticker","preserveAlpha":true}`,
plus `org.webmproject.webp` bytes, `public.file-url` and PNG fallback data.
Purple’s native marker handling requires its updated installed build; the
document-checkbox step also supports older clients.
Test decoded transparent pixels and a named-pasteboard round trip; an alpha
channel alone does not prove that Telegram's photo path preserves transparency.

Normal Unicode paste still depends on the receiving app's font support.

Search includes official names and `~/code/misc/unicode-emoji-json/aliases.json`.
That versioned file maps exact emoji strings to keyword arrays, is read whenever
the chooser opens, and is separate from generated Unicode data. For example,
`"😒": ["unimpressed", "eye", "meh"]`. Keywords are indexed without cluttering
row labels. Add personal terms there and sync the emoji repository.
Both pickers use `emojiChooserAliasesFor`: exact-entry keywords first, then base
keywords, deduplicated without case distinctions. It ignores emoji presentation
selectors and skin tones for lookup, removes a trailing gender sign and maps a
leading man/woman in a joined profession to the neutral person. Thus `smh` finds
both gendered facepalms, `coder` finds gendered/skin-tone professions, and Telegram's
unqualified `❤` finds the aliases stored on `❤️`. Keep the words on their canonical
data entries; do not copy them into every variant.

## Telegram premium emoji

Hyper+Shift+A opens the cached premium library. Each row shows its pack title, falling back to the pack short name. Clipboard-only entries show “Unknown pack” until catalog metadata is available. Duplicate recent/installed-set records enrich missing pack names instead of discarding them. The patched Purple client's
opt-in export feature supplies exact first-frame PNGs and a catalog under
`~/Library/Caches/Purple/emoji-export/`. Until that feature is installed and
enabled, saved clipboard entries work with Unicode preview images. No socket
or client automation is invoked by opening this chooser.

The catalog contract is version 1 with an `entries` array. IDs are decimal
strings; entries include `alt`, `setTitle`, `setShortName`, `recentRank` and an
optional relative `png` path. Default icons are `png/<id>-128.png`. Client recent
order comes first, then installed-set order. Search includes Unicode names,
set titles, short names and aliases. `NIGHT_TELEGRAM_EMOJI_CATALOG` or the
Hammerspoon setting `telegramEmoji.catalogPath` can select another export path.

Copy one premium emoji from Telegram, open the picker and press Cmd+S to save
it explicitly. The personal library is `~/.local/share/night-emoji/telegram.json`
(mode 600 in a mode-700 directory); it is never committed. Each entry preserves
its Qt rich clipboard tag, Unicode fallback, optional name and aliases. The
picker does not inspect account/session stores or messages. Saved entries remain
usable while Purple or its export feature is off.

Choosing an entry in Telegram restores its rich clipboard types and presses
Cmd+V without sending. The old clipboard is restored after the paste if no newer
copy replaced it. Outside Telegram it copies the rich entry for a later manual
paste. This does not create Telegram Premium entitlement; the target client and
account still decide which custom emoji they can use.
