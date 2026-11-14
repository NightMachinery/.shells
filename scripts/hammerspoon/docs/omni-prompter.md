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
- Cmd+Enter builds a lossless transparent WebP from native 512px artwork in
  the Auto layout (see [Sticker layouts](#sticker-layouts)), with its longest
  side at 512px, and pastes the actual file. Telegram needs the file URL to recognize a sticker.
  Purple reads the export marker and sends a transparent sticker whichever
  compression option is chosen. For older clients the chooser still ticks an
  English “Send as a document” checkbox when one appears, and says nothing when
  none does. It never presses Send.
- Cmd+Shift+Enter copies and pastes a transparent PNG in the same layout, at
  512px per emoji (at most 4096px on the long side). Telegram's photo mode
  flattens PNG transparency; send PNG as a document to retain its original
  bytes.
- Ctrl+Enter lists the other layouts for the selection.
- The image shortcuts use the multi-selection (up to 128 emoji), or the
  highlighted emoji when empty.

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
Purple has handled the marker since its 2026-10-27 build; the quiet
document-checkbox step remains only for older clients.
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
For fast opening, the picker uses native JSON decoding, retains parsed aliases
while the file bytes are unchanged, and reuses static row metadata. The small
file is still read on every opening, so edits take effect immediately even if
its timestamp and size stay the same. Each opening sorts a separate row array
using current recency, so the cache never freezes usage order.
These caches live in the Lua state, so each reload starts cold. Five seconds
after loading, Hammerspoon prewarms them on idle timers: previews in batches of
64 (about 25 ms each), then the aliases, then the rows. Measured on 2026-10-26,
the first opening after a reload took 99–113 ms instead of about 1 s. The
longest stall was about 0.19 s, the alias JSON decode, which cannot be split.
Set `emojiChooserPrewarmDelay=false` before loading to turn it off.

## Sticker layouts

Telegram shows a sticker at most 512px wide, so a 1×N strip of emoji gets
tinier with every emoji added. Both pickers therefore lay several emoji out
in a compact grid by default, and Ctrl+Enter offers other arrangements.

**Auto** is the default for Cmd+Enter and Cmd+Shift+Enter: a near-square grid
whose short row is centred on top. Three emoji become one over two, five
become two over three, and two stay side by side (512×245). Rows whose counts
differ by an odd number interlock, half a cell apart.

Every export trims each glyph's transparent margin before placing it, then
crops the finished image to its visible pixels, so glyphs fill their cells
and no empty border is left. Each glyph is resampled once (Lanczos), straight
from its source to its final size; rotated glyphs get one extra rotation.

Ctrl+Enter replaces the emoji list with layout rows. Each row has a 64px
preview of the actual selection and the approximate size, such as
`≈512×476, 245px cells`, flagged `small in Telegram` when the short side drops
below 128px. Enter pastes the highlighted layout as WebP, Cmd+Shift+Enter as
PNG, Tab re-rolls Jumble, Ctrl+Enter returns to the emoji list (restoring the
query), and Shift+Backspace still removes the last pick and refreshes the
previews. The last layout used is listed first. The layouts, with the
selection sizes that offer them:

- **Auto**, **Row** (one line), **Column** (one stack).
- **Each**: one 512px sticker per emoji, pasted one after another,
  `EmojiLayout.sequenceDelay` (0.6 s) apart. Purple accepts one WebP per paste
  and adds each to its open send box as a separate sticker, not an album.
- **Pages** (17 or more): Auto grids of up to 16, pasted the same way.
- **Beehive** (5+): interlocked rows, alternating long and short.
- **Daisy** (5+): the first emoji large in the middle, the rest around it.
- **Marquee** (3 to 25): the first emoji as a hero over the rest.
- **Heart**: emoji around the parametric heart outline up to 20, a filled
  heart beyond.
- **Orbit**: a ring up to 12, concentric rings around a centre beyond.
- **Sunflower**: the golden-angle spiral.
- **Pyramid** (3 to 28): rows shrinking upwards.
- **Nautilus** (5 to 30): a growing spiral, first emoji largest.
- **Jumble**: random cells of a sparse grid, each nudged, turned and resized
  a little. The seed is shown in the row (`Jumble #3`) and Tab picks the next.

Heart, Orbit and Sunflower repeat a short selection (to 6, 3 and 8 cells), so
even one emoji makes a shape.

The layout code is `core/emoji-layout.lua`: each layout turns N emoji into
cells (position, size, rotation) in arbitrary units, which drive both the
previews and the export. `bin/emoji-compose.py` (Homebrew Python with Pillow)
does the trimming, placement and resampling, and `cwebp` encodes the result.
Tests: `lua hammerspoon/tests/emoji-chooser-test.lua`,
`lua hammerspoon/tests/telegram-emoji-test.lua` and
`/opt/homebrew/bin/python3 -I hammerspoon/tests/emoji-compose-test.py`, run
from `~/scripts`.

## Telegram premium emoji

Hyper+Shift+A opens the cached premium library. Like Hyper+A, it switches the
input source to English while open and restores the previous one on close. Each row shows its pack title, falling back to the pack short name. Clipboard-only entries show “Unknown pack” until catalog metadata is available. Duplicate recent/installed-set records enrich missing pack names instead of discarding them. The patched Purple client's
opt-in export feature supplies exact first-frame PNGs and a catalog under
`~/Library/Caches/Purple/emoji-export/`. Until that feature is installed and
enabled, saved clipboard entries work with Unicode preview images. No socket
or client automation is invoked by opening this chooser; only an image export
asks Purple to render (below).

The catalog contract is version 1 with an `entries` array. IDs are decimal
strings; entries include `alt`, `setTitle`, `setShortName`, `recentRank` and an
optional relative `png` path, plus an optional `installed` flag. Default icons
are `png/<id>-128.png`. Emoji you picked recently in this chooser come first,
then client recent order, then installed sets, then packs marked
`installed: false`, each in catalog order. The placeholder shows the row count
and whether the catalog is missing, unreadable or more than a day old. The
catalog, merged entries and searchable rows are reused until catalog, saved
library or alias bytes change. Pack PNGs are cached until their mtime changes;
usage order is refreshed on each opening. Rich clipboard tags are built only
for the entries actually pasted. Idle prewarming loads pack icons in batches
after a reload (`telegramEmojiPrewarmDelay=false` disables it). Search includes Unicode names,
set titles, short names and aliases. `NIGHT_TELEGRAM_EMOJI_CATALOG` or the
Hammerspoon setting `telegramEmoji.catalogPath` can select another export path.

Cmd+Enter pastes a transparent WebP and Cmd+Shift+Enter pastes a transparent
PNG of the selection in the Auto layout, and Ctrl+Enter offers the others (see
[Sticker layouts](#sticker-layouts)). With no selection they use the
highlighted entry. The PNG uses each emoji's own source size per cell.

The 128px catalog previews are too small for a sticker, so exports use
Purple's largest real render of each emoji instead: 512px for vector (TGS)
stickers, and the native size, often 100px, for raster (WebP) ones, which no
request can improve. For each emoji, in order:

1. A finished `large/<id>-<px>.png` in the export folder.
2. An `emoji.render` request on the control socket named by the catalog's
   `controlSocket` field, with the ID sent as a JSON string, as 64-bit IDs
   exceed a double. At most three are in flight, each with its own 10 s
   budget (`EmojiLayout.renderTimeout`), and `busy` replies are retried.
   Three uncached emoji took 2.6 s on 2026-11-14.
3. With automation off, an empty `requests/<id>` file, which Purple's
   periodic scan picks up, within the same budget.
4. The 128px preview, then the Unicode image.

Shift+Enter also leaves the `requests/<id>` file for a pick without a large
render, so Purple usually finishes it before the export asks. Esc cancels a
pending export. Normal Enter keeps pasting the original rich premium-emoji
tags.

Copy one premium emoji from Telegram, open the picker and press Cmd+S to save
it explicitly. The personal library is `~/.local/share/night-emoji/telegram.json`
(mode 600 in a mode-700 directory); it is never committed. Each entry preserves
its Qt rich clipboard tag, Unicode fallback, optional name and aliases. The
picker does not inspect account/session stores or messages. Saved entries remain
usable while Purple or its export feature is off.

Shift+Enter adds the highlighted emoji to a selection shown as a tally, and
Shift+Backspace, Shift+Tab or backslash removes the last one, as in Hyper+A.
Enter then pastes the whole selection as one rich paste: the text of every emoji
with one custom-emoji tag each, at ascending UTF-16 offsets, the layout tdesktop's
`DeserializeTags` accepts. Without a selection Enter pastes the highlighted
emoji. A multi-emoji paste has not yet been tried in the real client, and
Telegram may cap custom emoji per message.

Choosing an entry in Telegram restores its rich clipboard types and presses
Cmd+V without sending. The old clipboard is restored after the paste if no newer
copy replaced it. Outside Telegram it copies the rich entry for a later manual
paste. This does not create Telegram Premium entitlement; the target client and
account still decide which custom emoji they can use.
