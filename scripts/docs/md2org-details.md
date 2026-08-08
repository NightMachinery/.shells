# md2org: `<details>`/`<summary>` blocks convert badly

## Symptom

Markdown containing collapsible sections — LLM chat transcripts (tool
calls, sources, thinking blocks), GitHub READMEs — converted to org with
`md2org` produced walls of `#+begin_html` noise instead of anything
readable:

```org
#+begin_html
  <details>
#+end_html

#+begin_html
  <summary>
#+end_html

📚 Sources (8)

#+begin_html
  </summary>
#+end_html

1. [[https://github.com/itkach/aard2-android/wiki][Home · itkach/aard2-android Wiki]] --- GitHub
...

#+begin_html
  </details>
#+end_html
```

Observed with pandoc 3.7.0.2 / Org 9.6.15. Repro input:
`~/Downloads/German_English_dictionary_for_Android_with_IPA.md` — 11
`<details>` blocks became **44** `#+begin_html` blocks.

## Cause

Three independent defects stack up.

### 1. Pandoc shreds the wrapper into four sibling blocks

The `<details>` idiom puts a blank line before the body so that the body
is still parsed as markdown. A blank line *terminates* an HTML block in
pandoc's markdown reader, and pandoc's AST has no node for "raw wrapper
containing parsed blocks", so the nesting cannot be represented at all:

```
$ printf '<details>\n<summary><strong>T</strong></summary>\n\nbody\n</details>\n' \
    | pandoc -f markdown -t native
[ RawBlock (Format "html") "<details>"
, RawBlock (Format "html") "<summary>"
, Plain [ RawInline (Format "html") "<strong>", Str "T"
        , RawInline (Format "html") "</strong>" ]
, RawBlock (Format "html") "</summary>"
, Para [ Str "body" ]
, RawBlock (Format "html") "</details>" ]
```

The org writer then wraps *each* raw block on its own — hence 4 blocks
per `<details>`. This is structural, not an upstream bug.

### 2. `#+begin_html` is dead syntax on Org 9.2+

pandoc's org writer hardcodes the Org 7/8-era wrapper
(`src/Text/Pandoc/Writers/Org.hs`, still current in 3.7.0.2):

```haskell
blockToOrg (RawBlock "html" str) =
  return $ blankline $$ "#+begin_html" $$
           nest 2 (literal str) $$ "#+end_html" $$ blankline
blockToOrg b@(RawBlock f str)
  | isRawFormat f = return $ literal str   -- isRawFormat = latex | tex | org
  | otherwise     = report (BlockNotRendered b) >> return empty
```

Org replaced that with `#+begin_export html` in 9.2 (2018). On Org 9.6.15
`org-element-at-point` reports `special-block`, **not** `export-block`, so
the HTML is no longer passed through on export — it is treated as org
prose inside a `<div class="html">`.

pandoc never notices because its own org *reader* accepts both spellings
and yields an identical `RawBlock (Format "html")`, so every pandoc
round-trip is lossless. Only real Emacs Org-mode sees the breakage. That
also makes rewriting safe: pandoc can still read back what we emit.

### 3. The summary silently loses its formatting

`<strong>` inside `<summary>` becomes `RawInline (Format "html")`, and
html is not in `isRawFormat`, so the org writer drops it — deliberately,
and it says so:

```
$ printf 'a <strong>b</strong> c\n' | pandoc --verbose -f markdown -t org
[INFO] Not rendering RawInline (Format "html") "<strong>"
[INFO] Not rendering RawInline (Format "html") "</strong>"
a b c
```

So the summary landed as an ordinary paragraph, visually indistinguishable
from body text — which is what made the output read as garbage rather than
as a labeled section. No writer flag recovers this; it needs an AST filter.

The same mechanism eats the `<sup>[[Title](url)]</sup>` citation markers
these transcripts use, leaving `[[[https://...][Title]]]`. Org still parses
that as a valid link, so it is cosmetic stray-bracket noise only — **not
currently fixed**.

## Fix

Two Lua filters, injected by `pandoc-convert`
(zshlang/auto-load/others/pandoc.zsh) for org output.

### `python/pandoc_filters/org_details.lua`

Regroups the four sibling raw blocks into one foldable org special block:

```org
#+begin_details 📚 Sources (8)
1. [[https://github.com/itkach/aard2-android/wiki][Home · itkach/aard2-android Wiki]] --- GitHub
...
#+end_details
```

Verified on Org 9.6.15: parses as `special-block` with `:type` `details`
and `:parameters` set to the summary text, and folds with TAB
(`org-cycle` → `org-fold-block`). Contents are parsed as normal org.

Applied when `trim_extra` is on (the default), so
`pandoc_convert_trim_extra=n` disables it.

Details:

- Nested `<details>` are handled. Only a *leading* `<summary>` is consumed
  (it is by spec the first child); scanning the whole body would swallow a
  nested block's summary, retitling the outer block with the inner text.
- `<strong>`/`<em>` around the summary are stripped and the text kept,
  since the writer would otherwise drop them (cause 3).
- An unbalanced `<details>` is left alone and falls through to
  `org_raw_html.lua`.
- The result is wrapped in an attribute-less `Div` so the org writer
  separates it from its neighbors with blank lines — bare raw blocks are
  glued tightly. Same trick as `org_math_env.lua`.

### `python/pandoc_filters/org_raw_html.lua`

`@upstreamBug` workaround for cause 2: re-tags every remaining
`RawBlock "html"` as a raw *org* block containing
`#+begin_export html` … `#+end_export`. `org` is in `isRawFormat`, so
re-tagging is enough to control the exact output.

Applied unconditionally for org output — this is a correctness fix, not a
cleanup, and pandoc's reader accepts both spellings so round-trips are
unaffected.

Done at the AST level rather than as a textual
`#+begin_html` → `#+begin_export html` postprocess so that a literal
`#+begin_html` inside a source/example block is not rewritten.

### Ordering

`org_details.lua` **must** precede `org_raw_html.lua`; otherwise the
`<details>` raw blocks are re-tagged as org before the details filter can
match them.

## Gotcha: Lua `%s` corrupts emoji

Lua patterns are byte-based and `%s` calls the locale's `isspace()`, which
matches `0xA0` (Latin-1 NBSP) under some locales. That byte also occurs
*inside* UTF-8 sequences — 🛠 (U+1F6E0) is `F0 9F 9B A0` — so collapsing
whitespace in summary titles with `%s+` shredded the emoji:

```
#+begin_details ��� ️ web_fetch — Fetching: ...
```

Both filters use an explicit `[ \t\r\n\v\f]` class instead.

## Results

On the repro file, via the full `md2org` pipeline:

- `#+begin_html` blocks: 44 → **0**
- `<details>` blocks: all converted, `#+begin_details`/`#+end_details`
  balanced
- `org-element-parse-buffer` finds 10 special blocks, all of type
  `details` (11th is inside the trailing section
  [agfi:md-strip-german-teachings] intentionally drops)
- emoji in titles intact
- `md2org` on docs/md2org-latex/repro.md still emits its `equation*`
  environment — no regression on the LaTeX path

## Reproduction commands

```zsh
f=~/Downloads/German_English_dictionary_for_Android_with_IPA.md

#: before/after
pandoc --wrap=none --from markdown --to org "$f" | grep -c '#+begin_html'
md2org "$f" | grep -c '#+begin_html'
md2org "$f" | grep '#+begin_details'

#: the AST that causes it
printf '<details>\n<summary>T</summary>\n\nbody\n</details>\n' \
    | pandoc -f markdown -t native

#: dropped inline html, straight from pandoc
printf 'a <strong>b</strong> c\n' | pandoc --verbose -f markdown -t org
```

To check the org side, parse the output with `org-element-parse-buffer`
and confirm the blocks come back as `special-block` of type `details`.
