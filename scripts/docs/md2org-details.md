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
these transcripts use, leaving `[[[https://...][Title]]]` — three opening
brackets that read as broken org. It does parse as a link, but only by
accident of the outer pair being literal text. Fixed by
`org_sup_sub.lua`, below.

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

#### Exception: a `<details>` wrapping one code block is unwrapped

Chat exporters wrap *every* fenced block in its own collapsible section:

````markdown
<details>
<summary><strong>💻 Code Block (bash) — 9 lines</strong></summary>

```bash
jq '...' > out.json
```

</details>
````

Regrouping that gives two layers of wrapper and a summary that only restates
the language and the line count already visible on the `#+begin_src` line —
and an org src block folds with TAB on its own, so the special block buys no
folding either. In `~/Downloads/Backup_Brave_Settings.md`, 18 of 31
`<details>` are this shape.

So when the body left after the summary is *exactly* one code block, the
filter emits that code block bare and drops both the wrapper and the summary.

- The trigger is **structural**, not a match on the summary wording. Another
  exporter phrasing its label differently is handled for free, and there is no
  phrase list to maintain. The accepted cost: a hand-written
  `<summary>Full nginx config for reference</summary>` around a lone code
  block loses that label, since the summary is the only place it exists.
- `#body == 1` exactly. Pandoc's reader emits no node for the blank lines that
  delimit the body, so a wrapped code block arrives as precisely
  `[CodeBlock ("",["bash"],[]) "..."]`; anything richer is a real body and
  keeps its wrapper.
- Checked before the recursion into nested `<details>`. A lone `CodeBlock`
  contains none, so the order cannot change the verdict — and an outer
  `<details>` holding prose plus a nested code-block one keeps its own wrapper
  while the inner one unwraps.
- The summary-less form (`<details>`, code block, `</details>`) unwraps too.
- No `Div` wrapper here: unlike a bare raw org block, a real `CodeBlock` is
  already separated from its neighbors by the writer.

Set `pandoc_convert_details_unwrap_code_p=n` to keep every `<details>`
wrapped. `pandoc-convert` passes it through as the pandoc metadata key
`org_details_unwrap_code`, and only when switching the unwrap *off*, so the
usual command line stays clean. The filter reads it in a `Meta` pass of its
own — a single filter table walks the blocks before the metadata — and then
deletes the key so it cannot leak into `--standalone` output.

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

### `python/pandoc_filters/org_sup_sub.lua`

Rebuilds `<sup>`/`<sub>` raw inlines as real `Superscript`/`Subscript`
nodes, so the org writer emits `^{...}` / `_{...}` instead of dropping
them (cause 3).

Round-trip verified: org parses `^{[[url][title]]}` as a `superscript`
containing a `link`, and ox-html exports it back to
`<sup><a href="...">title</a></sup>`.

When the whole superscript is exactly `[` + one link + `]` — the
transcripts' citation style — the redundant literal brackets are dropped,
since org links already carry their own. Otherwise `^{[[[url][t]]]}` would
still show three.

Applied unconditionally for org output: recovering silently-dropped
content is a correctness fix.

### Ordering

`org_details.lua` **must** precede `org_raw_html.lua`; otherwise the
`<details>` raw blocks are re-tagged as org before the details filter can
match them.

## Is `#+begin_details` valid org?

Yes. `#+begin_NAME` / `#+end_NAME` is org's **special block**, a
first-class greater element — the same construct as `#+begin_quote`, just
with a name org has no built-in meaning for. `org-element-at-point`
returns `special-block` with `:type` `details` and `:parameters` set to
whatever follows on the begin line. Contents are parsed as normal org, and
TAB folds it (`org-fold-block`).

It is also the *canonical* choice here, not an invention: `details` and
`summary` are both in `org-html-html5-elements`, so ox-html knows them
natively.

### Export caveat

`org-html-special-block` only reaches HTML5 output when **both**
`org-html-html5-fancy` is non-nil and `org-html-doctype` is `"html5"`.
The defaults are `nil` and `"xhtml-strict"`, so by default you get:

```html
<div class="details" id="org30faab2">
<p>body text</p>
</div>
```

With html5-fancy on, `#+begin_details` + a nested `#+begin_summary`
exports to a genuine collapsible element — verified:

```html
<details id="org14adad9">
<summary id="org09fff6e"><p>The Summary</p></summary>
<p>body text</p>
</details>
```

**However**, `org-html-special-block` ignores the block's `:parameters`
entirely (it reads only `#+ATTR_HTML`), so the summary text in
`#+begin_details 📚 Sources (8)` is *dropped on export*.

### Why parameters rather than a nested `#+begin_summary`

Two shapes are possible, and they trade off against each other:

- `#+begin_details <summary>` — the summary is visible on the fold line,
  so a collapsed block still reads as "📚 Sources (8)". Dropped on HTML
  export.
- `#+begin_details` + nested `#+begin_summary`…`#+end_summary` — exports
  to a real `<details>/<summary>`, but folding hides the summary along
  with everything else, so a collapsed block shows no label at all.

These files are read in Emacs far more often than they are exported, so
the fold-line label wins and we emit the parameters form. If both are
wanted, emitting the title in *both* places costs only a duplicated line
and is otherwise harmless — the parameters are simply ignored on export.

### Alternatives rejected

- **Org drawer** (`:details:` … `:END:`) — folds too and is visually
  quieter, but drawer names are restricted to `[A-Za-z0-9_-]+`, so an
  arbitrary summary cannot be the name; it would have to move inside as a
  bold line, losing the fold-line label.
- **Headline** — folds natively and would put the summary in the heading,
  but a headline has no terminator: everything after `</details>` would
  become its child, wrecking the document outline.
- **One `#+begin_export html` block** (re-serializing the inner markdown
  to HTML) — preserves `<details>` exactly, but makes the whole body
  opaque in Emacs, which defeats the point of converting to org.

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
  [agfi:md-strip-german-lessons-last] intentionally drops — see
  `./md-strip-german-lessons.md` for that function and its
  conversation-export sibling)
- emoji in titles intact
- all 23 `<sup>` citation markers become org superscripts; 0 `[[[`
  leftovers. `org-element-parse-buffer` finds 23 `superscript` elements,
  each containing a `link`
- `md2org` on docs/md2org-latex/repro.md still emits its `equation*`
  environment — no regression on the LaTeX path

For the code-block unwrap, on `~/Downloads/Backup_Brave_Settings.md` (31
`<details>`, 18 of them single-code-block wrappers):

- `#+begin_details`: 31 → **13**, still balanced with `#+end_details`, and
  `org-element-parse-buffer` reports all 13 as `special-block` of type
  `details`
- `#+begin_src` blocks: 24, unchanged — the unwrapped ones survive as ordinary
  src blocks, which org parses as `src-block`
- `#+begin_html`: 0
- `pandoc_convert_details_unwrap_code_p=n` restores all 31
- the German dictionary file has no such wrappers, and its output is
  byte-identical with the unwrap on and off

## Reproduction commands

````zsh
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

#: the code-block unwrap
b=~/Downloads/Backup_Brave_Settings.md
md2org "$b" | grep -c '#+begin_details'
pandoc_convert_details_unwrap_code_p=n md2org "$b" | grep -c '#+begin_details'

#: nested: outer keeps its wrapper, inner code block comes out bare
printf '<details>\n<summary>outer</summary>\n\nprose\n\n<details>\n<summary>inner</summary>\n\n```sh\necho hi\n```\n\n</details>\n\n</details>\n' \
    | md2org
````

To check the org side, parse the output with `org-element-parse-buffer`
and confirm the blocks come back as `special-block` of type `details`.
