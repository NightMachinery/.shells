# html2org: stray `__` from Outlook emails

## Symptom

`html2org` (and the other `pandoc-convert`-based converters) emitted
literal `__` sequences, e.g. `Hi Feraidoon,__` or whole lines of `__ __`.

## Cause

Outlook-composed emails, as rendered by Gmail, are littered with empty
`<u></u>` elements — Outlook's `<o:p></o:p>` paragraph markers rewritten
by Gmail. Pandoc's HTML reader (observed with pandoc 3.7.0.2) parses each
as `Underline []`, and the org writer emits an empty underline as a
literal `__`. Other output formats produce equivalent junk.

Minimal repro:

```zsh
printf '%s' 'Hi<u></u><u></u>' | pandoc -f html -t org
# => Hi__
```

Note this was unrelated to the `org_soft_linebreaks.lua` change (see
[md2org-latex](./md2org-latex/readme.md)); that filter only touches hard
line breaks, and the textual trim it replaced only stripped `\\`.

## Fix

`pandoc-convert` (zshlang/auto-load/others/pandoc.zsh) injects
`$NIGHTDIR/python/pandoc_filters/drop_empty_inlines.lua` whenever
`trim_extra` is on (the default), for every output format. The filter
drops empty inline formatting nodes (`Underline`, `Emph`, `Strong`,
`Strikeout`, `Superscript`, `Subscript`, `SmallCaps`) at the AST level.
Pandoc walks innermost-first, so nested cases like `<b><u></u></b>`
collapse fully. Spans are left alone, as they may carry
anchors/attributes.

A textual postprocessing regex on `__` was rejected: it would corrupt
legitimate `__`, e.g. Python dunders inside code blocks.
