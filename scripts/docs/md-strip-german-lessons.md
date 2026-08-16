# Stripping "Learning German" sections from markdown

My LLM custom instructions append a `Learning German ^_^` section to
answers. Two functions remove it, and they are not interchangeable.

## Which one to use

[agfi:md-strip-german-teachings] (`zshlang/auto-load/others/pandoc.zsh:63`)
handles the *single-answer* case. It deletes from the last
`---\n\n# Learning German ^_^` to EOF. Because it only ever matches the last
occurrence, an earlier quoted copy cannot truncate the document. This is what
[agfi:h-pandoc-md-preprocess] calls, so it is on the `md2org` /
[agfi:pandoc-convert] path, gated by `$pandoc_convert_strip_german_p`.

[agfi:md-strip-german-lessons] handles *conversation exports*, where a lesson
follows every assistant message. `Saving_contact_photos_on_Xiaomi_Note_13_Pro.md`
carries 9 of them, `ChatGPT-MoE_Gradients_and_Routing.md` 4,
`German_English_dictionary_for_Android_with_IPA.md` 3. The teachings version
would remove only the last of those and take everything after it with it.

The lessons version is not wired into `pandoc-convert`; run it yourself.
Arguments are file paths, not text — [agfi:in-or-args-or-files] rather than
the usual [agfi:in-or-args], because passing a path to a filter that treats
it as content is a silent footgun:

```zsh
md-strip-german-lessons ~/Downloads/export.md > clean.md
md-strip-german-lessons < export.md          #: same
md-strip-german-lessons                      #: clipboard
```

## How a section is delimited

The heading is a strict output contract, but parsing is lax about heading
level (`#` through `######`) and surrounding horizontal whitespace. The `^_^`
is required. Override the whole pattern with `$md_german_lesson_re`.

Heading level genuinely varies in practice: 12 of the observed sections are
`# Learning German ^_^` and 9 are `### Learning German ^_^`, in exports from
the same source.

A section runs to the first of four things, all of which are needed:

- A `---` rule. Most Claude-export sections end this way.
- A `<details>` block. This is the `📚 Sources (n)` footer, which the
  exporter emits *after* the lesson even though it belongs to the preceding
  message. Stopping here is what keeps the citations.
- A heading at the same level or shallower.
- A speaker heading at any level, such as `#### ChatGPT:` or `## [9] USER`.
  ChatGPT exports use a level-1 lesson heading whose own subheadings are
  `##`/`###`, so the level test above never fires there, and the next `---`
  is thousands of lines away.

## Separators

Every lesson sits between two `---` rules, so deleting the section alone
leaves the two rules back to back. Exactly one adjacent rule goes with it:

- terminator is a rule — take the trailing rule, keep the preceding one
- terminator is `<details>` or EOF — take the preceding rule, so the Sources
  footer reattaches to its message
- terminator is a heading — take neither; the surrounding rules are already
  correct

The seam is then normalised to a single blank line. Nothing else is
rewritten: regions with no lesson in them are copied byte for byte, so this
cannot perturb whitespace elsewhere in the document.

## Fenced code blocks are skipped

`md-strip-german-teachings` gets an implicit guard from matching only the
last occurrence. A global pass has no such luck, so
`perllang/md_strip_german_lessons.pl` tracks ``` and `~~~` fences (only a
fence of the same character closes one) and refuses to match inside them.

This fires on real input. `ChatGPT-MoE_Gradients_and_Routing.md` opens a
```` ```markdown ```` block at line 4797 and does not close it until 7688 —
ChatGPT reissuing a whole document as literal markdown source — and there is
a `# Learning German ^_^` at 7563, inside that block. Three of the file's
four lessons are stripped; that one is left, because it is code-block
content rather than a live section. If you want it gone, close the fence in
the source first.

## Verifying a change

```zsh
f=~/Downloads/Saving_contact_photos_on_Xiaomi_Note_13_Pro.md
md-strip-german-lessons "$f" > /tmp/out.md

grep -c 'Learning German' /tmp/out.md          #: 0
grep -c '📚 Sources' "$f" /tmp/out.md          #: must match
diff <(grep -o '^## \[.*' "$f") <(grep -o '^## \[.*' /tmp/out.md)   #: empty

#: no doubled separators
perl -0777 -ne 'print "DOUBLED\n" if /^---[ \t]*\n(?:[ \t]*\n)*---/m' /tmp/out.md

#: idempotent
md-strip-german-lessons /tmp/out.md | diff - /tmp/out.md

#: fence guard — all six lines come back
printf '%s\n' 'keep' '```' '# Learning German ^_^' 'fenced' '```' 'keep2' |
    md-strip-german-lessons
```

The other three exports in `~/Downloads` are worth running too. The ChatGPT
one is the interesting case: check that its technical `##` headings survive
while the lesson subheadings do not.
