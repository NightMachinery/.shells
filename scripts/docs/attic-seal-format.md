# Attic record format (seal / unseal / exor)

The `seal` family stores one-liners in flat "attic" files: `seal` appends,
`unseal` picks one with fzf, `exor` deletes one. Aliases per attic live at the
top of `zshlang/auto-load/others/seal.zsh` — `emadd`/`ems`/`emrm`,
`td`/`todos`/`tdn`, `quotes-add`/`quotes`/`quotes-rm`, `ej`/`temoji-add`.

The files are plain text; nothing here is encrypted (the `.darkattic` name
notwithstanding). Paths come from `bash/auto-load/configvars.bash`.

## Layout

Records are separated by ASCII RS (`\x1e`, octal `\36`), written as a literal
byte. `seal` emits the separator as a *prefix* on every record except the
first, and terminates each record with a newline:

    record one\n <RS> record two\n <RS> record three\n

Two consequences are easy to trip over, and both have caused bugs:

- **The newline before each separator is a display convention, not record
  content.** It exists so `cat` on an attic (`tds` is literally
  `cat "$attic_todo"`) stays readable and greppable. Readers must strip it.
- **The last record has no separator after it.** Anything that keys off the
  separator — trimming, deletion, splitting — needs an end-of-input case, or it
  will silently do nothing for exactly one record.

`.temojis` predates the convention and uses a variant: no newline before the
separator, and some records separated by a space plus RS. Treating `\s*<RS>` as
the separator handles both shapes.

## Reading

`unseal` pipes the file through `RS2NUL` (`zshlang/auto-load/others/string.zsh`)
and hands NUL-separated records to `fzf --read0`. `RS2NUL` reads
`unseal_trim_trailing_whitespace_p`, which `unseal` defaults to `y`; standalone
it defaults to `n`. With trimming on, the separator is `\s*<RS>` and trailing
whitespace on the final record is stripped too.

Empty and whitespace-only records are dropped regardless of the flag. They can
appear from a leading separator — `seal` writes one when the attic file exists
but is empty — or from a blank line left by hand-editing the file.

`unseal-get2note` is a second reader, used by `notes-search`; it converts the
separator with `prefixer` so that `PREFIXER_LINENUMBER` stays meaningful, and
does no trimming. Its view of a record therefore differs from `unseal`'s by
trailing whitespace.

## Deleting

`exor` gets the record to delete from `unseal`, i.e. already trimmed, then
matches it against the raw file. The pattern must allow the untrimmed newline
back in:

    s/(<RS>|\A)\Q$FROM\E\s*(?<sep><RS>|\Z)/$+{sep}/g

Replacing with the captured trailing separator, rather than deleting it, keeps
exactly one separator between the surviving neighbours. Deleting the first
record leaves a leading separator behind, which `exor` strips afterwards with
`s/\A<RS>//`.

## Gotcha: the clipboard

`unseal` copies the selection with `pbcopy "$l"`, in argument form. Piping
instead (`ec "$l" | pbcopy`) copies the newline `ec` adds, because `pbcopy`
reads stdin through `in-or-args`, which is a plain `cat`.
