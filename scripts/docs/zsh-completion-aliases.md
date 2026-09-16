# Completing an alias in zsh

Two separate rules decide whether `<TAB>` after one of our aliases does anything
useful. Both bite, and they bite in different places.

## Aliases are expanded for completion only in command position

Zsh expands an alias before running the completion system **only when the alias
is the first word of the line**. There, `xz <TAB>` is completed as its expansion
(`\noglob llm-run`), which is why the bare form works.

Anywhere else the word is left exactly as written. In

    reval xz <TAB>
    with-lab-gemini xz <TAB>

nothing ever turns `xz` into `llm-run`, so the alias contributes nothing to the
completion and the expansion's own completer is not what runs. This is a
property of where the completion system expands, not of which completer a name
is bound to, so no `compdef`/`comp-set` line in
`zshlang/interactive/completions.zsh` can fix it.

**Workaround: write the bare form when you want completion.** Type
`xz prompt-<TAB>` first and wrap it afterwards, or use the expansion
(`reval llm-run prompt-<TAB>`) directly.

## `compdef` keys on the name, and real commands win by default

Completion dispatches on the *typed word*, so an alias whose name matches a real
command silently keeps that command's shipped completion. Shadowing at execution
time does not shadow at completion time.

`xz` is the example: it is our alias for `\noglob llm-run`, but zsh ships `_xz`
for the xz **compressor**, so `_comps[xz]` was `_xz` and `<TAB>` offered files to
compress. The fix is to bind the name explicitly --- `xz` is listed in the
`comp-set '=eval'` line, which makes `_comps[xz]` `_precommand`.

So when a new alias does not complete, check the name against a real command
first:

    print -r -- ${_comps[xz]}      #: which completer the NAME is bound to
    whence -p xz                   #: is there a real binary with this name?

A name with no colliding binary needs no entry; a colliding one needs to be
listed.
