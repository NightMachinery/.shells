# ntag-filter-or-toggle

`ntag-filter-or-toggle TAG` behaves like `ntag-filter-or-add TAG` when no files are
passed: it filters files by `TAG`.

When file paths are passed, it toggles `TAG` on each file:

```zsh
ntag-filter-or-toggle red file-a file-b
```

Files that already have `red` in their filename tags have it removed. Files without
`red` have it added. The command resolves stale untagged paths through
`ntag-recoverpath` before deciding whether to add or remove the tag.
