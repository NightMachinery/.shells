# rsp: copying, mirroring, and moving files

[agfi:rsp] wraps [agfi:rsp-safe] with `--delete-after --force-delete`.
These options delete extraneous files on the receiving side when synchronizing
directories. They do not remove the source files after copying them.

To move files, use `rsp-safe --remove-source-files SOURCE DESTINATION`.
Rsync removes source files only after successfully duplicating them at the
destination; source directories remain. `rsp-safe` keeps the checksum and
resume options without enabling destination cleanup. Use this with finished
files that are no longer being written.

## Optional extended attributes

[agfi:rsp] and the other `rsp` variants preserve extended attributes when
rsync supports them. If the client or server reports that extended attributes
are unsupported, they print a notice and retry once without the default
`--xattrs`. File contents and the other transfer options stay the same.

The fallback lives in [agfi:h-rsync-optional-xattrs], shared by [agfi:rsp-safe]
and [agfi:rsp-dl]. Other errors retain rsync's exit status and do not trigger
a retry. stdout and progress remain live; stderr is displayed and captured
to recognize the capability error.

`--include-from=-`, `--exclude-from=-`, and `--files-from=-` (including their
two-argument forms) are buffered in a temporary file so both attempts read
the same list. The temporary files are removed when the wrapper finishes.

Pass `--no-xattrs` to skip attributes immediately, or explicitly pass
`--xattrs` to require them even on the retry. Plain `rsync` is unchanged.

Reload in an existing shell with:

```zsh
source "$NIGHTDIR/zshlang/auto-load/others/rsync.zsh"
```

Rsync documents the option behavior in its
[manual](https://download.samba.org/pub/rsync/rsync.1).
