# rsp: copying, mirroring, and moving files

[agfi:rsp] wraps [agfi:rsp-safe] with `--delete-after --force-delete`.
These options delete extraneous files on the receiving side when synchronizing
directories. They do not remove the source files after copying them.

To move files, use `rsp-mv SOURCE DESTINATION`. [agfi:rsp-mv] wraps
`rsp-safe --remove-source-files`.
Rsync removes source files only after successfully duplicating them at the
destination; source directories remain. `rsp-safe` keeps the checksum and
resume options without enabling destination cleanup. Use this with finished
files that are no longer being written.

`rspm` preserves creation times (`--crtimes`); it is not a move variant.

## Picking movies or audiobooks from the phone

`tealy-mv-movies .` opens [agfi:fz] with files and directories directly under
`storage/movies/` on the SSH host alias `tealy`, plus one level inside each
directory. For example, `storage/movies/anime/x` appears, but entries inside
`x/` do not. Hidden files and directories are included. Directories have a
trailing `/` in the picker.

`tealy-mv-audiobooks .` offers the same picker for `storage/shared/audiobooks/`.

Press Tab to select several entries, then Enter to review each remote source
and its absolute local destination. [agfi:ask] requests confirmation with No
as the default before [agfi:rsp-mv] runs. Escape in the picker or declining
confirmation cancels without moving anything. The optional destination defaults
to `.` and must be an existing local directory. Matching destination files may
be updated; unrelated destination files remain.

Selected entries keep their own names at the destination: selecting
`storage/movies/anime/x/` creates `./x/`, with its full subtree. Selecting both
a directory and a child moves the directory once. Selections that would land
under the same name are rejected before transferring. Empty source directories
remain, as with `rsp-mv`. Spaces and newlines in filenames are preserved.

Set `tealy_mv_movies_root` or `tealy_mv_audiobooks_root` to override the remote
directory. Keep private paths in local environment configuration. The code,
documentation, and tests contain only generic paths and synthetic filenames;
library entries are fetched at runtime and are not written to repository files.

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
