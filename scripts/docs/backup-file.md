# backup-file

Takes a dated copy of a file before it can be lost or damaged, and keeps those
copies from growing without bound.

The logic is `zshlang/auto-load/others/backup.zsh`.

## Where the copies go

    backup-file FILE

copies FILE to

    ~/base/backup/auto/<name> <md5 of the path>/<YYYY Mon DD HH:MM:SS>/<name>

The folder name carries a hash of the path as given, so two files with the
same name in different places do not share a folder. `backup_file_root`
overrides `~/base/backup/auto`.

If FILE is byte-for-byte the same as the newest snapshot, no copy is made: a
second identical copy restores nothing the first cannot. On a machine where
the file rarely changes this keeps the folder to one snapshot per change.

Callers:

- `chronic-backup`, daily, for `$timetracker_db` and `$HISTFILE`.
- `tt-rename` and `tt-reval-diff`, just before they edit the timetracker
  database.

## Retention

After each copy, `backup-file` runs `backup-file-prune` on that file's folder.
It keeps:

- every snapshot from the last `backup_file_keep_days` days (default 30);
- the oldest snapshot of every calendar month, forever;
- the newest snapshot, always.

Everything else goes to the trash with `trs`, not `rm`, so `trs-restore` can
bring a snapshot back. The space is freed only when the trash is emptied
(`trash-empty-all`, which `cleanup` and `rm-caches` call); nothing empties it
on a schedule.

A folder whose name does not parse as a snapshot date is never touched. The
names use `%b` (`Sep`), so they do not sort by date as text; the pruner parses
each one into an ISO key and sorts on that.

A failed prune prints a trace but does not fail `backup-file`: the copy has
already been made by then.

To see what a prune would do without doing it:

    backup_file_prune_dry_run_p=y backup-file-prune "$timetracker_db" "$HISTFILE"

## Why

Before retention, every call added a full copy and nothing ever removed one. A
year of daily copies of a 15 MB database reached 5.5 GB on a small server and
helped fill its disk.
