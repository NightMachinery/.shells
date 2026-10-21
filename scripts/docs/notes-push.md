# Scheduled notes pushes

`notes-push-logged` runs `notes-push` and appends its stdout and stderr to
`~/logs/notes-push-YYYY-MM-DD.log`. Each run has start and finish timestamps
with a UTC offset, plus its exit status. The helper creates `~/logs` when
needed and returns the status from `notes-push`. If the log cannot be opened,
it fails before pushing.

Both active cron jobs use `brishzgo` at minute 30 of each hour:

```cron
30 * * * * "$HOME/go/bin/brishzgo" headphones-battery-alert-low
30 * * * * "$HOME/go/bin/brishzgo" notes-push-logged
```

Keep date formatting in the helper: cron treats `%` specially in command lines.
The helper is in `zshlang/auto-load/others/notes.zsh`; `notes-push` is in
`zshlang/personal/shortcuts.zsh`. Run `brishz-restart` after changing the helper
so the persistent garden loads it.

For a manual logged push, run `notes-push-logged`, or
`"$HOME/go/bin/brishzgo" notes-push-logged` from outside the configured shell.
Set `notes_push_logged_dir` to override the log directory.
Logs append throughout each day and are retained until removed manually.
