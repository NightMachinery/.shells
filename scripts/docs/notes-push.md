# Scheduled notes pushes

`notes-push-logged` uses `log-to` to append dated entries, stdout and stderr
from `notes-push` to the single file `~/logs/notes-push.log`. The existing
logging helper adds a timestamp and separators around each run, creates the
log directory when needed, and locks each append.

Both active cron jobs use `brishzgo` at minute 30 of each hour:

```cron
30 * * * * "$HOME/go/bin/brishzgo" headphones-battery-alert-low
30 * * * * "$HOME/go/bin/brishzgo" notes-push-logged
```

The wrapper is in `zshlang/auto-load/others/notes.zsh`; `log-to` is in
`zshlang/auto-load/others/logging.zsh`. Run `brishz-restart` after changing the
wrapper so the persistent garden loads it.

For a manual logged push, run `notes-push-logged`, or
`"$HOME/go/bin/brishzgo" notes-push-logged` from outside the configured shell.
The log is retained until removed manually.
