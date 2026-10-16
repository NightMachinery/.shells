# Disconnected mosh cleanup

Start with `mosh-cleanup --dry-run`. It samples cumulative inbound bytes with
`nettop` twice, 15 seconds apart, and prints every owned parent mosh-server's
PID, age, input idle time, non-shell descendants and decision. It deliberately
does not use `who`, which depends on mosh's optional utmp support.

A session is eligible only when inbound traffic does not grow and its pty
has received no input for at least 96 hours (4 days). `--idle-hours N` changes that
threshold. A sleeping laptop or suspended phone can still resume a silent
session, which is why traffic alone is insufficient. Unknown traffic, reset
counters and unavailable tty metadata always skip the session.

Non-shell jobs anywhere below the session protect it. Shells (`zsh`, `bash`,
`sh`, `dash`, including login shells) are ignored as jobs because interactive
shells may have shell-only prompt helpers. A `less`, `emacsclient` or `tmux`
below such a helper still protects the session. `--force` overrides job
protection, while preserving the silence and idle requirements.

Without `--dry-run`, the same decisions are printed, then candidates receive
SIGTERM by PID. After three seconds survivors receive SIGKILL. Known session
shells and their descendants are checked for orphans and cleaned by PID too.
Identity checks include UID, start time and executable, and input/jobs are
checked again before termination. No process-name pattern is used for killing.
The traffic observation is a snapshot, not proof that a client cannot reconnect
immediately afterwards; use the future-session timeout controls if the server
must make that decision itself.

`--only PID ...` restricts inspection and cleanup to the named owned parent
servers. For tests, use only a disposable server you created. Never run a live
kill test against somebody else's sessions. A never-contacted disposable
server may have no nettop row, which correctly skips it; briefly connect your
own disposable client before testing traffic-based cleanup.

For future sessions the owner can choose startup environment settings:

- `MOSH_SERVER_NETWORK_TMOUT`: seconds without contact before automatic exit.
- `MOSH_SERVER_SIGNAL_TMOUT`: seconds without contact required for a
  SIGUSR1-triggered exit.

Set these before mosh-server starts, for example in `~/.zshenv`; setting them
inside the already-started child shell has no effect on its parent server.
No dotfiles are edited by this tool. In the
[Mosh 1.4.0 source](https://github.com/mobile-shell/mosh/blob/mosh-1.4.0/src/frontend/mosh-server.cc),
`serve()` permits SIGUSR1 shutdown unconditionally when the signal timeout is
unset or zero. Do not send SIGUSR1 to existing sessions expecting it to protect
connected clients. The cleanup tool uses TERM/KILL only.

Validation: `python3 -m unittest discover -s python/tests -p test_mosh_cleanup.py`.
