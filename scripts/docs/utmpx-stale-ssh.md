# Stale SSH rows in utmpx

Local kitty tabs showed `user@host` in the prompt, as if they were SSH logins.
The cause was stale rows in the macOS login table, `/var/run/utmpx`. There are
two fixes: a root LaunchDaemon that ends those rows, and a prompt override that
stops pure from trusting them in the first place.

## The cause

A **stale row** is a `USER_PROCESS` row with a remote host (an SSH login) whose
process no longer exists. sshd ends its row at logout. A session that dies
without doing that leaves the row behind, and `who` and `last` keep reporting
it as "still logged in". When this was found, 16 such rows from Tailscale IPs
had piled up over a week, on a machine with 53 days of uptime. The logs from
that week had rotated away, so why those sessions died uncleanly is unknown.

Terminal emulators like kitty write no utmpx rows of their own. So when macOS
gives a new tab a tty that once belonged to a dead SSH session, `who -m` on that
tty still returns the old SSH row.

## How pure is fooled

pure (`sindresorhus/pure`) decides whether to show `user@host` in
`prompt_pure_state_setup`, in this order:

1. `$SSH_CONNECTION`;
2. an inherited `$PROMPT_PURE_SSH_CONNECTION`;
3. `who -m`: if the row for the current tty ends in an IP, the shell counts as
   SSH, and pure **exports** `PROMPT_PURE_SSH_CONNECTION` with that IP.

Step 3 reads the stale row. The export in step 3 then makes step 2 fire in every
process started from that tab, including tools and agents launched there. So
the false verdict spreads even to processes that have no tty at all.

## Fix 1: the prompt override

`~/.zshrc`, right after `source-plugin sindresorhus/pure`, clears
`prompt_pure_state[username]` and unsets `PROMPT_PURE_SSH_CONNECTION` unless
`isSSH` is true, the shell is root, or it is inside a container. `isSSH` (in
`zshlang/basic/ssh.zsh`) looks only at `SSH_CLIENT`, `SSH_TTY` and
`SSH_CONNECTION`. Those are set by sshd and kept by mosh, so `isSSH` cannot be
fooled by utmpx.

The cost is that pure no longer detects a remote `su` that dropped
`SSH_CONNECTION`. `isSSH` never caught that case either.

## Fix 2: the cleaner daemon

`c/utmpx_clean_stale.c` walks utmpx and lists the stale rows. With `--apply`,
and only as root, it rewrites each one as `DEAD_PROCESS` with the current time
through `pututxline`. That is the record a clean logout writes, so `last` then
shows the session as ended. It acts only on a pid that `kill(pid, 0)` reports
as nonexistent (`ESRCH`). Console rows, local rows and live sessions are never
touched, and a row whose pid has been reused waits until that process exits.

The LaunchDaemon `com.user.utmpx-clean` (`launchers/utmpx-clean/`) runs it at
load and every 15 minutes, logging one line per row it ends to
`/var/log/com.user.utmpx-clean.log`.

The daemon never runs anything from the repository. A root job that executes a
user-writable file would let anything running as the user become root. The
installer therefore compiles a **snapshot** and installs it `root:wheel 755` as
`/usr/local/libexec/utmpx-clean-stale`. It first checks that every directory
above both install locations is root-owned and not group- or world-writable.

## Commands

All in `zshlang/auto-load/others/macOS/utmpx.zsh`:

- [agfi:utmpx-stale-list] compiles to a temp dir and prints the stale rows.
  It needs no root and installs nothing.
- [agfi:utmpx-clean-daemon-install] installs the daemon or brings it up to
  date. It is idempotent: it compares a fresh build with the installed binary
  using `cmp`, checks the plist, the ownership and modes, and whether the job
  is loaded. When all of that matches, it changes nothing and asks for no
  password. Otherwise it does every root step in a single sudo call, so an
  askpass setup prompts once. `setup/setup_macOS.zsh` calls it.
- [agfi:utmpx-clean-daemon-uninstall] boots the job out and removes the plist
  and binary.

Re-run the installer after editing the C source or the plist. A compiler
update also changes the build, which triggers one reinstall.

The `cmp` comparison relies on same-named builds being byte-identical. The
ad-hoc code signature embeds the output file name, which is why the build is
always named `utmpx-clean-stale`.

## Checking by hand

- `who`: should list only `console` and live sessions.
- `last | grep 'still logged in'`: should list only live sessions.
- `utmpx-stale-list`: should print nothing.
- `launchctl print system/com.user.utmpx-clean`: the job's state and last exit
  code.

A reboot also clears utmpx.
