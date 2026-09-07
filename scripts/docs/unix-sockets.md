# Where our UNIX sockets live

Every long-running local service we own — kitty's remote control, the Emacs
server, the iTerm focus helper — binds a UNIX domain socket. They all used to
bind it under `~/tmp`. In September 2026 a stray deletion there killed kitty's
remote control outright, and `cmd+shift+o` (open the Claude Code session in the
focused kitty window) began reporting only "could not find kitty's socket".

They now bind under `$NIGHT_SOCKETS_DIR`, which defaults to `~/.local/state`.

## Why a deleted socket is not a small problem

Unlinking the path does not stop the listener. The socket itself lives in the
kernel; the file is only a rendezvous name. So the process keeps serving on the
bound inode, `lsof` still shows it holding the path, and every client that
tries to connect gets `ENOENT`. It looks exactly like a service that is somehow
both running and unreachable, because that is what it is.

There is no way back. A UNIX socket path cannot be re-linked once unlinked:
macOS has no `linkat(AT_EMPTY_PATH)`, and nothing else can hand the kernel's
socket a new name. Recovery means restarting the process.

For kitty it is worse than that, because `listen_on` is applied only at
startup. Its own documentation is explicit: "Changing this option by reloading
the config is not supported." So editing `kitty.conf` and reloading does
nothing, and only a full restart — losing every process in every window —
gets the socket back.

`~/tmp` is a scratch directory, which is the whole problem. Things get deleted
there by hand; [agfi:rm-caches] already prunes inside it; and until this change
`kitty-remote` deliberately deleted kitty's own sockets whenever a `kitty @`
call failed. The names made it worse: they began with a dot, so they did not
show up before an `rm`, and nothing about `.kitty-548` suggests that deleting
it costs anything. `~/.local/state` is mode 0700, exists on every host, and is
swept by nothing.

## The one name, and the literals that cannot use it

`NIGHT_SOCKETS_DIR` is defined in `~/.shared.sh`, which both `.zshenv` and
`.bashrc` source, and overridable so that a host can point it elsewhere.

Three consumers cannot read it, because they run with no shell environment at
all, and each therefore repeats the path as a literal:

- `listen_on` in `configFiles/kitty/kitty.conf`. kitty is launched by macOS
  Launch Services from the Dock. `listen_on` has no `${VAR:-default}` syntax
  either, so an unset name would expand to nothing and kitty would try to bind
  at the filesystem root — silently.
- `night/emacs-socket-dir` in `~/doom.d/config.el`. Emacs.app is launched by
  Finder. This is why the fix there was to change the *fallback* rather than to
  set `NIGHT_EMACS_SOCKET_DIR`, which would have left a Finder-launched Emacs
  in the old directory and split the socket directory in two.
- `python/iterm/iterm_focus.py`, which prefers `$iterm_socket`, then
  `$NIGHT_SOCKETS_DIR`, then the literal.

Grep for `NIGHT_SOCKETS_DIR` before moving any of this; every site names the
variable in a comment for exactly that reason.

On the CIS cluster the variable is not a convenience but a requirement.
`$HOME` there is one NFS mount shared by about twelve machines, and a socket on
a share cannot work at all: `AF_UNIX` has no network transport, so a client on
another host gets `ECONNREFUSED`. Worse, the hosts would collide on one path,
and `server-start` deletes what it judges to be a stale socket — so starting
Emacs on one host would clobber another's. Those hosts point the variable at
host-local storage through `~/.night-bootstrap.env`, and
`night/emacs-socket-dir` derives its own per-host path for the same reason.

## kitty: finding the socket, and never deleting one

`listen_on` writes kitty's pid into the socket's name, as
`kitty-<pid>.sock`. That is load-bearing. It means a socket left behind by a
kitty that crashed is *identifiable* rather than merely ambiguous:

    kitty-sockets-list      # sockets whose pid is still a live kitty
    kitty-socket-get        # the one live socket, as unix:<path>
    kitty-socket-pid        # <pid> back out of a socket path

`kitty-remote` used to trash any socket a `kitty @` call failed against, and
carried a comment asking whether that had ever deleted a live one. It could: a
bad subcommand, or a kitty too busy to answer, is indistinguishable from a dead
socket, and guessing wrong costs a running kitty its remote control with no way
back. Nothing deletes sockets now. Filtering by pid makes a stale one cost a
`pgrep` lookup instead.

Use `pgrep -x kitty`, never `-f`: `-f` matches whole command lines, including
the command doing the matching.

`kitty-socket-get` distinguishes its failures, because they want different
responses and only one of them needs an instruction:

- kitty is not running.
- kitty is running but has no socket, so restart kitty — a config reload
  cannot re-create it. This is the case that used to be invisible.
- several live kitty sockets.

`h-claude-code-view-session-focused` shows that text verbatim in its
notification, since the hotkey runs detached and stderr goes nowhere a person
will look.

## Emacs, after a move

A running Emacs server keeps listening on the path it bound at startup, so
moving the directory does not migrate it. Either restart the daemon, or bridge
the new path to the live socket with a symlink:

    ln -s ~/tmp/.emacs-servers/server ~/.local/state/emacs-servers/server

Connecting through a symlink to a socket works. `server-start` deletes the
symlink and binds a real socket in its place on the next Emacs start, so the
bridge converges rather than lingering.

## After changing any of this

BrishGarden keeps persistent shells, so run `brishz-restart`. Testing in a
fresh `zsh -ic` proves nothing about what the garden — and therefore the kitty
hotkey and the Claude Code hooks — is actually running.
