# Hardening and watching sshd on a Termux phone

`termux-harden HOST` installs a key-only sshd policy on a Termux host, drops
its password credential, and puts sshd under `termux-services` so that
authentication is finally recorded. `termux-ssh-list HOST` shows who is
connected now; `termux-ssh-log HOST` shows who has connected before.
`tealy-harden`, `tealy-ssh-list` and `tealy-ssh-log` are the phone's wrappers,
in the same shape as `tealy-alarm` in `./docs/remote-termux-alarms.md`.

```zsh
termux_harden_dryrun=y tealy-harden   #: print the drop-in, change nothing
tealy-harden
tealy-ssh-list
tealy-ssh-log 40
```

## What a stock Termux sshd gives you

Nothing, deliberately: `sshd_config` there is three lines, and everything else
is an OpenSSH default. That means passwords are accepted, every interface is
bound, and `MaxAuthTries` is per-connection, so an attacker who reconnects gets
unlimited guesses. If `passwd` was ever run, password login genuinely works and
is the weakest thing on the host.

## fail2ban is not available, and not needed

It is not packaged for Termux, and building it would not help: an unrooted
Android exposes no `iptables` and no `nft`, so there is nothing to ban with,
and OpenSSH dropped tcpwrappers in 6.7, so `hosts.deny` is gone too.

OpenSSH 9.8 added `PerSourcePenalties`, which does the same job inside the
listener with no privileges at all. It is on by default. The drop-in raises
`authfail` to 30s and the cap to 1800s, and sets **`PerSourceNetBlockSize
24:64`**, which is the line that matters: at the stock `32:128` an attacker
holding a /64 rotates source addresses for free and never accrues a penalty at
all. Penalties show up in the log as `srclimit_penalise`.

## Source filtering, not ListenAddress

sshd keeps binding `0.0.0.0`. Access is restricted by a `Match Address` block
that refuses everything outside `$termux_harden_trusted_cidrs` (the Tailscale
CGNAT range plus RFC1918 by default).

Binding to the VPN address instead is the obvious alternative and it is worse.
The bind fails outright whenever sshd starts while the VPN happens to be down,
which turns a routine toggle into an unreachable phone; and it breaks the LAN
route used when the phone is the hotspot. Refusing by source costs only that
the port answers before refusing.

`RefuseConnection` (OpenSSH 9.8+) is preferred inside that block: it drops the
connection *before* authentication, does not depend on the login name, and
earns the source a penalty. `AllowUsers` with a CIDR list only refuses after a
username has been offered. `termux-harden` probes for it with `sshd -t` and
falls back to `DenyUsers *` where it is not supported.

The drop-in is only left in place if `sshd -t` accepts it; otherwise the
previous file is restored and the run fails, so a bad config can never be what
the daemon restarts into.

## The logging gap this exists to close

A hand-started `sshd` logs through syslog, and an unprivileged Android app
cannot read logcat, so those events go nowhere. Meanwhile the `termux-services`
unit sits `down` with its `svlogd` attached to a file that stays empty. The
result is a host with no authentication log whatsoever, which reads exactly
like a quiet one.

`termux-harden` fixes this by running sshd under the service (`sshd -D -e`, so
stderr reaches `svlogd`) and setting `LogLevel VERBOSE`, which records the key
fingerprint of each accepted login. Without VERBOSE the log can say that
someone got in but not who. `sv-enable` also clears the service's `down` file,
so the daemon returns after a reboot.

An empty `termux-ssh-log` is therefore a finding, not a quiet day, and it says
so.

## Why these read /proc directly

`ss`, `netstat` and `/proc/net/tcp` are all denied to apps from Android 10 on,
so there is no socket table to consult. `termux-ssh-list` walks `/proc` and
reads `SSH_CONNECTION` out of each process environment instead.

Two traps are baked into it:

- **`sshd` and `sshd-session` inherit a stale `SSH_CONNECTION`** from whatever
  shell started the daemon. Reading it off them reports a peer from whenever
  that was, which on a hand-started daemon can be months ago and a port the
  host no longer even listens on. Only the process *under* a session carries a
  value set for that session, so the daemon and its per-connection children are
  skipped.
- **Termux does not set the OpenSSH process title**, so the command line is a
  bare `sshd-session -R` and cannot be parsed for a peer either.

A peer outside the trusted ranges is called out in the output. Globally
routable IPv6 counts as untrusted; ULA does not, because that is where
Tailscale puts its own v6 addresses.

## Keeping the phone reachable at all

Doze will drop the host off the network mid-session. `termux-wake-lock` on the
phone holds a partial wakelock and stops that, at a real cost in battery;
`termux-wake-unlock` releases it. Exempting both Termux and the VPN client from
battery optimisation is a separate, GUI-only step, and without it Android can
still freeze the tunnel while Termux holds its wakelock.

`ps`, `pgrep` and `pkill` appear to hang on a dozing phone. They are fine once
it is awake; the timeouts are the network dropping, not the tools. `termux-harden`
uses `pgrep -x sshd` to find the master, exact-name rather than `-f`, so it
cannot match the `sshd-session` carrying the very connection it is running on.

## Verifying it took

```zsh
ssh tealy 'sshd -T | grep -Ei "passwordauth|authenticationmethods|persource|loglevel"'

#: Which methods does the server actually offer? No password is sent.
ssh -o ControlPath=none -o BatchMode=yes -o PubkeyAuthentication=no \
    -o NumberOfPasswordPrompts=0 tealy true      #: want: Permission denied (publickey).
```

`ControlPath=none` is not optional in that second command. Without it ssh
reuses the multiplexed connection, never authenticates at all, and the probe
appears to succeed no matter what the server would have done.

To see the `Match` block resolve per source without connecting from there:

```zsh
ssh tealy 'sshd -T -C addr=8.8.8.8,user=$(id -un),host=x | grep -i refuseconnection'
```
