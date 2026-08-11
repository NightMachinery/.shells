# Hardening redis

Redis binds to `127.0.0.1`. That excludes other *hosts*; it does not exclude the
other *users* of the same machine, who can otherwise read and write everything
in it — shell history, memoi caches, brishgarden state. On any box where someone
else can log in, redis needs `requirepass`.

## The two halves

Hardening is two separate things, and it is easy to do only the first and
believe you are done:

1. **A secret exists and our clients send it.** `~/.redis-auth`, mode 600,
   exported as `REDISCLI_AUTH`, which `redis-cli` picks up on its own.
   This is `h-redis-auth-ensure`.
2. **The server demands it.** `requirepass` on the running server, and on the
   next one it starts as. This is `redis-harden`.

Half 1 alone buys **no security at all**. It only makes our clients present a
password to a server that is still happily accepting anonymous connections from
every local user. Worse, it is not free: against an unprotected server,
`redis-cli` prints

```
AUTH failed: ERR AUTH <password> called without any password configured ...
```

to stderr on *every single call*, while returning the right answer. So a host
that has half 1 without half 2 is both insecure and noisy.

## h-redis-auth-ensure

Called from `redism` and `ensure-redis`, so it runs on essentially every redis
access. It:

- exports `REDISCLI_AUTH` from `~/.redis-auth` if the variable is unset — this
  is what heals shells that started *before* the password existed, and processes
  that inherited a stale environment, instead of failing every call with NOAUTH;
- generates `~/.redis-auth` if it does not exist at all.

It is on a hot path, so it is written to cost one `test` per call once the
variable is set, and it remembers a failed generation attempt in
`h_redis_auth_attempted` rather than forking `od` on every subsequent call.

Generation writes 32 bytes of `/dev/urandom` as **hex**, not base64. base64 on
some hosts emits CRLF, and a stray `\r` that `tr -d '\n'` misses would silently
become part of the password — see `docs/` history and the linuxbrew base64 on
the VPS. `od` is used rather than `xxd` because `xxd` ships with vim and is not
guaranteed present.

The write is `write-to-temp` then `ln`, not a plain redirect. `ln` fails if the
target exists, atomically and over NFS, so two shells racing on the shared CIS
home cannot end up believing in two different secrets; the loser adopts the
winner's file. Set `redis_auth_generate_disable=y` to turn generation off.

## redis-harden

Applies the secret to the running server:

- probes `CONFIG GET requirepass` **with** the secret already in the
  environment. An unauthenticated probe cannot distinguish "no password is set"
  from "a password is set and we did not send it" — both come back empty.
  Sending it against a passwordless server is harmless: redis rejects the AUTH,
  says so on stderr, and the connection keeps working;
- refuses to act if redis already requires a *different* password, rather than
  overwriting it;
- `CONFIG SET requirepass`, which takes effect at once and does **not** drop
  existing connections. Already-connected clients keep working; every *new*
  connection without the secret gets NOAUTH;
- `CONFIG REWRITE`, then **`chmod 600` on the config file**. This last step is
  not optional. `CONFIG REWRITE` writes the password in plaintext and leaves the
  file's mode alone, and Homebrew ships `redis.conf` as `644` — so on exactly
  the multi-user hosts this is meant to protect, the naive rewrite hands the
  secret to every local user through a world-readable file.

`CONFIG REWRITE` fails when redis was started without a config file. That is the
case for `night-startup-redis` in `setup/bootstrap-sudoless/stages/70-services.sh`,
which passes everything on the command line — harmlessly, because that path
re-reads `~/.redis-auth` and passes `--requirepass` on every start anyway.

## What is still not covered

`h-redis-auth-ensure` only reaches clients that go through `redism` or that
inherit `REDISCLI_AUTH` from a shell which ran it. Hardening the server breaks
everything else. Known clients that do not go through it:

- `python/redis/redis_smembers0.py` and `python/redis/redis-delete-idle.py`
  construct `redis.StrictRedis(host='localhost', port=6379, db=0)` with no
  password. These fail outright against a hardened server.
- `zshlang/wrappers/bicon_zsh.dash` calls `redis-cli --raw get` directly. With
  `--raw`, a NOAUTH error arrives on *stdout with exit status 0*, so its
  `|| dis=y` fallback does not trigger; `dis` becomes the error string, which is
  non-empty, so bicon silently switches off. Fails safe, but silently.
- `sh/power_from_adapter_event.sh` and `sh/power_from_battery_event.sh` call
  bare `redis-cli set`. These run from power events, whose environment is
  whatever launchd handed them.
- `python/iterm/iterm_focus.py` shells out to `redis-cli set` via brish, so it
  inherits the garden's environment.
- `hammerspoon/core/redis.lua` calls `redis.connect('127.0.0.1', 6379)` with no
  AUTH. Connecting still succeeds against a hardened server — redis only
  rejects the *commands* — so its retry logic sees a live client whose every
  `set` fails. Today this is masked by `redisModalityUpdateP = false`, which
  makes `redisSetMode` return before touching redis at all; turning that flag
  on without adding AUTH would fail a write, null the client and schedule a
  reconnect on every hyper-key press.

These all work today only because they inherit `REDISCLI_AUTH` from an
environment that had it. On a host where the secret is minted *after* those
processes started, they break until restarted.

Also note that a password in the environment is visible in `/proc/PID/environ`,
which is owner-readable only — so exporting it does not leak it to other users
on Linux. It is not in argv, which `ps` does show to everyone; that is why
`REDISCLI_AUTH` is used rather than `redis-cli -a`.

## Applying it

After changing any of this, run `brishz-restart`: BrishGarden keeps persistent
zsh shells and does not see zshlang edits on its own.

On a host bootstrapped by `setup/bootstrap-sudoless`, stage 45 already generates
`~/.redis-auth` (skipped only where the profile declares `NIGHT_MULTIUSER=n`)
and stage 70 starts redis with it, so both halves are covered on the next start.
Anywhere redis is started by brew, systemd or the distro, `redis-harden` is the
step that actually protects it.
