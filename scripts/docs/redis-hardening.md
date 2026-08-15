# Hardening redis

Redis binds to `127.0.0.1`. That excludes other *hosts*, and nothing else.

On a shared box — the CIS login nodes — it does not exclude the other *users*,
who can otherwise read and write everything in it: shell history, memoi caches,
brishgarden state.

On a single-user laptop like mb2 the threat is different but not absent. Anything
that can reach loopback can talk to redis: a web page you visit, which can make
your browser POST to port 6379; a sandboxed build step; a compromised dependency
in some project's `node_modules`. None of those are "another user", and all of
them get in through a passwordless loopback port. This is the same reasoning as
`docs/api-keys.md`, which is why the localhost HTTP services grew API keys.

Either way, redis needs `requirepass`.

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

It is called at the bottom of `redis.zsh` as well, so every zsh exports the
secret at startup rather than only after its first `redism` call. That is what
covers the callers which never go through `redism` — memoi's write path uses
`redis-cli` directly, and `python/iterm/iterm_focus.py` shells out to it through
brish — since those inherit their shell's environment.

### Generating the secret

`openssl rand -hex 32` when openssl is present, otherwise
`od -An -tx1 -N32 /dev/urandom`.

Both draw from the same kernel CSPRNG: `openssl rand` seeds from
`/dev/urandom`, so neither is "more random" than the other, and the `od` form
was never the weak link. openssl is preferred because it emits the digits
directly, with no whitespace-stripping step to get subtly wrong. The `od`
fallback is POSIX and needs no openssl, which is not guaranteed on a stripped
host.

Hex rather than base64 either way: base64 on some hosts emits CRLF, and a stray
`\r` that `tr -d '\n'` misses would silently become part of the password. `od`
rather than `xxd` because `xxd` ships with vim and is not guaranteed present.

Both calls are `command`-prefixed. `od` and `tr` are exactly the kind of short
name a wrapper is likely to have claimed, and here the surrounding logic depends
on getting the real binary's output format.

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
case for `night-startup-redis` in `setup/bootstrap/stages/70-services.sh`,
which passes everything on the command line — harmlessly, because that path
re-reads `~/.redis-auth` and passes `--requirepass` on every start anyway.

## The other clients

Hardening the server breaks every client that does not send the password, and
`redism` is not the only way we talk to redis. Three kinds of caller, three
fixes:

**Anything descended from a zsh** now inherits `REDISCLI_AUTH`, because
`redis.zsh` calls `h-redis-auth-ensure` at load time. This covers memoi's
`redis-cli` write path and `python/iterm/iterm_focus.py`, which reaches
`redis-cli` through brish.

**POSIX-sh scripts launched by something that is not a shell** source
`sh/redis-auth.sh`, a fork-free snippet that exports the variable and nothing
else:

- `sh/power_from_adapter_event.sh`, `sh/power_from_battery_event.sh` — power
  events hand these launchd's environment.
- `zshlang/wrappers/bicon_zsh.dash` — iTerm launches it with whatever
  environment iTerm itself started with.

`sh/redis-auth.sh` deliberately only *reads* the secret; minting stays in
`h-redis-auth-ensure` alone, so there is only one generator to keep correct.

**Python** uses `libs/redis_client.py`:

```python
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from libs.redis_client import redis_client
r = redis_client()
```

`redis_auth_get()` returns None when the host has no secret, so
`redis_client()` reproduces the old passwordless behaviour exactly and keeps
working against an un-hardened redis.

Adding `~/scripts/python/` to `sys.path` is safe even though
`~/scripts/python/redis/` looks like a package named `redis`: a directory with
no `__init__.py` is only a namespace *portion*, so the import machinery keeps
scanning `sys.path` and `import redis` still resolves to the installed library.
Verified, not assumed.

### bicon's NOAUTH trap

`bicon_zsh.dash` keeps `--raw`, because without it a string reply comes back
quoted and the `test -z "$dis"` check would see a non-empty `""` and disable
bicon unconditionally. The cost of `--raw` is that server-side errors arrive on
**stdout with exit status 0** — `NOAUTH Authentication required.` becomes the
value of `$dis` instead of tripping the `|| dis=y` fallback. That fails in the
safe direction, since any non-empty `$dis` disables bicon, but silently. Do not
"fix" it into a fail-open check.

### Lua

`hammerspoon/core/redis.lua` authenticates in `connectToRedis`, after
connecting. Connecting is not the step that fails: `redis.connect` *succeeds*
against a hardened server, because redis rejects the commands and not the
connection, so an unauthenticated client looks healthy while every write returns
NOAUTH — nulling the client and scheduling a reconnect on each hyper-key press.

The `client:auth()` call is wrapped in `pcall`. AUTH against a server with *no*
password configured is an error, and letting it propagate would turn a working
unprotected redis into a connection that retries forever. A genuinely wrong
secret still surfaces through the existing reconnect path.

Worth knowing when editing this file: `init.lua` loads the core files with a
bare `dofile` loop and no `pcall`, so an error raised in `core/redis.lua` takes
down every file listed after it — wifi-watcher, hyper-mode, mouse, app-hotkeys,
stt, reload. Reload deliberately and check, rather than assuming a Lua error
would be local to the file that raised it.

### Why the environment, not `-a`

A password in the environment is visible in `/proc/PID/environ`, which is
owner-readable only, so exporting it does not leak it to other users on Linux.
It is *not* in argv, which `ps` shows to everyone — that is why every caller
here uses `REDISCLI_AUTH` rather than `redis-cli -a`. Same reasoning as the
`curl --header @file` convention in `docs/api-keys.md`.

## Applying it

After changing any of this, run `brishz-restart`: BrishGarden keeps persistent
zsh shells and does not see zshlang edits on its own.

On a host bootstrapped by `setup/bootstrap`, stage 45 already generates
`~/.redis-auth` (skipped only where the profile declares `NIGHT_MULTIUSER=n`)
and stage 70 starts redis with it, so both halves are covered on the next start.
Anywhere redis is started by brew, systemd or the distro, `redis-harden` is the
step that actually protects it.
