# API keys for our localhost services

BrishGarden, blackbutler and JupyterGarden are HTTP services bound to
`127.0.0.1`. Until now, that binding was the whole of their access control, and
all three run arbitrary code: BrishGarden evaluates zsh, JupyterGarden evaluates
Python, blackbutler shells out and reads and writes arbitrary paths.

Loopback binding excludes other *hosts*. It does not exclude:

- other users of the same machine, on any shared box;
- a web page you visit, which can make your browser POST to `127.0.0.1`;
- any process that has network access but no business running your shell — a
  sandboxed build step, a compromised dependency, a container on the host
  network.

So every endpoint now requires an API key.

## Where the keys live

One file per service, `~/.keys/<service>`: `brishgarden`, `blackbutler`,
`jupytergarden`. The directory is `0700` and each file is `0600`.

Only BrishGarden actually runs today. JupyterGarden is not installed, and
blackbutler is disabled — see `blackbutler_disabled_p` in
`zshlang/auto-load/others/blackbutler.zsh`. Both are wired up regardless, so
whichever comes back does so with a key already required.

Per-service rather than one shared key, so that rotating or leaking one does not
touch the others, and so each server can generate its own without coordinating.

Each file holds a **complete header line**, not a bare key:

```
X-API-Key: 6Y3gNP1obJYhBZP6MsrG5ZdUx7YRI4UJ7KeUnE5nRvU
```

That is what lets a client send it as `curl --header @~/.keys/brishgarden`. The
format exists for one reason: a key passed as `curl --header "X-API-Key: $key"`
would sit in the process's argv, and `ps` shows argv to **every local user on the
machine** — precisely the people the key is meant to exclude. Reading the file
directly keeps the secret out of argv and out of the environment.

Requiring a custom header, rather than something like a token in the URL or a
cookie, also blocks the browser case structurally: a cross-origin page cannot
send `X-API-Key` without a CORS preflight, which these services never approve.

## Who creates them

Whichever side gets there first; both are idempotent and an existing non-empty
file always wins, so a restart never invalidates keys clients already hold.

- The servers, at boot, via `api_key_ensure()` in `pynight/common_apikey.py`.
- The shell, via [agfi:api-key-get], which the launchers use because they may
  need a key before the server that owns it has finished starting.

```zsh
api-key-get brishgarden   # prints the key, creating the file if absent
```

## Clients

The wrappers attach the key automatically. Each one sends it only when the
endpoint is local (`127.0.0.1` or `localhost`) **and** the key file is readable,
so a machine without a key file behaves exactly as before:

- `zshlang/wrappers/brishz/brishzq.zsh`, `brishz.dash`, `brishzb.dash`
- `zshlang/wrappers/black_butler/bb_say.dash`, `bb_image_to_latex.dash`
- `zshlang/wrappers/jg_eval.sh`

Everything layered on top of those — `brishz2.dash`, `bsh.dash`,
`brishz_para.dash`, [agfi:brishz], the agent hooks, Hammerspoon and kitty
bindings, `lua/pipe.lua` — inherits the key without changes of its own.

To hand-roll a request:

```zsh
curl --header @$HOME/.keys/brishgarden --header 'Content-Type: application/json' \
     --request POST --data '{"cmd":"ec hi","verbose":"0"}' \
     http://127.0.0.1:7230/zsh/
```

Without a key you get `401`. Note that `curl --fail`, which the wrappers use,
hides the response body and merely exits `22`; use `--verbose` or
`--write-out '%{http_code}'` when a call fails and you want to know why.

## Remote access

Remote callers reach the garden through Caddy at `garden.lilf.ir`, and they do
**not** hold the key. They authenticate to Caddy with HTTP basic auth as before;
Caddy then vouches for them by injecting this host's key upstream:

```
route /api/v1/* {
      uri strip_prefix /api/v1
      reverse_proxy localhost:7230 {
              header_up X-API-Key {env.GARDEN_KEY}
      }
}
```

`basicauth` is ordered ahead of the route, so the key is only ever added to a
request that already passed authentication, and `header_up` *overwrites* whatever
the client sent, so nobody can smuggle a key past Caddy.

`GARDEN_KEY` is in Caddy's environment. `launchers/various.zsh` starts Caddy
through [agfi:caddy-run-garden], which reads the key itself and then `exec`s
`caddy` with it, so the key appears in no argv. A process's environment is
readable only by its owner and root, while `ps` shows every argv to every local
user. The launcher used to pass `GARDEN_KEY=<key>` to tmux, which put the key in
the argv of the pane's shell. The remote wrappers (`brishzr.dash`,
`brishzrb.dash`, `brishzrq.dash`) are unchanged and still send only the
basic-auth credentials.

Keys are per host and never leave the host they were generated on. A consequence
worth remembering: if you SSH-tunnel a *remote* garden to a local port, the
wrapper sees `127.0.0.1`, sends the **local** key and gets a `401`. Point
`bshEndpoint` at a hostname that is not `127.0.0.1`/`localhost`, or copy the
remote key over.

`launchers/Caddyfile.json` is an unused alternate that still proxies to `7230`
without injecting anything. If it is ever revived it needs the same header. It
cannot carry a comment saying so: Caddy rejects unknown JSON fields.

## Rotating a key

```zsh
api-key-rotate brishgarden
```

[agfi:api-key-rotate] writes a new random key, replacing the file in one rename,
so a reader sees the old key or the new one, never a partial file. It never
prints a key. Clients read the file on every call, as all the wrappers do, so
they switch at once.

The servers read their key once, at boot, and so does Caddy through
`GARDEN_KEY`. Until they restart, the garden refuses the new key with a `401`,
and Caddy keeps sending the old one. So `api-key-rotate` then restarts every
session in `api_key_holders[<service>]` that runs on this host. For
`brishgarden` these are `BrishGarden`, the garden, and `serve-dl`, Caddy on the
VPS.

- A tmux session gets [agfi:tmux-session-restart]. It kills the pane's
  processes and respawns the pane with its original command, which needs tmux's
  `remain-on-exit on` (set in `~/.tmux.conf`).
- A job that [agfi:tmux2kitty] moved into kitty gets [agfi:tmux2kitty-restart].
  A job moved before tmux2kitty began recording its command cannot be restarted
  this way. Restart it once with its launcher and move it again.
- A holder that does not run on this host is skipped.

It refuses to run inside a Brish worker, or inside a tmux session it would
restart. The restart would kill it after the new key is written but before the
holder had read it, which leaves every client refused. Run it from a terminal.
Expect the garden to be down for a few seconds while it restarts. A holder that
fails to restart is reported, and keeps the old key until you restart it by
hand.

For another service, set its holders first, e.g. in `~/.privateShell`:
`api_key_holders[jupytergarden]='JupyterGarden'`.
