# `brishzgo`: a Go drop-in for `brishzq.zsh`

`brishzgo` runs one zsh command in a BrishGarden and passes on its stdout,
stderr and exit status. It takes the same argv and environment variables as
`brishzq.zsh` and exits with the same statuses, but it is one static binary:
no `zsh -f` start, no `jq`, no `base64`, no `curl`. It talks to the garden's
**raw API** (`POST /zsh/raw/`, bytes as bytes), and falls back to the JSON API
for a garden that has none.

The source is `golang/brishzgo/`; its `readme.org` covers building and the
tests. Nothing calls it yet: Hammerspoon, `lua/pipe.lua`, the agent hooks and
the zsh functions all still run `brishzq.zsh` or `brishz.dash`. Switching a
caller is a separate decision.

## Install

```zsh
go-install-local "${NIGHTDIR}/golang/brishzgo"   #: into ~/go/bin
```

`setup/setup_go` does the same on a full setup run. The module is standard
library only, so the build needs no network.

## Use

Exactly like `brishzq.zsh`:

```zsh
brishzgo print -r -- ok
brishz_in=MAGIC_READ_STDIN brishzgo cat < x.bin | cmp - x.bin
brishz_session=demo brishzgo typeset -g x=1
```

A leading `-c` is dropped, as `brishzq.zsh` does.

### The command text

The words become one command line, quoted as `brishzq.zsh`'s `gquote` does:

- the first word stays bare when it is made only of ASCII letters, digits
  and `_ . / , : @ % + -`, so aliases, functions and reserved words still work
  in command position; otherwise it is single-quoted;
- every other word is single-quoted, as zsh's `${(qq)...}` does;
- no words at all give `''`.

For a local endpoint (one matching `^https?://127.0.0.1`, with the dots
unescaped, exactly as in `brishzq.zsh`), the command is wrapped the same way:
a subshell that marks itself with `mark-me 'BRISHZQ_MARKER' ...`, `cd`s to our
working directory, runs the command, then `cd /tmp` and `return-code`s its
status. With `NIGHT_EMACS_P` set, `NIGHT_EMACS_P`, `EMACS_SOCKET_NAME` and
`emacs_night_server_name` are forwarded as `local -x NAME=VALUE` lines,
quoted as `typeset -p` quotes them. `brishz_noquote` set means the words are
only joined by spaces, with no wrapping and no forwarding.

### Environment variables

- `bshEndpoint`, else `http://127.0.0.1:${GARDEN_PORT:-7230}`; `/zsh/` is
  appended.
- `brishz_in`: the command's stdin. `MAGIC_READ_STDIN` streams our own stdin
  into the request; any other value is the stdin itself; unset means empty.
- `brishz_session`, `brishz_nolog`, `brishz_failure_expected`: as in
  `brishzq.zsh`. A non-empty `brishz_nolog` also picks the `nolog/` route.
- `brishz_noquote`: see above.
- `brishz_binary`: the exact-bytes opt-in of [brishz-binary](brishz-binary.md),
  parsed like the scripts' `bool`. See "Transport" for what it changes here.
- `brishz_raw`: the raw API, on unless set to a false value (`n`, `no`, `0`).
  `brishz_raw=n` uses the JSON API directly.
- `brishz_debug`: a true value prints the request and reply headers and the
  command text to stderr. The values of the API key file's headers and of
  `Authorization` are printed as `<redacted>`.
- `DISABLE_BRISH=y`: exit 1 at once, as `brishzq.zsh` does.

Authentication follows `brishzq.zsh`: for an endpoint matching
`^https?://(127\.0\.0\.1|localhost)`, the header lines of
`~/.keys/brishgarden` are sent as `curl --header @file` would send them, and
never logged; an endpoint whose URL contains `garden` gets basic auth as
`Alice` with `$GARDEN_PASS0`.

## Transport

The raw request's body is the command's bytes, then stdin's, with
`X-Brish-Cmd-Length`; the options go in the query string. With
`MAGIC_READ_STDIN`, stdin is streamed (chunked), never read whole into memory
first, and the reply's stdout part is streamed to our stdout and the rest to
our stderr. The exit status is `X-Brish-Retcode`.

A garden older than the raw API answers 404 (or 405) and runs nothing; then
`brishzgo` sends the same command to the JSON API (`json_output: 1`, `cmd`
and `stdin` as strings, or `cmd_b64` and `stdin_b64` when either is not valid
UTF-8). The raw request carries `Expect: 100-continue` and `Connection: close`
when it streams stdin. So such a garden answers before any of stdin is sent,
and Go's transport sends none of it afterwards (to keep a connection open, it
would send the body after the 404). Some of stdin can still go out first: to
a server that asks for the body before answering 404 (a buffering proxy), or
past the transport's 2 s wait for a 100 Continue. The fallback then resends
it from a copy, in order, and the transport stops reading our stdin the
moment the fallback starts. Up to 16 MiB is kept that way; beyond that the
fallback fails with exit status 1 and nothing runs. The fallback costs one
extra round trip, about 3 ms on this machine.

`brishz_binary=y` goes straight to the JSON API's binary transport (`cmd_b64`,
`stdin_b64`, `binary: 1`, and `b64_only: 1` for the smaller reply), as
`brishzq.zsh` does. That opt-in promises that a garden without binary mode
runs nothing, and the raw API cannot keep the promise: a garden in legacy
mode serves it, runs the command through text, and only says so in its
`X-Brish-Binary: 0` reply header. Without the opt-in, the raw API of a
binary-mode garden (the default) is exact anyway.

## Exit statuses

The same as `brishzq.zsh` for every outcome:

- the command's own status, from the garden (9000, a refused request, exits
  as 40, as `exit 9000` does in zsh);
- 200: a reply that is not a command's result, such as a notice (an empty
  command, a `%GARDEN_` magic command), printed with its trailing newlines
  replaced by one;
- 201: `brishz_binary=y` and no `X-Brish-Binary: 1` in the reply, with the
  same message on stderr; nothing ran;
- curl's exit codes for a failed request, which `brishzq.zsh` passes on:
  7 connection refused, 6 host not resolved, 28 timeout (curl's default
  connect timeout of 300 s, no overall one), 22 an HTTP status of 400 or more
  (a missing key gets 401 or 403), 52 an empty reply, 18 a reply shorter than
  announced, 35 and 60 TLS failures, 47 too many redirects, 1 an unsupported
  scheme. Like `curl --silent`, these print nothing; `brishz_debug=y` shows
  the error.

## Where it differs from `brishzq.zsh`

- The first word: `brishzq.zsh` quotes it with zsh's `(q+)`, which also leaves
  `!` and printable non-ASCII bare and writes `$'...'` for control
  characters. `brishzgo` uses the rule above. Both quote correctly; only the
  text differs, for first words like `x!y`, `é` or one with a newline.
- Forwarded variables: a value with a character outside ASCII that is not
  printable is written with one `\M-` escape per byte, where zsh's `typeset -p`
  writes `\uXXXX` (refused by a shell in the C locale) or a single `\M-` byte
  for U+0080 to U+00FF (which loses a byte). The value arrives intact either
  way; zsh's own text does not always give it back.
- `brishz_out_file_p`, `brishz_eval_file_p`, `brishz_copy` and
  `brishz_summary_p` are ignored. The first two work around the legacy
  transport's losses, which the raw API does not have.
- `GARDEN_PASS0` comes from the environment only: `brishzgo` does not source
  `~/.privateShell`, as `brishzq.zsh` does. A remote endpoint through the proxy
  needs it exported.
- Debug output goes to stderr, never stdout.
- The JSON fallback sends `MAGIC_READ_STDIN` input in the request rather than
  in a temp file, so it also works against a remote garden.
- A JSON reply that is valid JSON but not a command's result is a notice
  (exit 200); `brishzq.zsh` would print its `.out` as `null`.

## Measurements

On 2026-10-01, against a smoke garden from BrishGarden's `raw-endpoint`
branch (binary mode, 4 workers) on a loaded laptop (load average 22 to 28),
wall time per call including process start, 200 interleaved rounds of
`print -r -- ok` after 10 warm-ups:

- `brishzgo`: p50 36 ms, p90 65 ms;
- `brishzgo` with `brishz_raw=n` (JSON API): p50 35 ms, p90 77 ms;
- `brishzq.zsh`: p50 169 ms, p90 245 ms;
- `brishz2.dash`: p50 58 ms, p90 110 ms (it has no `mark-me` wrapper).

Most of `brishzgo`'s 36 ms is the garden's side and the local wrapper: the
same call through `localhost` (no wrapper) took p50 16 ms, the same as
`curl` straight to the raw API, and a call to a closed port, which measures
only the client, p50 10 ms (`/usr/bin/true` takes 3 ms to spawn here).

1 MiB of random bytes through `cat`, 20 rounds, every one exact:

- `brishzgo` (raw API): p50 731 ms, p90 1058 ms;
- `brishzgo` with `brishz_binary=y` (JSON binary): p50 1040 ms, p90 1470 ms;
- `brishzq.zsh` with `brishz_binary=y`: p50 2417 ms, p90 3671 ms;
- `brishz2.dash` with `brishz_binary=y`: p50 1314 ms, p90 1765 ms.
