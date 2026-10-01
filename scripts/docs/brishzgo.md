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
  Any non-empty value of the last two is true, even `n` or `0`: the JSON API
  gets the value as it is and takes any non-empty string as true, and the
  raw request sends `1`, as `brishzq.zsh` does.
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

Two replies to the raw request mean that nothing ran, and send the
request to the JSON API instead (the **fallback**):

- HTTP 404 or 405, from a garden older than the raw API;
- `X-Brish-Refused: 1`, which the garden sets only on a request it refused
  before running anything: a malformed one, or, in legacy mode
  (`BRISH_BINARY=0`), a command or stdin that is not valid UTF-8 or holds a
  NUL. A reply without the header is the command's own, even with status
  9000 and an error on stderr, so a command that ran is never sent twice.

The fallback sends the JSON API the request `brishzq.zsh` sends there, so a
garden of any version and mode runs what it runs for `brishzq.zsh`:
`json_output: 1`, the command as `cmd`, and stdin as `stdin`, or for
`MAGIC_READ_STDIN` in a temp file that the command reads with
`< file { ... }`. The temp file carries any bytes, NUL and invalid UTF-8
included, to a garden on this machine, and is removed when `brishzgo` exits,
also on SIGHUP, SIGINT and SIGTERM. Invalid UTF-8 in `cmd` or `stdin`
becomes U+FFFD, the way jq replaces it.

The fallback has all of stdin even though the raw request streamed it. A
refusing garden read all of it, and the raw request keeps a copy of what it
sent, in memory up to 16 MiB and in a temp file beyond that, which the
fallback resends first, in order; the transport stops reading our stdin the
moment the fallback starts. A garden without the raw API normally gets none
of it: the raw request carries `Expect: 100-continue` and
`Connection: close`, so that garden answers 404 before any of stdin is
sent, and Go's transport sends none afterwards (to keep a connection open,
it would send the body after the 404). Some can still go out first, to a
server that asks for the body before answering 404 (a buffering proxy), or
past the transport's 2 s wait for a 100 Continue. The fallback costs one
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
- For `MAGIC_READ_STDIN` to an endpoint that is not on this machine (one that
  does not match `^https?://(127\.0\.0\.1|localhost)`), the JSON request
  carries stdin itself, where `brishzq.zsh` names a temp file that such a
  garden cannot read: as `stdin` when it is valid UTF-8, else as `stdin_b64`
  with the command as `cmd_b64`, so that a garden older than those fields
  runs nothing.
- `brishzq.zsh`'s jq (1.6) reads a literal `brishz_in` line by line: invalid
  UTF-8 cut short by a newline takes the newline with it, and a character
  that straddles a 4095-byte boundary of a long line becomes two U+FFFD.
  `brishzgo` decodes it whole.
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
