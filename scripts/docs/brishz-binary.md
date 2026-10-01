# Exact bytes through BrishGarden: `brishz_binary=y`

BrishGarden's JSON API carries text, and a garden whose Brish runs in legacy
mode loses bytes on the way: a NUL, a `\r`, invalid UTF-8. The clients add
losses of their own: `brishz.dash` reads stdin with `$(cat)`, which drops
trailing newlines, and its `brishz_quote` mode appends a space to it.

The **opt-in** is the env var `brishz_binary=y`. With it, `brishzq.zsh` and
`brishz.dash` use the garden's binary transport, so the command, stdin,
stdout and stderr all arrive exactly as they were sent. Without it, both
clients send and print exactly what they did before, byte for byte.
Hammerspoon, `lua/pipe.lua` and the agent hooks never set it.

## What the garden needs

A BrishGarden with the binary request fields (`cmd_b64`, `stdin_b64` and
`binary`), running in binary mode. That is the garden's default; a garden
process started with `BRISH_BINARY=0` runs in legacy mode instead. See
"Binary data (opt-in)" in BrishGarden's readme. The mode belongs to the
garden process: `brishz-restart` restarts the zsh workers but keeps the
process's mode, so switching needs a restart of the garden process itself.

## Using it

```zsh
head -c 4096 /dev/urandom > x.bin
brishz_binary=y brishz_in=MAGIC_READ_STDIN brishzq.zsh cat < x.bin | cmp - x.bin
brishz_binary=y brishzq.zsh print -rn -- $'\xff\r' | od -c
brishz_binary=y brishz_in=MAGIC_READ_STDIN brishz.dash 'wc -c' < x.bin
```

`brishz_binary` is parsed like the scripts' `bool`: `n`, `no`, `0` and the
empty string mean off, anything else means on.

- `brishzq.zsh` sends the command in `cmd_b64` and stdin in `stdin_b64`.
  With `brishz_in=MAGIC_READ_STDIN`, stdin is streamed into `base64`, never
  held in memory as text. It writes the decoded `out_b64` to stdout and
  `err_b64` to stderr, and exits with the command's `retcode`. It also sends
  `b64_only: 1`, so a garden that knows that field leaves the duplicate text
  fields `out` and `err` out of its reply; an older garden ignores it.
  `brishz_binary=y` keeps `brishzq.zsh` on this JSON path even with
  `brishz_raw=y`, since only this path guarantees that nothing runs on a
  garden without binary support.
- `brishz.dash` sends the same fields plus `binary: 1`. On the plain reply
  path (the default, `brishz_json_output=0`) it prints the exact bytes of
  stdout followed by stderr. With `brishz_json_output=1` it prints the JSON
  reply, which then also has `out_b64` and `err_b64`. Under the opt-in it
  always encodes the command and stdin itself, as `brishz_quote=y` does, so
  pass them raw rather than JSON-escaped, whatever `brishz_quote` says.
  `brishz2.dash` and `bsh.dash` pass the opt-in through unchanged.
- Remote endpoints (`bshEndpoint`) work too, since stdin travels in the
  request. The text path of `brishzq.zsh` redirects the command's stdin from
  a local temp file, which a remote garden cannot read.
- Pipe into `brishzq.zsh` directly. The zsh function `brishz-in` reads stdin
  with `$(cat)` first, which drops trailing newlines.

Payloads pass through the `base64` binary before `jq` sees them, since `jq`
turns invalid UTF-8 into U+FFFD, and through pipes or temp files, never argv,
which is size-limited and shown by `ps`. The macOS, GNU and Homebrew (John
Walker's) `base64` all work: the garden ignores the line breaks some of them
add. The opt-in path of `brishz.dash` appends `/usr/local/bin` and
`/opt/homebrew/bin` to `PATH` for `jq`, so it works under launchd's bare
`PATH` like the rest.

## A garden without binary support

A binary-mode garden marks every reply to a `binary: 1` request with the
header `X-Brish-Binary: 1`. Every reply is HTTP 200, so the clients look for
that header instead. When it is missing, they print this on stderr, print
nothing on stdout, and exit with status 201:

```
brishzq.zsh: garden lacks binary support (no X-Brish-Binary header); it predates binary mode or runs with BRISH_BINARY=0
```

A garden without binary support has run nothing:

- a garden in legacy mode refuses a `binary: 1` request without running it;
- an older garden ignores the new fields, and the command travels only in
  `cmd_b64`, never also in `cmd`, so it sees an empty command.

`brishz.dash` with `brishz_async=y` reads no reply, so it checks nothing.

A reply that carries the header but is not a command's result, such as the
log of a `%GARDEN_` magic command, is printed as is, and `brishzq.zsh` exits
with 200, as it does without the opt-in.

## The older workarounds

`brishz_out_file_p=y` and `brishz_eval_file_p=y` (in `brishzq.zsh`) and
`bsh.dash` route output or the command through temp files, to get around the
legacy transport's losses. The opt-in supersedes them. They stay for gardens
without binary support, and they still work under the opt-in, but only
against a local garden, since they rely on files on this machine.

## Exact bytes without base64: `brishz_raw=y`

`brishz_raw=y` makes `brishzq.zsh` use the garden's raw API, which carries
the command, stdin, stdout and stderr as bytes, with no JSON, no base64 and
no `jq`. It is exact on a binary-mode garden and falls back to the JSON API
on a garden without the raw API. See [brishz-raw](brishz-raw.md).

## A faster client

`brishzgo` (in `golang/brishzgo/`) is a Go drop-in for `brishzq.zsh` with the
same argv, variables and exit statuses, over the garden's raw API. It honors
`brishz_binary=y` the same way. See [brishzgo](brishzgo.md).
