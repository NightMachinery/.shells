# Exact bytes through BrishGarden: `brishz_binary=y`

BrishGarden's JSON API carries text, and a garden whose Brish runs in legacy
mode loses bytes on the way: a NUL, a `\r`, invalid UTF-8. The clients add
losses of their own: `brishz.dash` reads stdin with `$(cat)`, which drops
trailing newlines, and its `brishz_quote` mode appends a space to it.

The **opt-in** is the env var `brishz_binary=y`. With it, `brishzq.zsh`,
`brishz.dash` and `brishzgo` promise exact bytes both ways: the command,
stdin, stdout and stderr all arrive exactly as they were sent. Without it,
the clients send and print exactly what they did before, byte for byte.
Hammerspoon, `lua/pipe.lua` and the agent hooks never set it.

## What the garden needs

A BrishGarden running in binary mode. That is the garden's default; a
garden process started with `BRISH_BINARY=0` runs in legacy mode instead. See
"Binary data (opt-in)" and "Raw API" in BrishGarden's readme. The mode
belongs to the garden process: `brishz-restart` restarts the zsh workers but
keeps the process's mode, so switching needs a restart of the garden process
itself.

## Using it

```zsh
head -c 4096 /dev/urandom > x.bin
brishz_binary=y brishz_in=MAGIC_READ_STDIN brishzq.zsh cat < x.bin | cmp - x.bin
brishz_binary=y brishzq.zsh print -rn -- $'\xff\r' | od -c
brishz_binary=y brishz_in=MAGIC_READ_STDIN brishz.dash 'wc -c' < x.bin
brishz_binary=y brishz_in=MAGIC_READ_STDIN brishzgo cat < x.bin | cmp - x.bin
```

`brishz_binary` is parsed like the scripts' `bool`: `n`, `no`, `0` and the
empty string mean off, anything else means on.

## How it travels

The shell clients first send the request to the garden's **raw API**
(`POST /zsh/raw/`); `brishzgo` defaults to the
**streaming API** (`POST /zsh/stream/`). Both carry the command, stdin,
stdout and stderr as bytes, with no JSON and no base64 (see
[brishz-raw](brishz-raw.md) and [brishzgo](brishzgo.md)). The
opt-in adds the query option `binary=1`, which asks for exact bytes: a
binary-mode garden replies exactly anyway, and marks the reply with
`X-Brish-Binary: 1`; a legacy-mode garden refuses the request without
running it (`X-Brish-Refused: 1`).

When the garden ran nothing (it refused the request, or it lacks that API and
answered HTTP 404 or 405), the client sends the request again to the JSON
API's **binary transport** (`POST /zsh/`): the command in `cmd_b64`, stdin in
`stdin_b64`, and `binary: 1`, which a legacy-mode garden refuses there too.
That retry is the request the client sent before the raw path existed, with
the same stdin bytes. Payloads pass through the `base64` binary before `jq`
sees them, since `jq` turns invalid UTF-8 into U+FFFD, and through pipes or
temp files, never argv, which is size-limited and shown by `ps`. The macOS,
GNU and Homebrew (John Walker's) `base64` all work: the garden ignores the
line breaks some of them add.

Options that need the JSON API go straight to the binary transport, as they
always did: `brishz_raw=n` (for `brishzgo`, also set `brishz_stream=n` to
skip its default streaming request),
`brishzq.zsh`'s `brishz_out_file_p` and `brishz_eval_file_p`, and
`brishz.dash`'s `brishz_json_output` other than `0`.

- `brishzq.zsh` handles an exact raw reply as it handles its raw replies
  without the opt-in: stdout and stderr byte for byte, the command's exit
  status, and exit 200 for a notice. With `brishz_in=MAGIC_READ_STDIN` it
  reads stdin into memory first (a zsh variable holds any byte, NUL
  included), since stdin can be read only once and a fallback sends the
  same bytes again. It reads it with `cat`, not with a bare redirection
  such as `</dev/stdin`, which zsh runs through `$READNULLCMD` (`more`),
  where a `LESSOPEN` preprocessor or an exported `READNULLCMD` could change
  the bytes. A stdin it cannot read (a directory, say) sends nothing, and
  the client exits 1. On the JSON path it reads only `out_b64`, `err_b64`
  and `retcode`, and sends `b64_only: 1`, so a garden that knows that field
  leaves the duplicate text fields `out` and `err` out of its reply; an
  older garden ignores it. With `brishz_raw=n` or a file option, stdin is
  streamed into `base64` and never held in memory; after a raw fallback,
  the bytes already read are sent again.
- `brishz.dash` sends the raw request on its plain reply path (the default,
  `brishz_json_output=0`), with `merge=1`, so the garden merges stderr into
  stdout in the shell, as the JSON API's plain path does. It prints the
  exact bytes of that output and keeps that path's exit statuses: 0 once
  the reply has come, whatever the command's own status, 201 as below, and
  curl's own status for a failed request. A fallback's stdin waits in a
  temp file. With `brishz_json_output=1` it uses the JSON API with
  `binary: 1` as before and prints the JSON reply, which then also has
  `out_b64` and `err_b64`. Under the opt-in it always encodes the command
  and stdin itself, as `brishz_quote=y` does, so pass them raw rather than
  JSON-escaped, whatever `brishz_quote` says. `brishz2.dash` and `bsh.dash`
  pass the opt-in through unchanged. The opt-in path appends
  `/usr/local/bin` and `/opt/homebrew/bin` to `PATH` for `jq`, so it works
  under launchd's bare `PATH` like the rest.
- `brishzgo` sends the streaming request (`POST /zsh/stream/`) by default,
  or the raw request with `brishz_stream=n`, with `binary=1`. It handles an
  exact reply as without the opt-in. A refusal, or a 404 or 405 from either API, sends
  it to the JSON API's binary transport. See [brishzgo](brishzgo.md).

Remote endpoints (`bshEndpoint`) work too, since stdin travels in the
request. Pipe into the client directly, or use [agfi:brishz-in], which now
passes stdin directly to the Go client without dropping trailing newlines.

## What each garden does

What a client with the opt-in gets, by the garden's generation and mode:

- **Binary mode**, with the raw API: an exact reply. A garden older than
  `binary=1` ignores the option, but it is exact anyway.
- **Binary mode**, older than the raw API but with the JSON binary fields
  (BrishGarden 42ddc9d, say): the raw request gets 404, and the JSON
  binary transport gives the exact reply. This costs one extra round trip.
- **Older than the binary fields** (ec61c63, say): the raw request gets
  404; the JSON API ignores `cmd_b64`, sees an empty command and runs
  nothing. Exit 201.
- **Legacy mode**, from BrishGarden 8571467 on: it refuses `binary=1` on the
  raw API and `binary: 1` on the JSON API, and runs nothing. Exit 201.
- **Legacy mode**, with the raw API and `X-Brish-Refused` but older than
  `binary=1` (BrishGarden 0fd2752 until 8571467, such as 7de4a42): it
  ignores the option and **runs the command in text mode**, and its reply
  says `X-Brish-Binary: 0`. The client withholds the output and exits 201
  (see below). A command or stdin with a NUL or invalid UTF-8 is still
  refused there with `X-Brish-Refused: 1`, and the JSON retry is refused
  too, so that one runs nothing.
- **Legacy mode**, with the raw API but older than `X-Brish-Refused`
  (BrishGarden d0a9514 until 0fd2752): the same, except that such a garden
  refuses a command or stdin with a NUL or invalid UTF-8 with retcode 9000
  and no refusal header. That looks like a command that ran and returned
  9000, so nothing runs, but the client cannot tell, and says so.
- **Legacy mode**, older than the raw API: the raw request gets 404, and
  the JSON API refuses `binary: 1`. Exit 201; nothing ran.

On a garden without the raw API, a `brishz_nolog` request still leaves an
access-log line, for its 404.

## A garden without binary support

So the guarantee is: exact bytes, or nothing runs, or the output is
withheld; in the last two cases the client exits with status 201 and prints
nothing on stdout. Only a legacy-mode garden older than the `binary=1`
option (BrishGarden before 8571467) can still run the command, in text mode.

When nothing ran, the JSON reply lacks the header `X-Brish-Binary: 1`, and
the client prints this on stderr (with its own name):

```
brishzq.zsh: garden lacks binary support (no X-Brish-Binary header); it predates binary mode or runs with BRISH_BINARY=0
```

A raw or streaming reply that is neither refused nor marked
`X-Brish-Binary: 1` comes from a legacy-mode garden older than the
`binary=1` option, which ran the command in text mode. The client then
prints nothing on stdout and never prints the reply's stdout or stderr,
since they may have lost bytes. It prints one line on stderr that names the
retcode as the garden reported it (`unknown` when the reply has none):

```
brishzq.zsh: garden ran the command in text mode (no X-Brish-Binary: 1); it predates the binary=1 option and runs with BRISH_BINARY=0; output withheld; retcode 3
```

All three clients print the same line, with their own name, and choose
between three openings:

- `garden ran the command in text mode`, as above;
- `garden answered in text mode with a notice, not a command's output`,
  when the reply is a notice (an empty command, a `%GARDEN_` magic
  command's log), which is withheld too;
- `garden ran the command in text mode, or refused it and ran nothing`,
  when a raw reply's retcode is 9000, since a garden older than
  `X-Brish-Refused` (0fd2752) refuses input that text mode cannot carry
  with retcode 9000 and nothing marks it. Streaming replies never get this
  opening: the streaming API came after `X-Brish-Refused`.

`brishzgo` with streaming enabled (the default) reads the stream to its exit
frame first, dropping the output, so that its going away does not kill the command half
way.

`brishz.dash` with `brishz_async=y` reads no reply, so it checks nothing:
on a legacy-mode garden older than `binary=1`, its command runs in text mode
unnoticed.

A reply that carries `X-Brish-Binary: 1` but is not a command's result, such
as the log of a `%GARDEN_` magic command, is printed as is, and
`brishzq.zsh` and `brishzgo` exit with 200, as they do without the opt-in.

## The older workarounds

`brishz_out_file_p=y` and `brishz_eval_file_p=y` (in `brishzq.zsh`) and
`bsh.dash` route output or the command through temp files, to get around the
legacy transport's losses. The opt-in supersedes them. They stay for gardens
without binary support, and they still work under the opt-in, over the JSON
API's binary transport, but only against a local garden, since they rely on
files on this machine.

## Speed

On 2026-10-04, `brishzq.zsh` against a binary-mode smoke garden on this
laptop (load average about 4 to 6), wall time per call including the zsh
start, interleaved:

- `print -rn -- ok`, 100 calls after 10 warmups: with the opt-in p50 25 ms
  (p90 37 ms), against 43 ms (53 ms) over the JSON binary transport it used
  before; without the opt-in p50 25 ms.
- `abc` through `cat` as stdin, 100 calls after 10 warmups: with the opt-in
  p50 34 ms, against 51 ms before; without the opt-in p50 35 ms.
- 1 MiB through `cat`, 20 calls: text p50 215 ms with the opt-in, against
  421 ms before; random bytes p50 364 ms, against 559 ms. Without the
  opt-in, the raw path took p50 201 ms and 429 ms.

The raw path runs only `curl` (and `cat` for `brishz_in=MAGIC_READ_STDIN`);
the JSON binary transport adds four `base64` runs (two encodes, two
decodes), two `jq` runs, a temp file for the reply's headers and, for that
file's cleanup, a `perl` run.

`brishz.dash`, the same day, against another binary-mode test garden: an inert opt-in call took p50
17.5 ms over the raw API, against 23 ms over the JSON API before (100
interleaved calls each; 22 against 29 ms and 34 against 43 ms in two busier
runs), and 9 ms without the opt-in. The foreground of a `brishz_async=y`
call fell from 12 to 5 ms.

`brishzgo`, the same day (load average 3 to 4, interleaved, 10 warmups):
an inert opt-in call, 200 rounds, took p50 10.5 ms over either API, as
before, since it already built its JSON request in process. 1 MiB through
`cat`, 30 rounds: p50 93 ms over the raw API and 30 ms over the streaming
API, against 125 ms over the JSON API before. Without the opt-in it took
92 ms and 33 ms, so `binary=1` itself costs nothing measurable.
