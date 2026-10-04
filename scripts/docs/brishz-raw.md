# `brishzq.zsh` over the raw API

BrishGarden's **raw API** (`POST /zsh/raw/`, see "Raw API" in BrishGarden's
readme) carries a request as bytes: the body is the command followed by its
stdin, and the reply body is stdout followed by stderr, with the exit status
and the split point in headers. There is no JSON and no base64, so nothing is
lost on the way, and the client runs no `jq` at all.

`brishzq.zsh` uses it by default. `brishz_raw=n` makes it use the JSON API
(`POST /zsh/`) exactly as before. `brishz_raw` is parsed like the scripts'
`bool`, with a default: `n`, `no` and `0` mean off; unset or empty means
on, as in `brishz.dash` and `brishzgo`.

```zsh
brishzq.zsh print -r -- ok
head -c 4096 /dev/urandom > x.bin
brishz_in=MAGIC_READ_STDIN brishzq.zsh cat < x.bin | cmp - x.bin
```

## The default since 2026-10-01

The raw API became the default once the running garden on port 7230 had it.
On that garden a small call took 41 ms at p50 (49 ms at p90) over the raw
API, against 97 ms (121 ms) over the JSON API, 30 interleaved calls each.

A garden older than the raw API answers `404` for `/zsh/raw/` without running
anything, and `brishzq.zsh` then sends the same request to the JSON API. That
second request costs about 25 ms per call (measured below), so against such a
garden (a remote one, say) `brishz_raw=n` is faster. Such a garden also
writes an access-log line for the `404` of a `brishz_nolog` request
(`POST /zsh/raw/nolog/?session=...&nolog=1`, session name included), since
its silent routes cover only `/zsh/nolog/`; the JSON retry stays silent.

## What it sends

- The body is the command's bytes, then stdin's: the bytes of
  `brishz_in=MAGIC_READ_STDIN`'s stdin, or of a literal `brishz_in`, or
  nothing. `X-Brish-Cmd-Length` is the command's length in bytes, counted
  with `nomultibyte`, since zsh's `${#var}` counts characters otherwise.
- `brishz_session` goes in the query string, percent-encoded byte by byte. A
  non-empty `brishz_nolog` or `brishz_failure_expected` sends `nolog=1` or
  `failure_expected=1`, since the JSON API takes any non-empty string there as
  true. `brishz_nolog` also selects `/zsh/raw/nolog/`. `brishz_binary=y`
  sends `binary=1` (see below).
- The command text is the same as on the JSON path: the local wrapper (the
  `mark-me` subshell that `cd`s to `$PWD`), `brishz_noquote`, and the
  variables forwarded from Emacs all apply. Stdin is the one difference.
  With `brishz_in=MAGIC_READ_STDIN` the JSON path writes stdin to a temp file
  and wraps the command as `< 'file' {`, a newline, the command, a newline
  and `}`. So there the command's stdin is a regular file, which can be
  seeked, and the command starts on line 2. On the raw path stdin travels in
  the body and the garden pipes it in: `[[ -f /dev/stdin ]]` is false, a
  reader cannot seek back or leave an exact offset for the next reader, and
  with `brishz_noquote` `$LINENO` is 1, not 2. No byte is lost either way.
  Nothing in zshlang tests what kind of file its stdin is. (Under
  `brishz_binary=y`, the JSON request carries stdin itself, as `stdin_b64`,
  with no temp file and no `< 'file' {` wrapper.)
- `bshEndpoint`, `GARDEN_PORT`, the API key header and the remote basic auth
  work as on the JSON path. The caller's `bshEndpoint` or `GARDEN_PORT` beats
  the one `~/.privateShell` sets, so `GARDEN_PORT=7299 brishzq.zsh ...`
  reaches a test garden on 7299. Stdin travels in the request, so a remote garden
  gets it too. (The remote proxy's route was not tested; it forwards
  `/api/v1/zsh/raw/` to `/zsh/raw/` like any other path.)
- `brishz_copy` copies the equivalent `print | curl` pipeline.

## What it prints, and its exit status

- A command's result: stdout's part of the reply to stdout and stderr's part
  to stderr, byte for byte, and the exit status from `X-Brish-Retcode` (like
  `exit`, it keeps the low 8 bits, so 300 becomes 44).
- A notice (`X-Brish-Notice: 1`), such as an empty command or a `%GARDEN_`
  magic command's log: the notice without its trailing newlines plus one
  newline, and exit 200, as the JSON path prints it.
- HTTP 404 or 405: the garden has no raw API and ran nothing. The request goes
  to the JSON API, unchanged.
- Any other HTTP error: exit 22 with no output, as the JSON path's
  `curl --fail` gives.
- A transport failure: curl's own exit status, so a refused connection is
  still 7, which Hammerspoon reads as "not sent" (see
  [hammerspoon-garden](hammerspoon-garden.md)).
- A refusal (`X-Brish-Refused: 1`): the garden ran nothing, because the
  request was malformed, or because it runs in legacy mode (`BRISH_BINARY=0`)
  and the command or stdin is not valid UTF-8 or holds a NUL, which legacy
  mode cannot carry, or the request has `binary=1`. The request goes to the
  JSON API, which handles it as it always has (and refuses `binary: 1` in
  legacy mode too). Only that header sends a request there: a reply without
  it is a command's result, even with retcode 9000 and a stderr that reads
  like a refusal, so no command runs twice.
- With `brishz_binary=y`, a reply that is neither a refusal nor a 404 or 405
  and lacks `X-Brish-Binary: 1`: a legacy-mode garden older than `binary=1`
  ignored that option and ran the command in text mode. Nothing goes to
  stdout, one line on stderr names the command's retcode, and the exit
  status is 201 (see [brishz-binary](brishz-binary.md)).

Two options need the JSON API and always use it: `brishz_out_file_p` and
`brishz_eval_file_p`. `brishz_binary=y` ([brishz-binary](brishz-binary.md))
used to as well, since only the JSON API's binary transport guaranteed that
nothing runs on a garden without binary support. It now takes the raw API
with the query option `binary=1`, which a legacy-mode garden from
BrishGarden 8571467 on refuses without running anything; such a refusal, or
a garden without the raw API, sends it to that binary transport, where it
fails with 201 on a garden without binary support. Only a legacy-mode garden
older than the option can still run the command, in text mode, and then the
client withholds the output and exits 201. With `brishz_raw=n`,
`brishz_binary=y` goes straight to the binary transport, as before.

## How it compares with the JSON path

Two corpora ran through both paths against test gardens. The first has 50
calls: the earlier 29-call comparison's `brishzq.zsh` cases, extended with
stdin, stderr, sessions, `nolog`, failing commands, notices, special
characters, Unicode, invalid UTF-8, NUL, CR and 1 MiB payloads. The second
has 66: argument lists as Hammerspoon sends them, unusual first words, stdin
as a file, long multibyte commands, sessions with odd names and the
options. Both ran under a terminal-like environment and under Hammerspoon's
(launchd `PATH`, no `LANG`). Stdout, stderr and the exit status were
compared byte for byte with the client from before this work.

- Against a binary-mode garden, the raw path differs only where the JSON
  path loses information, and in stdin's file type:
  - a command with invalid UTF-8 in it (3 calls): `jq` turns those bytes
    into U+FFFD before the garden sees them, so the JSON path ran a
    different command;
  - a literal `brishz_in` with invalid UTF-8: the same, for stdin;
  - output with invalid UTF-8 (all 256 bytes, and 1 MiB of random bytes):
    the JSON reply's text field shows those bytes as `\xHH` text;
  - `[[ -f /dev/stdin ]]` under `brishz_in=MAGIC_READ_STDIN`: true on the
    JSON path, false on the raw path (see "What it sends").
- Against a legacy-mode garden, the same apart from the stdin file type. A
  NUL or invalid UTF-8 in the command or stdin is refused with
  `X-Brish-Refused: 1` and goes to the JSON API. Sentinel files showed each
  such command running exactly once, and a command that ran and returned
  9000 with a `brishgarden: ` stderr running once, not twice.
- Against a garden older than the raw API: identical, through the 404
  fallback.
- HTTP 401, 404, 405 and 500 give exit 22 on both paths, a closed port gives
  7, and a connection the server drops gives 52.
- With `brishz_raw=n`, every call and the request it sends is identical
  to the old client's, except that `brishz_binary=y` requests now carry
  `b64_only: 1`, and the changes listed at the end of this page.

## Speed

Wall time per call, including the zsh start, against a test garden on this
laptop, interleaved, after 10 warmups:

- `print -r -- ok`, 200 calls: JSON path p50 129 ms and p90 209 ms; raw path
  p50 60 ms and p90 117 ms.
- 1 MiB of text through `cat`, 20 calls: JSON path p50 154 ms; raw path p50
  59 ms; `brishz_binary=y` p50 about 110 ms, over the JSON binary transport
  it then took. It now takes the raw path; see
  [brishz-binary](brishz-binary.md) for its numbers.
- Against a garden without the raw API, 200 calls: JSON path p50 148 ms;
  raw path with its fallback p50 165 ms (and 178 against 204 in an earlier
  run), so the fallback costs about 20 to 25 ms per call.

The JSON path spends its time in five `jq` runs per call besides `curl`; the
raw path runs only `curl`. The machine was busy, so the absolute numbers are
noisy; the ratios held across runs.

## Other changes in `brishzq.zsh` from the same work

- The first word of a quoted command is emitted bare only when it is made of
  ASCII letters, digits and `_ . / , : @ % + -` and does not start with `=` or
  `~`; otherwise it is single-quoted like the other words. It was quoted with
  `(q+)`, which mis-escapes some bytes and code points (an ideographic space
  stayed bare), so a crafted first word could inject code. Ordinary command
  names are emitted exactly as before. A first word that is exactly `!`
  stays bare as well, as `(q+)` left it, so `brishzq.zsh ! cmd` still
  negates `cmd`'s status. Of the printable ASCII characters, `!` was the only
  one `(q+)` left bare that the rule above quotes. A first word with
  non-ASCII characters is now quoted, which changes only a non-ASCII alias
  (zshlang defines none), and in the C locale (Hammerspoon's) it now arrives
  exact, where `(q+)` corrupted an ideographic space.
- Temp files (stdin for the JSON path, `brishz_out_file_p`,
  `brishz_eval_file_p`, the header dump of `brishz_binary=y`'s JSON
  request) are removed on exit and on HUP, INT and TERM; after INT or TERM
  the script still dies of that signal, and HUP exits 1 as it always did.
  All but the header dump used to be left behind. The raw path creates
  none, and neither do Hammerspoon's calls.
- A signal the parent ignored stays ignored, as it always was: `nohup`'s
  HUP, the INT that a non-interactive shell's `cmd &` ignores, a
  `trap '' TERM`. The client then survives it and prints the command's
  result. zsh lets a script trap a signal that was ignored on entry, so the
  client traps INT and TERM only when a child process reports them not
  ignored: one `perl` run, which costs about 4 ms (10 ms on a busy machine),
  on the paths that make a temp file. Without `perl` it sets no INT or TERM
  trap, and those signals leave the files behind. HUP needs no trap: zsh's
  own HUP handler runs the exit cleanup, and zsh installs it only when HUP
  was not ignored.
- `brishz_binary=y` requests carry `b64_only: 1`, so a garden that knows the
  field leaves the text fields `out` and `err` out of its reply; `brishzq.zsh`
  reads only `out_b64`, `err_b64` and `retcode` there. Older gardens ignore
  the field.
