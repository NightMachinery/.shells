# `brishzq.zsh` over the raw API: `brishz_raw=y`

BrishGarden's **raw API** (`POST /zsh/raw/`, see "Raw API" in BrishGarden's
readme) carries a request as bytes: the body is the command followed by its
stdin, and the reply body is stdout followed by stderr, with the exit status
and the split point in headers. There is no JSON and no base64, so nothing is
lost on the way, and the client runs no `jq` at all.

`brishzq.zsh` uses it when `brishz_raw=y` is set. Without it, `brishzq.zsh`
uses the JSON API (`POST /zsh/`) exactly as before. `brishz_raw` is parsed
like the scripts' `bool`: `n`, `no`, `0` and the empty string mean off.

```zsh
brishz_raw=y brishzq.zsh print -r -- ok
head -c 4096 /dev/urandom > x.bin
brishz_raw=y brishz_in=MAGIC_READ_STDIN brishzq.zsh cat < x.bin | cmp - x.bin
```

## Why it is not the default yet

A garden older than the raw API answers `404` for `/zsh/raw/` without running
anything, and `brishzq.zsh` then sends the same request to the JSON API. That
second request costs about 25 ms per call (measured below). The running
garden on port 7230 has no raw API until it is restarted on a BrishGarden
that has one, so a raw default today would make every Hammerspoon and hook
call slower. Once the running garden has the raw API, make it the default by
changing `brishz_raw:-n` to `brishz_raw:-y` in `brishzq.zsh`.

## What it sends

- The body is the command's bytes, then stdin's: the bytes of
  `brishz_in=MAGIC_READ_STDIN`'s stdin, or of a literal `brishz_in`, or
  nothing. `X-Brish-Cmd-Length` is the command's length in bytes, counted
  with `nomultibyte`, since zsh's `${#var}` counts characters otherwise.
- `brishz_session` goes in the query string, percent-encoded byte by byte. A
  non-empty `brishz_nolog` or `brishz_failure_expected` sends `nolog=1` or
  `failure_expected=1`, since the JSON API takes any non-empty string there as
  true. `brishz_nolog` also selects `/zsh/raw/nolog/`.
- The command text is the same as on the JSON path: the local wrapper (the
  `mark-me` subshell that `cd`s to `$PWD`), `brishz_noquote`, and the
  variables forwarded from Emacs all apply.
- `bshEndpoint`, `GARDEN_PORT`, the API key header and the remote basic auth
  work as on the JSON path. Stdin travels in the request, so a remote garden
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
  mode cannot carry. The request goes to the JSON API, which runs it as it
  always has. Only that header sends a request there: a reply without it is
  a command's result, even with retcode 9000 and a stderr that reads like a
  refusal, so no command runs twice.

Three options need the JSON API and always use it: `brishz_binary=y`
([brishz-binary](brishz-binary.md)), `brishz_out_file_p` and
`brishz_eval_file_p`. `brishz_binary=y` keeps its guarantee there: exact bytes
or nothing runs. The raw API cannot give that guarantee, because a legacy-mode
garden runs a valid UTF-8 request and only says afterwards that its bytes went
through text.

## How it compares with the JSON path

A corpus of 50 calls (the earlier 29-call comparison's `brishzq.zsh` cases,
extended with stdin, stderr, sessions, `nolog`, failing commands, notices,
special characters, Unicode, invalid UTF-8, NUL, CR and 1 MiB payloads) ran
through both paths against test gardens, under a terminal-like environment
and under Hammerspoon's (launchd `PATH`, no `LANG`). Stdout, stderr and the
exit status were compared byte for byte.

- Against a binary-mode garden, 45 calls are identical and 5 differ, each
  where the JSON path loses information and the raw path is exact:
  - a command with invalid UTF-8 in it (2 calls): `jq` turns those bytes into
    U+FFFD before the garden sees them, so the JSON path ran a different
    command;
  - a literal `brishz_in` with invalid UTF-8: the same, for stdin;
  - output with invalid UTF-8 (all 256 bytes, and 1 MiB of random bytes):
    the JSON reply's text field shows those bytes as `\xHH` text.
- Against a legacy-mode garden: all 50 identical (with the refusal fallback
  above).
- Against a garden older than the raw API: all 50 identical, through the
  fallback.
- HTTP 401, 404, 405 and 500 give exit 22 on both paths, and a closed port
  gives 7 on both.
- Without `brishz_raw=y`, all 50 calls and the requests they send are
  identical to the old client's, except that `brishz_binary=y` requests now
  carry `b64_only: 1` (below).

## Speed

Wall time per call, including the zsh start, against a test garden on this
laptop, interleaved, after 10 warmups:

- `print -r -- ok`, 200 calls: JSON path p50 129 ms and p90 209 ms; raw path
  p50 60 ms and p90 117 ms.
- 1 MiB of text through `cat`, 20 calls: JSON path p50 154 ms; raw path p50
  59 ms; `brishz_binary=y` p50 about 110 ms.
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
  names are emitted exactly as before.
- Temp files (stdin for the JSON path, `brishz_out_file_p`,
  `brishz_eval_file_p`, the header dump of `brishz_binary=y`) are removed on
  exit, and on HUP, INT and TERM, after which the script still dies of that
  signal. All but the header dump used to be left behind. The raw path
  creates none.
- `brishz_binary=y` requests carry `b64_only: 1`, so a garden that knows the
  field leaves the text fields `out` and `err` out of its reply; `brishzq.zsh`
  reads only `out_b64`, `err_b64` and `retcode` there. Older gardens ignore
  the field.
