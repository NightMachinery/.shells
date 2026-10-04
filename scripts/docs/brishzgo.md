# `brishzgo`: a Go drop-in for `brishzq.zsh`

`brishzgo` runs one zsh command in a BrishGarden and passes on its stdout,
stderr and exit status. It takes the same argv and environment variables as
`brishzq.zsh` and, except for the cases under "Where it differs", exits with
the same statuses, but it is one static binary: no `zsh -f` start, no `jq`,
no `base64`, no `curl`. By default it uses the garden's **streaming API**
(`POST /zsh/stream/`), which passes the output
on while the command runs, and stops the command when `brishzgo` is
interrupted. It falls back to the **raw API** (`POST /zsh/raw/`, bytes as
bytes) and then JSON when a garden lacks streaming. A refusal goes straight
to JSON. `brishz_stream=n` selects the previous raw/JSON behavior.

The source is `golang/brishzgo/`; its `readme.org` covers building and the
tests. [agfi:brishz] now runs it, including callers of [agfi:bsh],
[agfi:brishzr] and [agfi:brishz-all]. The previous shell implementation is
[agfi:brishz-v1], still using `brishzq.zsh`. Normal Lua and Hammerspoon calls,
agent hooks and standalone wrappers use the Go client too.

The shell wrapper builds or refreshes the binary through [agfi:go-local-dep],
exports `brishz_in`, `brishz_nolog` and `brishz_session` (including the
`brishz_s` shorthand), and passes argv unchanged. `brishz_in=MAGIC_READ_STDIN`
reads the caller's stdin directly; other non-empty values are literal input.
[agfi:brishz-in] uses that direct stdin path, preserving trailing newlines
and avoiding a whole-input shell buffer.
The clipboard option `brishz_copy` / `brishz_c` copies a replay command;
`brishz_async` launches a detached request. A remote garden needing basic
auth requires `GARDEN_PASS0` exported before calling the Go client.

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
brishz_stream=n brishzgo print -r -- buffered
brishz_stream=n brishz_raw=n brishzgo print -r -- json
```

Sole `-h` or `--help` prints local help and exits 0, even when
`DISABLE_BRISH=y`, without contacting a garden or reading stdin. A leading
`--` is dropped and everything after it goes to the garden:
`brishzgo -- --help` runs a command named `--help` there.
`brishzgo command --help` passes `--help` to `command`. The old leading
`-c` still works as an alias for `--`, for compatibility with `brishzq.zsh`.

### The command text

The words become one command line, byte for byte the one `brishzq.zsh`'s
`gquote` builds (the parity test in `readme.org` compares them):

- the first word stays bare when it is made only of ASCII letters, digits
  and `_ . / , : @ % + -`, so aliases, functions and reserved words still work
  in command position, and when it is exactly `!`, so `brishzgo ! cmd`
  negates `cmd`'s status (a `brishzq.zsh` from before that rule quotes it);
  otherwise it is single-quoted;
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
- `brishz_copy` (or `brishz_c`): any non-empty value copies a replayable shell
  command through `pbcopy`, when installed. This copies the request, not its
  output, as in `brishzq.zsh`. The replay restores the original working
  directory, argv, session and transport flags. Direct stdin is embedded as base64,
  preserving binary bytes and trailing newlines without stale temp paths.
  Copying direct stdin consumes and buffers it before sending the request;
  normal streaming is unchanged when copy is off or `pbcopy` is absent.
  API key headers and passwords are not embedded: replay uses the usual key
  file and exported authentication/proxy variables. If `bshEndpoint` contains
  URL credentials, it too must remain exported for replay. Replay runs
  synchronously with copying disabled, even if the original call was async.
  An unavailable `pbcopy` is silently skipped; a failed `pbcopy` prints a
  diagnostic but does not prevent execution or replace the command's status.
- `brishz_async`: any non-empty value launches a detached client and returns
  0 after local launch, without waiting for an HTTP round trip or the command.
  Both output streams and later errors are discarded. As in `brishz.dash`,
  even `n` or `0` enables it; unset or empty disables it. With
  `MAGIC_READ_STDIN`, the parent first consumes stdin into an unlinked temp
  file, so the worker receives every byte after the caller exits. The worker
  owns its session and drains the streaming reply to completion, including
  raw/JSON fallbacks, so the parent's exit does not cancel the command.
  A local input or launch failure returns 1. `DISABLE_BRISH=y` still prevents
  launching. Async success confirms launch, not delivery or command success.
- `brishz_binary`: the exact-bytes opt-in of [brishz-binary](brishz-binary.md),
  parsed like `brishzq.zsh`'s `bool`: empty, `n`, `no` and `0` (in any case)
  are false, and anything else is true, `false` included, as in
  `brishzq.zsh`. See "Transport" for what it changes here.
- `brishz_raw`: the raw API, on unless set to a false value (`n`, `no`, `0`).
  With streaming enabled, this controls its fallback. To use JSON directly,
  set both `brishz_stream=n` and `brishz_raw=n`.
- `brishz_stream`: the streaming API, on when unset or empty. Explicit `n`,
  `no`, `0` and `false` (in any case) disable it; other non-empty values
  enable it, following `bool` in `zshlang/basic/core.zsh`. See "The
  streaming API" below. `brishzq.zsh` has no such mode, and ignores the
  variable.
- `brishz_debug`: a true value (as for `brishz_binary`) prints the request
  and reply headers and the command text to stderr. The values of the API key file's headers and of
  `Authorization` are printed as `<redacted>`.
- `DISABLE_BRISH=y`: exit 1 at once, as `brishzq.zsh` does.
- The proxy variables, read as curl reads them: `http_proxy` for an http
  endpoint (never `HTTP_PROXY`, which curl ignores), `https_proxy` or
  `HTTPS_PROXY` for https, else `all_proxy` or `ALL_PROXY`, and none for a
  host that `no_proxy` (or `NO_PROXY`) lists. An empty one counts as unset. A
  loopback endpoint is proxied too unless `no_proxy` lists it, which the
  scripts' `no_proxy` (`127.0.0.1,localhost`) does. Go speaks http, https,
  socks5 and socks5h proxies, and resolves names at a socks5 proxy, which
  curl does only for socks5h; a socks4 proxy fails with exit status 7.

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
also on SIGHUP, SIGINT and SIGTERM (after which `brishzgo` dies of that
signal). A signal that was ignored when `brishzgo` started stays ignored: a
background job of a non-interactive shell starts with SIGINT ignored, so
that a Ctrl-C meant for the script leaves it running. Invalid UTF-8 in
`cmd` or `stdin` becomes U+FFFD, the way jq replaces it.

The fallback has all of stdin even though the raw request streamed it. A
refusing garden read all of it, and each request keeps a copy of what it
sent, in memory up to 16 MiB and in a temp file beyond that, which the next
request resends first, in order; the transport stops reading our stdin for
a request the moment the next one starts. A garden without the raw API
normally gets none of it: the raw request carries `Expect: 100-continue` and
`Connection: close`, so that garden answers 404 before any of stdin is
sent, and Go's transport sends none afterwards (to keep a connection open,
it would send the body after the 404). Some can still go out first, to a
server that asks for the body before answering 404 (a buffering proxy), or
past the transport's 2 s wait for a 100 Continue. The fallback costs one
extra round trip, 1 to 2 ms at p50 on this machine (see "Measurements").

### `brishz_binary=y`

The opt-in sends the same request with the query option `binary=1`, which
asks for exact bytes: to the raw API, or with `brishz_stream=y` to the
streaming API. The command and stdin travel as bytes, with no base64. Each
garden generation answers it in its own way:

- A binary-mode garden (the default) replies exactly anyway, so the option
  changes nothing there. Its reply has `X-Brish-Binary: 1`, and is handled
  exactly as without the opt-in: stdout, stderr, the retcode, notices (exit
  200), and every exit status below.
- A legacy-mode garden (`BRISH_BINARY=0`) from BrishGarden 8571467 on
  refuses the request before running anything (`X-Brish-Refused: 1`).
- A garden without the API asked for answers 404 or 405, and runs nothing.
- A legacy-mode garden older than the option (BrishGarden before 8571467)
  ignores it, as gardens ignore unknown query parameters, runs the command
  in text mode, and replies with `X-Brish-Binary: 0`.

After a refusal, a 404 or a 405, the request goes to the JSON API's binary
transport (`cmd_b64`, `stdin_b64`, `binary: 1`, and `b64_only: 1` for the
smaller reply), as `brishzq.zsh` sends it, with all of stdin, as for the
fallback above. That is also the next step after the streaming API's 404,
rather than the raw API: a garden without the streaming API predates
`binary=1`, so in legacy mode its raw API would run the command in text
mode, where its JSON API refuses it. The JSON reply must carry
`X-Brish-Binary: 1`. Without it, `brishzgo` prints the message
`brishzq.zsh` prints there (see [brishz-binary](brishz-binary.md)), under
its own name, and exits 201, and the garden has run nothing: a legacy-mode
garden refuses `binary: 1`, and a garden older than the binary fields
(`ec61c63`, say) gets an empty command. A garden older than the raw API but
with those fields (`42ddc9d`) runs it exactly.

A 200 reply of the raw or streaming API that is not a refusal and lacks
`X-Brish-Binary: 1` comes from the last kind of garden, which has run the
command through text. Its output may have lost bytes, so `brishzgo` passes
none of it on. It prints one line on stderr instead, with the command's
retcode, and exits 201:

```
brishzgo: garden ran the command in text mode (no X-Brish-Binary: 1); it predates the binary=1 option and runs with BRISH_BINARY=0; output withheld; retcode 3
```

A notice from such a garden (an empty command, a `%GARDEN_` magic command)
is withheld the same way, and the line opens with `garden answered in text
mode with a notice, not a command's output` instead. A raw reply with
retcode 9000 opens with `garden ran the command in text mode, or refused it
and ran nothing`: a legacy-mode garden older than `X-Brish-Refused`
(BrishGarden d0a9514 until 0fd2752) refuses input that text mode cannot
carry with that retcode and no refusal header. `brishzq.zsh` and
`brishz.dash` print the same lines (see [brishz-binary](brishz-binary.md)).

The streaming API's headers come before any output, so `brishzgo` writes
none there either. It reads such a reply to its exit frame first, dropping
the output, since closing the connection earlier would make the garden
kill the command half way. A reply that ends before its exit frame gives
`retcode unknown`, and still exit 201. The streaming API came after
`X-Brish-Refused`, so its replies never get the retcode 9000 opening.

So under the opt-in, a garden without binary support runs nothing, or runs
the command while `brishzgo` withholds its output and exits 201. Only a
legacy-mode garden older than the `binary=1` option can still run the
command, in text mode.

With `brishz_raw=n`, the streaming API still comes first by default, and
JSON is its fallback. Also set `brishz_stream=n` to keep the opt-in directly
on the JSON API's binary transport.

### The streaming API

By default, the client sends the raw request, unchanged, to `/zsh/stream/` (or
`/zsh/stream/nolog/`). The garden answers at once with its headers and then
sends the output in **frames** while the command runs: a type byte (1
stdout, 2 stderr, 3 exit), the payload's length as 4 bytes big-endian, and
the payload. The body ends with exactly one exit frame, whose payload is
the retcode in ASCII. BrishGarden's readme ("Streaming API") has the whole
protocol.

`brishzgo` writes each payload to stdout or stderr as it reads it, with no
buffer of its own in between, so a line reaches a pipe or a terminal when
the command prints it, not when the command ends. It exits with the exit
frame's retcode (its low 8 bits, as for the raw API). A frame of an unknown
type is skipped. A notice (`X-Brish-Notice: 1`) is printed as the other APIs'
notices are, and exits 200; so is a 200 reply that is not a streaming reply
at all.

Fallbacks, when nothing ran:

- HTTP 404 or 405, from a garden older than the streaming API: the raw API
  next (and the JSON API after it, if that is missing too), or the JSON API
  with `brishz_raw=n` or `brishz_binary=y`;
- `X-Brish-Refused: 1`: the JSON API next, as for the raw API, since the
  raw API would refuse the same request.

Each fallback resends all of stdin, as above.

**Interrupting.** SIGHUP, SIGINT or SIGTERM while a command streams closes
the connection at once, and `brishzgo` then dies of the same signal, so a
shell reports 129, 130 or 143. (With `brishz_debug=y`, the debug line about
it is written after the connection is closed, and dropped if stderr does not
take it within 0.1 s.) A closed stdout does the same through SIGPIPE:
`brishzgo yes | head -1` ends at once, and the garden kills `yes`. This is
the one transport where interrupting `brishzgo` stops the command. The raw
and JSON APIs run it to its end whatever happens to the client, since their
garden never looks at the connection until it has the whole reply. A signal
that was ignored when `brishzgo` started stays ignored here too, so a
script's background job that streams is not stopped by the Ctrl-C meant for
the script.

The garden sees the client go away and kills the command. A request that
still waits for its worker (its `brishz_session` runs another command, the
session's worker is still starting, or every pool worker is busy) runs
nothing. A running command gets SIGINT first, which ends most commands at
once, with status 130, and the worker keeps its state, so a session keeps
its variables, functions and directory. A command that traps or ignores
SIGINT in the worker itself goes on: its processes get SIGTERM about 2 s
later and SIGKILL 2 s after that, and each time one dies, the command runs
its next statement. It ends only when the garden kills its worker: about
4.5 s after the interrupt when nothing runs below the worker, about 7 s when
something does, and up to about 13 s while it keeps writing. The worker
then dies, and Brish replaces it, so a session loses its state, and its next
request waits for a new worker to start. These later steps also stop
background jobs that earlier commands left on that worker. BrishGarden's
readme ("Streaming API", "Disconnects") has the details.

A reader that stops reading without going away (a full pipe whose reader
sleeps) holds the command: the garden queues at most 256 KiB for it, past
the sockets' own buffers, and then the command blocks on its own output and
keeps its worker, as a local command would.

## Exit statuses

As `brishzq.zsh`'s, except where "Where it differs" below says otherwise:

- the command's own status, from the garden (9000, a refused request, exits
  as 40, as `exit 9000` does in zsh);
- 200: a reply that is not a command's result, such as a notice (an empty
  command, a `%GARDEN_` magic command), printed with its trailing newlines
  replaced by one;
- 201: `brishz_binary=y` and no `X-Brish-Binary: 1` in the reply, with a
  message on stderr. From the JSON API, the message of
  [brishz-binary](brishz-binary.md) under `brishzgo`'s name; nothing ran.
  From the raw or streaming API, the one in "`brishz_binary=y`" above,
  with the command's retcode; the command ran in text mode, and its output
  was withheld;
- curl's exit codes for a failed request, which `brishzq.zsh` passes on:
  7 connection refused (and an unsupported proxy scheme), 6 host not
  resolved, 28 timeout (curl's default connect timeout of 300 s, no overall
  one), 22 an HTTP status of 400 or more (a missing key gets 401 or 403),
  52 an empty reply, 18 a reply shorter than announced, 1 a reply that is
  not HTTP or an unsupported scheme, 8 a malformed reply header, 35 and 60
  TLS failures (35 also for an https endpoint that answers in plain HTTP),
  47 too many redirects, 97 a SOCKS proxy that could not connect us. Any
  other transport failure exits 56, curl's code for a failed receive, which
  for some broken servers is not the code curl itself would give. Like
  `curl --silent`, these print nothing; `brishz_debug=y` shows the error.
- With `brishz_stream=y`, also: 18 a reply that ends before its exit frame
  (after the output that arrived has been written), 8 an exit frame without
  a number, 23 a failed write of our own output (as curl's write error); and
  on SIGHUP, SIGINT or SIGTERM, death by that signal.

## Where it differs from `brishzq.zsh`

- Forwarded variables: `brishzgo` quotes a value as zsh's `typeset -p` does in
  a UTF-8 locale, whatever our own locale is. In the C locale, zsh escapes
  the non-ASCII bytes that locale calls unprintable (on macOS 0x80 to 0x9F,
  so most of U+3000 and of an emoji, but not `é`), where `brishzgo` writes
  every printable character bare. A character outside ASCII that is not
  printable `brishzgo` writes with one `\M-` escape per byte, where zsh writes
  `\uXXXX` (refused by a shell in the C locale) or a single `\M-` byte for
  U+0080 to U+00FF (which loses a byte). The value arrives intact either way;
  zsh's own text does not always give it back.
- `brishz_out_file_p`, `brishz_eval_file_p` and
  `brishz_summary_p` are ignored. The first two work around the legacy
  transport's losses, which the raw API does not have.
- `GARDEN_PASS0` comes from the environment only: `brishzgo` does not source
  `~/.privateShell`, as `brishzq.zsh` does. A remote endpoint through the proxy
  needs it exported.
- Debug output goes to stderr, never stdout.
- A raw reply cut short exits 18, as in `brishzq.zsh`, but `brishzgo` has
  already passed on the part of stdout that arrived, since it streams the
  reply; `brishzq.zsh`, which holds the whole reply in memory, prints
  nothing.
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
- The streaming API, enabled by default, is `brishzgo`'s alone; see above.

## Caller migrations

The active shell/config callers now use the installed Go client:

- Claude Code, Codex and Antigravity hooks keep `brishz_async=y` and direct
  stdin, with quoted argv. Hooks launch silently and retain their existing
  JSON acknowledgements and agent-managed async settings.
- Date-parser reloads, search postprocessing and subagent previews use the
  absolute client path quoted into fzf's shell command. Cancelled streaming
  requests can stop obsolete work.
- The standalone agent picker uses the client for rows, preview configuration
  and selected actions. Alt+enter launches a detached request with quoted paths.
- Menubar date queries use argv; the stopwatch's shell assignment and the
  deliberate pipeline use `eval`. Captured output still feeds the same menu.
- Emacs, zopen, STT, lock/unlock/audio hooks, reminder notifications and the
  audio-guard launcher use the Go client. STT keeps its input file until the
  synchronous call finishes and then removes it, including on failure.
- Scheduled alarms quote the absolute client path into the `at` job.
- Claude Code and Antigravity report delegation, and Maccy image export, use
  [agfi:brishz] with `brishz_binary=y`. This replaces the legacy output-file
  workaround for Unicode with exact-byte transport. A garden without binary
  support fails explicitly instead of returning potentially corrupted text.
- Standalone `bsh.dash` and `bsh_old.dash` keep their persistent session and
  raw shell-code interface. The source-file form removes its input file after
  success, failure or interruption. `brishz_para.dash` keeps raw shell code
  and cwd cleanup but streams stdout/stderr and returns the command's status
  directly. `brishzrq.dash` keeps its endpoint and uses Go; remote password
  authentication must be exported as `GARDEN_PASS0`, since Go sources no
  private shell file. [agfi:brishz] forwards an already-loaded shell password
  to Go even when the parent shell variable was not exported.
- The PHP status/TLDR pages use a shared command builder, quoting both the
  absolute Go path and each argument. Their server environment can set
  `BRISHZGO_BIN` if its HOME differs from the shell's.
- Kitty actions and Sioyek's PDF-location command use `/bin/sh` solely to
  expand the binary path, forwarding file paths as arguments. Sioyek's custom
  parser joins backslash-escaped spaces before macro substitution; the shell
  code therefore uses escaped spaces and receives macros as positional args.

GUI and standalone callers default to `${HOME}/go/bin/brishzgo`; an exported
`BRISHZGO_BIN` overrides that path. This works with launchd's bare PATH and
needs no `/usr/local/bin` symlink or root install. `setup/setup_go` installs
this binary. Shell-generated fzf commands also accept the binary resolved in
`$commands`. Re-run `brishz-restart` after changes to their garden functions.

`python3 zshlang/tests/brishzgo-callers.py` verifies hook stdin, shell quoting,
wrapper exit statuses, source/STT file lifetime, menu queries, picker actions,
exact-byte report flags, scheduled-job quoting and GUI command parsing using
a recording client and a synthetic HOME.

Normal Lua helpers drain stdin/stdout/stderr concurrently. Hammerspoon helpers
and the STT backend capture Go output in private temporary files, preserving
Unicode and avoiding full pipes while retaining completion callbacks. See
[Hammerspoon garden helpers](hammerspoon-garden.md) for their failure policy
and tests. The JSON-envelope launcher now consumes Go's output/status
directly. The original clients and [agfi:brishz-v1] remain available for
compatibility, as do explicit Lua `evalFile`/`outFile` options; no normal
active caller still depends on the old transports.

## Measurements

The measurements below predate the default change: their plain `brishzgo`
calls used the raw API, equivalent to today's `brishz_stream=n`.

On 2026-10-01, on a laptop at load average 7 to 9, against smoke gardens
with 4 workers: wall time per call including process start, 200
interleaved rounds of `print -r -- ok` after 10 warm-ups.

A binary-mode garden with the raw API (BrishGarden's `raw-refused` branch):

- `brishzgo`: p50 24 ms, p90 50 ms;
- `brishzgo` with `brishz_raw=n` (JSON API): p50 24 ms, p90 47 ms;
- `brishzq.zsh`: p50 107 ms, p90 193 ms;
- `brishzgo` through `localhost`, so without the `mark-me` wrapper: p50
  16 ms, p90 37 ms.

A garden without the raw API (`42ddc9d`), where every call falls back:

- `brishzgo`: p50 30 ms, p90 51 ms, against p50 28 ms, p90 51 ms with
  `brishz_raw=n`;
- through `localhost`: p50 20 ms, p90 37 ms, against p50 17 ms, p90 30 ms.

Most of `brishzgo`'s time is the garden's side and the local wrapper: a
call to a closed port, which measures only the client, took p50 6 ms, and
`/usr/bin/true` takes 2 ms to spawn here.

An earlier run (against BrishGarden's `raw-endpoint` branch, at load
average 22 to 28, 20 rounds) sent 1 MiB of random bytes through `cat`,
every one exact:

- `brishzgo` (raw API): p50 731 ms, p90 1058 ms;
- `brishzgo` with `brishz_binary=y` (JSON binary): p50 1040 ms, p90 1470 ms;
- `brishzq.zsh` with `brishz_binary=y`: p50 2417 ms, p90 3671 ms;
- `brishz2.dash` with `brishz_binary=y`: p50 1314 ms, p90 1765 ms.

On 2026-10-03, against smoke gardens of BrishGarden's `output-streaming`
branch with 24 workers, at a load average of about 2:

- per call, `brishzgo true` with the `mark-me` wrapper, 100 runs: the
  streaming API 8.4 ms on a binary-mode garden and 8.6 ms on a legacy-mode
  one, against 9.1 ms and 8.1 ms for the raw API, so the same within noise;
- `print -r -- one; sleep 1; print -r -- two`: the first byte reaches our
  stdout after a median 36 to 38 ms with the streaming API (the garden sends
  the first frame about 6 ms after the request), against 1.10 to 1.16 s, at
  the very end, with the raw API;
- Ctrl-C (SIGINT) to `brishzgo` during `sleep 5; print -r -- ran >> file`:
  `brishzgo` dies of SIGINT and the file is never written; with the raw API
  the file appears 5 s later.

On 2026-10-04, `brishz_binary=y` before and after it moved from the JSON API
to the raw and streaming APIs, against a binary-mode smoke garden of
BrishGarden `af16f49` with 2 workers, at a load average of 3 to 4,
interleaved, after 10 warm-up rounds, with the `mark-me` wrapper:

- `print -rn -- ok`, 200 rounds: p50 10.6 ms (p90 15.6 ms) over the JSON
  API's binary transport, against 10.4 ms (15.8 ms) over the raw API and
  10.5 ms (15.8 ms) over the streaming API, so the same within noise, since
  `brishzgo` encodes the JSON request itself;
- 1 MiB of random bytes through `cat`, 30 rounds, every one exact: p50
  125 ms (p90 140 ms) over the JSON API, against 93 ms (104 ms) over the raw
  API and 30 ms (32 ms) over the streaming API. Without the opt-in, 20
  rounds took 92 ms over the raw API and 33 ms over the streaming API, so
  `binary=1` itself costs nothing measurable.
