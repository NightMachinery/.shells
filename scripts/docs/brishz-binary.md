# Exact bytes through BrishGarden: `brishz_binary=y`

BrishGarden's JSON API carries text, and a garden whose Brish runs in legacy
mode loses bytes on the way: a NUL, a `\r`, invalid UTF-8.

The **opt-in** is the env var `brishz_binary=y`. With it, `brishzq.zsh`
uses the garden's binary transport, so the command, stdin, stdout and stderr
all arrive exactly as they were sent. Without it, it sends and prints exactly
what it did before, byte for byte. Hammerspoon, `lua/pipe.lua` and the agent
hooks never set it.

## What the garden needs

A BrishGarden with the binary request fields (`cmd_b64`, `stdin_b64` and
`binary`), whose process was started with `BRISH_BINARY=1`. See "Binary data
(opt-in)" in BrishGarden's readme. The mode belongs to the garden process:
`brishz-restart` restarts the zsh workers but keeps the process's mode, so
switching needs a restart of the garden process itself.

## Using it

```zsh
head -c 4096 /dev/urandom > x.bin
brishz_binary=y brishz_in=MAGIC_READ_STDIN brishzq.zsh cat < x.bin | cmp - x.bin
brishz_binary=y brishzq.zsh print -rn -- $'\xff\r' | od -c
```

`brishz_binary` is parsed with the script's `bool`: `n`, `no`, `0` and the
empty string mean off, anything else means on.

- `brishzq.zsh` sends the command in `cmd_b64` and stdin in `stdin_b64`.
  With `brishz_in=MAGIC_READ_STDIN`, stdin is streamed into `base64`, never
  held in memory as text. It writes the decoded `out_b64` to stdout and
  `err_b64` to stderr, and exits with the command's `retcode`.
- Remote endpoints (`bshEndpoint`) work too, since stdin travels in the
  request. The text path of `brishzq.zsh` redirects the command's stdin from
  a local temp file, which a remote garden cannot read.
- Pipe into `brishzq.zsh` directly. The zsh function `brishz-in` reads stdin
  with `$(cat)` first, which drops trailing newlines.

Payloads pass through the `base64` binary before `jq` sees them, since `jq`
turns invalid UTF-8 into U+FFFD, and through pipes or temp files, never argv,
which is size-limited and shown by `ps`. The macOS, GNU and Homebrew (John
Walker's) `base64` all work: the garden ignores the line breaks some of them
add.

## A garden without binary support

A binary-mode garden marks every reply to a `binary: 1` request with the
header `X-Brish-Binary: 1`. Every reply is HTTP 200, so `brishzq.zsh` looks
for that header instead. When it is missing, it prints this on stderr, prints
nothing on stdout, and exits with status 201:

```
brishzq.zsh: garden lacks binary support (no X-Brish-Binary header); restart the garden process with BRISH_BINARY=1
```

A garden without binary support has run nothing:

- a garden in legacy mode refuses a `binary: 1` request without running it;
- an older garden ignores the new fields, and the command travels only in
  `cmd_b64`, never also in `cmd`, so it sees an empty command.

A reply that carries the header but is not a command's result, such as the
log of a `%GARDEN_` magic command, is printed as is, and `brishzq.zsh` exits
with 200, as it does without the opt-in.

## The older workarounds

`brishz_out_file_p=y` and `brishz_eval_file_p=y` (in `brishzq.zsh`) route
output or the command through temp files, to get around the legacy
transport's losses. The opt-in supersedes them. They stay for gardens
without binary support, and they still work under the opt-in, but only
against a local garden, since they rely on files on this machine.
