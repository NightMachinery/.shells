# Fill-in-the-middle completion

Fill-in-the-middle (FIM) is the thing code models are actually trained for and
chat models are not: you hand over the text *before* the cursor and the text
*after* it, and get back only what belongs in between. On the command line
that means `alt+.` completes the line you are halfway through writing, with the
rest of it still standing to the right of the cursor.

Two files, split along the interactive boundary:

- `zshlang/auto-load/others/fim.zsh` — [agfi:fim-get], the request. Loaded in
  every shell, so it works in a pipe, in a script and over brish.
- `zshlang/interactive/auto-load/FIM.zsh` — the widget, the async plumbing and
  the bindings. `zshlang/interactive/` is only sourced from `.zshrc`, so none
  of this exists in a non-interactive shell.

The Emacs twin is `night/fim-get` in `~/doom.d/autoload/night-mistral-fim.el`,
documented at `~/doom.d/docs/mistral-fim.md`. It speaks the same body to the
same providers, so the two want changing together.

## Providers

Every native FIM API takes an *identical* request body — `model`, `prompt`,
`suffix`, `max_tokens`, `stop`, `temperature` — so a provider here is four
strings, held in four parallel assoc arrays: `fim_provider_endpoint`,
`fim_provider_model`, `fim_provider_key_var` and `fim_provider_extract`.

Configured, with measured round-trips for a one-line completion:

- `codestral` — `codestral.mistral.ai/v1/fim/completions`, `codestral-latest`,
  about 0.3s. The default, because at this latency the completion is there
  before you have decided whether you wanted it.
- `deepseek` — `api.deepseek.com/beta/completions`, `deepseek-v4-pro`, about
  1.4s, and noticeably better output.
- `deepseek-flash` — same endpoint, `deepseek-v4-flash`, about 1.1s.

Mistral answers at `.choices[0].message.content`; DeepSeek's `/beta` endpoint
is OpenAI-shaped and answers at `.choices[0].text`. That difference is the
whole of `fim_provider_extract`.

`fim_provider_key_var` holds the *name* of the global holding the key
(`codestral_api_key`, `deepseek_api_key`, both from `~/.privateShell`), never
the key. Nothing puts key material in argv, where `ps` would show it to every
local user — the same reasoning as `./docs/api-keys.md`.

[agfi:fim-provider-select] changes the default for the current shell,
[agfi:fim-provider-show] echoes it, [agfi:fim-providers] lists them. There is
deliberately no automatic fallback to a second provider on failure: a hotkey
that silently switches models hides an expired key for weeks, and switching by
hand is one command.

Who else has a FIM API is a short and mostly negative list — no Google, no
OpenAI to speak of, no Anthropic, no OpenRouter, no Groq. It is written up in
`~[nt]/public/subjects/ML/NLP/LLM/FIM/gen.org`.

## Using it non-interactively

```zsh
fim-get '<prefix>' '<suffix>'
```

It prints the completion with **no trailing newline** unless stdout is a tty,
so a caller can splice the bytes in verbatim. Keyword arguments are the usual
namespaced globals, and `@opts` works too:

- `fim_provider` — default `codestral`; also the current shell's default, so
  assigning it once changes every later call
- `fim_model` — override the provider's model
- `fim_max_tokens` — 64
- `fim_stop` — a newline. Together with `max_tokens` this caps the completion
  at one line *during generation*, rather than paying for a longer one and
  truncating it. Explicitly empty disables stopping; unset means the newline.
- `fim_temperature` — 0
- `fim_timeout` — 20 seconds, as `curl --max-time`
- `fim_proxy_p` — `y`, and a no-op unless a proxy is configured
- `fim_strip_space_p` — drop one leading space; `n`, see below

```zsh
fim_provider=deepseek fim_max_tokens=200 fim-get 'def is_prime(n):
    '
```

## The leading space is not a bug

Both this and the Emacs twin used to drop one leading space from every
completion, on the belief that Codestral had a bug that prepended one. It is
now a flag, `fim_strip_space_p` / `night/fim-strip-leading-space`, and it is
**off**. Measured over 29 contexts against each of the three providers:

- All three do it at the same rate — Codestral 7 of 29, both DeepSeeks 8 of 29
  — so it was never a Codestral bug. It is the ordinary whitespace ambiguity
  of infilling: nothing in the prompt says whether the boundary space belongs
  to the prefix or to the middle.
- Where the prefix ends in an operator, the space is simply *correct*. All
  three return ` 0` for `count =`, ` b` for `return a +`, ` {` for
  `const f = (x) =>`, and ` tr -s ...` for `cat file.txt |`. Stripping gives
  you `count =0` and `cat file.txt |tr`.
- Where point sits on an otherwise empty line, the model supplies the whole
  indent. All three answered `        self.x = 1` inside a Python
  `__init__`; dropping one space makes it seven and breaks the file.
- Where the space really was spurious it was usually *two* of them — the model
  re-emitting an indent the prefix already carried — so dropping one leaves
  the line misaligned either way.

Across 87 samples exactly one came out better for the strip: DeepSeek
returning ` import random` after a four-space indent. The flag stays, unset,
because a later model may well go back to prepending one; the probes are in
this session's scratchpad and are cheap to re-run.

Notably, the completions where a stray space would actually *corrupt* code —
prefix ending mid-token, like `os.pa` or `arr.len` — never had one, on any
provider.

Errors go to stderr as exactly one line and the return code propagates:

```
fim-get: codestral: HTTP 401 — Invalid API Key
fim-get: deepseek: HTTP 401 — Authentication Fails, Your api key: ****ogus is invalid
fim-get: no API key for codestral (expected $codestral_api_key)
fim-get: unknown provider 'nope'; known: codestral deepseek deepseek-flash
fim-get: codestral: curl error 28
```

Providers disagree about where the message lives — Mistral uses `detail` for
auth and validation failures and `message` elsewhere, DeepSeek the
OpenAI-shaped `error.message` — so [agfi:h-fim-error-message] tries all three
before falling back to the raw body, and collapses the result to one line
because that is all `zle -M` shows.

`curl --fail-with-body` is deliberately *not* used. The status code comes back
on its own last line via `--write-out` instead, so both halves of a failure,
the code and the API's own words, are available to report.

Note that after editing either file you must `brishz-restart` before the garden
— and therefore Emacs's `z` — sees the change.

## The widget

`alt+.` sends everything left of the cursor as the prefix and everything right
of it as the suffix. That is `${PREBUFFER}${LBUFFER}`, not just `LBUFFER`:
once zsh is reading a continuation — an unclosed quote, a `for` still waiting
for its `done` — the lines you already typed live in `PREBUFFER` and `BUFFER`
holds only the line being edited. Without it, completing

```
: python3 -c "
def is_prime(n):
    <alt+.>
```

asks the model to continue four spaces of nothing. With it you get
`if n <= 1:`. Buffers can also hold real newlines with no continuation
involved, from `^V^J` or `edit-command-line`, and `LBUFFER` covers that on its
own.

The completion is still capped at one line (`fim_stop`), which is a separate
question from how much context goes in.

It reports just past the end of the line, in gray, with failures in red:

```
FIM: requesting codestral-latest…
FIM: inserted 13 chars in 0.3s
FIM: empty completion in 0.3s
FIM: line changed, discarded completion in 0.2s
FIM: aborted
FIM: codestral: HTTP 401 — Invalid API Key
```

The message goes in `POSTDISPLAY` with a `region_highlight` entry over it —
the mechanism zsh-autosuggestions greys its ghost text with — rather than in
the `zle -M` area below the prompt. `zle -M` was the obvious choice and is the
wrong one, because **it cannot carry colour at all**: see below.

Status is gray because it is a footnote to what you are typing; errors are the
one thing here worth looking up for, so they are not.

Async, following the `exec {fd}< <(...)` plus `zle -F` pattern from
zsh-autosuggestions' `src/async.zsh`. The child prints its pid first so it can
be cancelled, then one payload of `<retcode> US <stderr> US <stdout>`. stderr
travels separately rather than merged, so that anything unexpected written to
it cannot be spliced into the middle of your code.

One request at a time. A second `alt+.` supersedes the first rather than
racing it, so an impatient double press cannot produce two insertions. On
arrival the completion is dropped unless `BUFFER` and `CURSOR` still match the
snapshot taken when it was asked for — if you kept typing, it no longer fits
where it was going to go.

`Escape` cancels. It is bound permanently rather than only while a request is
in flight, because a binding installed for the duration of a request has to be
removed again on every exit path and a crash between the two leaves Escape
wedged. One press does one job: if there was a request to cancel it cancels
and stops there, and only if there was nothing of ours does it fall through to
what it replaced (`vi-cmd-mode` in `viins`, `beep` in `vicmd`). So idle Escape
behaves exactly as before, and cancelling costs you a second press to leave
insert mode. Note that cancelling costs up to `KEYTIMEOUT` (0.4s here) because
Escape is also the prefix of every arrow key — a delay that is already there
today when entering command mode.

The widgets are named `zle-fim-*` on purpose: `zle-*` is in the default
`ZSH_AUTOSUGGEST_IGNORE_WIDGETS`, so zsh-autosuggestions leaves them alone
instead of wrapping them and clearing the message.
[agfi:zle-complete-with-dots] in `.zshrc` is named that way for the same
reason. Fast-syntax-highlighting has no such list, so it is dealt with
separately — see [agfi:h-fim-zle-unwrap] below.

## Things that cost real time

**ZLE's special parameters are not bound in a `zle -F` handler.** `BUFFER` and
`CURSOR` read as empty there, so the snapshot check compared the live buffer
against `''` and reported `line changed` on every single request, including
ones where nothing had changed. Anything touching line state has to go through
a widget; the handler calls [agfi:zle-fim-accept] with `zle`, which is also
why the insertion lives in its own widget rather than inline. `POSTDISPLAY`
and `region_highlight` are in the same boat, hence [agfi:zle-fim-say].

**`zle -M` cannot show colour.** It renders the string through ZLE's display
code, which *visualises* control characters instead of emitting them, so an
SGR escape arrives as a reverse-video `^[` followed by a literal
`[38;2;170;170;170m` printed as text. `zle -R` behaves identically. Confirmed
on the wire: for `zle -R $'\e[31mX\e[0m'` the terminal received
`\x1b[7m` `^[` `\x1b[27m` `[31mX`. `POSTDISPLAY` plus a `region_highlight`
entry is the mechanism that works, because there zle applies the colour itself
and what reaches the terminal is a real `\e[38;5;242m`.

The trap in measuring this: **`cat -v` renders a real ESC byte and a literal
`^`+`[` pair identically**, so a pty capture piped through it cannot tell a
working colour from a broken one. That is how a first attempt got confirmed as
working when it was in fact printing escape codes as text. Compare raw bytes —
`\x1b` versus `0x5E 0x5B` — or check for `\x1b[` in the capture with Python.

**Fast-syntax-highlighting takes the colour back off.** It wraps every widget
that exists when it loads and runs `_zsh_highlight` *after* the widget body,
which rebuilds `region_highlight` from scratch and discards our entry; the
message then rendered in whatever style f-sy-h had left covering that column
(38;5;16, near-black). Widgets created *after* it loads are never wrapped —
which is why an ad-hoc test widget sourced at the prompt kept its colour while
the real one did not, since this file loads at `.zshrc:271` and f-sy-h at
`:572`. [agfi:h-fim-zle-unwrap] re-runs `zle -N` from a one-shot `precmd`,
after the whole of `.zshrc`, binding the names straight back to our functions.

Two consequences of being unwrapped. [agfi:zle-fim-accept] must call
`_zsh_highlight` itself, or the code it just inserted stays uncoloured until
the next keystroke — and it must do so *before* posting its own message, since
that call rebuilds `region_highlight`. And [agfi:zle-fim-escape] must not
chain into `vi-cmd-mode` after posting, because `vi-cmd-mode` is still wrapped
and its `_zsh_highlight` would strip the colour again; posting after the call
instead leaves the message a redraw behind and it never appears at all. Hence
one press, one job.

**Do not kill the process group.** zsh-autosuggestions cancels with
`kill -TERM -$pid` to reap whatever its strategy forked. With job control on,
two presses in quick succession can put both process substitutions in one
group, so killing the group takes the *replacement* request down along with the
one being cancelled — and the replacement then reports nothing at all, which
reads exactly like a race in the fd handling. It failed two times in five
against a real endpoint and every single time with the network stubbed out.
[agfi:h-fim-zle-cancel] kills the pid alone; nothing is orphaned, because curl
is writing into the pipe that child holds and takes SIGPIPE as soon as it dies.

All of these were found by driving a real line editor through `zsh/zpty`
rather than by reading the code. Stubbing `fim-get` out to a `sleep` is what
turned the process-group bug from intermittent into deterministic, and that is
the move worth remembering: an async bug that reproduces 40% of the time is
usually a timing race in something you are not looking at.

One note on the harness itself, `fim-zpty.zsh` in the session scratchpad: it
must wait for the prompt (`\e[?2004h`, bracketed-paste on) before typing, not
for a fixed number of seconds. A five-second guess was enough until the
machine got busy, and then every scenario went intermittent in a way that
looked exactly like a bug in the widget.
