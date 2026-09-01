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
- `fim_strip_space_p` — `y`; see below

```zsh
fim_provider=deepseek fim_max_tokens=200 fim-get 'def is_prime(n):
    '
```

Codestral is buggy and often prepends a single space, which
`fim_strip_space_p` drops. Removing it is occasionally wrong too, just much
less often than keeping it. The Emacs twin does the same thing.

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

`alt+.` sends `LBUFFER` as the prefix and `RBUFFER` as the suffix. It reports
in the `zle -M` message area below the prompt:

```
FIM: requesting codestral-latest…
FIM: inserted 13 chars in 0.3s
FIM: empty completion in 0.3s
FIM: line changed, discarded completion in 0.2s
FIM: aborted
FIM: codestral: HTTP 401 — Invalid API Key
```

`zle -M` rather than `POSTDISPLAY`, which is where a ghost-text plugin would
put it: POSTDISPLAY only renders at the end of the buffer, which is the wrong
place whenever there is a suffix, and zsh-autosuggestions clears it from under
you.

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
in flight, and chains into what it replaced: `vi-cmd-mode` in `viins`, `beep`
in `vicmd`, so idle behaviour is unchanged. A binding installed only for the
duration of a request has to be removed again on every exit path, and a crash
between the two leaves Escape wedged. Note that cancelling costs up to
`KEYTIMEOUT` (0.4s here), because Escape is also the prefix of every arrow key
— that delay is already there today when entering command mode.

The widgets are named `zle-fim-*` on purpose: `zle-*` is in the default
`ZSH_AUTOSUGGEST_IGNORE_WIDGETS`, so zsh-autosuggestions leaves them alone
instead of wrapping them and clearing the message.
[agfi:zle-complete-with-dots] in `.zshrc` is named that way for the same
reason.

## Two things that cost real time

**ZLE's special parameters are not bound in a `zle -F` handler.** `BUFFER` and
`CURSOR` read as empty there, so the snapshot check compared the live buffer
against `''` and reported `line changed` on every single request, including
ones where nothing had changed. Anything touching line state has to go through
a widget; the handler calls [agfi:zle-fim-accept] with `zle`, which is also
why the insertion lives in its own widget rather than inline.

**Do not kill the process group.** zsh-autosuggestions cancels with
`kill -TERM -$pid` to reap whatever its strategy forked. With job control on,
two presses in quick succession can put both process substitutions in one
group, so killing the group takes the *replacement* request down along with the
one being cancelled — and the replacement then reports nothing at all, which
reads exactly like a race in the fd handling. It failed two times in five
against a real endpoint and every single time with the network stubbed out.
[agfi:h-fim-zle-cancel] kills the pid alone; nothing is orphaned, because curl
is writing into the pipe that child holds and takes SIGPIPE as soon as it dies.

Both were found by driving a real line editor through `zsh/zpty` rather than by
reading the code. Stubbing `fim-get` out to a `sleep` is what turned the second
one from intermittent into deterministic, and that is the move worth
remembering: an async bug that reproduces 40% of the time is usually a timing
race in something you are not looking at.
