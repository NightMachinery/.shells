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

Outside a terminal it is `hammerspoon/core/fim.lua`, on `hyper+shift+right` —
the same request, at the cursor of whatever text field is focused. See
**The Hammerspoon hotkey** below.

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

It reports through `zle -M`, below the prompt, after a blank line so the status
can never be read as a continuation of the command you are writing. The kind of
message is carried by a symbol rather than by colour:

```

❄ FIM ⋯ requesting codestral-latest
❄ FIM ✓ inserted 13 chars in 0.3s
❄ FIM ∅ empty completion in 0.3s
❄ FIM ∅ line changed, discarded completion in 0.2s
❄ FIM ∅ aborted
❄ FIM ✗ codestral: HTTP 401 — Invalid API Key
```

`❄` says the line is ours; then four states, which are the whole vocabulary:
`⋯` in flight, `✓` something was inserted, `∅` nothing happened but nothing is
wrong, `✗` it failed. Overridable as `fim_zle_sym_lead` / `_wait` / `_ok` /
`_none` / `_err`.

The leading newline is safe even though `zle -M` visualises control characters:
that treatment is for the unprintable ones, and a newline comes out as a real
line break. Verified on the wire rather than assumed.

Symbols rather than colour because **`zle -M` cannot carry colour**, and the
one mechanism that can must not be used here. Both halves of that are worth
reading before trying to improve it — see below.

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

Inserting invalidates the autosuggestion, via `zle autosuggest-fetch`. Our
widgets are named `zle-*`, so zsh-autosuggestions never sees the edit and will
not do this itself, and the suggestion it computed for the *old* line does two
bad things. Its `region_highlight` entry still covers the columns the
completion just landed in, so the code we inserted is painted in the suggestion
colour and reads as ghost text; and it is still live, so right arrow appends
something that no longer follows from the line. With `ec hello` suggesting
` world and then some`, completing to `ec hello world` and pressing right
arrow gave `ec hello world world and then some`. `autosuggest-fetch` rather
than `autosuggest-clear`, so a suggestion that fits the new line takes its
place instead of nothing — measured, the suggestion becomes ` and then some`
and right arrow now yields `ec hello world and then some`.

`Escape` cancels, and then does whatever it did before — `vi-cmd-mode` in
`viins`, `beep` in `vicmd` — so idle Escape is unchanged. It is bound
permanently rather than only while a request is in flight, because a binding
installed for the duration of a request has to be removed again on every exit
path and a crash between the two leaves Escape wedged. Cancelling costs up to
`KEYTIMEOUT` (0.4s here) because Escape is also the prefix of every arrow key —
a delay that is already there today when entering command mode.

The widgets are named `zle-fim-*` on purpose: `zle-*` is in the default
`ZSH_AUTOSUGGEST_IGNORE_WIDGETS`, so zsh-autosuggestions leaves them alone
rather than wrapping them. [agfi:zle-complete-with-dots] in `.zshrc` is named
that way for the same reason.

## The Hammerspoon hotkey

The same completion, everywhere else on the machine. `core/fim.lua` binds two
chords, and both insert one line at the cursor of whatever text field happens
to be focused:

- `hyper+shift+right` — the default provider, `fimDefaultProvider`, which is
  `codestral`. At 0.3s the answer is usually up before you have decided whether
  you wanted it.
- `hyper+ctrl+right` — `deepseek`, for when the output matters more than the
  1.4s.

`hyper+shift+right` used to move the mouse pointer. Those four
`hyper_bind_v2` arrow bindings in `hammerspoon/core/mouse.lua` are retired —
wrapped in `if false then`, not deleted — because purple mode's bare arrows
already do the same job and the chord is worth more here.

### Two ways to read the text

There is no per-app hook worth writing: a Telegram draft, a browser textarea
and a native Cocoa field all want the same completion and none of them share
an extension point. What they do not share is a way to *read* the text, so
there are two capture paths, and which one ran is shown in the in-flight
status: `❄ FIM ⋯ codestral (ax)` or `(keys)`.

`ax` asks the Accessibility API for the focused element's `AXValue` and
`AXSelectedTextRange`, and slices the value at the cursor. It is exact, it
costs nothing, it is invisible to the user, and it is the only path that can
tell afterwards whether the cursor moved. Native Cocoa fields have it, and so
do some Qt apps — sioyek does.

`keys` is the fallback, and it exists because Purple Telegram does not expose
its message draft field through Accessibility **at all** — not an empty value,
no element — and kitty exposes a text area that is always empty. Those are the
two apps this feature is most wanted in, so the ugly path is not optional. It
is `shift+cmd+up`, `cmd+c`, `right`, `shift+cmd+down`, `cmd+c`, `left`: the
first selection is cursor-to-start and gives the prefix, the right arrow
collapses it back to the cursor, the second selection is cursor-to-end and
gives the suffix, and the left arrow collapses that one back to the cursor
again. The clipboard is saved before the first copy and put back before the
request goes out.

The two arrow keys in the middle of that are not decoration, and leaving them
out is the bug this was first written with. The obvious assumption is that the
anchor stays at the original cursor across both extensions, so that
`shift+cmd+down` swings straight from prefix to suffix. It does not. Measured
in TextEdit with the cursor at offset 28 of a three-line document:
`shift+cmd+up` leaves the selection's *origin* at the top, so the following
`shift+cmd+down` grows `[0, end)` and hands back the entire document as the
"suffix", and the final left arrow collapses to 0 rather than to 28 — the
user's cursor silently teleports to the top of their document. A right arrow
collapses a selection to its right edge, which is exactly the cursor we
started from, and a left arrow to its left edge; with both in place the same
test captured `"def add(a, b):\n    return a "` and `"+ b\nprint(add(1, 2))"`
and put the cursor back at 28.

Each collapse is skipped when its copy timed out, because a timeout means the
selection was empty — and on an empty selection an arrow key is an ordinary
cursor move that would walk one character in the wrong direction.

A synthetic `cmd+c` into an *empty* selection changes nothing at all, and
there is no event to wait for and no way to ask the app, so the code polls the
pasteboard's change count every 20ms and treats `fimCopyTimeoutSeconds` (0.35)
of no change as "that selection was empty". Both selections empty means there
was nothing to complete, and the run stops there. Every bit of this is a chain
of timer callbacks rather than a loop: Hammerspoon's Lua thread is also its
event thread, and blocking it freezes every keystroke on the machine.

`fimForceKeystrokePath = true` makes the module skip `ax` and always take the
keystroke path. It is a debugging global — set it from the console with
`hs -c 'fimForceKeystrokePath = true'` — and it is the only way to exercise
half this code without opening Telegram.

### The state machine

A keyDown eventtap is started the moment the request goes out and stopped when
the run ends. It never exists at any other time: a permanently installed tap
sees every keystroke of every app, which is both a privacy cost and a latency
one, and the tap is deliberately started *after* the last synthetic keystroke
of the capture so it can never mistake our own `cmd+c` for the user typing.

While **requesting**:

- `Escape` cancels. The task is terminated, the key is swallowed, and the band
  says `❄ FIM ✗ cancelled`.
- Any other key **detaches**. The key goes through to the app untouched, the
  tap stops, and the request keeps running — you already paid for it. When it
  lands, the completion goes to the clipboard:
  `❄ FIM 📋 copied to clipboard (you kept typing) 0.4s`. The same happens if
  the frontmost application changed while you waited.

Once the completion is back it becomes a **ghost**: the band shows it verbatim
under a line reading `❄ FIM ✓ 0.3s · any key inserts, Esc discards`, and
nothing has been inserted yet.

- `Escape` discards it: `❄ FIM ∅ discarded`.
- A chord carrying `cmd` or `ctrl` — `cmd+tab`, `cmd+c` — is a command rather
  than typing, so the ghost gets out of the way: the completion goes to the
  clipboard and the chord is delivered. So does a change of frontmost app.
- Anything else **accepts**. The completion goes onto the pasteboard and the
  tap returns three replacement events — `cmd+v` down, `cmd+v` up, and a copy
  of the key you actually pressed — so the paste is delivered *before* your
  keystroke rather than racing it. Swallowing the key and re-posting it from a
  timer would leave a window in which the two arrive the wrong way round.
  The clipboard is restored 0.3s later, and only if it still holds the
  completion, so a copy you made in between is not clobbered.
- Nothing at all, for `fimGhostSeconds` (45), and the ghost times out — to the
  clipboard again, `❄ FIM 📋 copied to clipboard (timed out)`.

On the `ax` path the cursor position is re-read at accept time, and a
completion computed for a cursor that has since moved is *not* pasted: it
corrupts the line rather than completing it, so it goes to the clipboard with
`❄ FIM 📋 cursor moved, copied to clipboard`. The `keys` path cannot check
this, which is one more reason to prefer `ax` where it works.

The clipboard is the fallback destination for every case where we could not
paste. A completion that was paid for is never silently thrown away.

A second press supersedes the first rather than racing it, exactly as the zsh
widget does. `fimCancel()` — the public escape hatch, and what a new press
calls first — stops the tap, terminates the task, stops the timers and
dismisses the band. The run id it bumps is what makes the superseded request's
callback a no-op when it eventually arrives. `fimState` is a global for the
same reason: after `hs.reload()` the previous chunk's eventtap is still alive,
held by the objc runtime rather than by any Lua reference, and the module
finds it there and stops it at load time.

Status goes to a band through `alert_gateway` with a fixed id, on the active
screen only and with no fullscreen flash, and carries the same symbols as the
zsh widget: `❄` marks it as ours, then `⋯` in flight, `✓` inserted, `∅`
nothing but nothing wrong, `✗` failed, and `📋` for the clipboard fallback,
which the zsh side has no need of. Markup is explicitly plain, because
completions are code and `*` and `_` in code must not be eaten.

### What it sends, and what it refuses to

Whatever field is focused when you press the chord is what gets sent to the
provider. There is no filter on which app, which field, or what the text looks
like. That is the deal, and it is worth being plain about rather than burying.

Two limits on it. `fimPrefixMaxChars` (4000) and `fimSuffixMaxChars` (1000) cap
the context — the prefix keeps its last N characters, the suffix its first N,
cut on code-point boundaries so the provider never receives invalid UTF-8 —
so a chord pressed at the bottom of a very long document does not ship the
whole thing. And if `hs.eventtap.isSecureInputEnabled()`, the run refuses
before touching anything at all: `❄ FIM ✗ Secure Input is on`. Nothing is
read and no keystroke is sent. A password field somewhere has told the system
that nobody may observe the keyboard, and that is exactly what both capture
paths would otherwise do.

`AXSelectedTextRange.location` counts UTF-16 code units, not bytes and not
code points, so slicing a Lua byte string at it needs a walk that charges two
units for anything outside the BMP. Getting this wrong splits an emoji and
sends the provider a broken byte, which is the sort of bug that only shows up
in someone's chat message months later.

### What is verified

Driven end to end in a scratch TextEdit document, with the completions and
documents quoted as they came back.

The **ax** path, from `def add(a, b):` and a four-space second line with the
cursor at offset 19: band `❄ FIM ⋯ codestral (ax)`, then a ghost holding
`return a + b` at 0.4s. Typing `#` accepted it and the document read

```
def add(a, b):
    return a + b#
```

so the paste landed *before* the typed character. The returned-replacement-
events route worked on the first try; the swallow-and-re-post fallback was
never needed and is not in the code.

The **keys** path, forced with `fimForceKeystrokePath`, cursor at offset 19 of
a three-line document: captured prefix `"def add(a, b):\n    "` and suffix
`"\nprint(add(1, 2))"`, both exactly what lay either side of the cursor;
ghost `return a + b`; accepting with `#` gave

```
def add(a, b):
    return a + b#
print(add(1, 2))
```

with the third line untouched, the cursor back where it started and the
clipboard back to its previous contents. The whole run took 1.6s against the
ax path's 0.4s, which is the honest price of the clipboard dance.

`Escape` on a ghost left the document byte-identical and cleared the state.
Typing during flight detached as intended: the key reached the document
(`def add(a, b):\n    X`), the tap stopped, the task kept running, and the
completion — DeepSeek's `" return a + b"`, leading space intact — arrived on
the clipboard instead of in the text. A second press superseded the first: the
run id went 6 to 7, there was never more than one band, `❄ FIM ⋯ deepseek (ax)`
was replaced in place by `❄ FIM ⋯ codestral (ax)`, and the abandoned task's
callback never fired. A bogus provider put its own stderr on the band:

```
❄ FIM ✗ fim-get: unknown provider 'NOSUCH'; known: codestral deepseek deepseek-flash
```

Asking for a completion between `return a ` and `+ b`, where nothing belongs,
gave `❄ FIM ∅ empty completion 0.4s` and changed nothing.

Checked separately in Hammerspoon's own Lua: the UTF-16 offset walk against a
string containing `é`, an emoji and a right-to-left mark (offset 3 of
`a é 😀 b` lands after the emoji, offsets 1/2/5 where they should); the caps
against ten `é` and five emoji, both cut to the requested number of code points
with valid UTF-8 on both sides; the invalid-UTF-8 fallbacks, which return
something rather than throwing.

Not verified: **Telegram**, which is the app the keystroke path exists for. Try
`hyper+shift+right` in a draft, then a key to accept it. If the band says
`(ax)` rather than `(keys)` then Telegram has grown an accessible draft field
since this was written. If the capture comes back wrong, the thing to compare
against is a plain TextEdit window with `fimForceKeystrokePath` set — and the
first thing to suspect is the selection-anchor behaviour above, since nothing
guarantees a Qt text widget collapses selections the way NSTextView does.

Also unverified: kitty, sioyek, and any browser text area.

## Things that cost real time

**ZLE's special parameters are not bound in a `zle -F` handler.** `BUFFER` and
`CURSOR` read as empty there, so the snapshot check compared the live buffer
against `''` and reported `line changed` on every single request, including
ones where nothing had changed. Anything touching line state has to go through
a widget; the handler calls [agfi:zle-fim-accept] with `zle`, which is also
why the insertion lives in its own widget rather than inline. `POSTDISPLAY`
and `region_highlight` are bound the same way, which matters if you ever try
the colour route below.

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

## Why the status is not coloured

Worth writing down, because "just make it gray" looks like a five-minute job
and is not. Two independent walls, and a measurement trap that hid both.

**`zle -M` cannot carry colour.** It renders its argument through ZLE's
display code, which *visualises* control characters instead of emitting them,
so an SGR escape arrives as a reverse-video `^[` followed by a literal
`[38;2;170;170;170m` printed as text. `zle -R` behaves identically. Measured:
for `zle -R $'\e[31mX\e[0m'` the terminal received
`\x1b[7m` `^[` `\x1b[27m` `[31mX`.

**The mechanism that can carry colour must not be used here.** `POSTDISPLAY`
plus a `region_highlight` entry does work — zle applies the colour itself, so a
real `\e[38;5;242m` reaches the terminal — and it is how zsh-autosuggestions
greys its ghost text. But `POSTDISPLAY` is not a neutral scratch area, it *is*
the suggestion slot, and `_zsh_autosuggest_accept` does

```zsh
BUFFER="$BUFFER$POSTDISPLAY"
```

whenever the cursor is at the end of the line. So right arrow silently splices
`  FIM: inserted 2 chars in 0.3s` into your command line as real text, and so
do `end-of-line` (`^E`), `vi-forward-char`, `vi-end-of-line` and `vi-add-eol`,
while the eight `ZSH_AUTOSUGGEST_PARTIAL_ACCEPT_WIDGETS` — `forward-word`
(alt-f), `vi-find-next-char` and friends — splice in a *fragment*. If
`ZSH_AUTOSUGGEST_EXECUTE_WIDGETS` is ever populated, the same text gets run.
Squatting there also evicts the suggestion you were about to accept, and puts
grey text just past the cursor where the trained reflex is to accept it.

Defending it would mean wrapping thirteen widgets to clear our message before
delegating, and it would still evict suggestions and still invite the reflex.
Not worth it for a colour.

**Fast-syntax-highlighting would fight it too**, if you go that way anyway: it
wraps every widget existing when it loads and runs `_zsh_highlight` *after* the
widget body, rebuilding `region_highlight` from scratch and discarding the
entry. Widgets created after it loads are never wrapped, which is why an
ad-hoc test widget sourced at the prompt kept its colour while the real one did
not — this file loads at `.zshrc:271` and f-sy-h at `:572`. Re-running `zle -N`
from a one-shot `precmd` takes the names back, at the cost of having to call
`_zsh_highlight` by hand after inserting.

**The measurement trap**, and the reason a first attempt was reported as
working when it was printing escape codes to the screen: **`cat -v` renders a
real ESC byte and a literal `^`+`[` pair identically**. A pty capture piped
through it cannot distinguish working colour from broken colour. Compare raw
bytes — `\x1b` versus `0x5E 0x5B` — and note that this cuts the other way too,
since the UTF-8 status symbols come out of `cat -v` as `M-` sequences that no
plain grep will match.

The remaining colour-capable option is `RPROMPT`, which never touches `BUFFER`.
It needs composing with pure prompt and with the existing
`$(vi_mode_prompt_info)` rather than assigning over them, and zsh hides
`RPROMPT` entirely when the command line is long — which would swallow exactly
the error messages that most need seeing.

## The test harness

One note on `fim-zpty.zsh` in the session scratchpad: it
must wait for the prompt (`\e[?2004h`, bracketed-paste on) before typing, not
for a fixed number of seconds. A five-second guess was enough until the
machine got busy, and then every scenario went intermittent in a way that
looked exactly like a bug in the widget.
