# Completion in agent terminals

`llm_complete` completes the input area of Claude Code and Codex. **Dabbrev**
means expanding the word fragment before the caret from text already visible
or recorded locally. The detached kitty worker receives a screen snapshot
from `configFiles/kitty/kittens/agent_complete.py`, then checks the current
prompt and caret before inserting. The kitten performs no network IO.

The module builds through [agfi:go-local-dep]. `bin/agent-complete.zsh` is the
launcher. Its arguments contain only an operation; screen and request text
travel through stdin. Go owns screen parsing, candidates, cycling and insertion.

Dabbrev searches the current screen nearest first and deduplicates candidates.
A repeated invocation replaces the inserted remainder with the next candidate
only if the complete prompt and caret still match the saved state. Moving the
caret, changing text, or switching CLIs starts a new expansion. Non-ASCII
candidates can expand once but cannot cycle: different editors delete combining
marks and emoji differently. This preserves useful Unicode completion without
guessing how many characters a backspace will remove.

Claude vim NORMAL and VISUAL modes refuse insertion. Codex has no vim mode.
Both completion paths remove CR, LF and control characters from inserted text.
A completion never includes an Enter key. Errors appear in kitty's existing
error overlay, which can be dismissed with Escape.

Cycling state contains prompt text. It lives under
`${XDG_STATE_HOME:-$HOME/.local/state}/llm_complete/`, with directory mode 0700
and file mode 0600. Each target has a separate state file keyed by the terminal
socket and window id, so two kitty instances cannot share cycling state.

Automated tests cover ordering, deduplication, cycling validation, screen
extraction, vim mode, UTF-8 trimming and sanitization. The screen fixtures are
fabricated from CLI layouts, with no real session text.

In kitty, `alt+/` invokes dabbrev only when a Claude Code or Codex command
line is in the foreground. The kitten checks the foreground process again,
so an old agent title never steals zsh bindings. Reload kitty.conf with Cmd+F5.

The corpus order is current screen, other windows in the same kitty OS window,
the agent's transcript, then tracked git paths in the foreground cwd. The
transcript is resolved with [agfi:h-agent-session-of-kitty-window], and parsed
by `agent_session completion-context` as a separate binary. Its bounded result
is cached by transcript mtime and size. The worker refreshes this cache after
insertion, so the first expansion can use screen and paths immediately and a
later press has transcript text ready. A cache miss never waits for the session
resolver on the insertion path. Git path lookup has a 15 ms deadline.

In tmux, `prefix /` invokes dabbrev and replaces the default describe-key
binding. It runs in the background and accepts only Claude Code or Codex panes.
Copy mode refuses completion. Other panes of the current window contribute
text before the transcript corpus. The tmux `@agent_session` option identifies
the transcript when hooks have recorded it.

The worker reads both physical and joined tmux captures to retain soft-wrap
information and uses the real pane caret. Insertion uses a private tmux buffer
and deletes it after pasting, keeping prompt text out of argv. Backspaces are
sent only for validated ASCII cycling remainders. Errors use display-message.

## Agent FIM

Alt-. in an agent kitty window and prefix-. in an agent tmux pane ask for
fill-in-the-middle completion. The detached worker extracts the CLI input
around the real caret, joins soft wraps, and preserves hard newlines. Claude
vim NORMAL or VISUAL mode refuses insertion. Some versions omit the NORMAL
label, so the worker reads only `editorMode` from applicable Claude settings
and requires the INSERT indicator when vim is configured. Dialogs without an input marker
also refuse insertion. Screen fixtures are fabricated examples for both CLIs.

The prefix uses the last 2000 prompt characters, the suffix the first 1000,
and context uses the last 1500 characters of the most recent assistant reply.
The `agent_session completion-context` command supplies that reply. If session
resolution fails, text above the input is the fallback. `agent_fim` in
`~/.config/llm_complete/providers.json` changes these budgets. Trimming counts
Unicode code points, never slices a UTF-8 byte sequence.

The default provider is codestral; `default_provider` changes it. Agent FIM
stops at newline by default. Every inserted result has CR, LF, escape and other
control characters removed. A result is inserted only if the foreground agent
process, complete extracted prompt and caret still match. A newer request or
dabbrev insertion supersedes an earlier FIM. Brief per-target locks protect
insertion and cycling state; no lock is held across a network request.

Kitty failures use its existing error overlay. tmux failures use its status
message. A stale completion is reported and discarded. A request may finish
after typing or switching vim mode; the re-read decides whether it can insert.

## Context logs

Agent requests log by default to `~/logs/llm_complete/completion.log`. The
folder is 0700 and every file 0600. Records are human-readable: UTC timestamp,
source, window or pane ID, provider, resolved model and generation parameters,
verbatim prefix and suffix with `⟦CURSOR⟧` at their boundary, duration, outcome
and verbatim completion. Keys and headers are excluded. Keep this private
runtime folder outside repositories.

Defaults cap each file at 1 MiB and keep four files, including the active one.
`logging.max_bytes` and `logging.files` configure rotation. A request too large
to fit is reported as a logging error rather than silently truncating context.
`agent_fim.log: false` disables the agent default. `logging.sources` overrides
individual sources, for example `{"emacs": true, "kitty": false}`. A per-call
`log` boolean takes precedence. zsh, Emacs and Hammerspoon do not log by default.
Shell calls can opt in with `fim_log_p=y`; Emacs calls can pass `:log t`.

Kitty remote text uses bare CR for a terminal soft wrap and CR+LF for a hard
break. Native snapshots and remote rereads now preserve the same physical rows.
The CLIs can also wrap their own editor rows without terminal continuation
flags; full-width rows are joined using the captured window width. A deliberate
hard newline exactly at the editor margin is indistinguishable from that wrap.
Codex also word-wraps short rows before long tokens without a terminal flag.
Those boundaries retain a newline in captured context; the screen does not
reveal whether the original separator was a space or a hard newline.
Blank hard lines inside the input remain part of the prefix or suffix.

Screen capture cannot distinguish padding from deliberate trailing spaces on
previous hard lines. Avoid depending on those spaces as model context. Cycling
with a non-ASCII remainder stays disabled until the two CLIs' deletion behavior
is established for the full set of combining and emoji sequences.

## Verification

The maintained `golang/llm_complete/tests/live-terminal.py` is opt-in. Give it
only dedicated scratch CLI targets. It never submits a prompt. It verifies
expansion and cycling, vim refusal, Persian/combining/emoji deletion behavior,
a delayed HTTP reply discarded after typing, an unchanged input accepting a
reply, and exact context logging. `--multiline-only` checks hard blank lines,
soft wraps and suffix capture with the caret before the remaining text.
Terminal rendering can canonically compose
combining marks, so assertions compare equivalent Unicode text. Current CLIs
delete Persian code points and whole combining/emoji graphemes; arbitrary
non-ASCII cycling remains conservative.

The hot launcher execs an existing fresh Go binary immediately. Missing or
outdated binaries still rebuild through [agfi:go-local-dep]. Observed live
latency includes the terminal's redraw and remote-control calls; this is slower
than pure candidate generation. The transcript warms in a detached helper after the first press, including a
press with no screen candidates. Press again once the cache is ready.

Live acceptance covered Claude Code and Codex in kitty and an isolated tmux
server: actual shortcuts, cycling, vim refusal, unchanged insertion and delayed
stale replies. TextEdit covered Hammerspoon's AX and key-capture paths. The real
zsh widget passed both a local stub and Codestral with pxa-local; the maintained
zpty harness also checks pipe bytes and sentinel argv privacy. Emacs was tested
in batch, including the actual plz rollback, without reloading the live server.
Other-window and other-pane corpora, detached transcript warming and the last
assistant reply were exercised with fabricated text. Parser parity fixtures and
transcript rendering parity use fabricated data. Observed end-to-end dabbrev
latency was roughly 150 to 320 ms including remote control and terminal redraw;
the tens-of-milliseconds aim remains unmet. Non-ASCII cycling remains disabled.

Git path queries have a 15 ms keypress deadline. A detached helper also caches
tracked paths, validated against the Git index's modification time and size.
A cold command that exceeds the deadline does not lose the corpus permanently;
the next press uses the warmed cache. A live scratch completion used tracked
paths from an existing repository; tests also cover inherited vcsh environment
removal and invalidation after an index change.
