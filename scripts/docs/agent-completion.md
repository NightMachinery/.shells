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
