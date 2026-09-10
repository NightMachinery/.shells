# Naming a tmux session after the agent inside it

A tmux session with an agent session inside it is renamed after that
session, by a hook the agent fires: `scripts-claudework2` becomes
`+Claude/work tidy-up-the-lint-config` on the first prompt and follows the
title as it changes. It works for Claude Code, Codex and Antigravity (`agy`)
alike; the names are `+Claude/work <name>`, `+Claude/default <name>`,
`+Codex <name>` and `+Agy <name>`. The `+` marks a session with an agent
inside; a space separates the agent from the name. Without it a server with
a dozen agent sessions is a list of launch directories and counters.

The marker is [agfi:agent_tmux_name_marker], one `typeset` in
`zshlang/auto-load/others/agent-tmux.zsh`. It used to be `@`, which tmux
reads as window-id syntax; see "Session names are not tmux targets" below.

## By hand

- `tsrc NAME` ([agfi:tmux-session-rename-current]) renames the session
  containing this shell, no prefix.
- `tsrcag NAME` ([agfi:tmux-session-rename-current-with-agent]) gives the
  hooks' shape with a name you chose. Outside an agent it fails and says so.
- `tnameme` (`tsrca`, [agfi:tmux-session-rename-current-auto]) uses the agent
  session's own name -- `! tnameme` at a Claude Code prompt. Outside any agent
  it is a silent no-op that succeeds, so launchers can run it in either shell.

Agent detection is [agfi:ai-agent-name] in `zshlang/basic/conditions.zsh`.

## The shared core

Everything lives in `zshlang/auto-load/others/agent-tmux.zsh`. Each agent's
hook body is a thin parser that ends in one call,
[agfi:h-agent-tmux-autoname] `<agent> <pane> <id> <transcript>`, which

- records the identity on the tmux session as the user option
  `@agent_session` (agent, id, transcript, tab-separated; read it back with
  [agfi:agent-tmux-identity-get]);
- stops if the session is named `ag--*`. Those belong to the tmux-subagents
  skill, whose names carry lineage and model identity;
- stops unless the user option `@agent_autoname` resolves to `on`. It is read
  with `show-option -A`, so a session value beats the global default, which
  `~/.tmux.conf` sets to `on`;
- computes the name, runs it through [agfi:h-tmux-session-name-sanitize]
  (`.` and `:` become `-`, whitespace squeezed, cut at 60) and renames only
  when the result differs from the current name. That last test is also the
  migration path: a session still carrying an old `@` name renames itself at
  its next prompt.

Every early return is an ordinary outcome and every path is silent: a broken
rename must not cost a prompt.

The `@agent_session` option has a second reader: the resolver behind
`cmd+shift+o`, which jumps to the kitty window showing an agent session. When
that window holds a tmux client the option answers directly, for any agent,
and it beats that feature's own record of where a session was last seen. See
`docs/agent-sessions.md`.

The hook bodies are [agfi:claude-code-session-tmux-autoname],
[agfi:codex-session-tmux-autoname] and [agfi:agy-session-tmux-autoname], each
`<tmux-pane> [payload]` with the payload on stdin. They run in the brish
garden, which has no pane of its own, so every hook line passes `"$TMUX_PANE"`
in. All three use `brishz_async=y`, so the agent never waits on the garden or
even the HTTP round trip; measured 0.15 s down to 0.03 s per prompt.

`agents-md-doctor` checks all three hook and settings symlinks
(`agents_md_settings` in `agents-md.zsh`) and warns when an app has replaced
one with a plain file.

Every agent's hook config also carries a second handler beside the autoname
one, calling `claude-code-session-register` or [agfi:agent-session-register]
to record which kitty window is showing the session, under
`$XDG_STATE_HOME/agent-sessions`. Antigravity keeps it in its own top-level
group `kitty-register`, leaving `tmux-autoname` alone. The two are
independent, and neither waits on the other.

## Toggles

    tnameme-on                           # this session: rename now and keep renaming
    tnameme-off                          # this session: leave my name alone
    tnameme-status                       # which value applies, and from where
    tmux-session-autoname unset          # drop the session option, follow the global
    tmux-session-autoname-global on|off|unset   # the running server's default

`tnameme-on` also renames right away when run inside an agent. The
usage-notification scheduler sets the option off on its own session, because
it re-arms by name.

## Claude Code

Hooks sit in the shared `configFiles/claude-code/settings.json` (both
profiles symlink to it) on `SessionStart` and `UserPromptSubmit`; stdin
carries `session_id` and `transcript_path`. The profile is whichever one's
projects directory the transcript sits under
([agfi:claude-code-profile-of-transcript]); the name is
[agfi:h-claude-code-session-name]: the user's title, else the generated
title, else the slug, else the uuid.

`tnameme` finds its own session through `CLAUDE_CODE_SESSION_ID`, exported
into every shell Claude Code spawns, `! cmd` included
([agfi:claude-code-session-current-id], `-file`, `-name`). Claude Code
snapshots hooks at startup, but in practice a newly added hook applied to a
running session at its next prompt; if it does not, `/hooks` or a resume.

## Codex

Verified in source at tag `rust-v0.153.4`; the hook body has been run with
real payloads, not yet by a real Codex turn. Hooks live in
`~/.codex/hooks.json`, symlinked to `configFiles/codex/hooks.json`, on
`SessionStart` and `UserPromptSubmit`, in the grouped shape
`{"hooks":[{"type":"command","command":...,"async":true,"timeout":10}]}`.
The command runs through the user's shell with a snapshot of Codex's
environment from session start, so `TMUX_PANE` is present. stdin carries
`session_id` (the thread uuid, also the rollout file id; a subagent gets the
root session's) and `transcript_path`.

The name is `thread_name` from `$CODEX_HOME/session_index.jsonl`, last line
per id wins ([agfi:codex-thread-name]). Codex writes it on `/rename`, as a
provisional first-36-characters title, then as a model-generated title; a
cleared name appends an empty row, and we fall back to the first 8
characters of the id.

## Antigravity (agy)

Verified by disassembling the 1.1.28 binary; the hook body has been run with
real payloads, not yet by a real agy turn. Hooks live in
`~/.gemini/config/hooks.json`, symlinked to `configFiles/antigravity/hooks.json`,
keyed at the top level by a hook name (`"tmux-autoname"`). Events used:
`SessionStart` (undocumented but parsed; conversation start) and `Stop` (once
per turn end). Non-tool events take the handler object directly,
`{"type":"command","command":...,"timeout":5}`, not a matcher group.

Hooks run via `sh -c`, synchronously (default timeout 30 s, blocking the
agent loop), with agy's environment plus `ANTIGRAVITY_CONVERSATION_ID` (its
`run_command` shells get that too, plus `ANTIGRAVITY_AGENT=1`), and
the directory containing `hooks.json` as cwd. stdin carries `conversationId`,
`transcriptPath`, `workspacePaths`, `lastUserInput` and more, but no title.
Empty stdout is tolerated but logs an ERROR per turn, so the hook line ends
with `echo '{}'`.

The name is `title` (set with `/rename` or F2 in `/resume`), else `preview`
(the model-generated title), from
`~/.gemini/antigravity-cli/conversation_summaries.db`
([agfi:agy-conversation-name]), else the first 8 characters of the id.

## Usage

    ! tnameme                    # inside an agent: +Claude/work <session name>
    tsrcag fix-wifi              # +Claude/work fix-wifi, +Codex fix-wifi or +Agy fix-wifi
    tsrc scratch                 # any shell in tmux, no prefix
    tnameme-off                  # keep a hand-set name
    tnameme-status
    agent-tmux-identity-get      # who the hook says lives here

## Session names are not tmux targets

A leading sigil in a `-t` target declares a *type*, not a name: `$` is a
session id, `@` a window id, `%` a pane id, and `=` asks for an exact name
match. Session names are free to start with any of those, so a name handed
straight to `-t` can be read as something else entirely. Measured on tmux
3.6a, against a session actually called `@Claude/work tidy-up-the-lint-config`:

    tmux has-session -t '@Claude/work tidy-up-the-lint-config'    # can't find window
    tmux has-session -t '=@Claude/work tidy-up-the-lint-config'   # ok
    tmux lsp -s -t '=@Claude/work tidy-up-the-lint-config'        # can't find window
    tmux lsp -s -t '=@Claude/work tidy-up-the-lint-config:'       # ok
    tmux lsp -s -t '$41'                                          # ok

So `=` rescues only the *session*-typed targets -- `attach-session`,
`has-session`, `kill-session`, `switch-client`, `set-option`. `list-panes`
declares a **window** target even under `-s`, and wants a trailing `:` on top
of the `=`. A session id needs neither.

Use [agfi:tmux-session-id]. It takes a name or an id and prints an id, which
is unambiguous in every target position. It also closes a race the autoname
hooks make routine: they rename on every prompt, so a name captured from
`tmux ls` can be gone by the time you act on it. [agfi:fftmux] (`fft`,
`fftk`, `fftr`), [agfi:fftmux-agent] (`ffta`, the picker over the sessions
running an agent) and [agfi:tmux-alive-p] all go through it now; use
[agfi:tmux-session-name-of] to turn an id back into something a human reads.

`ffta` is where a rename bites hardest, since the tmux name a Claude session
reports is the one it was launched under: see the tmux picker section of
`./agent-sessions.md` for how a name that no longer names anything is resolved
through the pane instead.

One more helper lives beside those: [agfi:tmux-session-goto], the verb `fft`
now uses. `attach-session` refuses to nest, so a bare `tmux a -t` from inside
tmux only ever prints *sessions should be nested with care* -- which is where
a session picker is most useful. `tmux-session-goto` switches the client
instead when `$TMUX` is set, and attaches when it is not; `tmux-attach` and
`tma` go through it too.

This is why the marker is `+` and not `@`. `@` was window-id syntax, which
made every agent-named session unusable as a target -- `fft` on one failed
with *can't find window* -- and no amount of `=` fixed the window-typed half.
`+` is an ordinary character to tmux, and still sorts agent sessions to the
top of `tmux ls`, ahead of where `@` put them.

Sessions whose agent is still running rename themselves at the next prompt.
Anything left over from before the change can be swept up with:

    command tmux ls -F '#{session_id}'$'\t''#{session_name}' |
        while IFS=$'\t' read -r id name ; do
            [[ "${name}" == @* ]] && command tmux rename-session -t "${id}" "+${name#@}"
        done

## Gotchas

- An agent's shell has no attached client, so tmux has no "current session"
  for it. Every helper derives one from the pane,
  `tmux display-message -p -t "$TMUX_PANE" '#S'`, and inherits `$TMUX_PANE`'s
  failure modes from `./tmux-tty-title.md`: `tmux run-shell` and garden shells.
- A hand-set name (`tsrc scratch`, `tsrcag x`) in a session with an agent
  inside lasts until the next prompt, when the hook puts the `+` name back.
  Run `tnameme-off` there first.
- The global default catches scheduled and scripted sessions too. Launchers
  that care about their name should set the option off:
  `tmux set-option -t "$session" @agent_autoname off`.
- Claude Code's Bash tool runs with `NO_BARE_GLOB_QUAL` and
  `NO_EXTENDED_GLOB`, so a bare `(N)` is a literal there and a function using
  one fails with "no matches found" under `!`; it needs
  `setopt localoptions bareglobqual`.
- Codex trusts no new hook handler until you accept it in the review it shows
  at startup (persisted as `hooks.state.<key>.trusted_hash` in
  `config.toml`), so the first `codex` launch after adding hooks asks once.
  Both handlers in that file are hashed, so editing either one makes both
  untrusted and the next launch asks again.
- Codex injects any non-JSON stdout from a hook into the model's context as
  `additionalContext`. Exit 0 with empty stdout is harmless; the `>/dev/null`
  in the hook line is load-bearing.
- `CODEX_THREAD_ID` and `CODEX_SESSION_ID` reach Codex's shell tool, so
  `tnameme` works from the environment there, but not hooks or notify.
- agy wants the flat handler shape for non-tool events. The grouped form makes
  it drop the whole file with a warning visible only under `--log-file`
  (issue #925).
- agy's `run_command` shells carry `ANTIGRAVITY_AGENT=1`,
  `ANTIGRAVITY_TRAJECTORY_ID`, `ANTIGRAVITY_CONVERSATION_ID`, `TERM=dumb` and
  `PAGER=cat` (read off `command.(*goRunner).Run` in the 1.1.28 binary). That
  is what [agfi:antigravity-p] keys on; agy is not Gemini CLI and sets no
  `GEMINI_CLI`. So `tnameme` works from the environment there. A shell agy
  did not spawn falls back to the identity the hook recorded on the tmux
  session, which exists once the first hook has fired, not before.
- The trajectory id is not the conversation id (the two differ in
  `conversations/<id>.db`), so only `ANTIGRAVITY_CONVERSATION_ID` names a
  session.
