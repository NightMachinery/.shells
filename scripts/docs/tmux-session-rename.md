# Naming a tmux session after the agent inside it

`tmux-session-rename-current-auto` (`tnameme`) renames the tmux session a
shell runs in after the Claude Code session running inside it. Type
`! tnameme` at a Claude Code prompt and `scripts-claudework2` becomes
`claude/work-wifi-dns-captive-portal`.

## Why

A tmux server with a dozen agent sessions is a list of names like
`scripts-claudework2` and `scripts-codex1`: the launch directory and a
counter, nothing about the work. The agent already knows what it is working
on -- Claude Code titles its own session -- so the shell it spawns can ask for
that name and hand it to tmux.

## The helpers

- `tmux-session-current-get` prints the name of the session containing this
  shell.
- `tmux-session-rename-current NAME` (`tsrc`) renames it. `.` and `:` in NAME
  become `-`.
- `tmux-session-rename-current-with-agent NAME` (`tsrcag`) prefixes NAME with
  the agent that spawned the shell: `claude/<profile>-NAME`, `codex-NAME`,
  `agy-NAME`. Outside an agent it fails and says so.
- `tmux-session-rename-current-auto` (`tsrca`, `tnameme`) takes no argument
  and uses the agent session's own name.

Detection is [agfi:ai-agent-name] in `zshlang/basic/conditions.zsh`, built on
the existing [agfi:claude-code-p] (`CLAUDECODE=1` or `AI_AGENT=claude*`) and
[agfi:codex-p] (`CODEX_SANDBOX` or `AI_AGENT=codex*`), plus a new
[agfi:antigravity-p] (`GEMINI_CLI` or `ANTIGRAVITY_CLI` set, or
`AI_AGENT=antigravity*|agy*|gemini*`). The Claude profile, `work` or
`default`, comes from matching `CLAUDE_CONFIG_DIR` against the
`claude_code_profiles` table in `claude.zsh`; unset means `default`.

## Where the name comes from

Claude Code exports `CLAUDE_CODE_SESSION_ID`, and `CLAUDE_CONFIG_DIR` for a
non-default profile, into every shell it spawns. The transcript is at
`${CLAUDE_CONFIG_DIR:-~/.claude}/projects/*/${CLAUDE_CODE_SESSION_ID}.jsonl`.
The glob is over project directories because the directory name encodes the
launch cwd, which the shell has no other way to recover.
[agfi:h-claude-code-session-name] hands the file to `claude_session name`
(`golang/claude_session`), which returns the user-set title, else Claude's own
generated name, else the slug, else the UUID, sanitized for filenames.

The `!` prefix is what makes this work. It runs the command in the agent's own
shell, where that environment exists. From another pane of the same session
the variables are absent and `tnameme` has nothing to read.

## Usage

    ! tnameme                    # inside Claude Code: claude/work-<session name>
    tsrcag fix-wifi              # claude/work-fix-wifi, codex-fix-wifi or agy-fix-wifi
    tsrc scratch                 # any shell in tmux, no prefix
    tmux-session-current-get

## Gotchas

- An agent's shell has no attached client, so tmux has no "current session"
  for it. Every helper derives one from the pane instead:
  `tmux display-message -p -t "$TMUX_PANE" '#S'`. That inherits
  `$TMUX_PANE`'s failure modes from `./tmux-tty-title.md`: `tmux run-shell`
  and garden shells.
- `tnameme` is Claude Code only. Codex and Antigravity have no session-name
  lookup yet; use `tsrcag NAME` there.
- The `agy` detection marker is carried over from Gemini CLI, not verified
  against a real `agy` shell.
- tmux rejects `.` and `:` in session names, so they are replaced with `-`
  rather than failing: `v1.2:fix` lands as `v1-2-fix`.
