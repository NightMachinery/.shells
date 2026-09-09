# Naming a tmux session after the agent inside it

A tmux session that has a Claude Code session inside it is renamed after
that session automatically, by a hook: `scripts-claudework2` becomes
`@Claude/work wifi-dns-captive-portal` on the first prompt, and follows the
title as it changes. `tmux-session-rename-current-auto` (`tnameme`) does the
same by hand -- type `! tnameme` at a Claude Code prompt -- and is the entry
point for shells the hook does not reach.

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
  the agent that spawned the shell: `@Claude/<profile> NAME`, `@Codex NAME`,
  `@Agy NAME`. Outside an agent it fails and says so.
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

Claude Code exports `CLAUDE_CODE_SESSION_ID` into every shell it spawns. The
transcript is `<projects dir>/*/${CLAUDE_CODE_SESSION_ID}.jsonl`, searched in
every profile's projects directory ([agfi:h-claude-code-session-projects-dirs],
the same list the kitty `cmd+shift+o` picker uses), so it does not matter which
profile started the session. The glob is over project directories because the
directory name encodes the launch cwd, which the shell has no other way to
recover.
[agfi:h-claude-code-session-name] hands the file to `claude_session name`
(`golang/claude_session`), which returns the user-set title, else Claude's own
generated name, else the slug, else the UUID, sanitized for filenames.

The `!` prefix is what makes this work. It runs the command in the agent's own
shell, where that environment exists. From another pane of the same session
the variables are absent and `tnameme` has nothing to read.

## Automatic renaming

Two hook entries in `configFiles/claude-code/settings.json` (shared: both
profiles symlink to it) do the renaming without anyone typing `tnameme`.
`SessionStart` fires on startup, `--resume`, `/clear` and compaction;
`UserPromptSubmit` fires on every prompt, so a `/rename` or a title Claude
generates on its own is picked up at the next turn. Both run

    brishz_async=y brishz_in=MAGIC_READ_STDIN brishz2.dash claude-code-session-tmux-autoname "$TMUX_PANE"

`brishz_async=y` makes `brishz.dash` post the request from a background
process and return at once, so the prompt does not wait for the garden or
even for the HTTP round trip. Every garden hook line in `settings.json` uses
it now.

The body runs in the brish garden, which has no `$TMUX_PANE` of its own, so
the hook line passes it in. The hook payload supplies the rest:
`transcript_path` is the file the name comes from, and the profile is read
off it too -- whichever profile's projects directory it sits under
([agfi:claude-code-profile-of-transcript]) -- so nothing depends on the
garden's environment. It costs about 0.12 s per prompt on a 23 MB
transcript.

Automatic names carry an `@` prefix: `@Claude/work wifi-dns-captive-portal`,
`@Claude/default LinFine-1`. The `@` marks a session with an agent inside;
`tsrcag NAME` uses it too. `tnameme` produces exactly the hook's name, since
both go through [agfi:h-claude-code-session-tmux-name]; `tnameme-status`
says whether the hook will keep a session's name current.

The switch is the tmux user option `@claude_autoname`. It is on globally
(`set -g @claude_autoname on` in the tmux config), so every tmux session
with a Claude Code session inside gets renamed. A session option beats the
global, which is how you exempt one session:

    tnameme-off                          # this session: leave my name alone
    tnameme-on                           # this session: rename now and keep renaming
    tnameme-status                       # which value applies, and from where
    tmux-session-autoname unset          # drop the session option, follow the global
    tmux-session-autoname-global off     # change the default for the running server

Sessions whose name starts with `ag--` are never renamed, whatever the
option says. They belong to the tmux-subagents skill
(`~/code/skills/tmux-subagents`), which treats the session name as an
identity label with lineage and model fields; renaming one would destroy
it. The guard is a prefix check, so the skill needs no change.

The hook renames only when the computed name differs from the current one,
so on most prompts it is a no-op. Every failure path exits silently: the
hook discards output, and a broken rename must not cost a prompt.

## Usage

    ! tnameme                    # inside Claude Code: @Claude/work <session name>
    tsrcag fix-wifi              # @Claude/work fix-wifi, @Codex fix-wifi or @Agy fix-wifi
    tsrc scratch                 # any shell in tmux, no prefix
    tmux-session-current-get

## Gotchas

- An agent's shell has no attached client, so tmux has no "current session"
  for it. Every helper derives one from the pane instead:
  `tmux display-message -p -t "$TMUX_PANE" '#S'`. That inherits
  `$TMUX_PANE`'s failure modes from `./tmux-tty-title.md`: `tmux run-shell`
  and garden shells.
- `tnameme` renames only inside Claude Code. In a plain shell it does nothing
  and succeeds, so it is safe in launchers that run in both. Codex and
  Antigravity have no session-name lookup yet and get an error; use
  `tsrcag NAME` there.
- The `agy` detection marker is carried over from Gemini CLI, not verified
  against a real `agy` shell.
- tmux rejects `.` and `:` in session names, so they are replaced with `-`
  rather than failing: `v1.2:fix` lands as `v1-2-fix`.
- Claude Code's shell runs every command under `NO_BARE_GLOB_QUAL` and
  `NO_EXTENDED_GLOB`, so a bare `(N)` qualifier is a literal there and any
  function using one fails with "no matches found" when run via `!`. The
  session lookups set `bareglobqual` locally for that reason.
- A name you set by hand (`tsrc scratch`, `tsrcag x`) in a session with Claude
  Code inside lasts until the next prompt, when the hook puts the `@` name
  back. Run `tnameme-off` in that session first.
- The global default means any tmux session in which a Claude Code process is
  running gets renamed, including scheduled or scripted ones. Launchers that
  care about their session name should set the option off:
  `tmux set-option -t "$session" @claude_autoname off`.
