# `/done`: ending a session on purpose

`/done` is one skill, shared by Claude Code, Codex and Antigravity. It asks the
agent to check that the work is actually finished, write an executive summary,
and then end the session — leaving the summary on the screen the session was
using, rather than losing it with the TUI.

Two halves, and the split matters: the agent decides *whether* the work is
done, because only it knows what it was doing, and [agfi:agent-done] decides
*how* a finished session ends, because that turns out to be fiddly.

## One skill, three agents

The skill is a single tracked file, `configFiles/agent-skills/done/SKILL.md`,
symlinked into each agent's skills directory by [agfi:agent-skills-link]:

    ~/.claude/skills/done/SKILL.md
    ~/.claude-work/skills/done/SKILL.md
    ~/.codex/skills/done/SKILL.md
    ~/.gemini/config/skills/done/SKILL.md

All three agents converged on the same format — `<dir>/<name>/SKILL.md` with
YAML frontmatter carrying `name` and `description` — and all three expose such
a skill as `/<name>` typed at the prompt. Antigravity says so in its own
migrate-workflows skill, which exists to move people off its older
`~/.gemini/config/global_workflows/*.md` and onto skills, listing "first-class
slash command support" as the reason. So one file can serve every agent, and
the prose in it is the part worth getting right once.

The alternative was one file per agent in each agent's own preferred shape —
`commands/done.md` for Claude Code, `prompts/done.md` for Codex, a workflow for
Antigravity. That needs a generator, and it would have three copies of the
instructions to keep in step. Symlinks need neither.

Claude Code contributes two directories, not one: skills live under the config
home, so a skill installed in `~/.claude` is invisible to a work session. The
list of directories comes from [agfi:h-agent-skills-dirs], which reads the
seats from the generated profile tables, so a new seat needs nothing here. A
host without one of the agents contributes no directory and gets no link.

Antigravity's is the one path worth a note: its *conversations* live in
`~/.gemini/antigravity-cli`, but its *configuration* — hooks, MCP servers,
skills — is one level up in `~/.gemini/config`, which is also where the tracked
`hooks.json` is linked.

Installation happens in [agfi:h-agent-launch], next to the instruction-file
sync and for the same reason: an agent reads its skills at startup, so the
moment to make sure they are there is just before one starts. It costs a stat
per skill per directory. [agfi:agents-md-doctor] reports the links, and will
say `UNTRACKED` if an agent has replaced one with a file of its own.

## What `agent-done` does

    zsh -ic 'agent-done' <<'SUMMARY'
    ...
    SUMMARY

In order: resolve which agent this is ([agfi:ai-agent-name]) and which session
([agfi:h-agent-done-id], which dispatches to the adapter that knows how that
agent exports its id); find its name from the live registry; write the summary
plus a header — name, seat, id, transcript, directory, timestamp — to a file
under `$(h-agent-done-dir)`; find the agent's own process; send it `SIGTERM`;
and arrange for the report to appear once it is gone.

`--dry-run` does everything except the killing and prints what it resolved,
which is how the whole path is testable without ending a session.

## Why the summary needs all this

A TUI draws on the alternate screen. When it exits, the terminal restores what
was there before, so everything the agent printed — the summary included — is
gone at exactly the moment the user wants to read it. Writing the summary into
the transcript instead is worse: it is precisely the file nobody opens.

So the summary is written to a file, and something *outside* the agent puts it
back on the screen afterwards. Under tmux that something is
[agfi:h-agent-done-watch], run by the tmux server (`run-shell -b`), which
survives the pane by construction. It waits for the agent's process to go
(`agent_done_wait_s`, 20s, then `SIGKILL`), and then:

- `respawn-pane -k` replaces the pane's dead shell with the script that shows
  the report (see the next section). This is also what kills the pane: `-k`
  takes out whatever is still in it.
- `remain-on-exit` was turned on *before* anything was killed, so when that
  `cat` returns, the pane stays on screen with the report in it and tmux's own
  `Pane is dead (status 0, ...)` line beneath. `kill-pane` would have taken the
  report away along with the pane, which is the opposite of the point.

## Restarting the pane resumes the session

tmux gives a pane one command slot, and `respawn-pane -k` — `prefix-r` in this
configuration — re-runs whatever the pane last ran. Left alone that would mean
re-printing the report, so the command the dead pane is left holding is a small
generated script, `<report>.pane.sh`, that does two different things:

- first run: mark itself as shown, clear the pane, `cat` the report, exit. The
  pane dies with the summary on it, as before.
- any later run: `cd` to the session's directory and resume it.

Resuming does not restore the directory on its own, which is worth stating
because it looks as though it should: [agfi:claude-code-session-resume] runs
the launcher wherever you are and only *warns* when that disagrees with the
session's project, and Codex's and agy's resume verbs do not even warn. So the
script `cd`s first — and to the session's own directory, not the pane's.

Nothing is inferred to find it: every agent records the cwd in its transcript,
and `agent_session <agent> meta <transcript>` prints it as the third field for
all three. [agfi:h-agent-session-dir] asks for exactly that — the same
resolver the resume helpers themselves use, so the directory this report names
and the directory a resume lands in cannot disagree. Two fallbacks stand
behind it, in order: Claude Code's project directory, which
names the directory a session started in with every non-alphanumeric character
replaced by a dash — lossy, since `-Users-evar-my-dir` could be two different
paths, so it is accepted only when the result really is a directory — and then
the pane's own path. The report's `cwd:` line shows whichever won, so it names
the place a resume will land, and `agent_done_cwd` overrides all three.

The pane's path is a genuinely different answer, not a cheaper one. This
feature was written in a session that had been restarted into a scratch
directory: the pane said the scratch directory, the transcript's project said
`~/scripts`, and the recorded cwd said the scratch directory too — because
that is where the session really was working. Recorded wins, which is why the
answer can be checked rather than argued about.

Resuming goes through [agfi:agent-session-resume], which resolves the agent
from the transcript path and calls that agent's own launcher. So a work session
comes back on the work seat with its cues repainted, and none of this needs to
know which agent it just ended. A session whose transcript could not be
resolved gets an interactive shell instead, which is still better than the same
report twice. `agent_done_resume_cmd` replaces the resume outright — the escape
hatch for resuming with extra flags, and how the branch is tested without
starting a real session.

Since the resume helpers now `cd` on their own
([agfi:h-agent-session-resume-run]), the script's own `cd` is belt and braces:
it also covers the shell that a session with no resolvable transcript falls
back to.

`remain-on-exit` stays on for that pane afterwards, so the loop holds: quit the
resumed session and the pane dies visibly again, and `prefix-r` brings it back.

These panes are findable rather than only stumbled upon:
[agfi:fftmux-agent-all] (`fftaa`) lists them beside the live sessions with a
💀, and picking one puts you in the session with the report still on screen,
from where `prefix-r` is unchanged. It recognises such a pane by the command it
is left holding and reads the transcript back out of the script's resume line,
so the shape of that line is now a consumed contract and not only something the
pane runs. See "The sessions `/done` has ended" in `agent-sessions.md`.

The one thing that destroys them is `tzkill` ([agfi:tmuxzombie-kill]), which
clears every dead pane on the machine without asking what it was.

Outside tmux there is no pane to respawn, so the watcher writes the report to
the terminal directly. Two details make that work. The terminal is found from
the *agent's* process (`ps -o tty=`), not from the calling shell: the shell an
agent runs its tools in generally has no controlling terminal at all, so it
cannot name the terminal the session is attached to. And the watcher is
detached with [agfi:awaysh-sure] (`setsid zsh -c`), because it has two deaths
to outlive — the skill invokes `agent-done` on the right of a pipeline, so that
shell exits immediately, and an agent commonly takes its whole tool-shell tree
with it when it exits.

The reports are kept: [agfi:agent-done-reports] lists them newest first and
[agfi:agent-done-report-last] prints the newest, so nothing is lost if the
terminal is closed before anyone reads it. They live next to the session
registry rather than under `~/tmp`, which gets swept — see
[agfi:h-agent-session-registry-dir] for the time that mattered.

## The pane cues

The launcher paints per-seat cues on the pane (see `claude_code_usage.md`), and
undoes them in an `always` block on the way out. A killed pane never reaches
that block, so `agent-done` calls [agfi:h-claude-tmux-cues-teardown] itself
first. Most of those cues are pane options and die with the pane anyway; the
border row is a *window* option and does not, so without this the window would
be left with a border line labelling a session that no longer exists — on a
dead pane, no less.

The seat is not lost by removing the row, only moved: the report's header
carries a `seat:` line. That is the right place for it once the session is
over — the border says which session is *live*, and this one is not, but which
account did the work is worth recording.

## Two tmux behaviours worth knowing

Both were found by probing a throwaway session, and both look like bugs in your
own code until you know them:

- **The first line a respawned pane prints is swallowed.** `printf
  'ALPHA\nBETA\n'` in a respawned pane shows only `BETA`. The report is
  therefore preceded by a newline, so the line that gets lost is a blank one.
- **`respawn-pane` reuses the pane's screen.** Without an explicit clear the
  report starts wherever the previous shell's prompt left the cursor. The clear
  is `ESC[H ESC[2J` written by `printf`, not `clear`, which would need a `TERM`
  the tmux server may not have.

## Dead ends

- **`kill-pane` instead of `remain-on-exit`.** Obvious, and wrong: it removes
  the pane, and the summary with it. The requirement was a *visible* dead pane.
- **Backgrounding the watcher with `&!`, then with a double fork.** Neither is
  reliable when the invoking shell is a pipeline subshell that exits at once.
  `setsid` is what actually detaches, and the repository already had a wrapper
  for it.
- **A long red herring:** the watcher appeared to fail in the probe pane for
  several rounds, and none of the launcher changes seemed to help. The pane's
  interactive zsh had loaded the function at startup and was still running the
  *old* copy. When probing a zsh function in a long-lived shell, re-source the
  file in that shell — or restart it — before believing the result.
- **`tty` in the agent's tool shell.** It fails there (no controlling
  terminal), which is why the terminal is read off the agent process instead.

## Knobs

All dynamically scoped, all read with [agfi:bool] where they are boolean:

- `agent_done_wait_s` — how long the agent gets to exit before `SIGKILL`; 20.
- `agent_done_dry_p` — same as `--dry-run`.
- `agent_done_agent`, `agent_done_pid`, `agent_done_tty`, `agent_done_cwd` —
  override what would otherwise be detected. These exist for testing, and are
  what let a probe run the whole path against a `sleep` standing in for an
  agent.
- `agent_done_resume_cmd` — what `prefix-r` runs in the dead pane instead of
  the default resume.
- `agent_skills_link_verbose_p` — say which links were made.
- `agent_skills_src_dir`, `agy_config_dir` — where the tracked skills are, and
  where Antigravity's configuration is.
