# Background Jobs

Ways for an agent (or a script) to start a job that outlives its shell and
session. The global agent instructions (`PE/Agents/AGENTS.md`) keep only a
summary; this is the full reference.

## `tmux-job-start`: run once, output someone reads

`tmux-job-start NAME [VAR=val...] cmd...` runs the command in tmux session
NAME, in your working directory. Only the leading `VAR=val` arguments are
forwarded as environment. The output stays in the pane (`tmux attach -t NAME`,
`tmux capture-pane`).

- It refuses a NAME that is still running.
- `tmux-job-status NAME` reports it; `tmux-job-stop NAME` kills the whole
  process tree.
- A sandbox (Codex's) cannot reach the tmux server. There the status is
  `UNKNOWN` with exit 2, which is never proof that the job is gone. Run start,
  status and stop with the same elevated access.

Prefer it for a job that must run exactly once, or whose output someone will
read.

## `awaysh-bnamed`: fire and forget

`awaysh-bnamed NAME cmd...` runs the command under BrishGarden, in the
garden's working directory and environment, so pass absolute paths. Output
goes to `/dev/null` unless the command redirects it.

- NAME is only a `ps` marker (`zsh NAME_MARKER ...`, used by
  `kill-marker NAME`). It is not unique: a second call starts a second job.
- Brish sees edited zsh functions only after `brishz-restart`. When it may be
  stale, use `awaysh-v3 cmd...`: a setsid'ed `zsh -c` in your own working
  directory and environment that loads the functions fresh, with no marker
  and output on `/dev/null`.

## When you need to be notified

None of the above can wake an agent when the job ends. A job whose completion
must notify you stays a harness-tracked background task (Claude Code's
`run_in_background`, for example); see the `event-driven-agents` skill.
