# Concise notes with destination confirmation

The shared `note` skill lives at `configFiles/agent-skills/note/SKILL.md`.
Invoke it as `$note` in Codex or `/note` in Claude, or ask to save a concise
note under `~/notes`.

The parent drafts the note while a read-only worker searches the existing
notes organization: Luna (`gpt-5.6-luna`) for Codex, Sonnet for Claude. The
worker returns ranked paths and evidence; the parent checks the best fit,
matches the local format, and shows the exact destination and full draft.
Nothing is written to the notes tree until the user confirms that proposal.

This split makes directory discovery cheap while keeping wording, privacy,
and approval with the parent. A supplied destination skips discovery. If the
requested worker is unavailable, the agent reports that and searches narrowly
itself rather than silently choosing a more expensive model.

Install through [agfi:agent-skills-link], the same shared mechanism as `done`;
see `docs/agent-done.md`. The skill is self-contained so Claude's SKILL.md-only
links and Codex's directory links both work.
