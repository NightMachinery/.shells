# Claude Code

- Use the task list for any work of three or more distinct steps: `TaskCreate`
  up front, `TaskUpdate` to `in_progress` before starting each one and
  `completed` when it is actually done. Do not mark a task completed while
  tests fail or the implementation is partial. Skip it for single, trivial
  edits, where it is pure overhead.

- Prefer a skill over an ad-hoc procedure when one covers the task, and prefer
  a hook over an instruction for anything that must happen at a fixed point,
  such as before every commit.

- `~/.claude/rules/` is deliberately unused. Rules cannot be shared with Codex
  or Antigravity, and split instructions are the reason `VPS.md` sat wired up
  but unread for months. Everything lives in the assembled file instead; see
  `PE/Agents/readme.org`.

## Memory

- Write memories to `.memory/` at the repository root, not to the harness's
  own per-project memory path. They belong beside the code they describe,
  travel with the clone, and show up in review as a diff.

- A memory nothing reads is worthless. The harness only auto-loads `MEMORY.md`
  from the directory it configured, and `.memory/` is not that directory, so
  the repository's `CLAUDE.md` must import `@.memory/MEMORY.md`. Add that
  import in the same change that creates `.memory/`, or the memories go
  unread.

- Commit memory edits on their own, never mixed with code, with a `memory:`
  subject prefix. They are notes about the work, not part of it, and mixing
  them makes both harder to review and to revert.

- `.memory/` is committed, so a memory is exactly as public as its repository.
  Keep hostnames, credentials, tokens and personal details out of them; a fact
  that needs those does not belong in a memory at all.
  

