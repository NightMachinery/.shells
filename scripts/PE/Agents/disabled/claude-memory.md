# Memory

Parked. The memories accumulated so far were not worth keeping, so auto memory
stays in the harness's own ephemeral per-project directory for now.

To re-enable, move this file to `PE/Agents/claude.md` — but read the second
bullet first, because moving it alone is not enough.

- Write memories to `.memory/` at the repository root, not to the harness's
  own per-project memory path. They belong beside the code they describe,
  travel with the clone, and show up in review as a diff.

- A memory nothing reads is worthless, and this is the part that does not
  work by itself. The harness auto-loads `MEMORY.md` only from the directory
  it was configured with, and `.memory/` is not that directory: an instruction
  changes where memories are *written*, never where they are *read* at session
  start. So each repository using `.memory/` must also import
  `@.memory/MEMORY.md` from its own project `CLAUDE.md`.

  That import is project scope, which `agents-md-sync` does not manage — it
  assembles the four user-scope files and nothing else. Where a repository's
  `CLAUDE.md` is a symlink to its `AGENTS.md`, as this one's is, it has to
  become a real file first:

      @AGENTS.md
      @.memory/MEMORY.md

  Codex does not inline `@path` imports, so it will read those two lines
  literally. Keep them at the top where they read as a pointer rather than an
  instruction.

- Commit memory edits on their own, never mixed with code, with a `memory:`
  subject prefix. They are notes about the work, not part of it, and mixing
  them makes both harder to review and to revert.

- `.memory/` is committed, so a memory is exactly as public as its repository.
  Keep hostnames, credentials, tokens and personal details out of them; a fact
  that needs those does not belong in a memory at all.
