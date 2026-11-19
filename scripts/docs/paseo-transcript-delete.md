# Deleting archived Paseo sessions

`paseo-transcript-delete-fz [--dry-run] [--yes] [--filter TEXT] [QUERY]` lists
the archived Paseo agents on this host, oldest archive first, with their native
transcript size. Tab selects several. It prints the exact native files of each
selected agent, asks for confirmation (default No), then deletes each agent's
Paseo record and timeline with `paseo agent delete <id>` and, after that
succeeds, its native files.

- `--dry-run` stops after printing the files.
- `--yes` skips the confirmation; without a terminal it is required.
- `--filter TEXT` selects non-interactively (fzf `--filter`); `--filter ''`
  selects every row.

## What counts as native files

The agent's native session is found as for `paseo-view-transcript` (see
[paseo-transcript.md](paseo-transcript.md)), so provider aliases and their
homes are honoured.

- Claude Code: `<home>/projects/<project>/<session>.jsonl` and the session's
  directory beside it, `<project>/<session>/`, which holds its subagent
  transcripts.
- Codex: every rollout of the thread under `<home>/sessions/` and
  `<home>/archived_sessions/`.

## What is refused

An agent is not offered when:

- it is not archived, or is internal;
- it is pinned (`keep=true`, see `paseo-keep`);
- its provider is neither Claude Code nor Codex;
- another Paseo record, archived or not, names the same native session, since
  deleting the files would take that agent's history too;
- another record names it as its parent: other Paseo sessions are never
  cascaded, so a parent waits until its children are gone;
- `PASEO_HOST` points at a remote daemon, whose records are not on this disk.

The record is deleted before the files: a record left pointing at deleted files
is worse than files left behind by a deleted record. If `paseo agent delete`
fails, that agent's files are kept and the command exits non-zero.

This used to call `python/paseo_transcript_delete.py`, which was never committed;
the implementation is now zsh in `zshlang/auto-load/others/paseo.zsh`
(`h-paseo-transcript-delete-catalog`). Tests:
`zsh -f zshlang/tests/paseo-transcript.zsh` (fixtures only, with a fake `paseo`
on `PATH`).
