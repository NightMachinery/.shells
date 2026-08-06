You need to use `vcsh night.sh` instead of `git` to interact with this repository. Always ignore untracked files, and use `status -uno`.

The root of this git repo is at `~/` as it contains some dotfiles. But we only work with the files inside `~/scripts`; so the root of this project is `~/scripts`. I.e., when I say `./x`, I mean `~/scripts/x`, unless the PWD is otherwise specified.

Read `./PE/Zsh.org`. When you think of something that needs to be in this file, suggest it to me, but don't edit the file yourself unless I tell you to.

Read all scripts in `zshlang/basic`. Reuse functions when possible, DRY.

To link to a zsh function from comments/docs, use `[agfi:function-name]`, not `[help:function-name]`.

## Plugins
### Loading

We load our plugins manually in `zshlang/load-others.zsh`.

### Documentaion

If you want to document installation for a plugin, read the readme of another plugin first and use its style.

## Parallel Agent Sessions

Multiple agent sessions often work in this worktree at the same time. Follow this ownership discipline:

- Only commit files **you yourself edited in this session**. You know which files those are from your own conversation; never infer ownership from `status`.
- Treat any other modified files as another session's work in progress: do **not** commit, stage, stash, revert, or "clean up" those files. This **overrides** the general instruction to split-commit a dirty worktree before starting work.
- Commit with explicit paths only: `vcsh night.sh commit -m "..." -- <file> ...`. Never use bare `commit`, `commit -a`, or `stash` — the index and stash are shared between sessions.
- Immediately before committing, re-check `vcsh night.sh diff -- <files>`: if a file contains changes you did not make mixed with yours, stop and ask the user instead of committing.
- If your commit fails with "nothing to commit", a parallel session likely committed your files already. Verify via `vcsh night.sh log -p` that the committed content matches what you wrote, then continue; do not redo the work.
- If a file you need to edit already has uncommitted changes from another session, ask the user before touching it.

