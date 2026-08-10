You need to use `vcsh night.sh` instead of `git` to interact with this repository. Always ignore untracked files, and use `status -uno`.

The root of this git repo is at `~/` as it contains some dotfiles. But we only work with the files inside `~/scripts`; so the root of this project is `~/scripts`. I.e., when I say `./x`, I mean `~/scripts/x`, unless the PWD is otherwise specified.

Read `./PE/Zsh.org`. When you think of something that needs to be in this file, suggest it to me, but don't edit the file yourself unless I tell you to.

Read all scripts in `zshlang/basic`. Reuse functions when possible, DRY.

To link to a zsh function from comments/docs, use `[agfi:function-name]`, not `[help:function-name]`.

## Org-mode Links to Files

In org files, link to a file **outside the current directory** with a `zf:`
link and a zsh dynamic named directory, never with a relative `file:` path:

```org
[[zf:~[cod]/uni/papers/FairGrad/][the paper]]
[[zf:~[nt]/public/subjects/Qwen/open-weight/Qwen3.6/arch/Libra.org][Libra.org]]
```

`~[name]` is resolved by [agfi:path-unabbrev], which the `zf:` link handler
calls before opening. Names come from [agfi:aliasdir]; useful ones are `nt`
(`~/notes`), `cod` (`~/code`), `dom` (`$DOOMDIR`), `paper`, `base`, `dl`,
`tmp`, `jrl`, `cel`, `mu`.

Why, rather than `[[file:../../../foo]]`:

- Counting `../` across trees is easy to get wrong and fails silently — org
  renders a broken link exactly like a working one. Two links written that
  way were off by one level each: from
  `notes/private/research/J-Space/spectral-clipping/`, `../../../../code/`
  resolves to `notes/code/` and `../../../public/` to `private/public/`.
- A relative link breaks when *either* file moves; a `zf:` link only breaks
  if the target moves.

Rules:

- Same-directory or sibling links (`[[file:models.org]]`) stay relative.
  They are readable and move together with the file.
- `~[nightNotesPrivate]` and `~[nightNotesPublic]` are **not** named
  directories and resolve to nothing, despite the environment variables of
  those names existing. Use `~[nt]/private/...` and `~[nt]/public/...`.
- **Verify before committing.** A `zf:` link is only durable if it resolves:

  ```zsh
  path-unabbrev '~[nt]/public/subjects/Qwen/open-weight/Qwen3.6/arch/Libra.org'
  ```

  Check the printed path exists. Do not trust the link because it looks right.

## Brish

BrishGarden keeps persistent zsh shells, so it does **not** see zshlang edits on
its own. Run `brishz-restart` after changing any zsh code you intend to exercise
through `brishz`, `brishz2.dash`, or anything driven by them (agent hooks,
Hammerspoon bindings, iTerm triggers). Testing in a fresh `zsh -ic` proves
nothing about what the garden is running.

## Plugins
### Loading

We load our plugins manually in `zshlang/load-others.zsh`.

### Documentaion

If you want to document installation for a plugin, read the readme of another plugin first and use its style.

## Agent Instruction Files

The global instruction files the agents actually read — `~/.claude/CLAUDE.md`,
`~/.codex/AGENTS.md`, `~/.gemini/AGENTS.md` — are **generated**. Never edit
them: they are overwritten on the next agent launch. Edit the sources under
`./PE/Agents/` and run `agents-md-sync`, which the `claude`, `codex-m` and
`antigravity-m` launchers call for you; one sync refreshes every agent's file,
not just the caller's. `agents-md-doctor` reports what
each agent loads and whether anything has drifted. See
`./PE/Agents/readme.org`, which also explains why this is assembled rather than
symlinked or `@`-imported.

Which source to edit:

- `PE/Agents/AGENTS.md` — every agent, every host.
- `PE/Agents/agent-<name>.md` — one agent, every host. The `agent-` prefix is
  load-bearing.
- `PE/Agents/hosts/<hostname>.md` — every agent, one host.
- `${nightNotesPrivate}/configs/agents/` — the same names again, for anything
  that should not sit in a public repository.
- `~/.agents.local.md` and `~/.<agent>.local.md` — one machine, untracked.
- `PE/Agents/disabled/` — parked; assembled into nothing.

Later parts win, so a machine-local file overrides the shared spine.

This file, `./AGENTS.md`, is the project instruction file for this repository
rather than a global one, and `./CLAUDE.md` is a symlink to it.

Commit instruction edits on their own, never mixed with code, in either case.
They change how every future session behaves, so they need to be reviewable
and revertible without dragging unrelated work along.

## Parallel Agent Sessions

Multiple agent sessions often work in this worktree at the same time. Follow this ownership discipline:

- Only commit files **you yourself edited in this session**. You know which files those are from your own conversation; never infer ownership from `status`.
- Treat any other modified files as another session's work in progress: do **not** commit, stage, stash, revert, or "clean up" those files. This **overrides** the general instruction to split-commit a dirty worktree before starting work.
- Commit with explicit paths only: `vcsh night.sh commit -m "..." -- <file> ...`. Never use bare `commit`, `commit -a`, or `stash` — the index and stash are shared between sessions.
- Immediately before committing, re-check `vcsh night.sh diff -- <files>`: if a file contains changes you did not make mixed with yours, stop and ask the user instead of committing.
- If your commit fails with "nothing to commit", a parallel session likely committed your files already. Verify via `vcsh night.sh log -p` that the committed content matches what you wrote, then continue; do not redo the work.
- If a file you need to edit already has uncommitted changes from another session, ask the user before touching it.

