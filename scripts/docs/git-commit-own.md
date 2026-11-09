# git commit-own: commit only your own paths

`git commit-own` commits exactly the paths you name, as they are on disk,
through a **private index**: a throwaway index file, seeded from `HEAD`, that
only this one command ever touches. The **shared index** (`.git/index`, what
`git add` writes and `git commit` reads) is never used to build the commit.
It exists for repositories where several agents, or an agent and a human,
work in one worktree at the same time.

    git commit-own -m "subject" -m "body" -- <path>...
    git commit-own -m "..." -- .gitignore --untrack big.bin    #: stop tracking, keep the file
    git commit-own -m "..." --patch part.patch                 #: part of a file
    git commit-own --dry-run -- <path>...                      #: what would be committed
    git commit-own --amend -- <path>...                        #: fold into the last commit
    git commit-own -m "..." --author "Name <email>" -- <path>...
    vcsh night.sh commit-own -m "..." -- <path>...             #: the same, in a vcsh repository

`git commit-own -h` lists every option. (`--help` is taken by git itself,
which looks for a man page.)

## Why not `git commit -- <paths>`

The shared index is shared state. Anything one session builds in it is part
of the next `git commit` any session runs. `git commit -- <paths>` protects
you from what *others* staged, because it commits the named paths from disk
and ignores the rest of the index. It cannot protect what *you* build there:

- `git rm --cached big.bin` then `git commit -- big.bin .gitignore`: the
  pathspec form takes `big.bin` from disk and commits it again. This is how
  208 MB of audio landed in a private repository and had to be purged with a
  force-push.
- `git add -p` then `git commit -- file`: the pathspec form commits the whole
  file, not the hunks you picked. Plain `git commit` would commit the hunks,
  and also whatever anyone else staged.
- Between your `git add` and your `git commit`, another session's `git
  commit` sweeps your staged work into its commit.

`git commit-own` does the untracking (`--untrack`) and the partial commit
(`--patch`) in its private index, so none of it is ever visible to anyone
else.

## What it does

1. Reads `HEAD` (branch and commit; an unborn branch is fine).
2. Seeds a private index from it (`git read-tree`), in
   `$GIT_DIR/commit-own-<pid>-<n>.index`.
3. Stages each named path from disk with `git add -A`, so new files are
   added and deleted ones removed; then `--untrack` (`git rm --cached -r`);
   then each `--patch` (`git apply --cached`). A path that matches nothing is
   an error, and nothing is committed: a typo cannot turn into a commit that
   silently left out a file.
4. Notes any named path that has no changes, and stops when nothing changed
   at all (`--allow-empty` overrides).
5. Runs the hooks as `git commit` would: `pre-commit`, `prepare-commit-msg`,
   `commit-msg` (all with `GIT_INDEX_FILE` set to the private index, so a
   hook that inspects the index sees just this commit), then `post-commit`.
   `--no-verify` skips `pre-commit` and `commit-msg`.
6. Writes the tree and the commit (`git commit-tree`), signed when
   `commit.gpgSign` is set. `commit-tree` ignores that setting by itself, so
   the tool reads it and passes `-S`.
7. Moves the branch with `git update-ref HEAD <new> <old>`, which succeeds
   only if `HEAD` is still `<old>`. If another session committed in the
   meantime, the commit is rebuilt from step 1 on the new tip, up to five
   times. Building from the old tip and forcing the ref would silently revert
   their commit.
8. Resets the shared index entries of the committed paths to the new commit
   (`git reset -q <new> -- <paths>`). Without this the shared index would
   still hold their old contents, and `git status` would show the commit
   undone. No other entry is touched, so what others staged stays staged.
9. Prints `[branch hash] subject` and the `--stat`.

A path that is the whole work tree (`.`, `..` from a subdirectory, or the
top-level directory itself) is refused unless `--whole-tree` is given. In a
vcsh repository the work tree is the home directory, and `git add -A .` there
would take in every untracked file in it.

It refuses to run while `MERGE_HEAD`, `CHERRY_PICK_HEAD` or `REVERT_HEAD`
exists, because finishing those needs `git commit`.

## Committing part of a file

Write a patch against `HEAD` that holds only the hunks you want, and pass it
with `--patch`:

    git diff -U1 -- notes.md > /tmp/all.patch      #: then delete the hunks to leave out
    git commit-own -m "..." --patch /tmp/all.patch

Paths in the patch are relative to the top of the work tree, as `git diff`
writes them. The patch must apply to `HEAD`: if someone staged a different
version of that file in the shared index, make the patch with
`git diff HEAD -- <file>` instead. The rest of the change stays on disk,
unstaged.

## What it does not protect

- **A file two sessions both edit.** The tool commits the file as it is on
  disk, including the other session's edits in it. Look at
  `git diff -- <paths>` before committing a file you share.
- **Hooks that rewrite the worktree.** A `pre-commit` framework that stashes
  unstaged changes does so against the private index, just as it would
  against the shared one under `git commit`.

## Amending and the author

`--amend` replaces the last commit with one that also holds the named paths
(none is fine, to change only the message). As with `git commit --amend`, the
commit keeps its parents, its message unless `-m` or `-F` is given, and its
author name, email and date; `--author` replaces the name and email only.

Two refusals keep it from rewriting anything but your own fresh commit:

- A commit that any remote-tracking branch already contains is refused,
  because replacing it rewrites history others may have pulled. Make a new
  commit instead.
- If the branch moves while the amended commit is being built, the tool stops
  instead of rebuilding: the new tip is someone else's commit, and amending it
  would fold your change into their work.

It cannot tell whether the last commit is yours when another session made it
before you started, since the agents all commit as the same person. Amend
only a commit you made yourself, just now.

`--author` takes `Name <email>`, or, as with `git commit`, a pattern that is
looked up among the authors of existing commits.

## Installation

`bin/git-commit-own` is a zsh shim: git finds it on `PATH` as the
`commit-own` subcommand. It calls [agfi:go-local-dep], which builds
`golang/git_commit_own` into `~/go/bin/git_commit_own` when the binary is
missing or older than its source, and then runs it. So a `git pull` of
`~/scripts` is all a host needs, as long as it has Go. Run the tests with
`go test ./...` in `golang/git_commit_own`; they build scratch repositories
and cover the race, a vcsh-like `GIT_DIR`, signing and the hooks.
