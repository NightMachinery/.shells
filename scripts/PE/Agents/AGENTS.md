# General Working Guidelines

- Root commands must be run as `sudo -kA <command>`. Never run bare `sudo`: there is no TTY, so it will hang until the tool times out.

- Keep `./docs/` updated as you investigate or make changes.
  - If a `readme.org` (or `readme.md`) file exists, keep it updated too.
  - Avoid using tables in docs. They can be difficult to read in plain-text on a laptop screen.
  
- Before asking the user a multiple-choice question, first explain the choices in detail then ask the question. This way the user can scroll back and see the technical details of each answer if needed.

- Stay on the current branch and worktree unless the user explicitly asks you to switch.

- Do **not** use Superpowers’ Visual Companion skill unless the user explicitly asks you to.

- Do not worry if `npx @sveltejs/mcp svelte-autofixer` hangs or behaves unexpectedly; it is not yet production-ready.

- Always grab the clipboard content first thing when requested, before it gets overwritten or changed.

- Before asking a question from the user, always explain (in normal conversation text) all the choices and their tradeoffs and THEN ask the question normally. This way the user can read the conversation history to get a more detailed understanding of the choices before choosing. The explanation must remain readable in the history.

- Before showing an updated plan, always provide a quick conversational response at the top. Answer any questions the user asked, and highlight the specific changes made so they don't have to re-read the entire plan.

- Always analyze and discuss trade-offs of different solutions.

- We are usually pushing to public git remotes. Be deliberate about what personal information we put into commits. Ask the user if in doubt.

## `~/scripts`
- If you ever need to edit anything here, read `~/scripts/AGENTS.md` first.

## Svelte Guidelines

- Skip running `npx @sveltejs/mcp svelte-autofixer`. It often hangs and gives useless output.

## VPS / Local Machine Detection

- Only check whether the machine is a VPS if you actually need to know.
- To check, inspect `hostname`.
  - If it matches `.*\.local$`, it is likely a local machine.

## Low Disk Space

- If there is so little free disk space that you cannot continue working, stop and ask the user to handle it manually.
- Do **not** try to clean caches, temporary files, or other disk usage yourself.

## Matching Processes: `pgrep -f` / `pkill -f` Match You Too

`-f` matches against the whole command line, and your own shell's command
line contains the pattern you just typed. So `pkill -f emacs` kills the shell
running it, and `pgrep -f install.sh` reports "still running" forever because
it is finding itself.

- Prefer `pgrep -x NAME` (exact process name), or an explicit PID list.
- When `-f` is unavoidable, use a pattern that cannot appear in your own
  command line (`daemon=night-verify`, not `emacs`), and skip `$$`.
- Verify a "nothing is running" conclusion some other way — check that the
  resource is actually free (GPU memory, port, lock file), not just that a
  pattern failed to match.

**Killing a parent does not kill its children.** After terminating a process
tree, re-check for orphans and kill them by PID. A `doom sync --rebuild` that
outlived the parent I had killed went on rewriting the package tree while I
believed it was stopped, and corrupted it.

This is worth its own rule because it is silent: the failure mode is a
command that reports success while doing the opposite of what you intended.

## Use `command ...` When You Mean the Real Binary

In scripts, when you need a specific external program and not whatever the
user's environment has bound that name to, write `command od`, not `od`.

Aliases and functions shadow command names, and my shell defines a great many
of both. Short, common names are the dangerous ones — `od`, `tr`, `ln`, `rm`,
`grep`, `chmod`, `head` — because they are exactly the names a wrapper is
likely to have claimed. A script that silently gets a wrapper instead of the
binary can produce subtly wrong output rather than an error.

- Use `command` for the fixed, load-bearing calls in library code: the ones
  whose behaviour the surrounding logic depends on.
- Do **not** blanket-prefix everything. Where calling my wrapper is the point,
  calling it is correct; `command` there just breaks the customisation.
- Where recursion is the hazard — a function calling the command it wraps, or
  a helper called from inside the very wrapper it would re-enter — `command`
  is mandatory, not stylistic.

# Git Commit Guidelines

## General Rules

- If the directory is already a Git repository, commit your changes when you reach a natural endpoint.
- Push all commits at the end, after the work is complete.
- If there is no existing Git repository, do not create one unless the user asks.

## Atomic Commits

- Make commits atomic and logically grouped.
- If you are implementing multiple unrelated features or fixes, split them into separate commits.
- When planning work, include the intended atomic commit groups in the plan.
- Each commit should represent one cohesive change and have a clear commit message.
- Group related changes together, such as a code change and its corresponding documentation update.

## Dirty Worktree Handling

- Before making any mutating changes in a dirty worktree, first split the existing changes into fine-grained, logically related atomic commits.
- Do this unless the user explicitly tells you to leave the existing changes uncommitted.
- Read-only operations do not require committing existing changes first.

## Staging

- To stage changes in a single file selectively, you can use `git add -p`. Do not use this if you want to stage a whole file.
- If one file contains multiple unrelated changes, split those changes into separate commits.
- Do not treat “one file” as automatically meaning “one commit.”

## Concurrent Sessions

Another agent or a human may be staging and committing in the same repository
while you work. The index is shared state, so a plain `git commit` picks up
whatever *they* staged, even when you only ever `git add`ed your own paths.

- Commit with an explicit pathspec — `git commit -- <paths>` — so the commit
  contains only what you name, regardless of what else is sitting in the index.
- Re-check `status` immediately before committing. A check from earlier in the
  session proves nothing; the index may have changed since.
- After committing, confirm with `show --stat` that only the intended paths
  landed.
- Expect the mirror case too: work you leave uncommitted can be swept into
  someone else's commit. Commit your own work promptly rather than letting it
  sit in the worktree.
- If a commit does end up mixing their work with yours, do not rewrite or
  force-push to fix it. They may already be working from that history. Report
  it and let me decide.

# Sharing the Screen (macOS)

We often work on the same machine at the same time. When you need to drive the
GUI yourself — clicking through an app, taking screenshots that must not catch
my windows, anything where me touching the keyboard would corrupt your result —
do not ask me to stay away and do not silently steal focus. Put up a banner:

```
hs -c 'agentBannerOn("what you are doing", 900)'   # seconds; omit for 30 min
hs -c 'agentBannerOff()'                            # as soon as you are done
hs -c 'return agentBannerActive()'
```

It covers every screen for a moment so I cannot miss it, then collapses to a
strip across the top of each one, on every space. It never takes focus and
never swallows clicks. Turning it off flashes "Screen is yours" the same way.
Source: `~/scripts/hammerspoon/core/agent-banner.lua`.

- Turn it off the moment you no longer need it, including when you stop early
  or hand back with the job unfinished.
- It always expires on its own, so a crash cannot leave the screen branded.
  Call `agentBannerOn` again to refresh the countdown during a long stretch;
  re-sending the same message will not re-flash.
- Say in the message what you are actually doing, not just "working". I decide
  whether to wait based on that line.

If `hs` is missing, the machine has no Hammerspoon; just say what you need
instead.

# Unexpected File Changes

If you notice files changed since you last read/wrote them, it's possible the user updated them manually. Leave these changes be; if they conflict with your instructions, ask the user explicitly for instructions.

# Speech to Text

I sometimes use STT software to type, so be on the lookout for typos and intelligently guess the true prompt when you think STT has been used. You can rewrite the prompt and confirm with me, when there is significant ambiguity.
My STT software inserts its dictated text in markdown code blocks with the language set to `speech-to-text`.

# Collaboration Style

- Think critically and creatively about the user’s instructions.
- Offer feedback, suggestions, and alternatives.
- Be cooperative, constructive, and friendly critical.
