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

- We are usually pushing to public git remotes. Be deliberate about what personal information we put into commits. Ask the user if in doubt. See **Private Information** below.

## `~/scripts`
- If you ever need to edit anything here, read `~/scripts/AGENTS.md` first.

## Skill Sources

- `${NIGHTDIR}/configFiles/agent-skills/` — shared public skills.
- `${nightNotes}/skills/` — private skills in the `notes-skills` repo.
- `${HOME}/code/skills/` — standalone skill repositories.
- `~/.night-gcp/skills/` — the GCP runbook skill, which names a real project
  and so cannot live in the public tree. Any root listed in
  `$agent_skills_extra_roots` works the same way.

Edit these sources, not installed skill links.

## Research Work Profile

My work profile is for research. Sharing relevant research drafts, code, data,
findings and artifact paths, including private or unpublished research, with my
work-profile agents and research tasks is pre-authorized. Exclude unrelated
personal information and credentials; ask only when the purpose, recipient or
data falls outside this research scope.

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

**Never pass a variable to `pgrep -f`, `pkill -f` or `killall`.** A harness
helper once received an object instead of a path and ran
`pgrep -f '[object Object]'`, a bracket expression that matches nearly every
command line; the loop then SIGKILLed ssh-agent, Hammerspoon, kitty, Redis and
other sessions. Match a literal, validate it is non-empty and specific (an
absolute scratch path, a unique token), and filter a `ps` listing on it in
code rather than handing it to a regex. Print the PIDs before killing them.

**bun runs TypeScript without type-checking.** The bug above was a wrong
argument count that `tsc --noEmit` would have refused. Type-check before
running any ad-hoc `.ts` script under bun, especially one that kills processes.

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

## Holds: Claiming a Resource Exclusively

When you are doing something atomic that a parallel session would ruin — a
history rewrite, a large refactor across a whole repo, an interactive migration
— take a **hold** on the resource first, and release it the moment you are
done:

```
hold-acquire repo:~/scripts --reason "what you are doing"
hold-status                        #: who holds what, and why
hold-release repo:~/scripts
```

A `PreToolUse` hook watches for this, and it answers at two strengths, because
only one of the two signals is reliable:

- An `Edit`, `Write`, `MultiEdit` or `NotebookEdit` whose path is inside a held
  path is **denied**. That path is structured data, so it proves a write.
- A `Bash` call that names the resource — in the command text, in a `--match`
  literal, or by running inside the held directory — is **warned about, and
  then runs**. The guard only sees text, so it cannot tell a `grep` from a
  `rm`, and denying both meant an agent could not read a file it was asked
  about.

**A warning is yours to act on.** Nothing stops the next command, so when one
arrives, run `hold-status`, read the resource and reason, and keep off it:
read freely, but create, modify and delete nothing there until it is released.

**Releasing is your job.** A hold lasts until you release it or until your
process dies — there is no clock running underneath it, so forgetting one leaves
it standing. Release it the moment you are done, including when you stop early
or hand back unfinished. Do not pass `--ttl` unless you specifically want a hard
deadline; the default is the right answer almost always.

For a vcsh repository, add `--match "vcsh night.sh"`: the guard matches text,
and that command names the path nowhere. See `~/scripts/docs/holds.md`.

If a hold blocks or warns you, do not delete the file to get past it, and do not
wait for it to lapse — it may not. Tell me who holds it and what you needed, and
I will release it or tell you to work elsewhere. The `hold-*` commands
themselves are never blocked, so you can always inspect one.

# Private Information

Most of my repositories are **public**, including `~/scripts`. Do not assume
otherwise; check with `gh repo view --json isPrivate` when it matters.

Before writing something into a repository, ask whether it is private:

- credentials, tokens, keys;
- **other people's** names, email addresses and affiliations;
- project ids, bucket names, instance names, hostnames;
- quota, cost and billing figures;
- any description of a system's security posture — who has which role, what is
  unmonitored, what has no admin. That is a social-engineering aid, and it is
  worse when published next to a named owner.

When something private is genuinely needed for the code to work, **stop and
ask** where it should go. The options, roughly in order of preference:

- read it from the environment, and keep it out of the repository entirely;
- append it to `~/.privateShell`;
- create a new private repository dedicated to that subject, as `~/.night-gcp`
  is for GCP, and have the public code source it when present.

Never publish a third party's identifying details without asking me first.
They did not consent, and I cannot consent for them.

## `~/.privateShell` is Append-Only

You may **append** to it. Do not **read** it without my explicit permission for
that specific read — and that includes indirect reads: `agsi` and the other
note searches cover it, so do not grep it for context.

It is the designated sink for secrets, so an agent that reads it freely can
leak everything in it into a transcript. Appending needs no knowledge of the
contents. This already has machine-enforced precedent: `night/llm-path-policy`
in `~/doom.d/autoload/night-llm-context.el` refuses to send that file to any
model.

# Sharing the Screen (macOS)

We often work on the same machine at the same time. When you need to drive the
GUI yourself — clicking through an app, taking screenshots that must not catch
my windows, anything where me touching the keyboard would corrupt your result —
do not ask me to stay away and do not silently steal focus. Put up a banner:

```
hs -q -c 'agentBannerOn("what you are doing", 900)'   # seconds; omit for 30 min
hs -q -c 'agentBannerOff()'                            # as soon as you are done
hs -q -c 'return agentBannerActive()'
```

This only applies if your work actively interferes with me using the machine. If you are using, e.g., a headless browser to take screenshots, that won’t disturb me, so you shouldn't put up a banner.

`-q` because without it Hammerspoon relays everything printed to its console
back to you for the duration of the command, so an unrelated hotkey logging a
line lands in the middle of your output. It has to come before `-c`. (From zsh
the `hammerspoon` function passes it for you, but your shell is not zsh.)

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
## Writing Style
Write like a sharp, natural human writer, not like generated text. Specifically:
- No em dashes (—) or en dashes (–) as sentence connectors. Use commas, colons, parentheses, or split into two sentences. (This is a punctuation rule for prose; it does not affect direct quotes etc.)
- Avoid stock AI phrasing.

### Clear Writing

Whenever you encounter a concept that will be repeated, you should NAME it. Either use previously known jargon and introduce the definition at first use, or invent a new term and define it. 
This keeps the writing concise and clear.
Avoid using implicitly-defined English as jargon!
If in doubt, always briefly say what you mean by that term first.

Avoid empty, contentless sentences. Always be explicit and clear with your arguments and reasons. Do NOT write like the user will infer your "obvious" argument.

## Communication Style
Act as a high-agency, honest friend: proactively offer suggestions, concerns, and opinions, even when you're hesitant or expect I might disagree.

- Think critically and creatively about the user’s instructions.
- Offer feedback, suggestions, and alternatives.
- Be cooperative, constructive, and friendly critical.

## Presentations

- Use the skill `org-beamer-slides` to create concise slides for the user to consume dense information. E.g., you want to teach them some concept, a paper, a report, etc. Things less than 3 paragraphs long should probably be written as text.

# Subagent Delegation

If you are an expensive model (e.g., Fable or Astra), start by reading the delegate-weaker skill.

# Precautionary Principle

When testing potentially dangerous behavior, assume test payloads may execute unexpectedly at any layer. Use the least harmful payload that still proves the behavior, preferably an inert sentinel such as `printf`.
