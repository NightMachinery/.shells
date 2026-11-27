# General Working Guidelines

- Root commands must be run as `sudo -kA <command>`. Never run bare `sudo`: there is no TTY, so it will hang until the tool times out.

- Your running shell is Zsh. Use proper Zsh. Quote everything conservatively, to avoid Zsh glob patterns etc. on literal strings. In particular, '===' should be quoted.
  - Always use full syntax for variables, e.g., `${...}`. Zsh treats `sth` in `$variable_name:sth` as modifier flags.
  - I have many aliases defined on 2 or 3 letter names. These are enabled even for noninteractive scripts. Use distinct names for your own functions to avoid name collision.

- Keep `./docs/` updated as you investigate or make changes.
  - If a `readme.org` (or `readme.md`) file exists, keep it updated AND streamlined: README files are meant for humans. These files should be concise and let humans know why this project might provide value for them and teach how to try it out quickly, with links to other docs for more details.
  - Avoid using tables in docs. They can be difficult to read in plain-text on a laptop screen.
  
- Stay on the current branch and worktree unless the user explicitly asks you to switch.

- Do **not** use Superpowers’ Visual Companion skill unless the user explicitly asks you to.

- Do not worry if `npx @sveltejs/mcp svelte-autofixer` hangs or behaves unexpectedly; it is not yet production-ready.

- Always grab the clipboard content first thing when requested, before it gets overwritten or changed.

- Before asking a question from the user, always explain (in normal conversation text) all the choices and their tradeoffs and THEN ask the question normally. This way the user can read the conversation history to get a more detailed understanding of the choices before choosing. The explanation must remain readable in the history.

- Whenever you list questions still waiting on the user, restate each one completely: its context, every option with its tradeoffs, and your recommendation. Never point back to an earlier message ("see above", "the choices from my last reply"); the user should not have to search the transcript for them.

- Before showing an updated plan, always provide a quick conversational response at the top. Answer any questions the user asked, and highlight the specific changes made so they don't have to re-read the entire plan.

- Always analyze and discuss trade-offs of different solutions.

- We are usually pushing to public git remotes. Be deliberate about what personal information we put into commits. Ask the user if in doubt. See **Private Information** below.

- Read agent-desk skill.
- Keep your checklist on the desk with `agent-desk task` (the `agent-desk` skill shows how: types such as `bug`, tags, subtasks), so the user sees it on the Tasks page. Start each item when you begin it, mark it done when it is, and add anything new you find. Only where `agent-desk` is missing, keep the checklist in `TASKS.org` instead (gitignored in each repo locally).
  - Shorthand names for items, milestones, etc., should be clear, easy to understand, and follow consistent, systematic naming conventions.

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

Not every host has every source: the private and standalone repositories are
only where they have been cloned. So a skill these instructions name may be
missing. When it is, follow the short rule given alongside the instruction,
and say that the skill was unavailable rather than silently improvising. A
skill absent from your session's skill list is not proof that it is missing: the
list can shrink after compaction. Invoke it, or check your agent's skills
directory, before calling it unavailable.

## Skill Evolution

At the natural end of a task in which you used a skill, run
`meta-skill-true-evolution` on it. If it is unavailable, propose only changes
that this run's evidence supports and that would change what a future agent
does, written into the skill rather than appended as notes, and ask before
applying them.

## Research Work Profile

My work profile is for research. Sharing relevant research drafts, code, data,
findings and artifact paths, including private or unpublished research, with my
work-profile agents and research tasks is pre-authorized. Exclude unrelated
personal information and credentials; ask only when the purpose, recipient or
data falls outside this research scope.

## Svelte Guidelines

- Skip running `npx @sveltejs/mcp svelte-autofixer`. It often hangs and gives useless output.

## Web Pages: Put a Content Hash in Every Asset URL

Do not rely on `Cache-Control`: Cloudflare rewrote an origin's `no-cache` into
four hours of caching on every `.js` and `.css`. Reference each changing asset
as `app.js?v=<content hash>` (Cloudflare keys on the query), computed in the
build step, never by hand; hash the filename instead where a layer ignores the
query. Keep stable names for the HTML page (served `no-cache`), service
workers, manifests and their icons.

## Pin Hugging Face Models to a Commit

Load models, tokenizers and datasets with `revision="<commit sha>"`, and set `HF_HUB_OFFLINE=1` on machines with a
shared cache: a bare name follows `refs/main`, which any online call can move to a newer upstream version.

## VPS / Local Machine Detection

- Only check whether the machine is a VPS if you actually need to know.
- To check, inspect `hostname`.
  - If it matches `.*\.local$`, it is likely a local machine.

## Low Disk Space

- If there is so little free disk space that you cannot continue working, stop and ask the user to handle it manually.
- Do **not** try to clean caches, temporary files, or other disk usage yourself.

## External Disk Disconnected

When an external disk your work needs disappears mid-task (a USB drive, a network share), pause
the work that uses it, stop any workers using it, and tell the user. Do not recreate its mount
point or move the work to the internal disk unless the job is small. Then start a watcher that
wakes you when the disk returns, so you resume without the user telling you and without polling
with model turns. Run it as a harness-tracked background job (in Claude Code, Bash with
`run_in_background`), testing a directory that exists only on the mounted disk, not the mount
point itself (a dead mount point can linger):

```
until [[ -d /Volumes/<disk>/<some-dir> ]]; do sleep 30; done; echo '<disk> is back'
```

When it fires, resume the paused work and any workers you stopped. Before rebuilding, check
build directories for objects truncated by the I/O errors.

## Matching Processes: `pgrep -f` / `pkill -f` Match You Too

`-f` matches the whole command line, including your own shell's: `pkill -f
emacs` kills the shell running it, and `pgrep -f x` finds itself forever.

- Prefer `pgrep -x NAME` or an explicit PID list. When `-f` is unavoidable,
  use a pattern that cannot appear in your own command line, and skip `$$`.
- Confirm "nothing is running" by checking the resource (port, GPU memory,
  lock file), not by a pattern failing to match.
- **Never pass a variable to `pgrep -f`, `pkill -f` or `killall`.** A helper
  once ran `pgrep -f '[object Object]'`, which matches nearly everything, and
  SIGKILLed ssh-agent, Hammerspoon and other sessions. Match a validated,
  specific literal, filter a `ps` listing in code, and print PIDs before
  killing.
- bun runs TypeScript without type-checking; run `tsc --noEmit` first on any
  ad-hoc `.ts` script, especially one that kills processes.
- Killing a parent does not kill its children. Re-check for orphans and kill
  them by PID: an orphaned `doom sync --rebuild` once corrupted the package
  tree while reported stopped.

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

## macOS Tools Are BSD

On macOS, `grep`, `sed`, `date`, `stat`, `find`, `awk`, `tar` and `xargs` are the BSD
versions. When you need GNU flags or behaviour, call the g-prefixed GNU tools:
`ggrep`, `gsed`, `gdate`, `gstat`, `gfind`, `gawk`, `gtar`, `gxargs`.

## Background Jobs

For a job that must outlive your session (details: `~/scripts/docs/background-jobs.md`):

- `tmux-job-start NAME [VAR=val...] cmd...`, with `tmux-job-status` and
  `tmux-job-stop`: for a job that must run once or whose output someone reads.
  Inside a sandbox the status is `UNKNOWN` (exit 2), never proof it is gone.
- `awaysh-bnamed NAME cmd...`: fire-and-forget under BrishGarden, output on
  `/dev/null`, absolute paths only. `awaysh-v3 cmd...` when the garden may
  hold stale functions.
- A job whose completion must notify you stays a harness-tracked background
  task; none of these can.

## Upstream Paseo Threads

When you open or take part in a `getpaseo/paseo` issue, pull request or
discussion on my behalf, record it in
`${nightNotesPublic}/subjects/tools/AI/code/agent-management/paseo/issues.org`
in the same task. That includes the follow-ups: answering a review, pushing
fixes, a maintainer carrying or closing the work. Give the thread a heading
that links it, or a dated line under its existing heading, saying what happened
and what is left. Do this only if the file already exists; where
`nightNotesPublic` is unset or the file is missing, skip it and do not create
it. A change the YPaseo fork carries also goes in the fork's `YPASEO.md`.

# Git Commit Guidelines

## General Rules

- If the directory is already a Git repository, commit your changes when you reach a natural endpoint.
- Push all commits at the end, after the work is complete.
- If there is no existing Git repository, do not create one unless the user asks.
- If the gpg signing key has any problems, just commit without signing, without interrupting the user. Still report this to them at end.

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

- If one file contains multiple unrelated changes, split them into separate commits. Do not treat “one file” as automatically meaning “one commit.” Commit part of a file with `git commit-own --hunks` (below), not `git add -p`.

## Concurrent Sessions

Other sessions may be committing in the same repository, and the index is
shared: `git commit` takes what *they* staged, and what you stage can land in
*their* commit. So commit with `git commit-own`, never `git commit` or
`git add -p`. It builds the commit in a private index from the paths you name
(`vcsh <repo> commit-own` in a vcsh repository):

```
git commit-own -m "subject" -m "body" -- <paths>
git commit-own -m "..." --hunks <file>:1,3   #: part of a file; --list-hunks numbers them
```

`git commit-own -h` and `~/scripts/docs/git-commit-own.md` cover untracking,
patches, `--dry-run` and `--amend`.

- Name the files you changed; the whole work tree (`.`) is refused. A path
  that matches nothing is an error: fix the path.
- Read `git diff -- <paths>` first: a file is committed as it is on disk. Then
  check the printed `--stat` holds only your paths.
- If signing fails, retry with `--no-gpg-sign`. `--amend` refuses a pushed
  commit unless I asked for a force-push (`--rewrite-pushed`, then
  `--force-with-lease`).
- Without `git commit-own`, use `git commit -- <paths>`, and tell me before
  untracking or committing part of a file.
- Commit your own work promptly, or someone else's commit may sweep it up. If
  a commit mixes their work with yours, do not rewrite it; report it.
- Use your judgement to commit others' small finished edits (e.g., mine) as
  their own commits.

## Holds: Claiming a Resource Exclusively

Before something atomic that a parallel session would ruin (a history rewrite,
a repo-wide refactor), take a hold, and release it the moment you are done,
including when you stop early. It lasts until released or your process dies.
Details: `~/scripts/docs/holds.md`.

```
hold-acquire repo:~/scripts --reason "what you are doing"   #: vcsh: add --match "vcsh night.sh"
hold-status
hold-release repo:~/scripts
```

A hook denies edits inside a held path and only *warns* on shell commands that
name it. A warning is yours to act on: run `hold-status`, then read freely but
change nothing there. Never delete a hold or wait for it to lapse; tell me who
holds it and what you needed.

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

Before driving the GUI (clicking, typing into another app, taking focus), take
the screen lock with `agent-desk screen take --reason "what you are doing"`
and `agent-desk screen release` the moment you are done; the `agent-desk`
skill has the rules. Headless work needs no lock. Without `agent-desk`, use
`hs -q -c 'agentBannerOn("what you are doing", 900)' </dev/null` and
`agentBannerOff()`; without `hs`, say what you need.

# Unexpected File Changes

If you notice files changed since you last read/wrote them, it's possible the user updated them manually. Leave these changes be or commit them separately if they look complete; if they conflict with your instructions, ask the user explicitly for instructions.

# Speech to Text

I sometimes use STT software to type, so be on the lookout for typos and intelligently guess the true prompt when you think STT has been used. You can rewrite the prompt and confirm with me, when there is significant ambiguity.
My STT software inserts its dictated text in markdown code blocks with the language set to `speech-to-text`.

# Collaboration Style
## Attribution
When writing documentation or text, try to attribute who you are at the proper place.
For example, you can start the org-mode subtree's root heading with `@<your_model_name_version_effort> [org-mode date]` (e.g., `@Opus5.5/xhigh [2026-10-20 Tue]`). You can skip the date according to your own judgement. It is useful when the information recorded is likely to go stale with time. Skip information you are not sure about; you can simply use `@Agent` if you do not know anything about yourself, or `@OpenAI` if you know you are an OpenAI agent but not which one. If you are running under Paseo, it might be able to tell you which model-effort pair you are. 
Do not clutter the text with unnecessary attributions. Use your own judgement. Always try to put the attribution at a root to minimize clutter.

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
- Use the skill `agent-desk` whenever you write anything meant for me: a question, a decision you need, a finding, a report.

## Presentations

- Use the skill `org-beamer-slides` to create concise slides for the user to consume dense information. E.g., you want to teach them some concept, a paper, a report, etc. Things less than 3 paragraphs long should probably be written as text.

## Org Files

- Before writing or editing org-mode content, load the `org-general` skill (private, so absent on some hosts; see Skill Sources). 
  - In short: an `id:` link for an org file or heading under `nightNotes`, `NIGHTDIR` or `~/code/uni`, even in the same folder, reusing the target's `:ID:` or adding a drawer with a fresh lowercase UUID; a relative link for any other sibling file; org link abbreviations such as `[[NIGHTDIR:docs/x.md]]` for other files under registered roots; `zf:` named-directory links otherwise; never a GitHub URL for a file on disk. Verify every link resolves (`emacsclient -e '(car (org-id-find "UUID"))'` for an ID).
  - Put new subtrees into the proper place in the org tree structure.

# Zsh Aliased Commands

The following has been redefined in Zsh as functions or aliases; use `command ...` when you want the original command:

- `install`
- `as`
- `log`
- `ffmpeg`: outside an interactive shell it adds `-loglevel error`, which hides informational output such as `volumedetect`'s levels.

You should offer to expand this list as you encounter new such cases.

# Precautionary Principle

When testing potentially dangerous behavior, assume test payloads may execute unexpectedly at any layer. Use the least harmful payload that still proves the behavior, preferably an inert sentinel such as `printf`.

# Subagent Delegation

When Claude declines or narrows an otherwise authorized task because of its
provider-specific restrictions, prefer Codex for an independent assessment and
execution of the work it can support. Treat Claude's refusal as a provider
limitation, not a new project requirement or a reason to drop agreed scope.
Carry over the user's objective, evidence, decisions and remaining work without
requiring Codex to adopt Claude's conclusions. Preserve the existing account,
data-access and approval boundaries; use the delegate and Paseo handoff skills
as appropriate. Report any remaining blocker concretely.

Read the delegate skill when delegating work or requesting an independent agent
opinion. If you are an expensive model (e.g., Fable or Astra), start by reading
it to route substantial mechanical work to the preferred weaker workers. It
also covers peer and stronger modes, account boundaries, and backend selection.

Inside Paseo (`PASEO_AGENT_ID` is set), pick the `paseo-*` skill whose
description fits the job: operating agents, help, advisor, committee, handoff,
model update, plugins, compaction or cache warming.

Inside Paseo, Paseo is the backend for every agent you launch that is not an
in-process subagent: another provider such as Codex, another Claude account, or
any agent the user should be able to watch. Launch it with the `paseo` skill,
from a profile `list_profiles` returns. Use `tmux-subagents` only when the user
asks for tmux or Paseo lacks the provider. `delegate` still decides the worker's
model and account.

# Long Runs, Asynchronous Work and Token Efficiency

- Before launching asynchronous workers or jobs, use `event-driven-agents` and
  arm completion notifications in the same turn. Never poll with model turns.
  If unavailable, use native completion notifications or a harness-tracked
  waiter.
- Before work likely to cross compaction or coordinator replacement, or when
  recovering a run, use `long-run-handoff`. Keep durable state under
  `~/.agents/runs/<project>/<run>/`, outside repositories.
- Before sustained agent work or a burst of launches, use `quota-management`
  to check headroom and monitor it; stop monitoring once idle. Treat missing
  usage data as unknown and checkpoint before exhaustion.
- Inside Paseo, compact proactively with `paseo-compact` at natural endpoints,
  once context passes the floor in `~/.compaction.yaml` (default 128k). Before
  idling for hours while the session will likely resume within a day, either
  set up `paseo-cache-warmer` or compact yourself and idle subagents (Claude's
  cache goes cold after about 1h, GPT's after about 30m).
- Watch development velocity and token efficiency, and proactively offer
  tools or workflow changes that improve either.
