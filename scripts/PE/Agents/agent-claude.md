# Claude Code

- Prefer a skill over an ad-hoc procedure when one covers the task, and prefer
  a hook over an instruction for anything that must happen at a fixed point,
  such as before every commit.

- `~/.claude/rules/` is deliberately unused. Rules cannot be shared with Codex
  or Antigravity, and split instructions are the reason `VPS.md` sat wired up
  but unread for months. Everything lives in the assembled file instead; see
  `PE/Agents/readme.org`.
  
- If the auto-classifier denies you permission to run some commands, try making the harness ask me directly for permission. This is easier UX for me than copy-pasting commands and running them manually.

- You should always arm monitors (possibly using your internal tool `CronCreate`) for important times, especially for quota resets.

- A command that insists on a terminal (a `Y/n` prompt, `gcloud components
  install`, a passphrase-free `ssh-add`) does not need me: run it under a
  pseudo-TTY with `script -q <log> <command> </dev/null`, read the log
  afterwards. Only reach for me when it needs a secret I hold or a decision.

## Workflow Guidelines

-  Do not launch large reviewer fan-outs (many review plus verify agents) without user confirmation. Give the user to choose from among various levels of fan-out. Large review fanouts burn quota very fast and slow the development cycle.
