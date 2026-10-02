# Continue a Codex conversation in Claude

[agfi:codex-to-claude] starts a fresh Claude conversation with the complete
readable history of a stopped Codex session. Native Codex compaction can
contain opaque state, so the handoff uses the persisted transcript rather
than trying to feed compacted Codex items to Claude.

```zsh
codex-to-claude <transcript-or-session-id> [claude options...]
codex-to-claude-fz                  # current project's Codex sessions
codex-to-claude-all-fz              # Codex sessions across projects
codex-to-claude-compact <transcript-or-session-id> [claude options...]
codex-to-claude-compact-fz
codex-to-claude-compact-all-fz

agent_handoff_claude_profile=work codex-to-claude-all-fz --model opus
```

The destination profile defaults to `default`. Set
`agent_handoff_claude_profile` to another registered profile. A subshell pins
the selected profile, clears inherited source session and managed-pane IDs,
and starts its existing launcher in the source working directory. The
caller's directory and environment are restored when the destination exits.

The full rendered history, including available subagent histories, is saved
without code-block elision under
`${XDG_STATE_HOME:-$HOME/.local/state}/agent-handoffs/handoff.*/history.md`.
Each bundle has private directory permissions and its history file is mode
600. `agent_handoff_state_dir` overrides the bundle root. Bundles remain for
later destination resumes; nothing is written into the project tree and the
source transcript is retained.

Claude is asked to read the file in chunks before taking task actions, check
the current project files, and continue unfinished work while respecting
later corrections, pauses, and completed tasks. Historical tool calls are
already executed and source harness instructions do not replace Claude's
current instructions. Reading the file consumes destination context; this
path does not pre-compact it. Unavailable source images or external artifacts
remain references, rather than being reconstructed.

## Claude compaction before launch

[agfi:codex-to-claude-compact] first exports the same complete readable history.
It seeds a fresh conversation under the selected Claude profile's
`projects/<cwd-slug>/<new-uuid>.jsonl`, with private file permissions and no
ordinary model turn. A handoff instruction and a single assistant text block
hold the archive verbatim; historical tool calls remain inert text. This uses
Claude's local transcript format, rather than transferring Codex's opaque
compaction state.

It then runs Claude's native `/compact` on that exact new session using
[the existing compaction helper](claude-session-compact.md). Only verified
success, or the recognized insufficient-history no-op, opens the same ID
interactively. The final prompt continues unfinished work from the summary and
consults the full archive if needed. It respects recorded pauses and completed
tasks.

```zsh
agent_handoff_claude_profile=work codex-to-claude-compact-all-fz --model opus
```

Compaction uses the destination Claude account, model, and context limit.
It does not inherit Codex's context-window override. Oversized history, quota
errors, or transcript-format incompatibility stop the handoff without silently
truncating history or opening an uncompacted session. On failure, the printed
destination transcript and full archive remain available for inspection or a
later `claude-resume-compact` attempt. The retry knob is
`claude_code_session_resume_compact_retries`, as for Claude resumers.

A live source, missing directory, cancelled picker, ambiguous ID, malformed
source transcript, or failed export stops the launch. Session, print-mode, directory, and worktree
overrides are rejected so they cannot redirect the handoff.

See [Claude to Codex](agent-handoff.md) for native import and Codex compaction,
and [Claude compact-before-resume](claude-session-compact.md) for reducing
an existing Claude conversation before opening it.

Run `zsh -f zshlang/tests/agent-handoff-reverse.zsh` for the isolated reverse
handoff checks. `agent_session codex handoff-export <transcript>` validates
the full JSONL before rendering readable history without elision.
`agent_session codex handoff-claude -cwd DIR -config-home HOME -history FILE`
seeds the private destination transcript and returns its exact absolute path.
