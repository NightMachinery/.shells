# Transfer a Claude conversation to Codex

Two commands continue a stopped Claude Code conversation in Codex. Both
resolve the source through the existing session helpers, retain the source
transcript, and open the destination in the source working directory.

```zsh
claude-to-codex-native <transcript-or-session-id> [codex options...]
claude-to-codex-compact <transcript-or-session-id> [codex options...]

claude-to-codex-native-fz             # current project's Claude sessions
claude-to-codex-native-all-fz         # Claude sessions across projects
claude-to-codex-compact-fz
claude-to-codex-compact-all-fz
```

The pickers include the registered Claude profiles. Quit the source session
first. A live source, ambiguous ID, cancelled picker, or missing working
directory stops the command before preparation.

## Native importer

[agfi:claude-to-codex-native] submits one `SESSIONS` migration item through
Codex's `externalAgentConfig/import` API. It does not import configuration,
instruction files, hooks, plugins, or other conversations. The wrapper waits
for the matching import completion notification and validates the returned
target using `thread/read` before resuming it.

The native importer decides what formats and destinations it supports. An
import source must be discoverable by Codex's Claude session detector; an
arbitrary JSONL file outside the detected session roots can be rejected.
Compaction mode can convert such a source without the native detector.
An import that returns a remote or otherwise non-resumable target is reported
with its identifier; the wrapper never guesses from the newest local thread
or silently switches to the compaction implementation.

## Codex compaction

[agfi:claude-to-codex-compact] translates the complete source history into
chronological historical text, with author labels and inert tool calls and
results. Source harness instructions are excluded so that the destination
loads its own instructions. It creates a persistent Codex thread, injects the
history without starting an ordinary task turn, and requests native Codex
compaction. Only successful compaction opens the thread interactively.

The preparation and resume use a per-launch
`model_context_window=1050000` setting for supported GPT-6 models. This is an
upper bound, not a promise that all 1,050,000 tokens are available for source
history: Codex reserves room for instructions and model output. The configured
model is retained unless `--model` overrides it. An unsupported model or
context overflow fails explicitly; history is not silently truncated and no
other model is selected automatically. An error after thread creation prints
the destination ID for recovery.

Model and `--config` options apply to preparation and interactive launch.
Other supported local launcher options apply when opening the destination.
`--profile` is rejected because app-server preparation does not accept CLI
profile selection; use explicit `--config` overrides. Directory overrides,
remote targets, positional prompts, and worktree creation are also rejected.
`agent_handoff_timeout` controls the backend timeout (default `10m`).

```zsh
claude-to-codex-compact-fz --model gpt-6.1-sol \
    --config 'model_reasoning_effort="high"'
```

Codex's compacted state may include opaque encrypted items. It remains in the
Codex thread and is not a readable summary for another agent. For the reverse
direction, see [the readable and Claude-compacted handoffs](codex-to-claude.md). For native
Claude compact-before-resume, see [Claude compaction](claude-session-compact.md).

Implementation: `zshlang/auto-load/others/agent-handoff.zsh` keeps shell policy;
`golang/agent_session` handles history and JSON-RPC. Run its Go tests and
`zsh -f zshlang/tests/agent-handoff.zsh` for isolated checks with synthetic
sessions and fake agent processes.

Disposable integration tests with Codex CLI 0.160.0 verified both native
import from Claude's normal session directory and persistent native
compaction of a synthetic history. Claude's native `/compact` was also
invoked on the test conversation: a compaction error returned with a
successful CLI exit was correctly rejected by the result validator.
