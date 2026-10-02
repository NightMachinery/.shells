# Bind delegation to Paseo

Use the installed official `paseo` skill for the current tool and CLI syntax.
Do not modify the upstream skill or mirror its complete command reference.
Resolve the actual daemon host and selected account before launching. The
daemon's provider entry determines the account, not the caller's environment.
A remote host is also a data-access boundary.

1. Discover available launch profiles and read their notes when the interface
   supports it. Filter by the authorized account/provider and explicit model
   choice. If that discovery interface is unavailable, report the fallback and
   discover configured providers/models through the supported CLI or API.
2. Launch one bounded assignment with explicit model and settings. An advisor
   receives an analysis-only brief and permitted data-access scope.
3. Use Paseo's returned agent ID as the worker identity. Record assignment ID,
   root/lineage, and owner in supported labels or a private task record. Native
   parentage exists only when the actual calling context supplies it. A human
   shell or tmux process creating a top-level Paseo agent must not claim native
   subagent status unless it was explicitly established.
4. Let Paseo own its runtime registry. Do not duplicate its agents in tmux's
   `agents.json`. A tmux session used to observe output is not another worker.
5. Use structured prompt delivery and the available completion callback. CLI
   launches may require a bounded `paseo wait` in a tracked task; do not assume
   a detached shell wakes the parent. A completed turn still needs result review.
6. A routine result can be the persisted final message with assignment identity,
   outcome, evidence, and verification. Long runs need checkpoint/result files
   in an authorized private or workspace location. Never widen a sandbox just
   to write an artifact outside its allowed directories.
7. Recheck ownership/status before follow-ups, interruptions, or archive actions.
   User ownership is an explicit handoff, not inferred from elapsed time or
   whether the UI is open. Labels do not prevent concurrent human input.

`paseo attach` streams output; it is not a native chat TUI. Use the app or
structured `paseo send` for messages. Do not start the underlying native CLI
against the same live conversation to regain a TUI.

Quota reset continuation is a separate opt-in watcher, not implied by
delegation. Do not enable it, change daemon configuration, or restart the daemon
merely to launch or supervise a worker. Archive/close only within authorization;
retain results and conversation history.
