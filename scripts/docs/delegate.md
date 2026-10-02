# Delegating work and independent opinions

The shared [delegate skill](../configFiles/agent-skills/delegate/SKILL.md)
combines task selection, worker briefing, account boundaries, ownership, and
result review in one workflow. Select a mode explicitly:

```text
/delegate weaker apply the agreed edit and verify it
/delegate peer independently review this design without edits
/delegate stronger investigate this bounded reasoning bottleneck
```

Use `$delegate` with the same modes in Codex. The existing `/delegate-weaker`
and `/weaker` entrypoints remain weaker-mode aliases; existing global
instructions can continue referring to `delegate-weaker`.

Weaker defaults preserve the user's choices: Fable routes to available Opus;
Astra routes routine work to GPT 6 Luna at max effort and bounded implementation
to GPT 6.1 Sol or later available Sol. Peer normally uses the parent's model and
effort with fresh context. Stronger requires an explicit user request or evidence
of a reasoning bottleneck and a verified appropriate option. Without a user
override, stronger escalation cannot exceed the root session's model or its
effort on that same model. Effort is not comparable across models, and the
standing weaker defaults and Sonnet substitution remain pre-authorized. Explicit worker,
account, effort, and budget constraints remain binding. These are routing
preferences, not claims about prices or a universal ordering across providers.

For Claude, prefer Opus at low reasoning over Sonnet at medium or higher unless
the user explicitly requests Sonnet. Verify that the backend supports explicit
effort selection rather than running Opus at an unknown default effort.
Compare the exact variants using intelligence and cost per task, rather than
token price alone. On 2026-10-02, [Artificial Analysis compared Sonnet 5.5 medium
with Opus 5.5 low](https://artificialanalysis.ai/models/comparisons/claude-sonnet-5-5-medium-vs-claude-opus-5-5-low):
Intelligence Index 41 versus 42, and cost per task $0.59 versus $0.55. This
supports that specific combination; recheck the evidence when models change.

Execution is a separate decision. In-process tools fit bounded disposable
workers. Paseo fits persistent app-visible workers when the account is
configured. Native tmux sessions fit full TUI interaction and custom launchers.
The skill loads only the chosen backend's guidance; it does not duplicate
upstream Paseo's command catalog or move tmux's scripts and state.

Normal Paseo/in-process workers can return a final response with task identity,
outcome, evidence, and verification. Tmux retains its result-file protocol.
Persistent long runs need checkpoints and durable artifacts. Notifications and
idle status prompt review; they do not prove task completion.

Account selection is independent of worker capability. A display label or
sanitized prompt does not isolate accessible files and tools. The workflow
preserves authorized account/provider selection, write ownership, and recursion
limits. Workers do not launch stronger descendants without permission in their
brief. Persistent sessions are closed only within existing authorization.

Sources stay under `configFiles/agent-skills/delegate/`, with model policy in
`references/models.md` and Paseo binding in `references/paseo.md`. The existing
[agfi:agent-skills-link] installs directory links for all configured agents,
including both Claude accounts. It preserves reference files alongside the
skill. No daemon or native session restart is needed; skill discovery may need
a fresh turn or session depending on the client.
