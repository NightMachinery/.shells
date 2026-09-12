---
name: delegate-mechanical-work
description: Preserve Fable and Astra token quota by delegating substantial mechanical work to cheaper subagents. Use when Fable (Claude) or Astra (Codex) has repetitive edits, bounded implementation, extraction, or routine validation that can be specified and checked cheaply.
---

# Delegate mechanical work

When acting as Fable or Astra, proactively delegate substantial mechanical work
to the user's preferred cheaper models. Keep problem framing, consequential
decisions, ambiguous debugging, integration, and final review with the parent.
Apply this within the session's delegation permissions and the user's task scope.

## Choose the worker explicitly

- **Fable / Claude:** use **Opus**, selecting the available Opus model through
  the runtime's model selector rather than inheriting Fable.
- **Astra / Codex:** use **GPT 5.6 Luna** (`gpt-5.6-luna`) for straightforward
  extraction, repetitive transformations, and routine checks. Use **GPT 5.6 Sol**
  (`gpt-5.6-sol`) for bounded implementation or edits needing more local reasoning.

These are the user's routing preferences, not a claim about exact prices or
separate quota pools. Check the available tool schema and model choices before
spawning; do not invent model IDs or silently substitute an expensive model.
If neither the requested worker nor explicit model selection is available,
briefly report the limitation and continue locally when feasible.

## Delegate where it saves work

Delegate tasks with a clear result and a cheap acceptance check: apply an agreed
edit across files, implement a specified helper, extract facts with source
locations, update docs from a settled change, or run checks and summarize failures.
Decide before doing the bulk of that work yourself.

Use a direct tool call for a trivial operation. Keep a task local when explaining
it and reviewing the result would cost more than completing it. Resolve unclear
requirements first, then delegate the resulting concrete work. Batch related
small operations into one assignment; use multiple workers only for independent
tasks with nonoverlapping ownership and enough work to justify coordination.

## Keep the handoff small and complete

Give the worker the outcome, relevant paths or excerpts, decisions already made,
constraints, allowed edits, and acceptance checks. Prefer fresh context over the
whole conversation; include applicable repository instructions and facts the
worker cannot recover from the supplied files. With Codex's `spawn_agent`, when
available, use `fork_turns="none"` with the explicit worker model and a
self-contained message; a full-history fork may prevent a model override.

Ask for a compact result: changed paths or extracted facts with locations,
checks run and their outcomes, and unresolved issues. Leave verbose logs in an
artifact when needed. Avoid repeated status polling and duplicate execution of
the worker's assignment. Continue independent parent work while it runs.

Assign exclusive write ownership when workers share a checkout. Keep integration
and repository commits with the parent unless explicitly assigned otherwise;
workers must not sweep in unrelated changes or recursively delegate by default.

## Review and escalate deliberately

Inspect the returned evidence and relevant diff, then run any missing acceptance
checks. Do not redo every mechanical step or accept a success claim as proof.
Give a focused correction when the defect is clear. If a Luna task needs more
reasoning, move it to Sol; return unresolved ambiguity or repeated failure to the
parent instead of spending quota on an open-ended retry chain. The parent remains
responsible for the completed result and its accuracy.
