---
name: delegate
description: Choose model, account, and backend for delegated work. Preserve Fable/Astra quota with weaker workers for mechanical tasks; use peers for independent review or parallel work and stronger workers for reasoning bottlenecks. Not for routine search without model selection or manually relayed browser chats.
---

# Delegate

Choose the worker's role and reasoning needs separately from its account and
execution backend. Keep framing, integration, and final acceptance with the
parent. Explicit user choices override defaults. This skill does not authorize
delegation outside the session's permissions or expand the task's scope.

## Choose a mode

- **Weaker:** offload substantial work that has a clear specification and a
  cheap acceptance check: extraction, repetitive edits, bounded implementation,
  or routine validation. For Fable/Astra parents, choose this proactively when
  delegation is permitted. Decide before doing the bulk of that work yourself.
  Resolve consequential ambiguity in the parent first.
- **Peer:** obtain an independent design, review, or implementation from a
  comparably capable worker. For an opinion, ask for a recommendation and
  evidence without edits; do not frame the brief as endorsement of your answer.
- **Stronger:** escalate a specific reasoning bottleneck or a task that exceeds
  the current worker's capabilities. Give the stronger worker the evidence,
  attempted approaches, unresolved question, and a bounded stopping condition.

Use `/delegate weaker`, `/delegate peer`, or `/delegate stronger` to select a
mode explicitly. Use explicitly invoked advisor/handoff skills for their
workflow. An explicitly invoked skill may select another provider according to
its rules; it does not authorize silently changing accounts or bypassing the
data-access check below.

These are task-relative choices, not universal price or capability rankings.
Read [model routing](references/models.md) for selection and escalation rules.
Avoid delegation when briefing and reviewing it would cost more than doing it.
Batch related small operations; parallelize only independent assignments with
nonoverlapping ownership. Do not recursively delegate by default.

## Preserve account and scope

An **account profile** selects credentials and configuration, such as
`CLAUDE_CONFIG_DIR` or `CODEX_HOME`. A **launch profile** selects provider,
model, effort, and permissions. A Paseo **provider entry** may bind an account
profile through its environment. A display label alone proves none of these.

Inherit the parent's provider and account profile unless the user explicitly
chooses otherwise. Model selection alone
does not authorize changing accounts. Before exposing personal information to
a work account, review the brief and everything the worker can reach, including
workspace files, instructions, history, attachments, and connected tools.
Existing authorization for that scope is sufficient; otherwise obtain the
missing authorization and continue unaffected work. A sanitized brief, tmux
session, worktree, or profile switch does not isolate data access. Choosing a
work account does not itself authorize exposing personal data. When asking,
explain what could be exposed and why without quoting sensitive contents.
Carry these boundaries into descendant delegations.

Never widen permissions or recursion limits to make a launch succeed. Workers
return ambiguity or repeated failure to their parent; they do not
launch stronger workers unless the brief authorizes recursion. Keep the
current branch/worktree unless another workspace is authorized. Record root
limits and apply them across the whole descendant tree, not independently to
each worker. Use backend enforcement where available; otherwise report limits
that cannot be enforced rather than claiming a guarantee.

## Choose a backend

- **In-process:** prefer the runtime's agent tool for a bounded worker when it
  offers the requested model and no persistent user-accessible session is needed.
- **Paseo:** when persistence, user visibility, or an unavailable in-process
  model requires another backend, prefer Paseo when already inside Paseo and the
  selected provider/account is configured. Read [Paseo binding](references/paseo.md)
  and the installed official `paseo` skill before operating it.
- **tmux:** use native interactive CLI workers when the user needs the full
  TUI, a custom launcher, a provider/account unavailable in Paseo, or a host
  without a daemon. Read the installed `tmux-subagents` skill; preserve its
  launch, resume, registry, notification, and cleanup protocol.

Honor an explicitly selected backend. If its required skill or requested model
is unavailable, say so; do not substitute another session, account, or model
silently. Only read the chosen backend's instructions. Do not resume a second
writable copy of a running conversation. Cross-backend delegations must record
their real lineage and notification path, without inventing native parent links.

## Brief and assign ownership

Give each assignment an identity and a self-contained brief: objective,
deliverable, relevant paths, settled decisions, allowed edits, acceptance checks,
selected mode/model/account/backend, and scope/recursion limits. Prefer fresh
context to the whole conversation. Include applicable repository instructions
and facts the worker cannot recover from the supplied files.

For Codex `spawn_agent` with a model override, use `fork_turns="none"` and an
explicit model when the tool supports it. Backend-specific APIs and syntax live
in the backend instructions, not in a shared wrapper command.

Assign exclusive write ownership. Keep commits and integration with the parent
unless explicitly assigned otherwise. A worker must not stage, revert, or
commit unrelated changes. Record whether the parent or user owns follow-ups;
once handed to the user, do not inject messages, interrupt, resume, or close it
until ownership is returned. Backend labels are cooperative records, not locks.

Keep sensitive briefs, account mappings, conversation IDs, and results out of
public repositories and process arguments. Use an owner-only task directory
when files are needed. Persistent long runs require checkpoints sufficient for
another coordinator to resume, including task identity and artifact locations.

## Collect and accept the result

Ask for a compact result: assignment/worker identity, outcome (completed,
blocked, or failed), changed paths or findings with evidence, checks and their
outcomes, and unresolved issues. A native tmux worker publishes the result file
required by its backend. A normal Paseo or in-process worker may use its final
response; persistent long runs also need durable checkpoint/result artifacts.
Every follow-up gets a fresh assignment identity, so old results cannot satisfy
new work. Publish result files atomically and validate their identity on read.

Arm the supported completion channel once and re-arm after follow-ups when
needed. Avoid repeated polling and duplicate execution. Continue independent
work while workers run. Idle status, process exit, or a notification alone does
not prove successful completion.

Inspect the evidence and relevant diff, and run missing acceptance checks. Do
not redo every mechanical step. Give a focused correction for a clear defect;
return unresolved ambiguity or repeated failure to the parent rather than an
open-ended retry chain. Close/archive persistent workers only with existing
authorization, after checking ownership and active descendants. The parent is
responsible for the final result.
