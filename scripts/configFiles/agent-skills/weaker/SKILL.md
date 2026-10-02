---
name: weaker
description: Alias for delegate in weaker mode. Use when invoked as weaker or named by existing global instructions.
---

# weaker

Read [delegate](../delegate/SKILL.md) and its
[model routing](../delegate/references/models.md), then apply weaker mode unless
the user explicitly selected another mode. Keep shared delegation rules in
`delegate`; use its chosen backend instructions for execution. If the canonical
skill is missing, report that installation problem and continue locally when
feasible rather than launching a worker without its policy.

Loading this alias does not itself require spawning a subagent. Existing scope,
account, model, effort, and recursion constraints still apply.
