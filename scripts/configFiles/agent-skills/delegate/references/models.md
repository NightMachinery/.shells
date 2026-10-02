# Model routing

Verify the available runtime selector and model IDs before launch. The user's
explicit model, provider, account, and effort choices take precedence. Never
invent a model ID, imply exact prices or separate quota pools, or treat different
providers as a single reliable capability ladder.

## Weaker defaults

- **Fable / Claude:** use **Opus**, selecting the available Opus model through
  the runtime's model selector rather than inheriting Fable.
- **Astra / Codex:** use **GPT 6 Luna** (`gpt-6-luna`, with max effort level) for straightforward
  extraction, repetitive transformations, and routine checks. Use **GPT 6.1 Sol**
  (`gpt-6.1-sol`, OR ANY LATER AVAILABLE VERSIONS) for bounded implementation or edits needing more local reasoning.

These are the user's routing preferences, not a claim about exact prices or
separate quota pools.
If the requested family or explicit selection is unavailable, report it and
continue locally when feasible. Do not silently substitute an expensive model.

## Claude effort preference

Unless the user explicitly requests Sonnet, prefer **Opus at low reasoning**
instead of **Sonnet at medium or higher reasoning**, within the same authorized
account. This applies across modes, including peer mode for a Sonnet parent.
It is an explicit user routing preference, not an unauthorized escalation.
Verify the backend can actually set low effort; a model selector alone may not
provide effort control. If needed, use an authorized backend with explicit
effort selection. If none can honor it, report the limitation instead of running
Opus at an unknown effort or changing accounts. Use higher Opus effort only
when the bounded task warrants it and the user's effort/budget constraints allow.

When revising defaults or choosing an unlisted combination, compare it using
Artificial Analysis intelligence and cost per task, not token prices alone.
Recheck current measurements before revising defaults; the dated comparison is
recorded in the repository's delegation documentation. Benchmark results are
not a universal claim about every task, release, or account quota.

## Peer and stronger

For **peer**, prefer the parent's known model and effort with fresh context,
or an explicitly requested independent reviewer. A different provider can offer
a useful perspective when provider/account selection is authorized, but it is
not automatically stronger and does not bypass account boundaries.

For **stronger**, require an explicit user request or evidence of a reasoning
bottleneck, such as a failed focused correction. Use the user-selected model or
a verified appropriate higher tier within the same provider/account. The user's
known routing tiers are Opus/Fable for Claude and Luna/Sol/Astra for Codex;
resolve actual available model IDs and do not infer a tier from profile labels
or vendor marketing. Without an explicit user request, stronger escalation must
not exceed the root session's model, or its effort on that same model. Effort
levels are not comparable across models. The standing weaker defaults and
Sonnet substitution above are pre-authorized routes, not escalations; Luna max
remains valid under a root using another model at lower effort. Record the root
ceiling and standing routes in descendant briefs; if the ceiling is
unknown, return to the parent rather than guessing. Delegate one bounded attempt
per bottleneck and report the escalation. Increasing effort is separate from
model capability. If no defensible option is available, report that limitation.

When this skill's weaker defaults selected Luna, a reasoning defect may justify
escalation to an available Sol; repeated failure then returns to the parent.
An explicit user choice of model/family, effort, or budget is a constraint:
do not override it without authorization. Quota exhaustion does not justify
switching accounts or models by itself.
