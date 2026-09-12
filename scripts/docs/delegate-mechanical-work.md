# Delegating mechanical work

The shared [delegate-mechanical-work skill](../configFiles/agent-skills/delegate-mechanical-work/SKILL.md)
asks Fable and Astra to preserve their token quota by assigning substantial,
well-specified mechanical work to cheaper workers:

- Fable / Claude routes to Opus.
- Astra / Codex routes straightforward tasks to GPT 5.6 Terra and bounded
  implementation needing more reasoning to GPT 5.6 Sol.

The parent keeps decisions, integration, and review. Explicit worker selection,
small context handoffs, and concise results reduce overhead. Tiny tasks stay
local because delegation can cost more than the work itself. The model choices
are user preferences; this skill does not promise specific savings or independent
quota pools.

The existing [agfi:agent-skills-link] discovers the new directory automatically
and links it into the configured agents' skill directories. The agent launch
path already calls this helper; run `agent-skills-link` to link it immediately.
After discovery, invoke `/delegate-mechanical-work` in Claude or
`$delegate-mechanical-work` in Codex, or let the matching description trigger it.

This is skill guidance, not a runtime enforcement mechanism. It does not alter
global instructions or model defaults, and it respects session restrictions on
delegation. If the requested worker cannot be selected, the parent reports that
limitation and continues locally when feasible.
