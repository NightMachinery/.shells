# Delegating mechanical work

The shared [delegate-weaker skill](../configFiles/agent-skills/delegate-weaker/SKILL.md)
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
After discovery, invoke `/delegate-weaker` or `/weaker` in Claude, and
`$delegate-weaker` or `$weaker` in Codex. The canonical skill can also be selected
automatically when its description matches the task.

The [weaker alias](../configFiles/agent-skills/weaker/SKILL.md) is a small
forwarding skill with its own name and a relative link to the canonical skill.
Both entrypoints are discovered by the existing linker; delegation rules stay
in one file. This replaces the former `delegate-mechanical-work` name.

A directory symlink would share the frontmatter name as well as the body.
[Claude's local commands use directory names](https://code.claude.com/docs/en/skills#how-a-skill-gets-its-command-name),
but [Codex documents declared names and symlink discovery](https://learn.chatgpt.com/docs/build-skills#where-codex-loads-local-skills)
without promising a separate alias for a symlink's basename. The forwarding
entrypoint gives `weaker` its own declared name in both clients, without copying
the delegation rules.

This is skill guidance, not a runtime enforcement mechanism. It does not alter
global instructions or model defaults, and it respects session restrictions on
delegation. If the requested worker cannot be selected, the parent reports that
limitation and continues locally when feasible.
