# Weaker delegation compatibility

The former standalone `delegate-weaker` workflow is merged into the shared
[delegate skill](../configFiles/agent-skills/delegate/SKILL.md). It supports
weaker, peer, and stronger workers with separate model/account/backend choices.
See [delegation modes and backends](delegate.md) for current usage and policy.

`/delegate-weaker` and `/weaker` still select weaker mode; `$delegate-weaker`
and `$weaker` are the corresponding Codex entrypoints. Both are forwarding
skills with their own declared names, so old global instructions and explicit
invocations keep working. The canonical rules live only in `delegate`, with
routing and Paseo details in its reference files.

Weaker routing remains Fable to Opus, and Astra to GPT 6 Luna at max effort for
routine work or GPT 6.1 Sol (or later available Sol) for bounded implementation.
The installed runtime must expose the requested selector/model. Explicit model
choices override these defaults; unavailable choices are reported, not silently
substituted. These preferences make no claim about exact prices or separate
quota pools.

[agfi:agent-skills-link] discovers all three skill directories and installs
whole-directory links so their relative references resolve in every configured
agent profile. No global instruction or runtime script changes are needed.
