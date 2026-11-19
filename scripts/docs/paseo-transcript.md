# Opening a Paseo agent's transcript

`paseo-view-transcript [ID]` converts the native transcript of a Paseo agent to
org and opens it in emacs, the way `agent-view-session-transcript` does for a
transcript path. ID is a full agent id or a unique prefix (Paseo's short ids
work), and defaults to `$PASEO_AGENT_ID`, so inside a Paseo agent no argument is
needed.

- `paseo-view-transcript-md [ID]`: the same, as markdown.
- `paseo-view-transcript-raw [ID]`: opens the native `.jsonl` itself.
- `paseo-view-transcript-fz [query]`: picks an agent by title, status, provider
  and cwd; enter opens org, alt-m markdown, alt-r the raw transcript.
- `paseo-transcript-path [ID]`: prints the native transcript's path.

The YPaseo desktop app's Cmd+Shift+O runs
`zsh -lic 'paseo-view-transcript <agent-uuid>'`. With no terminal, the progress
and any failure show in a Hammerspoon band; see "Progress, and one conversion
per session" in [agent-sessions.md](agent-sessions.md). Pressing it again for
the same agent restarts the conversion.

## From an agent id to a transcript

Paseo stores each agent at `<paseo home>/agents/<dir>/<agent id>.json`
(`h-paseo-agent-record`). `h-paseo-native-rows` reads its `provider`, `cwd` and
session ids:

- The provider may be an alias such as `handoff-claude-<hash>`. It is followed
  through `agents.providers` in `<paseo home>/config.json` along `extends` until
  it reaches `claude` or `codex`; the first `CLAUDE_CONFIG_DIR` or `CODEX_HOME`
  met on the way is the native home. This mirrors `internal/paseo/provider.go`
  in agent-quotas. A provider that no longer resolves falls back to the native
  agent Paseo stored on the record (`persistence.metadata.provider`).
- The session id is taken from `persistence.metadata.threadId` (Codex),
  `persistence.sessionId`, `persistence.nativeHandle` and
  `runtimeInfo.sessionId`, in that order; each is tried.

`h-paseo-native-files` then looks for `<home>/projects/*/<id>.jsonl` (Claude)
or `<home>/sessions/YYYY/MM/DD/rollout-*-<id>.jsonl` and
`<home>/archived_sessions/rollout-*-<id>.jsonl` (Codex). Only when the
provider's own home holds nothing are the other homes this host knows searched
(`h-agent-session-call <agent> roots`, every Claude profile), since a provider
that sets no home ran with whatever the daemon had.

Antigravity agents (`antigravity`, `agy-acp`) are refused with a message: their
transcripts are not Claude or Codex sessions. A missing transcript, an
ambiguous prefix and an unknown id each fail with their own message.

Tests: `zsh -f zshlang/tests/paseo-transcript.zsh`.
