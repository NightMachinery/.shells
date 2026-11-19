# Opening a Paseo agent's transcript

`paseo-view-transcript [--host HOST] [ID]` converts the native transcript of a Paseo agent to
org and opens it in emacs, the way `agent-view-session-transcript` does for a
transcript path. ID is a full agent id or a unique prefix (Paseo's short ids
work), and defaults to `$PASEO_AGENT_ID`, so inside a Paseo agent no argument is
needed.

- `paseo-view-transcript-md [--host HOST] [ID]` opens markdown.
- `paseo-view-transcript-raw [--host HOST] [ID]` opens the native `.jsonl`.
- `paseo-view-transcript-fz [query]`: picks an agent by title, status, provider
  and cwd; enter opens org, alt-m markdown, alt-r the raw transcript.
- `paseo-transcript-path [ID]`: prints the native transcript's path.

In the YPaseo desktop app, Cmd+Shift+O opens the focused agent's transcript.
Progress and failures appear in a Hammerspoon band when launched from the app,
or on stderr in a terminal. Pressing the shortcut again for the same agent
restarts the conversion. [Transcript progress](agent-sessions.md)

## Registered hosts and SSH fallback

Run `paseo-view-transcript --host example-host <agent-id>` from the shell.
YPaseo supplies the host's label and server ID from its host list. The CLI
downloads the native file over the local daemon's connection to the registered host
and the viewer renders it locally. The progress display shows download and render
progress. Restarting the same request cancels the previous download or render.

If the CLI or either daemon is too old for this download, the viewer prints a
notice and uses SSH. The default SSH destination is the lowercased label. For a
custom SSH alias, add a line to `~/.config/ypaseo/transcript-hosts`. Each line ends
with the SSH alias.
Labels with spaces need a mapping. The key can be a label or a server ID, and
`#` starts a comment. A matching server ID takes precedence when YPaseo supplies one.

```text
# Paseo label or server ID   SSH alias
Example Workstation          example-host
srv_example                  example-host
```

Use `--host` with the local Paseo host name or server ID to read from disk.
The SSH fallback requires the renderer on both machines. SSH
connects in batch mode. The remote host resolves and converts the transcript,
then streams it into a local temporary file for Emacs. The viewer shows a
connection message and then the remote renderer's progress. Restarting a
request closes the first SSH channel and the remote host stops that conversion.

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
