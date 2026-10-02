# Compact Claude history before resuming

[agfi:claude-code-session-resume] accepts
`claude_code_session_resume_compact_p=y`. The compact variants set this knob
for one call, then use the existing profile-aware resume path.

```zsh
claude-resume-compact <transcript-or-id> [to-profile] [claude args...]
claude-resume-compact-fz [to-profile] [claude args...]
claude-resume-compact-all-fz [to-profile] [claude args...]

claude-resume-compact-default <transcript-or-id> [claude args...]
claude-resume-compact-work <transcript-or-id> [claude args...]
claude-resume-compact-default-fz [claude args...]
claude-resume-compact-work-all-fz [claude args...]

@opts compact_p y compact_retries 3 @ claude-resume <transcript-or-id>
```

Both destination variants have `-fz` and `-all-fz` forms. The canonical names
are [agfi:claude-code-session-resume-compact],
[agfi:claude-code-session-resume-compact-fz], and
[agfi:claude-code-session-resume-compact-all-fz]. All these names and the
ordinary resume variants share the `claude_code_session_resume` `@opts`
prefix. With no destination argument, the selected transcript's profile wins.
Pass an empty destination (`''`) when adding flags while keeping that profile.

The picker defaults to the current project across profiles; `-all-fz` includes
all projects. Cancelling it performs no import, compaction, or launch.

## Flow

1. Resolve the source transcript, destination profile, and registered launcher.
   Reject conflicting resume/print modes and require an exact resolved UUID.
2. Refuse a live source, even if an ordinary resume or import force knob is set.
   The check uses a fresh live listing rather than a picker's cached rows.
3. For another profile, use [agfi:claude-code-session-import] to create the
   destination copy with its new ID. Existing import consent and source-removal
   settings still apply. Within the original profile, retain the same ID.
4. Re-check that the destination is idle, then run the destination launcher in
   the transcript's recorded working directory with this preparation command:

   ```text
   <launcher> --resume <destination-id> [preparation options] \
       --print --output-format stream-json --verbose --tools '' -- /compact
   ```

5. Require a successful process exit and validate its JSONL stream with
   `agent_session claude compact-result <capture> <destination-id>`. The
   validator requires a successful result for that exact session plus a
   compaction boundary, or the recognized insufficient-history no-op. A quota
   error, mismatched ID, malformed result, or unsupported command stops the
   launch.
6. Call [agfi:h-agent-session-resume-run] with the ordinary resume arguments.
   Managed panes retain their existing redirect to saved launch settings, after
   preparation succeeds.

Claude documents native `/compact` through its non-interactive Agent SDK
command interface, including the resulting `compact_boundary` message:
[Compact history with /compact](https://code.claude.com/docs/en/agent-sdk/slash-commands#compact-history-with-compact).
The `--` separator terminates Claude's variadic `--tools` option so `/compact`
remains the prompt. Preparation submits this native command only. Caller task text and tool flags
are retained for the final resume, and do not enter the preparation command.

## Settings and limits

Compaction uses the destination account and launcher. The default profile runs
with `CLAUDE_CONFIG_DIR` unset, which preserves its default credential lookup;
other profiles receive their registered config home. This normalization applies
to preparation and the ordinary final launch in the compact path, even when the
calling shell inherited a different profile. Preparation clears inherited agent
identity and managed-pane hook markers in its own subshell, preventing stale
source IDs or nested-Claude detection from affecting the print process. The
ordinary final resume retains those markers and its existing managed semantics.

Caller-supplied `--model`, `--effort`, `--settings`, `--permission-mode`,
`--setting-sources`, `--system-prompt`, `--append-system-prompt`, `--max-turns`,
and `--max-budget-usd` options also enter preparation. Other launch arguments
stay on the final resume. A managed pane's final resume restores its stored
settings; preparation uses the destination launcher and supplied preparation
options, so pass the intended model/effort explicitly if those differ from the
launcher's defaults.

Session-selection, fork, print, output/input format, nonpersistent session,
remote/teleport, initialization, and slash-command-disabling overrides are
incompatible and rejected before import. The final resume otherwise receives
the caller's original argument list.

`claude_code_session_resume_compact_retries` defaults to `2` and accepts integers
from `0` to `20`. It overrides both `claude_max_retries` and
`CLAUDE_CODE_MAX_RETRIES` for preparation only; the final launch retains the
caller's retry policy. This bounds retries, not the duration of a single network
request. There is no automatic account fallback on quota errors.

The stream is captured in a mode-600 file from `gmktemp`, and the function's
`always` cleanup removes only that file on success or failure. Preparation and
verification diagnostics go to stderr. An unsuccessful cross-profile attempt
can leave the imported copy for inspection; no rollback removes that transcript.
A successful same-profile compaction changes that session's history.

The live guard cannot prevent a separate process starting after the final check.
Stop the source session before invoking the command. A missing working directory
prevents preparation even when `agent_session_resume_cd_p=n`; that knob only
changes the ordinary final resume directory policy.

Subagent-resume wrappers call the same resume function and therefore inherit
`claude_code_session_resume_compact_p`; their existing promotion rules still
apply. No additional subagent alias family is introduced. Ordinary variants with
the compact knob unset retain their existing behavior.

## Offline regression

```zsh
zsh -f zshlang/tests/claude-session-compact.zsh
```

The test stubs both destination launchers, live discovery, profile import, and
the Go verifier. It checks identity, profile, working directory, caller arguments,
finite preparation retries, private capture cleanup, failure/no-op behavior,
managed redirect ordering, picker scope/cancellation, and the ordinary path.
It performs no model calls and opens no real sessions. The Go verifier has its
own JSONL fixture tests under `golang/agent_session`.
