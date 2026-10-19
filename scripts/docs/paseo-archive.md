# Archive Paseo agents

`paseo-archive-fz` and `paseo-archive-all` default to **inactive agents last active
at least 72 hours ago**, across every directory. Creation age is deliberately
omitted: an old coordinator can still be doing active work. LAST ACTIVE uses
stored `lastActivityAt`, falling back to `updatedAt`, then `createdAt`, and
rows sort oldest activity first. Unknown activity timestamps are excluded by
the default age gate. Idle and terminal agents (`closed`, `error`, `errored`,
`failed`, `stopped`) are eligible both as top-level rows and cascade members,
subject to the same age, pin and ancestry checks. The CLI is queried with
`--all` to include closed agents; stored `archivedAt` values exclude agents
already archived, even when their displayed CLI status is also `closed`.

`--min-idle DUR` changes the interval: `72h`, `3d` and `90m` are accepted.
`--min-idle 0` or `--all-ages` disables the age gate, but keeps the
non-running guard, pins, caller and cascade protection. The picker alone accepts `--force`, which
overrides age and allows a selected agent that is itself running. A parent
with a running descendant is never eligible, including with force. Running
rows say `RUNNING!`. Archive-all never offers a running agent and never passes
force to the CLI. Unknown lifecycle states are excluded even with force.

## Parents and cascade protection

Paseo archives subagents when their parent is archived. An idle parent can be
waiting for a recently active or running child. The planner checks the complete
non-archived descendant tree, including descendants in other directories,
before offering a parent. If any descendant is ineligible, the parent is hidden.
`--force` can override a descendant's age guard in the picker, but never a
running descendant, pin, caller protection or directory restriction.

The picker shows `archives N subagents with it: ...`. Its wrapped preview
occupies the lower 60% of the picker: cascade context comes first, followed by
the shared rich native-session preview (conversation metadata, recent prompt
and reply). Native identity is joined by full Paseo UUID, including subagents;
preview fields do not change the archive target or eligibility checks.
The shared `agent_session` previewer handles missing or unreadable transcripts
with `No transcript available.`; pickers do not duplicate that fallback.
Printed selections and confirmations list that cascade under the parent.
Counts include all descendants, not just immediate children.
If parent and child are both selected, only the parent's archive request is
sent; Paseo handles the cascade. Eligibility is refreshed before each request;
a changed cascade requires a fresh listing instead of silently expanding the
confirmed scope. The daemon does not provide an atomic descendant-eligibility
check, so a lifecycle change after the last check remains a small race.

The caller (`PASEO_AGENT_ID`), all its ancestors found by walking parent
labels, and an explicitly supplied `PASEO_PARENT_AGENT_ID` and that parent's
ancestors are protected. This protection also applies when the picker is
forced. From a plain terminal, which has no caller ID, pins and descendant eligibility still apply.

`--show-hidden` prints excluded agents with a REASON column before the picker
(or alongside dry-run output). They never enter the selectable fzf input.
Reasons include recent activity, running, pinned, caller/ancestor, running
subagent and recent subagent. Other reasons spell out the condition, such as
`subagent abc1234 not in live listing`, `has unknown status`, `outside this
directory`, or `has unknown activity`. Footer categories use complete phrases
such as `parents of subagents outside this directory`. Short IDs match the CLI
(seven characters when it supplies no explicit short ID). A footer counts
hidden agents by reason, and the picker header repeats that count. Without this option hidden agents are omitted
entirely. A coordinator idle between turns is still busy when its workers run.

## Pin an agent

`paseo-keep ID` sets `keep=true`. Pinned agents are never offered, even with
`--force`; their ancestors cannot be archived through a cascade either.
`paseo-unkeep ID` clears the pin by setting `keep=false`. The installed CLI can
set labels using `paseo agent update --label`, but has no label-delete option,
so unpinning leaves the false label rather than removing the key. Both helpers
change only the requested label through the CLI.

## Scope and confirmation

Both commands default to every directory. `--local-dir` (`-l`) restricts them
to the current directory; `--global` (`-g`) explicitly selects the default.
A local parent with descendants outside that directory is hidden, even with
force. Global metadata is still collected to check those relationships.

`--dry-run` (`-n`) never archives. The picker uses the shared `fzf-gateway`
with multi-selection and stable full UUID keys, while showing short IDs,
status, LAST ACTIVE, provider/model, title and cwd. `--filter TEXT` selects
without an interactive picker; use it with `--dry-run` when inspecting real
agents. Canceling the picker makes no changes.

Archive-all prints its list and asks for confirmation when stdin is a terminal.
Non-terminal invocations require `--yes` (`-y`). Each CLI archive prints its
result, and a failure returns nonzero.

These safety checks require the local daemon records under the selected
Paseo home. Missing/inconsistent records or an unsupported remote daemon target
block planning. The planner reads only activity, labels and display metadata;
it does not print persistence data or credentials. A genuinely missing child
in the complete CLI listing still blocks its parent. Closed children are
normal cascade members: recent or pinned closed children still block.

Archive is soft deletion. The inspected CLI has no unarchive command. Check
the Paseo app for recovery support in the installed version.

Validation: `python3 -m unittest discover -s python/tests -p test_paseo_archive.py`.
Tests isolate both the fake CLI and fake records, so archive requests cannot
reach real agents. Any live archive test must use only disposable agents
created by the tester in a scratch directory.
