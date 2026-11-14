# Instruction sync reporting

[agfi:agents-md-sync] reports each generated file it writes or removes by
default. Unchanged targets produce no output and keep their modification times.
Empty overlays report removal only when a target actually existed.

Set `agents_md_sync_verbose_p=n` to suppress change reports explicitly. Error
diagnostics are still shown. Reports use the existing diagnostic stream rather
than stdout, keeping launcher output separate from generated content.

A `rules` target (agy's `~/.gemini/config/rules/`) is written as numbered
files, `agents-md-01.md` and up, each reported on its own; a file no longer
needed is reported as `removed extra`. The retired outputs that agy would
still load (`~/.gemini/GEMINI.md`, `~/.gemini/AGENTS.md`) are removed and
reported as `removed retired`, but only while they start with the sync's own
header. See `PE/Agents/readme.org` for why agy's rules are split.

Run `python3 zshlang/tests/agents-md-sync.py` to check creation, updates,
idempotence, overlay removal, symlink replacement, the quiet override, the
rules split (size limit, code fences, shrinking, an unsplittable section) and
retired-file removal. All
sources and generated targets in these checks live in temporary directories.
The checks do not read or rewrite installed instruction files.

The shared source `PE/Agents/AGENTS.md` also records the user's preference for
Codex to assess and carry out authorized work when Claude declines under its
provider-specific restrictions. This preserves project scope and existing
account, data-access and approval boundaries.

The shared source `PE/Agents/AGENTS.md` routes asynchronous launches to
`event-driven-agents`, resumable substantial runs to `long-run-handoff`, and
sustained agent work or worker bursts to `quota-management`. These are
conditional triggers; detailed procedures stay in the skills. The explicit
avoid-busy-waiting rule remains in Token Efficiency and refers to
`event-driven-agents` as a second reminder.
