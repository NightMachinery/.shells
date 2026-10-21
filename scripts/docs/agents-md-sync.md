# Instruction sync reporting

[agfi:agents-md-sync] reports each generated file it writes or removes by
default. Unchanged targets produce no output and keep their modification times.
Empty overlays report removal only when a target actually existed.

Set `agents_md_sync_verbose_p=n` to suppress change reports explicitly. Error
diagnostics are still shown. Reports use the existing diagnostic stream rather
than stdout, keeping launcher output separate from generated content.

Run `python3 zshlang/tests/agents-md-sync.py` to check creation, updates,
idempotence, overlay removal, symlink replacement, and the quiet override. All
sources and generated targets in these checks live in temporary directories.
The checks do not read or rewrite installed instruction files.

The shared source `PE/Agents/AGENTS.md` routes asynchronous launches to
`event-driven-agents`, resumable substantial runs to `long-run-handoff`, and
sustained agent work or worker bursts to `quota-management`. These are
conditional triggers; detailed procedures stay in the skills. The explicit
avoid-busy-waiting rule remains in Token Efficiency and refers to
`event-driven-agents` as a second reminder.
