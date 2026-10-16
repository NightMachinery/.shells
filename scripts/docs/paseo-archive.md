# Archive Paseo agents

`paseo-archive-fz` opens a multi-select picker of non-archived agents. Each
row shows short ID, status, age, provider/model, title and cwd. Running agents
say `RUNNING!`; archiving one requires the explicit `--force` option, which
interrupts it. The caller and its known parent say `PROTECTED` and cannot be
archived. The picker uses the shared `fzf-gateway` helper.

`paseo-archive-all --dry-run` lists eligible agents. Without `--dry-run`,
`paseo-archive-all` shows the list and asks for confirmation when stdin is a
terminal. Non-terminal invocations require `--yes` (`-y`). Only known inactive
statuses are eligible; running and unknown states are skipped. The caller
(`PASEO_AGENT_ID`) and its parent, discovered from local Paseo parent labels or
`PASEO_PARENT_AGENT_ID`, are always excluded when known.

Both commands default to agents in every directory. `--local-dir` (`-l`)
limits them to agents started in the current directory, with explicit cwd
filtering so an agent-context listing cannot include other directories.
`--global` (`-g`) is still accepted and is the default. `--dry-run` (`-n`) works in both
commands. `paseo-archive-fz --filter TEXT` uses fzf's non-interactive selection;
combine it with `--dry-run` when exploring real agents. Full UUIDs are retained
as selection keys even though the picker displays short IDs.

Eligibility is refreshed before archiving, and the CLI's own running-agent
guard remains enabled for archive-all. Each archive command prints its result;
a failure yields a nonzero exit status. Canceling the picker makes no changes.
Parent discovery from local records is unavailable for a remote daemon unless
the caller provides `PASEO_PARENT_AGENT_ID`.

Archive is a soft delete. The inspected CLI has no unarchive command. Check
the Paseo app for recovery support in the installed version; do not treat the
CLI as offering an automated undo.

Validation: `python3 -m unittest discover -s python/tests -p test_paseo_archive.py`.
The integration tests place a fake CLI first in PATH, so their archive requests
cannot reach real agents. Live testing must create its own disposable agents
in a scratch directory and archive only those agents.
