# Process memory tree

`mem-report2 [N]` prints the largest N top-level groups (15 by default),
with physical footprint, compressed bytes, estimated compressor RAM and process
counts. `mem-report` keeps its existing behavior. macOS `top` supplies MEM,
which includes compressed pages; RSS would omit them.

Shells and interpreters are charged to their nearest meaningful ancestor.
The `OPAQUE` list in `python/mem_report2.py` defines these runtimes. Named
scripts remain recognizable owners. Mosh servers collapse into one group with
a session count. Tmux descendants are split by session name when the selected
tmux socket can identify their pane. Other application processes share their
owning app bundle's group.

Claude and Codex each contain session rows and an actual process subtree.
MCP servers, shells, REPLs and browsers stay inside the session total.
Session labels prefer the Paseo agent title, then tmux session, then a
`--name`/`-n` argument, then cwd. Claude rows include account and every
session includes PID and process age. On the inspected Paseo version, Codex
app-server processes carry `PASEO_AGENT_ID` even under the daemon. A process
without a usable marker falls back to other labels; a daemon descendant
without a label says `Paseo (unmapped)` with cwd and age.

Collection uses one `top`, one `ps` including environments, one optional tmux
listing and one optional Paseo listing. Only the small set of ownership markers
is retained; environments and full command lines are never printed. `lsof`
is used only for Claude/Codex roots that need a cwd label. No process is signaled.
The Python engine follows the existing binary humanization convention, and
the shell wrapper reuses `mem-stats_` and its live compression ratio.

Footprints overlap on shared memory, so totals are an upper bound. Compressor
RAM assumes uniform compression and is an estimate. Process exits between
snapshots can appear under an unknown/exited label. Detached/reparented work
cannot always be charged back to its originating session. Optional title
lookups can fail without preventing the memory report.

Validation: `python3 -m unittest discover -s python/tests -p test_mem_report2.py`.
Tests check ancestry accounting, distinct nested sessions, named script owners,
environment handling and the unmapped fallback. A live report should include
all processes rather than imposing the original report's 400-process limit.
