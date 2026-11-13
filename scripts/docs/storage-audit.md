# Storage audits

Use `python3 ~/scripts/python/storage_audit.py --children ~` to measure each
immediate home subdirectory independently. Each completed job prints one JSON
line, including allocated bytes, errors and whether the scan completed.
The helper reads directory metadata through `/usr/bin/du`; it never removes,
moves or opens file contents. It needs no extra packages.

For a smaller scan:

```sh
python3 ~/scripts/python/storage_audit.py --timeout 120 --workers 4 --depth 2 \
  ~/code ~/Library/Caches > /tmp/storage-audit.jsonl
```

Keep reports outside public repositories: directory names can be private.
Start with `df -h` to check both the source and the intended destination.

## Interpreting measurements

- Values are allocated bytes reported by `du -k`, not apparent file sizes.
  APFS clones, snapshots, sparse files and hard links mean deleting a directory
  might recover less than its reported allocation. Use `df` after any cleanup.
- Parent and child rows overlap. Separate jobs can count shared hard links
  more than once. Do not add these rows together as an exact reclaim estimate.
- Scans stay on the starting filesystem and do not follow encountered symlinks.
  An explicitly supplied root symlink is rejected. `--children` omits loose files
  and symlinked directories, and measures child directories separately, including
  any mount supplied as a child root.
- A timeout or permission error is incomplete evidence, never an empty directory.
  The helper exits 1 if any job is incomplete and retains complete partial rows.
- Four workers limit I/O pressure. Reduce `--workers` during heavy builds.
  Scan results are a changing view of a live filesystem.

## Coordinate before cleanup

List agents globally, including background workspaces (`paseo ls --global
--background --json`). The connected `list_agents` tool defaults to the caller's
directory; use a common parent directory to find other workspaces. Match paths
to coordinator labels and parent-agent IDs, then message each coordinator only
about their project. An idle agent does not establish that its jobs have stopped.
Check running processes and open files as additional evidence.

Prefer package-manager cleanup for rebuildable caches. Deleting them saves space
but costs downloads or recompilation. Browser installations, emulator data,
active build directories and package environments need their owner's review.
Do not delete whole agent state directories: they contain conversations,
workspace registrations and potentially unfinished work.

For relocation, first agree on a quiet window with the owner. Shut down VMs and
detach disk images before copying their bundles. Copy to a unique destination,
preserve required metadata, verify file contents and metadata, then coordinate
the final cutover. Retain the source until verification succeeds. A compatibility
symlink may preserve paths, but requires the destination volume to stay mounted;
Git worktree registrations and application references may need explicit repair.
Keep repositories synchronized through their Git remotes, not file copying
between machines.
