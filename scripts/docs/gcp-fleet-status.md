# gcp-vm-status / gcp-storage-status / gcp-gpu-ssh-sync / gcp-gpu-spend

Three read-only reporting commands over our own GCP GPU fleet, in
`zshlang/auto-load/others/google/gcloud.zsh`, backed by
`python/gcp/gcp_status.py`. All three share one gcloud inventory call
(`h-gcp-gpu-status-run`) rather than each shelling out on their own, so
running more than one in a row costs one API round trip, not several.

## gcp-vm-status

`gcp-vm-status [--no-ssh] [--no-color]` (alias `gcp-gpu-fleet`) lists every
RUNNING instance carrying our ownership label, in every zone, GPU or not: a
small control-plane VM shows up with its GPU columns simply empty. Per
machine it prints the ssh alias, instance name, zone, machine type,
provisioning model (spot vs flex-start vs standard), uptime, hourly price
from the in-script price table, then cores and clock, RAM, 1/5/15 load
average, free space on the boot disk and the local-SSD scratch array, and one
line per GPU with model, VRAM used/total and utilisation.

The per-machine figures come from one ssh round trip each, fired in
parallel with a short timeout, so a box that is not answering prints
`unreachable` instead of stalling the whole listing. `--no-ssh` skips that
pass entirely and prints only what the inventory call already knows
(name, zone, machine type, uptime, price), for when the fleet is large and
you only want the roster.

Colour is 24-bit and applied only when stdout is a real terminal; `--no-color`
forces it off (for logs, pipes, or a report meant to render in a fixed-width
font that already renders it correctly, without help).

The ssh include (below) is regenerated first, before the probes, so a
machine created moments earlier already has a working alias by the time this
prints it.

## gcp-storage-status

`gcp-storage-status [--fast] [--no-ssh] [--no-color]` reports what is on
disk and what it costs: persistent disks (with their monthly price and,
where a running instance has one attached, which instance and mount path),
the local-SSD scratch array on every running instance (device, filesystem,
size and free space), and the project's buckets.

`--fast` skips the per-bucket size lookup. `gcloud storage du` walks the
whole bucket to answer that, which on a bucket of any size is slow enough
that it should never happen by accident; `--fast` prints the bucket list
without sizes instead of paying for the walk.

Local-SSD scratch and persistent-disk mounts are read from
`/proc/mounts` and `lsblk` over the same ssh pass `gcp-vm-status` uses, then
matched back to the disk/array they came from. The mount-parsing helper
returns a couple of side-channel keys (grouping information, not mount
entries) alongside the real per-path tuples, and the loop that walks its
result has to skip those rather than unpack them as if they were another
mount — the bug this doc exists partly to record: an early version unpacked
every value as a 4-tuple and crashed on the side-channel keys, which only
`--fast` happened to reach first because it runs before the (slower) bucket
listing that normally masks a partial failure.

## gcp-gpu-ssh-sync

`gcp-gpu-ssh-sync [--quiet]` rewrites the generated ssh include (referenced
in the main ssh config as its first `Include`) from the current instance
list: one block per RUNNING instance we own, and nothing else. It is
idempotent and cheap — one gcloud call, no ssh — which is why it is safe to
run unconditionally at the end of every fleet lifecycle command (create,
stop, destroy, reap) rather than requiring anyone to remember to call it.

Because the file is rebuilt in full each time, a machine that has stopped or
been deleted disappears from it automatically; nothing has to notice and
remove it. Each block answers to two names: the instance's own name, and a
short alias. An alias is pinned by adding a row to the alias table
(`instance<TAB>alias<TAB>note`) before the instance exists; anything unpinned
gets a deterministic alias derived from the instance name and is appended to
the table the first time it is seen, so the same instance always gets the
same alias even across a from-scratch rebuild.

Host keys are pinned with `HostKeyAlias compute.<numeric instance id>`,
reusing `gcloud compute ssh`'s own known-hosts file, so a restart (which
changes the external IP but not the instance id) never triggers a host-key
warning, while a genuine delete-and-recreate (a new id) correctly does.

## gcp-gpu-spend

`gcp-gpu-spend [WINDOW] [--actual] [--bare]` answers "what has this cost?"
over a selectable window, per instance and per disk, and says in every
readout whether the number is an estimate or billed euros. It is backed by
`python/gcp/gcp_spend.py`.

The windows are `--month` (this calendar month, the default), `--last-month`,
`--week` (the last seven days), `--today`, `--all`, and `--since ARG`, where
ARG is either a duration before now (`24h`, `7d`, `1h30m`, or a bare count of
seconds, parsed by `dur2sec`) or a timestamp (`2026-09-01`, or
`'2026-09-01 14:30'`). `--bare` prints the total alone, for arithmetic.

Three sections come out: every instance of ours that was alive for any part
of the window, with its zone, shape, provisioning model, current state, hours
in the window, hourly rate and euros; then the disks, with the monthly rate
and the euros attributable to the window; then the total.

### Why the numbers are honest now

The estimator used to reconstruct running intervals for the single
*configured* instance name. That is fine for one workstation and wrong for a
fleet: a batch of nodes created under generated names was invisible to it,
and it reported single-digit euros month-to-date against an hourly burn two
orders of magnitude larger. The budget guard that `gcp-gpu-up` consults was
reading the same number, so it never fired.

Everything is now keyed on `(zone, name)` pairs discovered from the audit
log, which means an instance that has since been deleted still appears. That
is what makes `--week` and `--all` mean anything: most of a fleet's spend is
on machines that no longer exist by the time you ask.

Three things the audit log will mislead a naive reader about, each of which
cost real money to learn:

- A failed create is logged exactly like a successful one. The operation's
  *last* log entry carries `status.code`, and 8 (RESOURCE_EXHAUSTED) is the
  stockout that a spot or flex-start request hits all day. Entries are paired
  by `operation.id` so a failed create or a retried start is discarded rather
  than counted as a run.
- Preemption and guest-initiated shutdown are not in the Admin Activity log
  at all. They are System Event entries, and without reading that second log
  a preempted VM appears to keep billing until somebody deletes it.
- Flex-start VMs cannot be stopped and resumed, and they bill for every hour
  the VM *exists*, not for the hours it was RUNNING. Those rows are therefore
  folded create-to-delete; spot and standard rows fold running intervals.

Instances that still exist are cross-checked against the API's own
`creationTimestamp`, `lastStartTimestamp` and `lastStopTimestamp`, which
covers anything whose lifecycle events predate the log scan.

Disks are attributed to us by owner label, by being attached to an instance
of ours, or by carrying the name of one (which is what a create does to the
boot disk, and those carry no label at all). A boot disk is auto-deleted with
its instance, so a deleted instance's disk is counted for the hours it
existed rather than dropped: without that, a window of past activity looks
cheaper than it was.

### Estimate versus actual

Without `--actual` the figures are an ESTIMATE: uptime from the audit logs
times the local list-price table. That models Compute Engine machine-hours
and persistent disk and nothing else. It does not see network egress,
external IP, snapshots, images, object storage, sustained-use or
committed-use discounts, or promotional credits, and the price table is list
price at one refresh date rather than a live feed. The footer says all of
this every time, because a number without its provenance gets quoted as if
it were a bill.

`--actual` reads the Cloud Billing export out of BigQuery, which is billed
euros net of credits. When no export exists, it prints exactly what has to be
enabled, by whom, and with which roles, then exits 2. It will not make that
change itself: on a shared project the billing account belongs to everybody,
and the detailed export is a setting other people depend on.

Two properties of that export are worth knowing before asking for it. It does
not backfill, so it only ever covers time after somebody switches it on,
which is the argument for switching it on early rather than when a bill
arrives. And the *detailed* variant is the one to ask for: the standard
export carries service and SKU cost only, while the detailed one adds the
resource name, which is the difference between a per-service figure and a
per-machine one.

### Cost of running it

An estimate over a month is three read-only gcloud calls and takes a few tens
of seconds, dominated by the log read. `--all` scans the full audit-log
retention window and takes minutes, so it is not something to put in a prompt
or a loop; `$gcp_gpu_audit_retention_days` sets how far back that reaches.
The budget preflight uses a memoised variant with a short TTL so that
creating a batch of machines does not pay for the scan once per machine.

## Where the fleet-status logic lives

The three functions above are thin zsh wrappers; the actual inventory,
ssh-probing and formatting logic is Python
(`python/gcp/gcp_status.py`), invoked once per call and reused across all
three subcommands (`vm`, `storage`, `ssh-sync`) so the parsing code is
written once.
