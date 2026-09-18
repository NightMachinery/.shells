# gcp-vm-status / gcp-storage-status / gcp-gpu-ssh-sync

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

## Where the fleet-status logic lives

The three functions above are thin zsh wrappers; the actual inventory,
ssh-probing and formatting logic is Python
(`python/gcp/gcp_status.py`), invoked once per call and reused across all
three subcommands (`vm`, `storage`, `ssh-sync`) so the parsing code is
written once.
