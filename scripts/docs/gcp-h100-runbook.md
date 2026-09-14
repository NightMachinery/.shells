# Running experiments on a GCP H100 — agent runbook

You have one NVIDIA H100 80GB available on Google Cloud, project
`REDACTED-PROJECT`. This file is everything you need to get one, use
it, and give it back. Read all of it before creating anything: the failure
modes here cost money rather than time.

Verified end to end on 2026-08-14 by creating a real VM. Human-facing detail
lives in `~/notes/private/research/REDACTED-NAME/GCloud/GPU/` (`report.org`
is the standing reference); the tooling is
`~/scripts/zshlang/auto-load/others/google/gcloud.zsh`.

---

## 1. Decide first: spot or flex-start

This is the only decision that really matters, and it is not reversible
mid-run. Pick by **how long the job must survive uninterrupted**, not by price.

| | **spot** | **flex-start** |
|---|---|---|
| command | `gcp-gpu-up` | `gcp-gpu-up --flex-start` |
| cost (Paris) | **EUR 2.33/hr** | **EUR 4.22/hr** |
| 5 days (120h) | ~EUR 279 | ~EUR 506 |
| interrupted? | yes, expect **~1 hour** of life | no, up to **7 days** |
| stop/resume | yes — `gcp-gpu-down` saves money | **no** — only destroy ends the bill |
| best for | evals, debugging, anything < 1h | unattended multi-day fits |

**Use spot** unless the job both (a) runs longer than about an hour and (b)
cannot resume from a checkpoint. Google's own capacity advisor reports
`estimatedUptime: 3600s` for spot H100s in every zone — five days on spot is
roughly 120 attempts at a one-hour run, not one long run.

**Use flex-start** when an interruption would waste real work. It runs
uninterrupted for up to 7 days on the same preemptible quota, and it is the
only way to hold a single H100 for days.

> There is no third option. `a3-highgpu-1g` **cannot be created on demand at
> all** — the type exists only as Spot or Flex-start. Do not pass
> `--on-demand` for an A3 machine; it will always fail. Reserving one is also
> not possible: calendar-mode reservations start at **eight** GPUs (~EUR
> 36/hr).

---

## 2. Before you create: ask where the capacity is

```zsh
gcp-gpu-advice
```

Free, instant, creates nothing. Prints obtainability per region:

```
REGION                 BEST-ZONE                  OBTAIN   EST-UPTIME
europe-west9           europe-west9-c             0.9      3600s
europe-west1           europe-west1-c             0.9      3600s
europe-west3           europe-west3-c             0.5      3600s
```

Read `OBTAIN` as: **0.9** go ahead / **0.5** expect retries / **0.1** pick
another zone. If your default zone is marginal, override for one command:

```zsh
gcp_gpu_zone=europe-west1-c gcp-gpu-up
```

**Always run this before concluding a GPU is unavailable.** A create failing is
not evidence about other zones, and price is not evidence about anything — see
§7.

---

## 3. The normal session

```zsh
gcp-gpu-advice                 # where can I get one?
gcp-gpu-up                     # spot, a3-highgpu-1g, europe-west9-c, 8h cap
gcp-gpu-attach                 # ssh into tmux session `work`
#   ... run the experiment, writing results to /mnt/data/runs/ ...
gcp-gpu-sync                   # push /mnt/data/runs/ -> gs://REDACTED-BUCKET-evar
gcp-gpu-down                   # stop. THIS is what saves money
```

For a multi-day run:

```zsh
gcp-gpu-up --flex-start        # defaults to the 7d maximum
gcp-gpu-up --flex-start --max-run 5d
```

Useful extras:

```zsh
gcp-gpu-status                 # state, burn rate, month-to-date vs cap
gcp-gpu-ssh 'nvidia-smi'       # one-shot command, no tmux
gcp-gpu-idle                   # is it running, and is it doing anything?
gcp-gpu-up --dry-run           # print the exact gcloud call, execute nothing
gcp-gpu-panic                  # stop everything of mine, no confirmation
```

**Always run long jobs inside `tmux`** (`gcp-gpu-attach` puts you there). An
ssh drop must not kill the experiment.

---

## 4. Where data must live

The environment is baked into the image; **the machine itself is disposable.**

- `/mnt/data/runs/` — write all results here, then `gcp-gpu-sync`.
- `gs://REDACTED-BUCKET-evar` (europe-west9) — the only durable tier. Survives
  everything.
- Everything else — `~`, `/tmp`, an edited script in the home directory — dies
  with the VM. By default there is **no persistent data disk**: `/mnt/data` is
  on the boot disk and does not survive deletion.

Rule: **if it is not in GCS, it does not exist.** Sync before stopping, and
periodically during long runs — not just at the end.

**One-time setup per service account:** a freshly created `gpu-runner` SA
has no bucket grant, so `gcp-gpu-sync` fails with a 403
(`storage.objects.get` denied). Fix once, from a machine authenticated as
the bucket owner (verified 2026-08-18):

```zsh
h-gcp-gpu-gcloud storage buckets add-iam-policy-binding gs://REDACTED-BUCKET-evar \
  --member=serviceAccount:gpu-runner@REDACTED-PROJECT.iam.gserviceaccount.com \
  --role=roles/storage.objectAdmin
```

**Checkpoint any job longer than ~30 minutes on spot**, and write checkpoints
to `/mnt/data/runs/` with a periodic sync. A spot preemption is a stop, not a
crash: the disk survives, but in-flight GPU state and running processes do not.

---

## 5. Cost control — read this before creating anything

Four independent guards, none a substitute for stopping the VM yourself:

1. `--max-run-duration` (default **8h**) — GCE terminates the VM. Raise per run
   with `--max-run 24h`.
2. A 30-minute on-VM idle timer.
3. An on-VM absolute deadline.
4. Preemption (spot only).

A soft cap refuses `gcp-gpu-up` when month-to-date spend exceeds it
(EUR 5000/month default; `gcp-gpu-budget` to inspect). Do **not** set
`GCP_GPU_BUDGET_OVERRIDE=1` on your own initiative — if the cap refuses you,
stop and report it to the human.

Numbers worth internalising:

- An H100 left running for a forgotten week: **~EUR 391** on spot.
- The same week stopped: ~EUR 5 of boot disk.
- Flex-start bills for the full window whether or not you use it: **~EUR 506**
  for 5 days.

### Ending the bill

- **Spot:** `gcp-gpu-down` stops it. Disks survive; compute stops billing.
- **Flex-start:** `gcp-gpu-down` **saves nothing** — a flex-start VM cannot be
  stopped and resumed. Only `gcp-gpu-destroy` ends the bill early.

> `gcp-gpu-destroy` **prompts for confirmation**, so it will hang a
> non-interactive agent. Either run it where a human can answer, or use
> `gcloud compute instances delete REDACTED-INSTANCE --zone=europe-west9-c --quiet`.

**Leave nothing running.** Finish with `gcp-gpu-status` (or `gcp-gpu-idle`) and
confirm it reports nothing running before you report the task complete.

---

## 6. What the machine already has

Image family `REDACTED-IMAGE` (`REDACTED-IMAGE-v1`), so none of this needs installing:

- Ubuntu 24.04, NVIDIA driver 580.173.02, CUDA 12.9
- PyTorch 2.9.1+cu129 — `torch.cuda.is_available()` is `True`, device is
  `NVIDIA H100 80GB HBM3` (81559 MiB)
- The full dotfiles bootstrap: zsh + zinit, mise (rg/fd/fzf/jq/bat/uv/gh/...),
  micromamba envs (`base`, `py312`, `tools`, `emacs`), Emacs 30.2 with
  native-comp and Doom
- Interactive zsh starts in ~2.7s; caches are baked into the image

Do **not** spend time installing drivers or CUDA. If `nvidia-smi` fails on a
GPU machine, something is wrong with the machine — report it, do not try to
fix it by installing a driver.

Model sizing on one H100 80GB: a 27B model in bf16 is ~54GB and fits on a
single card. A 27B Jacobian fit peaked near 75GB of 80GB — close to the limit,
so reduce batch size before assuming an OOM is a bug.

---

## 7. Traps that have already cost us

**A priced, catalogued machine type can still be impossible to create.**
Evidence hierarchy, weakest first:

```
Billing Catalog SKU        "it has a price"         proves nothing
accelerator-types list     "the GPU is in a zone"   proves nothing
machine-types list         "the type is in a zone"  proves nothing
Cloud Quotas API           "we are allowed N"       necessary, not sufficient
gcp-gpu-advice             "capacity exists"        best free signal
instances create           the only definitive test
```

`europe-north1-c` (Finland) prices an H100 at EUR 1.02/hr, lists the
accelerator, lists `a3-highgpu-1g`, and holds 64 spot quota — and every create
returns `reason: stockout`. **Do not chase the Finland price.** Likewise
`europe-west2` (London) lists `nvidia-h100-80gb` but its only A3 type is
`a3-edgegpu-8g`.

**`europe-west9` has no G2 and no A2 in any zone.** There is no cheap L4 or
A100 tier in the default zone. Small-model work needs a different zone
(`gcp_gpu_zone=...`); the image is global, so that costs nothing.

**Spot does not queue.** Creation fails immediately with
`ZONE_RESOURCE_POOL_EXHAUSTED`; `gcp-gpu-up` retries in a loop client-side.
That is expected, not an error to escalate — unless it exhausts its retries, at
which point run `gcp-gpu-advice` and try another zone.

**Do not create disks.** Nothing in the tooling deletes a disk, and a disk
bills forever from the moment it exists (~EUR 31/month for 300GB) whether or
not anything runs. `gcp_gpu_data_disk_p=y` exists but you should not need it.

**This is a shared lab project.** Seven people hold `roles/editor` and there is
no central admin. Every function here filters on `owner=evar`. **Never stop,
delete, or modify an instance or disk that is not yours** — there are
pre-existing terminated instances and ~3TB of other people's disks in this
project. Leave them alone.

---

## 7b. More than one GPU: multi-GPU lanes on one node

Verified end to end on 2026-09-14 by running a real two-lane training fleet
(`REDACTED-INSTANCE-8X`, `a3-highgpu-8g`, spot, europe-west9-c).

**One instance is not one GPU.** `gcp-gpu-up --machine a3-highgpu-8g` gives you
eight H100s on one node; `gcp_gpu_instance=<name>` puts it beside `REDACTED-INSTANCE`
instead of replacing it. Every guard still applies — the labels, the
`--max-run-duration`, the idle timer, the spend estimate:

```zsh
gcp_gpu_instance=REDACTED-INSTANCE-8X gcp_gpu_boot_gb=500 \
  gcp-gpu-up --machine a3-highgpu-8g --max-run 24h
gcp_gpu_instance=REDACTED-INSTANCE-8X gcp-gpu-status
```

Two 4-rank jobs then share the node via `CUDA_VISIBLE_DEVICES=0,1,2,3` and
`4,5,6,7`.

### The three ceilings, cheapest to discover first

- **`GPUS-ALL-REGIONS-per-project` is 8 — and it did NOT bind.** Read this as a
  worked example of the evidence hierarchy in §7 rather than as a limit. Seeing
  the value 8, I predicted the fleet could hold at most eight GPUs. Then a
  *second* `a3-highgpu-8g` created without complaint, putting sixteen H100s in
  the project at once. The quota evidently does not apply to preemptible/Spot
  GPUs on this path. A quota *number* is no more proof of a limit than a price
  is proof of availability: `instances create` remains the only definitive test,
  in both directions.
- **`PREEMPTIBLE_NVIDIA_A100_80GB_GPUS` is 0 in every region.** There is no spot
  path to an A100-80GB at all. On-demand quota exists only in `europe-west4` (8)
  and `us-central1` (12, of which 4 were already in use by another member of
  this shared project). So a 4x A100-80GB lane costs ~EUR 20.5/hr on-demand, not
  ~EUR 2.3/hr on spot, and `gcp-gpu-advice` happily reporting 0.9 obtainability
  for `a2-ultragpu-4g` is about capacity, not permission.
- **`PREEMPTIBLE_NVIDIA_H100_GPUS` is 64 per project-region**, in every
  candidate region. The legacy `compute regions describe` view — which is what
  `gcp-gpu-quota` reads — does not list A3-family metrics at all, so H100 quota
  looks *absent* there. Use the Cloud Quotas API instead:

```zsh
gcloud beta quotas info describe PREEMPTIBLE-NVIDIA-H100-GPUS-per-project-region \
  --service=compute.googleapis.com --project=REDACTED-PROJECT --format=json
```

### Two traps specific to the multi-GPU image

**NCCL dies under a login shell.** The image ships
`/etc/profile.d/nccl_env.sh`, which sets `NCCL_NET=gIB` and puts
`/usr/local/gib/lib64` on `LD_LIBRARY_PATH` — the GPUDirect fabric plugin for
multi-node A3-Ultra clusters, which a single-node VM does not have. Anything
launched through `bash -lc` then dies at the first collective with

```
ncclInvalidUsage ... Last error: Failed to initialize any NET plugin
```

while the same command under a plain `bash script` works. Single-node NCCL needs
no fabric plugin: set **`NCCL_NET=Socket`**. This is a nasty one because the
symptom looks like a hardware or driver fault and the smoke test that passed
used a different shell.

**Two `torchrun` jobs on one host collide** on the default rendezvous port
29500. Give each lane its own `--master_port` and `--rdzv-id`.

### Storage: use the local SSD, and treat GCS as the only tier that exists

`a3-highgpu-8g` comes with **16 local NVMe SSDs, 6 TB**, included in the machine
price. The 150-500 GB boot disk is nowhere near enough for real training output.

```zsh
sudo mdadm --create /dev/md0 --level=0 --raid-devices=16 \
  /dev/disk/by-id/google-local-nvme-ssd-* --force
sudo mkfs.ext4 -F -m 0 -E lazy_itable_init=0,lazy_journal_init=0,discard /dev/md0
sudo mount -o discard,defaults,nobarrier /dev/md0 /mnt/scratch
```

**Local SSD is DISCARDED on stop** — which includes preemption *and* the
`--max-run-duration` STOP, since `gcp-gpu-up` passes
`--discard-local-ssds-at-termination-timestamp=true` (GCE refuses
`instance-termination-action=STOP` on a local-SSD machine otherwise). So:

- push every finished unit of work to GCS and **verify it** (`rsync` exit code
  *and* an object count), before starting the next one;
- make the job stop launching new work well before the `--max-run-duration`
  deadline, or the deadline will stop the VM on top of unsynced results. The
  deadline is `lastStartTimestamp + maxRunDuration`, and **`lastStartTimestamp`
  changes after every preemption restart**, so recompute it each time.

### The idle timer will shut down a CPU-only phase

The on-VM timer shuts the box down after 30 minutes with GPU utilisation at 0
and no attached tmux client. A long CPU-only stage — evaluation, probing,
post-processing — looks exactly like that. Raise the threshold rather than
disabling the guard:

```zsh
sudo mkdir -p /etc/systemd/system/gcp-gpu-idle.service.d
printf '[Service]\nEnvironment=IDLE_THRESHOLD_MIN=90\n' \
  | sudo tee /etc/systemd/system/gcp-gpu-idle.service.d/override.conf
sudo systemctl daemon-reload
```

Note that `gcp-gpu-status` prints the *default* threshold, not the effective
one; check with `systemctl show gcp-gpu-idle.service -p Environment`.

### More GPUs is not more VRAM — but it is more headroom

Worth internalising, because it decides machine choice. An H100 80GB is not a
"bigger card" than an A100 80GB: a model that will not fit in 80 GB still will
not fit. What more GPUs buys is a *smaller resident footprint per rank*. A
9B-parameter FSDP job whose checkpoint load died all-gathering 3.79 GiB with
1.38 GiB free on 4 cards loaded fine on 8 cards of the same size, with ~7 GiB to
spare. If you are memory-bound at the margin, add ranks, not newer silicon.

## 8. Reporting back

When you finish, state plainly:

- which provisioning model you used and why
- wall-clock GPU time and the approximate euro cost (`gcp-gpu-status` prints
  burn and month-to-date)
- where the results landed in `gs://REDACTED-BUCKET-evar`
- whether anything is still running (it should not be)
- any preemption you hit, and whether the job resumed correctly

If a run is interrupted and cannot resume from a checkpoint, say so rather than
silently restarting it — a repeated 5-day flex-start run is a EUR 500 mistake,
not a retry.
