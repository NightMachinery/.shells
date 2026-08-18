# Running experiments on a GCP H100 — agent runbook

You have one NVIDIA H100 80GB available on Google Cloud, project
`relation-neuron-detection`. This file is everything you need to get one, use
it, and give it back. Read all of it before creating anything: the failure
modes here cost money rather than time.

Verified end to end on 2026-08-14 by creating a real VM. Human-facing detail
lives in `~/notes/private/research/Hinrich Schutze/GCloud/GPU/` (`report.org`
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
gcp-gpu-sync                   # push /mnt/data/runs/ -> gs://rnd-results-evar
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
- `gs://rnd-results-evar` (europe-west9) — the only durable tier. Survives
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
h-gcp-gpu-gcloud storage buckets add-iam-policy-binding gs://rnd-results-evar \
  --member=serviceAccount:gpu-runner@relation-neuron-detection.iam.gserviceaccount.com \
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
> `gcloud compute instances delete rnd-gpu --zone=europe-west9-c --quiet`.

**Leave nothing running.** Finish with `gcp-gpu-status` (or `gcp-gpu-idle`) and
confirm it reports nothing running before you report the task complete.

---

## 6. What the machine already has

Image family `rnd-night` (`rnd-night-v1`), so none of this needs installing:

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

## 8. Reporting back

When you finish, state plainly:

- which provisioning model you used and why
- wall-clock GPU time and the approximate euro cost (`gcp-gpu-status` prints
  burn and month-to-date)
- where the results landed in `gs://rnd-results-evar`
- whether anything is still running (it should not be)
- any preemption you hit, and whether the job resumed correctly

If a run is interrupted and cannot resume from a checkpoint, say so rather than
silently restarting it — a repeated 5-day flex-start run is a EUR 500 mistake,
not a retry.
