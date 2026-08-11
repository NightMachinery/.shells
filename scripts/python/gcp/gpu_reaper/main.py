"""Server-side anomaly reaper for labelled GPU instances.

Deployed by `gcp-gpu-reaper-deploy` (zshlang/auto-load/others/google/gcloud.zsh)
and driven by Cloud Scheduler. It exists because the two mechanisms that
normally stop a GPU box -- GCE's ``--max-run-duration`` and the on-VM idle
systemd timer -- both have failure modes that leave the machine running and
billing, and neither the VM nor a sleeping laptop can notice.

It is NOT a second idle policy. It stops an instance only when a safety net is
demonstrably broken:

  A. uptime exceeded ``--max-run-duration`` plus grace, so GCE did not act;
  B. the on-VM idle timer stopped publishing its heartbeat *and* CPU has been
     flat for the whole idle window, so the timer is dead and nothing is
     running.

A live heartbeat always wins. If the timer is reporting, it has direct sight of
``nvidia-smi`` and of attached tmux clients, which this has no access to -- so a
quiet box with a fresh heartbeat is someone sitting at a prompt, and killing it
would be exactly the false positive that teaches you to distrust the tooling.

Never disables billing. Never touches an instance it does not own: every query
is filtered on ``labels.owner``. See the module-level constant below.
"""

import os
import time
from datetime import datetime, timedelta, timezone

import googleapiclient.discovery

PROJECT = os.environ["GCP_GPU_PROJECT"]
OWNER = os.environ["GCP_GPU_OWNER"]
#: Minutes. Kept in sync with the zsh side by the deploy function, which reads
#: its own defaults and passes them as env vars.
MAX_RUN_MIN = int(os.environ.get("GCP_GPU_MAX_RUN_MIN", "480"))
GRACE_MIN = int(os.environ.get("GCP_GPU_GRACE_MIN", "30"))
IDLE_MIN = int(os.environ.get("GCP_GPU_IDLE_MIN", "60"))
CPU_PCT = float(os.environ.get("GCP_GPU_CPU_PCT", "3"))
HEARTBEAT_MAX_MIN = int(os.environ.get("GCP_GPU_HEARTBEAT_MAX_MIN", "10"))
#: Set to anything falsy to get a reporting-only run.
ENABLE_KILL = os.environ.get("GCP_GPU_ENABLE_KILL", "1") not in ("0", "", "false")


def _compute():
    return googleapiclient.discovery.build("compute", "v1", cache_discovery=False)


def _monitoring():
    return googleapiclient.discovery.build("monitoring", "v3", cache_discovery=False)


def _running_instances(compute):
    """Every RUNNING instance labelled as ours, across all zones."""
    out = []
    req = compute.instances().aggregatedList(
        project=PROJECT, filter=f'labels.owner={OWNER} AND status=RUNNING'
    )
    while req is not None:
        resp = req.execute()
        for _scope, block in (resp.get("items") or {}).items():
            for inst in block.get("instances") or []:
                out.append(inst)
        req = compute.instances().aggregatedList_next(req, resp)
    return out


def _heartbeat_age_s(compute, zone, name):
    """Seconds since the on-VM idle checker last reported, or None."""
    try:
        resp = (
            compute.instances()
            .getGuestAttributes(
                project=PROJECT, zone=zone, instance=name, queryPath="gcp-gpu/status"
            )
            .execute()
        )
    except Exception:
        return None

    value = ""
    for item in resp.get("queryValue", {}).get("items") or []:
        if item.get("key") == "status":
            value = item.get("value", "")
    if not value:
        value = resp.get("value", "") or ""

    for field in value.split():
        if field.startswith("ts="):
            try:
                return int(time.time()) - int(field[3:])
            except ValueError:
                return None
    return None


def _cpu_peak_pct(monitoring, instance_id, minutes):
    """Peak of 5-minute mean CPU utilisation over the window, as a percentage.

    Peak rather than mean: a single busy sample anywhere in the window has to be
    enough to call the machine "not idle".
    """
    end = datetime.now(timezone.utc)
    start = end - timedelta(minutes=minutes)
    try:
        resp = (
            monitoring.projects()
            .timeSeries()
            .list(
                name=f"projects/{PROJECT}",
                filter=(
                    'metric.type="compute.googleapis.com/instance/cpu/utilization" '
                    f'AND resource.labels.instance_id="{instance_id}"'
                ),
                interval_startTime=start.isoformat().replace("+00:00", "Z"),
                interval_endTime=end.isoformat().replace("+00:00", "Z"),
                aggregation_alignmentPeriod="300s",
                aggregation_perSeriesAligner="ALIGN_MEAN",
            )
            .execute()
        )
    except Exception as exc:
        print(f"monitoring query failed: {exc}")
        return None

    points = [
        p["value"]["doubleValue"]
        for series in resp.get("timeSeries") or []
        for p in series.get("points") or []
        if "doubleValue" in p.get("value", {})
    ]
    if not points:
        return None
    return max(points) * 100.0


def _uptime_s(inst):
    started = inst.get("lastStartTimestamp") or inst.get("creationTimestamp")
    if not started:
        return 0
    try:
        dt = datetime.fromisoformat(started.replace("Z", "+00:00"))
    except ValueError:
        return 0
    return int((datetime.now(timezone.utc) - dt).total_seconds())


def _verdict(compute, monitoring, inst):
    """Return ``(code, explanation)``. Mirrors `h-gcp-gpu-verdict` in zsh."""
    name = inst["name"]
    zone = inst["zone"].rsplit("/", 1)[-1]
    uptime = _uptime_s(inst)

    deadline_s = (MAX_RUN_MIN + GRACE_MIN) * 60
    if uptime > deadline_s:
        return (
            "ANOMALY",
            f"max-run-duration ({MAX_RUN_MIN}m) did not fire; up {uptime // 60}m",
        )

    hb = _heartbeat_age_s(compute, zone, name)
    if hb is not None and hb < HEARTBEAT_MAX_MIN * 60:
        return "OK", f"idle timer alive (heartbeat {hb}s ago); deferring to it"

    if uptime < IDLE_MIN * 60:
        return "OK", f"up only {uptime // 60}m; shorter than the {IDLE_MIN}m window"

    cpu = _cpu_peak_pct(monitoring, inst["id"], IDLE_MIN)
    if cpu is None:
        return "UNKNOWN", "no heartbeat and no CPU samples; not judging"

    if cpu < CPU_PCT:
        return (
            "ANOMALY",
            f"idle timer silent and CPU peaked at {cpu:.1f}% over {IDLE_MIN}m",
        )
    return "OK", f"no heartbeat, but CPU peaked at {cpu:.1f}%; something is running"


def reap(request):  # noqa: ARG001 -- HTTP entry point, request is unused
    compute = _compute()
    monitoring = _monitoring()

    lines = []
    stopped = 0
    for inst in _running_instances(compute):
        name = inst["name"]
        zone = inst["zone"].rsplit("/", 1)[-1]
        code, why = _verdict(compute, monitoring, inst)
        lines.append(f"{name} ({zone}): {code} -- {why}")

        if code != "ANOMALY":
            continue
        if not ENABLE_KILL:
            lines.append(f"{name}: would stop, but GCP_GPU_ENABLE_KILL is off")
            continue

        try:
            compute.instances().stop(project=PROJECT, zone=zone, instance=name).execute()
            stopped += 1
            lines.append(f"{name}: STOPPED")
        except Exception as exc:
            lines.append(f"{name}: stop failed: {exc}")

    if not lines:
        lines.append(f"no running instances labelled owner={OWNER}")

    body = "\n".join(lines)
    print(body)
    return (f"{body}\nstopped={stopped}\n", 200, {"Content-Type": "text/plain"})
