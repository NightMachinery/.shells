#!/usr/bin/env python3
"""Spend reconstruction for the ``gcp-gpu-*`` deployment: per instance, per disk.

The zsh side (``~/scripts/zshlang/auto-load/others/google/gcloud.zsh``) owns the
user-facing name ``gcp-gpu-spend``, the window flags and every *value* that
identifies a particular deployment.  This file is on the PUBLIC side, so it
learns the project id, the owner label, the price table and the billing dataset
from its arguments and from the environment; it has no default for any of them.

Two sources, and every readout says which one it used:

  ESTIMATE  instance uptime reconstructed from the Admin Activity and System
            Event audit logs, multiplied by the local price table.  Always
            available: those logs are on by default, free, and need no extra
            role.  Models compute and persistent disk and nothing else.

  ACTUAL    the BigQuery billing export, which is billed euros.  Only exists
            once somebody with billing-admin on the billing account enables it;
            ``--actual`` prints exactly how when it is missing.

Why this is not the old one-instance estimate
---------------------------------------------
The previous implementation folded start/stop events for the single configured
instance name, so a *fleet* -- nine flex-start nodes under nine different names
-- was invisible to it.  It reported EUR 9 month-to-date while EUR 300/hour was
burning.  Everything here is keyed on ``(zone, name)`` pairs discovered from the
log, so an instance that has since been deleted still appears, which is what
makes ``--all`` and ``--week`` honest.

Three details the audit log will mislead you about, all of them learned the
hard way from this project's own history:

1. A failed create is logged exactly like a successful one.  The operation's
   LAST entry carries ``status.code`` (8 = RESOURCE_EXHAUSTED, the stockout
   that a spot or flex request hits all day long).  Six consecutive failed
   ``instances.start`` calls on one preempted node would otherwise read as six
   billable runs.

2. Preemption and guest shutdown are NOT in the Admin Activity log.  They are
   ``compute.instances.preempted`` and ``compute.instances.guestTerminate`` in
   the System Event log, and without them a spot VM that died at hour 3 and was
   deleted at hour 20 reads as seventeen hours of H100 that nobody paid for.

3. Flex-start VMs cannot be stopped and resumed: their termination action must
   be DELETE, and they bill for every hour the VM *exists*, not for the hours
   it was RUNNING.  So a flex node whose guest shut itself down at 12:50 and
   that was deleted at 14:26 is charged to 14:26.  For FLEX_START rows this
   file therefore folds create->delete and ignores the intermediate stop-ish
   events; for SPOT and STANDARD rows it folds the running intervals.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import datetime as _dt
import json
import os
import re
import subprocess
import sys

##
#: Audit-log method names, reduced to a verb.  `v1.compute.instances.insert`,
#: `beta.compute.instances.insert` (which is what a flex-start create goes
#: through) and `compute.instances.preempted` all collapse here.
_METHOD_RE = re.compile(r"(?:^|\.)compute\.instances\.([A-Za-z]+)$")
_RESOURCE_RE = re.compile(r"/zones/([^/]+)/instances/(.+)$")

#: Verbs that open a billing interval, and verbs that close one.
UP_METHODS = {"insert", "start"}
DOWN_METHODS = {"stop", "delete", "preempted", "guestTerminate", "terminateOnHostMaintenance"}
#: Deliberately NOT a close: `deferredStop` is logged when GCE *schedules* the
#: --max-run-duration stop, and it has been observed firing hours after the VM
#: was already TERMINATED by a preemption.  Treating it as a close would end an
#: interval early, and under-reporting is the one direction a spend figure must
#: never fail in.  The real stop arrives as `stop`, `guestTerminate` or
#: `preempted`, and for an instance that still exists `lastStopTimestamp` from
#: the API is authoritative anyway.
IGNORED_METHODS = {"deferredStop"}

SECONDS_PER_DISK_MONTH = 730.0 * 3600.0


def eprint(*a):
    print(*a, file=sys.stderr)


def parse_ts(s):
    """RFC3339 -> epoch seconds (float). Honours the offset; `Z` included."""
    if not s:
        return None
    s = s.strip()
    if s.endswith("Z"):
        s = s[:-1] + "+00:00"
    #: Python < 3.11 cannot read more than 6 fractional digits.
    s = re.sub(r"\.(\d{6})\d+", r".\1", s)
    try:
        return _dt.datetime.fromisoformat(s).timestamp()
    except ValueError:
        return None


def basename(url):
    return url.rsplit("/", 1)[-1] if url else ""


def human_hours(sec):
    return sec / 3600.0


def fmt_window(frm, to):
    f = _dt.datetime.fromtimestamp(frm).strftime("%Y-%m-%d %H:%M")
    t = _dt.datetime.fromtimestamp(to).strftime("%Y-%m-%d %H:%M")
    return f"{f} -> {t}"


def run_json(cmd, what):
    """Run a command, parse its stdout as JSON, return [] on any failure."""
    try:
        out = subprocess.run(cmd, capture_output=True, text=True, timeout=900)
    except (OSError, subprocess.TimeoutExpired) as e:
        eprint(f"gcp_spend: {what} failed: {e}")
        return []
    if out.returncode != 0:
        eprint(f"gcp_spend: {what} failed: {out.stderr.strip().splitlines()[-1] if out.stderr.strip() else out.returncode}")
        return []
    try:
        return json.loads(out.stdout or "[]")
    except json.JSONDecodeError:
        eprint(f"gcp_spend: {what} returned unparseable JSON")
        return []


##
class Price:
    """The price table, handed over from zsh so there is only ever one copy."""

    def __init__(self, tables):
        self.ondemand = tables.get("ondemand", {})
        self.spot = tables.get("spot", {})
        self.flexstart = tables.get("flexstart", {})
        self.disk = tables.get("disk", {})
        self.unknown = set()

    def machine(self, machine, model):
        model = (model or "STANDARD").upper()
        price = None
        if model == "SPOT":
            price = self.spot.get(machine)
        elif model == "FLEX_START":
            price = self.flexstart.get(machine)
        if price is None:
            #: On-demand is the highest rate, so an unknown provisioning model
            #: errs towards OVER-reporting, which is the safe direction.
            price = self.ondemand.get(machine)
        if price is None:
            self.unknown.add(machine)
            return 0.0
        return float(price)

    def disk_month(self, dtype, gb):
        return float(self.disk.get(dtype, 0.11)) * float(gb)


##
class Machine:
    """One (zone, name) pair: everything known about it, from every source."""

    __slots__ = ("zone", "name", "machine_type", "model", "labels", "events",
                 "alive", "status", "creation", "last_start", "last_stop",
                 "boot_gb", "boot_type", "deleted_at", "owned")

    def __init__(self, zone, name):
        self.zone = zone
        self.name = name
        self.machine_type = ""
        self.model = ""
        self.labels = {}
        self.events = []          #: (epoch, "up"|"down", verb)
        self.alive = False
        self.status = ""
        self.creation = None
        self.last_start = None
        self.last_stop = None
        self.boot_gb = None
        self.boot_type = None
        self.deleted_at = None
        self.owned = False

    @property
    def key(self):
        return (self.zone, self.name)

    def is_flex(self):
        return (self.model or "").upper() == "FLEX_START"

    def intervals(self, frm, to, now):
        """Billing intervals clipped to [frm, to], as a list of (start, end)."""
        if self.is_flex():
            #: create -> delete, whatever happened in between.  A flex VM
            #: cannot be stopped and resumed, so there is exactly one span of
            #: existence per create, and every hour of it is billed.
            ups = [e for e in self.events if e[2] == "insert"]
            downs = [e for e in self.events if e[2] == "delete"]
            evs = sorted(ups + downs)
            if not downs and not self.alive:
                #: The delete fell outside the log window; the last stop-ish
                #: event is the best end we have.
                tail = [e for e in self.events if e[1] == "down"]
                if tail:
                    evs = sorted(evs + [max(tail)])
        else:
            evs = sorted(e for e in self.events if e[2] not in IGNORED_METHODS)

        out = []
        run = None
        for ts, kind, _verb in evs:
            if kind == "up":
                if run is None:
                    run = ts
            else:
                if run is not None:
                    out.append((run, ts))
                    run = None
        if run is not None:
            out.append((run, min(now, to)))

        clipped = []
        for a, b in out:
            a2, b2 = max(a, frm), min(b, to)
            if b2 > a2:
                clipped.append((a2, b2))
        return clipped


##
def fetch_audit(project, since_epoch, limit):
    """Both audit logs, narrowed to the lifecycle verbs, from `since_epoch`."""
    since = _dt.datetime.fromtimestamp(since_epoch, _dt.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    #: The timestamp bound is not an optimisation, it is the difference between
    #: seconds and minutes: unbounded, the backend walks the whole 400-day
    #: retention window.  The methodName bound cuts the entry count by an order
    #: of magnitude -- setMetadata and setLabels fire on every create.
    flt = (
        '(logName:"cloudaudit.googleapis.com%2Factivity"'
        ' OR logName:"cloudaudit.googleapis.com%2Fsystem_event")'
        ' AND resource.type="gce_instance"'
        ' AND protoPayload.methodName:("instances.insert" OR "instances.delete"'
        ' OR "instances.start" OR "instances.stop" OR "instances.preempted"'
        ' OR "instances.guestTerminate")'
    )

    #: `gcp-log-read` (golang/gcp-log-read) returns the same entries in the
    #: same order, but fetches the window as parallel time slices.  The API
    #: pages sequentially at about five seconds a page, so a month of fleet
    #: events took gcloud 30-45s and takes this about 5s.  It fails as a whole
    #: rather than returning a partial read, so falling back is always safe.
    logread = os.environ.get("GCP_SPEND_LOGREAD", "")
    if logread:
        try:
            out = subprocess.run(
                [logread, f"--project={project}", f"--filter={flt}",
                 f"--since={since}", f"--limit={limit}"],
                capture_output=True, text=True, timeout=900,
            )
            if out.returncode == 0:
                return json.loads(out.stdout or "[]")
            eprint(f"gcp_spend: {logread} failed, falling back to gcloud: "
                   f"{(out.stderr.strip().splitlines() or [out.returncode])[-1]}")
        except (OSError, subprocess.TimeoutExpired, json.JSONDecodeError) as e:
            eprint(f"gcp_spend: {logread} failed, falling back to gcloud: {e}")

    return run_json(
        ["gcloud", f"--project={project}", "logging", "read",
         f'{flt} AND timestamp>="{since}"',
         f"--limit={limit}", "--format=json"],
        "gcloud logging read",
    )


def fetch_instances(project):
    return run_json(
        ["gcloud", f"--project={project}", "compute", "instances", "list", "--format=json"],
        "gcloud compute instances list",
    )


def fetch_disks(project):
    return run_json(
        ["gcloud", f"--project={project}", "compute", "disks", "list", "--format=json"],
        "gcloud compute disks list",
    )


##
def ingest_instances(machines, instances, owner):
    """Seed from the live fleet: authoritative for everything alive right now."""
    for inst in instances:
        zone = basename(inst.get("zone"))
        name = inst.get("name", "")
        m = machines.setdefault((zone, name), Machine(zone, name))
        m.alive = True
        m.status = inst.get("status", "")
        m.machine_type = basename(inst.get("machineType"))
        m.model = (inst.get("scheduling") or {}).get("provisioningModel") or "STANDARD"
        m.labels = inst.get("labels") or {}
        if m.labels.get("owner") == owner:
            m.owned = True
        m.creation = parse_ts(inst.get("creationTimestamp"))
        m.last_start = parse_ts(inst.get("lastStartTimestamp"))
        m.last_stop = parse_ts(inst.get("lastStopTimestamp"))


def ingest_audit(machines, entries, owner):
    """Fold the two audit logs into per-machine up/down events.

    Admin Activity entries come in pairs sharing an `operation.id`: the FIRST
    carries the request body (machine type, provisioning model, labels, boot
    disk) and the LAST carries the status.  System Event entries stand alone.
    """
    ops = {}
    for e in entries:
        pp = e.get("protoPayload") or {}
        mm = _METHOD_RE.search(pp.get("methodName") or "")
        if not mm:
            continue
        verb = mm.group(1)
        rm = _RESOURCE_RE.search(pp.get("resourceName") or "")
        if not rm:
            continue
        zone, name = rm.group(1), rm.group(2)
        ts = parse_ts(e.get("timestamp"))
        if ts is None:
            continue

        if "system_event" in (e.get("logName") or ""):
            #: No operation pairing and no status: a system event is a fact.
            m = machines.setdefault((zone, name), Machine(zone, name))
            if verb not in IGNORED_METHODS:
                m.events.append((ts, "up" if verb in UP_METHODS else "down", verb))
            continue

        op = e.get("operation") or {}
        oid = op.get("id") or e.get("insertId") or f"{zone}/{name}/{verb}/{ts}"
        rec = ops.setdefault(oid, {"verb": verb, "zone": zone, "name": name,
                                   "first": None, "last": None,
                                   "status": None, "request": None})
        if op.get("first"):
            rec["first"] = ts
            rec["request"] = pp.get("request") or {}
        elif rec["first"] is None:
            rec["first"] = ts
        if op.get("last"):
            rec["last"] = ts
            rec["status"] = pp.get("status") or {}
        if rec["first"] is None:
            rec["first"] = ts

    for rec in ops.values():
        status = rec["status"] or {}
        if status.get("code"):
            #: A failed create or start.  Code 8 is RESOURCE_EXHAUSTED, which
            #: is what a stockout looks like, and it is logged identically to a
            #: success right up to this field.
            continue
        zone, name, verb = rec["zone"], rec["name"], rec["verb"]
        m = machines.setdefault((zone, name), Machine(zone, name))

        req = rec["request"] or {}
        if req.get("machineType") and not m.alive:
            m.machine_type = basename(req["machineType"])
        elif req.get("machineType") and not m.machine_type:
            m.machine_type = basename(req["machineType"])
        sched = req.get("scheduling") or {}
        if sched.get("provisioningModel") and not m.alive:
            m.model = sched["provisioningModel"]
        for lab in req.get("labels") or []:
            if lab.get("key") == "owner":
                m.labels.setdefault("owner", lab.get("value"))
                if lab.get("value") == owner:
                    m.owned = True
        for d in req.get("disks") or []:
            if d.get("boot"):
                init = d.get("initializeParams") or {}
                if init.get("diskSizeGb"):
                    m.boot_gb = float(init["diskSizeGb"])
                if init.get("diskType"):
                    m.boot_type = basename(init["diskType"])

        if verb == "delete":
            #: Billing ends when the deletion starts, not when it finishes.
            ts = rec["first"]
            m.deleted_at = ts if m.deleted_at is None else max(m.deleted_at, ts)
        elif verb in ("insert", "start", "stop"):
            #: Completion, not submission.  A flex-start create can sit queued
            #: in Dynamic Workload Scheduler for minutes before the VM exists,
            #: and a stop takes ~90s during which the VM is still billing.
            ts = rec["last"] or rec["first"]
        else:
            ts = rec["first"]

        m.events.append((ts, "up" if verb in UP_METHODS else "down", verb))


def seal_live(machines, now):
    """Close and open intervals from the API's own timestamps.

    This is what covers a machine whose lifecycle events predate the log window:
    `instances list` knows when it last started and last stopped regardless of
    how far back that was.  Duplicate events are harmless -- the fold only opens
    a run when none is open and only closes one when one is.
    """
    for m in machines.values():
        if not m.alive:
            continue
        if m.creation is not None and not any(e[2] == "insert" for e in m.events):
            m.events.append((m.creation, "up", "insert"))
        if m.status == "RUNNING" and m.last_start is not None:
            m.events.append((m.last_start, "up", "start"))
        if m.status != "RUNNING" and m.last_stop is not None:
            m.events.append((m.last_stop, "down", "stop"))


##
def instance_rows(machines, price, frm, to, now):
    rows = []
    for m in sorted(machines.values(), key=lambda x: (x.zone, x.name)):
        if not m.owned:
            continue
        spans = m.intervals(frm, to, now)
        if not spans:
            continue
        sec = sum(b - a for a, b in spans)
        rate = price.machine(m.machine_type, m.model)
        rows.append({
            "name": m.name, "zone": m.zone, "machine": m.machine_type or "?",
            "model": (m.model or "STANDARD"), "hours": human_hours(sec),
            "rate": rate, "eur": rate * sec / 3600.0,
            "alive": m.alive, "status": m.status if m.alive else "DELETED",
        })
    rows.sort(key=lambda r: -r["eur"])
    return rows


def disk_is_ours(d, owner, own_instances, extra_names):
    """A disk is ours when it carries our owner label, when it is attached to
    an instance of ours, when it shares a name with an instance of ours (which
    is what `instances create` does to the boot disk, and those are NOT
    labelled), or when it is named in the deployment config.

    Shared with gcp_status.py, so the spend estimate and the live burn agree
    on which disks they are counting.
    """
    name = d.get("name", "")
    labels = d.get("labels") or {}
    users = [basename(u) for u in (d.get("users") or [])]
    return (labels.get("owner") == owner
            or name in own_instances
            or any(u in own_instances for u in users)
            or name in extra_names)


def disk_rows(machines, disks, price, owner, frm, to, now, extra_names):
    """Disks that bill: the live ones that are ours, plus boot disks now gone.

    Which live disks are ours is [`disk_is_ours`].
    """
    own_instances = {m.name for m in machines.values() if m.owned}
    live_names = set()
    rows = []

    for d in disks:
        name = d.get("name", "")
        zone = basename(d.get("zone"))
        if not disk_is_ours(d, owner, own_instances, extra_names):
            continue
        live_names.add(name)
        gb = float(d.get("sizeGb", 0) or 0)
        dtype = basename(d.get("type"))
        created = parse_ts(d.get("creationTimestamp")) or frm
        sec = min(now, to) - max(frm, created)
        if sec <= 0:
            continue
        per_month = price.disk_month(dtype, gb)
        rows.append({
            "name": name, "zone": zone, "gb": gb, "type": dtype,
            "per_month": per_month, "state": "live",
            "eur": per_month * sec / SECONDS_PER_DISK_MONTH,
        })

    for m in machines.values():
        #: A boot disk is `autoDelete`, so it lived exactly as long as its
        #: instance.  Without this the disk figure silently covers only what
        #: happens to still exist, which makes `--week` and `--all` too cheap.
        if not m.owned or m.name in live_names or m.boot_gb is None:
            continue
        spans = [e[0] for e in m.events if e[2] == "insert"]
        if not spans:
            continue
        start = min(spans)
        end = m.deleted_at if m.deleted_at is not None else min(now, to)
        sec = min(end, to) - max(start, frm)
        if sec <= 0:
            continue
        per_month = price.disk_month(m.boot_type or "pd-balanced", m.boot_gb)
        rows.append({
            "name": m.name, "zone": m.zone, "gb": m.boot_gb,
            "type": m.boot_type or "pd-balanced", "per_month": per_month,
            "state": "gone", "eur": per_month * sec / SECONDS_PER_DISK_MONTH,
        })

    rows.sort(key=lambda r: -r["eur"])
    return rows


##
def render_estimate(args, inst_rows, dsk_rows, price, frm, to):
    compute = sum(r["eur"] for r in inst_rows)
    disk = sum(r["eur"] for r in dsk_rows)
    total = compute + disk

    if args.bare:
        print(f"{total:.4f}")
        return 0

    print(f"GCP spend, owner={args.owner}, {args.window_label}")
    print(f"window {fmt_window(frm, to)}")
    print("")

    #: The widths are duplicated in the subtotal lines on purpose: a subtotal
    #: that drifts out of its column reads as a different number.
    inst_head = 20 + 1 + 16 + 1 + 15 + 1 + 11 + 1 + 11
    disk_head = 20 + 1 + 16 + 1 + 7 + 1 + 19 + 1 + 6

    if inst_rows:
        print(f"{'INSTANCE':<20} {'ZONE':<16} {'SHAPE':<15} {'MODEL':<11} "
              f"{'STATE':<11} {'HOURS':>8} {'EUR/HR':>8} {'EUR':>10}")
        for r in inst_rows:
            print(f"{r['name']:<20} {r['zone']:<16} {r['machine']:<15} {r['model']:<11} "
                  f"{r['status']:<11} {r['hours']:>8.2f} {r['rate']:>8.2f} {r['eur']:>10.2f}")
        print(f"{'-' * inst_head} {'-' * 8} {'-' * 8} {'-' * 10}")
        print(f"{'compute subtotal':<{inst_head}} {'':>8} {'':>8} {compute:>10.2f}")
    else:
        print("no instances of ours ran in this window.")
    print("")

    if dsk_rows:
        print(f"{'DISK':<20} {'ZONE':<16} {'GB':>7} {'TYPE':<19} {'STATE':<6} "
              f"{'EUR/MONTH':>10} {'EUR':>10}")
        for r in dsk_rows:
            print(f"{r['name']:<20} {r['zone']:<16} {r['gb']:>7.0f} {r['type']:<19} "
                  f"{r['state']:<6} {r['per_month']:>10.2f} {r['eur']:>10.2f}")
        print(f"{'-' * disk_head} {'-' * 10} {'-' * 10}")
        print(f"{'disk subtotal':<{disk_head}} {'':>10} {disk:>10.2f}")
        print("  state 'live' is a disk that exists right now and is still billing;")
        print("  'gone' is the auto-deleted boot disk of an instance that is gone, counted")
        print("  for the hours it existed inside the window.")
        print("")

    print(f"TOTAL  EUR {total:.2f}   (compute {compute:.2f} + disk {disk:.2f})")
    print("")
    print("FIGURES ARE ESTIMATES, not billed euros.")
    print("  source  instance uptime reconstructed from the Admin Activity and System")
    print("          Event audit logs, times the local list-price table.")
    print("  counts  Compute Engine machine-hours (vCPU + RAM + GPU + local SSD) and")
    print("          persistent disk. FLEX_START rows bill create->delete, since such a")
    print("          VM cannot be stopped and resumed.")
    print("  misses  network egress, external IP, snapshots, images, GCS, sustained-use")
    print("          and committed-use discounts, promotional credits, and anything not")
    print(f"          labelled owner={args.owner}.")
    print("  ages    the price table is list price at one refresh date; it does not")
    print("          follow price changes or per-zone spot variation.")
    print("  For billed euros: gcp-gpu-spend --actual  (prints how to enable the export)")
    if price.unknown:
        eprint("gcp_spend: no price for " + ", ".join(sorted(price.unknown))
               + " -- counted as 0, so this total is UNDER-reported.")
    return 0


##
BQ_TABLE_DETAILED = "gcp_billing_export_resource_v1_"
BQ_TABLE_STANDARD = "gcp_billing_export_v1_"


def bq_tables(project, dataset):
    rows = run_json(
        ["bq", f"--project_id={project}", "ls", "--format=json", "--max_results=1000",
         f"{project}:{dataset}"],
        "bq ls",
    )
    out = []
    for r in rows or []:
        tid = ((r or {}).get("tableReference") or {}).get("tableId", "")
        if tid.startswith(BQ_TABLE_DETAILED) or tid.startswith(BQ_TABLE_STANDARD):
            out.append(tid)
    #: Detailed first: it is the only one with a per-resource column, which is
    #: the whole point of a per-instance breakdown.
    out.sort(key=lambda t: (not t.startswith(BQ_TABLE_DETAILED), t))
    return out


def bq_query(project, sql):
    return run_json(
        ["bq", f"--project_id={project}", "query", "--use_legacy_sql=false",
         "--format=json", "--quiet", sql],
        "bq query",
    )


def actual_instructions(args):
    """What the user must do, precisely, and why nothing here will do it."""
    if args.bare:
        #: `--bare --actual` is a PROBE: the budget guard asks for billed euros,
        #: falls back to the estimate when it cannot have them, and must not get
        #: a page of console instructions back on stdout where a number goes.
        return 2
    ds = args.billing_dataset or "<dataset>"
    print("ACTUAL spend is unavailable: the BigQuery billing export is not set up.")
    print("")
    print("Everything gcp-gpu-spend prints without --actual is an ESTIMATE built from")
    print("audit logs and a local price table.  Billed euros only ever come from the")
    print("Cloud Billing export, and enabling it is a change to the BILLING ACCOUNT,")
    print("which is shared -- so this tool will not make it for you.")
    print("")
    print("Steps, for whoever holds billing admin:")
    print("")
    print("  1. Console -> Billing -> select the billing account"
          + (f" {args.billing_account}" if args.billing_account else "")
          + " ->")
    print("     'Billing export' -> 'BigQuery export' tab.")
    print("  2. Enable 'Detailed usage cost', not just 'Standard usage cost'.")
    print("     Standard gives service- and SKU-level cost only.  DETAILED adds the")
    print("     resource.name column, which is the per-instance breakdown this command")
    print("     wants; without it --actual can show services but not machines.")
    print(f"  3. Point it at project '{args.project}' and dataset '{ds}'.")
    print("     The dataset must already exist and should sit in the same region as")
    print("     the rest of the project.  Enable the BigQuery Data Transfer API on")
    print("     that project if the console asks.")
    print("  4. Roles needed to perform steps 1-3:")
    print("       roles/billing.admin   on the billing account  ('Billing Account Administrator')")
    print(f"       roles/bigquery.user   on the project {args.project}")
    print("     Reading the table afterwards needs only roles/bigquery.dataViewer plus")
    print("     roles/bigquery.jobUser, which is a much smaller ask if the two have to")
    print("     be requested separately.")
    print(f"  5. Record the dataset in ~/.night-gcp/config.zsh as:")
    print(f"       typeset -g gcp_gpu_billing_dataset=\"${{gcp_gpu_billing_dataset:-{ds}}}\"")
    print("     Nothing else changes: this command finds the export table by name.")
    print("")
    print("Two things to expect:")
    print("  - THERE IS NO BACKFILL.  The export starts at the moment it is switched")
    print("    on, so everything spent before that stays an estimate forever.  This is")
    print("    the argument for enabling it today rather than when the bill arrives.")
    print("  - It lags.  Most usage lands within a few hours; some SKUs take up to a")
    print("    day, and credits and adjustments land later still.  A zero in a recent")
    print("    window means 'not yet exported', not 'free'.")
    return 2


def render_actual(args, frm, to):
    if not args.billing_dataset:
        return actual_instructions(args)
    tables = bq_tables(args.project, args.billing_dataset)
    if not tables:
        return actual_instructions(args)

    table = tables[0]
    detailed = table.startswith(BQ_TABLE_DETAILED)
    full = f"{args.project}.{args.billing_dataset}.{table}"
    owner_pred = ("EXISTS (SELECT 1 FROM UNNEST(labels) l "
                  f"WHERE l.key = 'owner' AND l.value = '{args.owner}')")
    window = (f"usage_start_time >= TIMESTAMP_SECONDS({int(frm)}) "
              f"AND usage_start_time < TIMESTAMP_SECONDS({int(to)})")
    #: Credits (sustained use, committed use, free tier, promotional credits)
    #: are negative amounts in a repeated field; a total that ignores them is
    #: list price wearing a billed-euros label.
    cost_expr = ("SUM(cost) + SUM(IFNULL((SELECT SUM(c.amount) "
                 "FROM UNNEST(credits) c), 0))")

    if detailed:
        rows = bq_query(args.project, f"""
SELECT COALESCE(resource.name, sku.description) AS item,
       service.description AS service,
       ROUND({cost_expr}, 4) AS eur
FROM `{full}`
WHERE {window} AND {owner_pred}
GROUP BY item, service
HAVING eur <> 0
ORDER BY eur DESC
""")
        head = "RESOURCE"
    else:
        rows = bq_query(args.project, f"""
SELECT service.description AS item, service.description AS service,
       ROUND({cost_expr}, 4) AS eur
FROM `{full}`
WHERE {window} AND {owner_pred}
GROUP BY item, service
HAVING eur <> 0
ORDER BY eur DESC
""")
        head = "SERVICE"

    total = sum(float(r.get("eur") or 0) for r in rows)
    if args.bare:
        print(f"{total:.4f}")
        return 0

    print(f"GCP spend, owner={args.owner}, {args.window_label}")
    print(f"window {fmt_window(frm, to)}")
    print("")
    if rows:
        print(f"{head:<30} {'SERVICE':<26} {'EUR':>12}")
        for r in rows:
            print(f"{str(r.get('item') or '?'):<30} {str(r.get('service') or '?'):<26} "
                  f"{float(r.get('eur') or 0):>12.2f}")
    else:
        print("(nothing exported for this window yet)")
    print("")
    print(f"TOTAL  EUR {total:.2f}")
    print("")
    print("FIGURES ARE ACTUAL: billed euros from the BigQuery billing export,")
    print(f"  table {full}")
    print("  net of credits.  The export lags a few hours, so a small or zero figure")
    print("  for the last few hours means 'not exported yet', not 'free'.")
    if not detailed:
        print("  Only the STANDARD export exists, so this is per service, not per")
        print("  instance.  Enable 'Detailed usage cost' for a per-machine breakdown.")
    return 0


##
def main(argv=None):
    p = argparse.ArgumentParser(
        prog="gcp_spend.py",
        description="Per-instance GCP spend over a window, estimated or actual.")
    p.add_argument("--project", required=True)
    p.add_argument("--owner", required=True)
    p.add_argument("--from-epoch", type=float, required=True)
    p.add_argument("--to-epoch", type=float, required=True)
    p.add_argument("--window-label", default="")
    p.add_argument("--lookback-days", type=float, default=14.0,
                   help="how far before the window to scan the audit log for a run "
                        "that was already in progress when the window opened")
    p.add_argument("--limit", type=int, default=50000,
                   help="max audit log entries to read")
    p.add_argument("--extra-disk", action="append", default=[],
                   help="a disk that is ours but carries no owner label "
                        "(the configured data disk, typically); repeatable")
    p.add_argument("--actual", action="store_true",
                   help="read the BigQuery billing export instead of estimating")
    p.add_argument("--billing-dataset", default=os.environ.get("GCP_SPEND_BILLING_DATASET", ""))
    p.add_argument("--billing-account", default=os.environ.get("GCP_SPEND_BILLING_ACCOUNT", ""))
    p.add_argument("--bare", action="store_true", help="print only the total")
    args = p.parse_args(argv)

    frm, to = args.from_epoch, args.to_epoch
    now = _dt.datetime.now().timestamp()
    to = min(to, now) if to > now else to

    if args.actual:
        return render_actual(args, frm, to)

    try:
        price = Price(json.loads(os.environ.get("GCP_SPEND_PRICES") or "{}"))
    except json.JSONDecodeError:
        eprint("gcp_spend: GCP_SPEND_PRICES is not valid JSON")
        return 1

    #: Three independent read-only calls, so they overlap: the wall time is
    #: the slowest of them rather than their sum.
    with concurrent.futures.ThreadPoolExecutor(max_workers=3) as pool:
        f_inst = pool.submit(fetch_instances, args.project)
        f_audit = pool.submit(fetch_audit, args.project,
                              frm - args.lookback_days * 86400, args.limit)
        f_disks = pool.submit(fetch_disks, args.project)
        instances, audit, disks = f_inst.result(), f_audit.result(), f_disks.result()

    machines = {}
    ingest_instances(machines, instances, args.owner)
    ingest_audit(machines, audit, args.owner)
    seal_live(machines, now)

    rows = instance_rows(machines, price, frm, to, now)
    drows = disk_rows(machines, disks, price, args.owner,
                      frm, to, now, set(args.extra_disk))
    return render_estimate(args, rows, drows, price, frm, to)


if __name__ == "__main__":
    sys.exit(main())
