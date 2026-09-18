#!/usr/bin/env python3
"""Fleet and storage views for the ``gcp-gpu-*`` deployment, plus the ssh include.

The zsh side (``~/scripts/zshlang/auto-load/others/google/gcloud.zsh``) owns the
user-facing names -- ``gcp-vm-status``, ``gcp-storage-status``,
``gcp-gpu-ssh-sync`` -- and every *value* that identifies a particular
deployment.  This file is on the PUBLIC side, so it learns the project id, the
owner label and the price table from the environment and from its arguments; it
has no defaults for any of them and refuses to run without a project.

Three subcommands:

  vm         every RUNNING instance of ours, anywhere, with CPU/RAM/load/GPU and
             a storage line probed over ssh in parallel; regenerates the include.
  storage    persistent disks, local-SSD scratch arrays and GCS buckets.
  ssh-sync   regenerate the ssh include only.  One gcloud call, no ssh.

The include is GENERATED: it is rewritten in full every time, so an instance
that has been stopped or deleted disappears from it by construction.  Aliases
are stable because they are recorded in the alias table the first time they are
minted; see ``alias_for``.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import datetime as _dt
import hashlib
import json
import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

##
#: Auto-alias vocabulary.  40x40 = 1600 pairs, which is far more than a
#: personal fleet will ever hold, and every word is short, cute and
#: pronounceable out loud -- these names get typed and said, not read.
ADJECTIVES = [
    "amber", "brisk", "calm", "cedar", "clever", "cobalt", "cosy", "crisp",
    "dapper", "dusky", "eager", "fleet", "fuzzy", "gentle", "glad", "hazel",
    "jolly", "keen", "lively", "lucky", "mellow", "merry", "mild", "nimble",
    "noble", "plucky", "quiet", "rapid", "rosy", "sable", "sandy", "silver",
    "sleek", "snowy", "solar", "spry", "sunny", "tidy", "velvet", "witty",
]
NOUNS = [
    "comet", "cricket", "dune", "ember", "falcon", "fern", "fjord", "gecko",
    "harbor", "heron", "isle", "kelp", "lantern", "lemur", "lily", "maple",
    "meadow", "mesa", "moth", "nimbus", "opal", "otter", "pebble", "puffin",
    "quartz", "quill", "raven", "reef", "ripple", "robin", "sparrow",
    "thistle", "tulip", "walrus", "willow", "zephyr", "acorn", "badger",
    "beacon", "bramble",
]

INCLUDE_HEADER = """\
# -*- mode: ssh-config -*-
#
# GENERATED FILE -- DO NOT EDIT.  Every edit here is lost on the next run.
#
# Written by:   gcp-gpu-ssh-sync
# Also run by:  gcp-vm-status, gcp-gpu-up, gcp-gpu-down, gcp-gpu-destroy, gcp-gpu-reap
# Generated:    {when}
#
# One block per RUNNING instance labelled owner={owner}.  The list is rebuilt in
# full, so a stopped or deleted machine disappears from here by itself.
#
# Aliases: pin one in {alias_file} (instance<TAB>alias<TAB>note);
# anything unlisted gets a deterministic auto-alias and is appended there the
# first time it is seen, so it never changes underneath you.
#
# HostKeyAlias is `compute.<numeric instance id>', which is exactly what
# `gcloud compute ssh' uses, so these blocks share its known-hosts file instead
# of starting a second one.  The external IP changes on every restart and the
# instance id does not, so the key keeps verifying across restarts -- and a
# delete-and-recreate DOES get a new id, which is correct: that really is a new
# host.  StrictHostKeyChecking is accept-new rather than gcloud's yes, because
# gcloud seeds the key from instance metadata before it connects and plain ssh
# cannot; with `yes' the first connection to a fresh instance would simply fail.
"""

BLOCK = """\
Host {alias} {name}
    HostName {ip}
    User {user}
    IdentityFile {key}
    IdentitiesOnly yes
    HostKeyAlias compute.{iid}
    UserKnownHostsFile {known_hosts}
    StrictHostKeyChecking accept-new
    CheckHostIP no
    HashKnownHosts no
"""

#: One ssh round trip per instance has to answer every column, so the probe is
#: one script emitting tagged TSV lines rather than a series of commands.
PROBE_VM = r"""
export LC_ALL=C
printf 'nproc\t%s\n' "$(nproc 2>/dev/null)"
lscpu 2>/dev/null | awk -F': +' '
  /^Model name/   && !m { printf "cpumodel\t%s\n", $2; m=1 }
  /^CPU max MHz/  && !x { printf "cpumhz\t%s\n",   $2; x=1 }
  /^CPU MHz/      && !c { printf "cpumhz_cur\t%s\n", $2; c=1 }
'
awk '/^MemTotal:/ { printf "memkb\t%s\n", $2 }' /proc/meminfo 2>/dev/null
printf 'load\t%s\n' "$(cut -d' ' -f1-3 /proc/loadavg 2>/dev/null)"
for m in / /mnt/scratch /mnt/data ; do
    df -P -k "$m" 2>/dev/null | awk -v m="$m" 'NR==2 { printf "df\t%s\t%s\t%s\t%s\n", m, $1, $2, $4 }'
done
if command -v nvidia-smi >/dev/null 2>&1 ; then
    nvidia-smi --query-gpu=index,name,memory.used,memory.total,utilization.gpu \
        --format=csv,noheader,nounits 2>/dev/null \
        | sed 's/^/gpu\t/'
fi
"""

PROBE_STORAGE = r"""
export LC_ALL=C
for l in /dev/disk/by-id/google-* ; do
    [ -e "$l" ] || continue
    printf 'bydev\t%s\t%s\n' "${l##*/google-}" "$(readlink -f "$l")"
done
lsblk -rno NAME,SIZE,MOUNTPOINT 2>/dev/null | sed 's/^/blk\t/'
findmnt -rno TARGET,SOURCE,FSTYPE 2>/dev/null | sed 's/^/mnt\t/'
for m in / /mnt/scratch /mnt/data ; do
    df -P -k "$m" 2>/dev/null | awk -v m="$m" 'NR==2 { printf "df\t%s\t%s\t%s\t%s\n", m, $1, $2, $4 }'
done
"""


##
class Style:
    """24-bit colour, or nothing at all.

    Padding happens before colouring, never after: an escape sequence has no
    width, so a coloured field that was padded first still lines up, and one
    padded afterwards is short by the length of the sequence.
    """

    def __init__(self, enabled: bool) -> None:
        self.enabled = enabled

    def __call__(self, text: str, rgb: tuple[int, int, int] | None) -> str:
        if not self.enabled or rgb is None:
            return text
        r, g, b = rgb
        return f"\x1b[38;2;{r};{g};{b}m{text}\x1b[0m"


GRAY = (150, 150, 150)
GREEN = (120, 190, 120)
YELLOW = (215, 180, 90)
RED = (215, 110, 100)
BLUE = (120, 170, 215)


def color_enabled(mode: str) -> bool:
    if mode == "always":
        return True
    if mode == "never":
        return False
    #: `auto': the same test the zsh side makes -- a tty that has said out loud
    #: that it speaks 24-bit colour.  NO_COLOR wins over everything.
    if os.environ.get("NO_COLOR"):
        return False
    if not sys.stdout.isatty():
        return False
    return os.environ.get("COLORTERM", "") in ("truecolor", "24bit")


##
def die(msg: str) -> "NoReturn":  # type: ignore[valid-type]
    print(f"gcp_status.py: {msg}", file=sys.stderr)
    raise SystemExit(1)


def gcloud(project: str, *args: str, timeout: int = 120) -> str:
    cmd = ["gcloud", f"--project={project}", *args]
    try:
        out = subprocess.run(
            cmd, capture_output=True, text=True, timeout=timeout, check=False
        )
    except subprocess.TimeoutExpired:
        die(f"timed out: {' '.join(cmd)}")
    if out.returncode != 0:
        die(f"failed ({out.returncode}): {' '.join(cmd)}\n{out.stderr.strip()}")
    return out.stdout


def human_bytes(n: float) -> str:
    for unit in ("B", "K", "M", "G", "T", "P"):
        if abs(n) < 1024 or unit == "P":
            return f"{n:.0f}{unit}" if unit in ("B", "K") else f"{n:.1f}{unit}"
        n /= 1024.0
    return f"{n:.1f}P"


def human_dur(seconds: int) -> str:
    d, rem = divmod(max(seconds, 0), 86400)
    h, rem = divmod(rem, 3600)
    m = rem // 60
    return f"{d}d {h:02d}h {m:02d}m" if d else f"{h:02d}h {m:02d}m"


def uptime_seconds(ts: str) -> int | None:
    #: Every GCE timestamp carries the ZONE's offset, so it must be parsed with
    #: it.  Dropping the offset is how an eight-minute-old box once reported a
    #: nine-hour uptime.
    if not ts:
        return None
    try:
        started = _dt.datetime.fromisoformat(ts)
    except ValueError:
        return None
    now = _dt.datetime.now(_dt.timezone.utc)
    return int((now - started).total_seconds())


##
def read_aliases(path: Path) -> tuple[dict[str, str], set[str]]:
    """instance -> alias, plus every alias already taken."""
    table: dict[str, str] = {}
    taken: set[str] = set()
    if not path.exists():
        return table, taken
    for line in path.read_text(encoding="utf-8").splitlines():
        line = line.rstrip("\n")
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        parts = line.split("\t")
        if len(parts) < 2:
            continue
        name, alias = parts[0].strip(), parts[1].strip()
        if not name or not alias:
            continue
        table[name] = alias
        taken.add(alias)
    return table, taken


def mint_alias(name: str, taken: set[str]) -> str:
    """A deterministic adjective-noun for ``name``, avoiding anything taken.

    Deterministic so two machines that have never spoken agree, and so the
    alias survives losing the table.  Salted retries handle the collision case
    -- including a collision with a hand-pinned alias, which is why the table's
    pinned rows are read before anything is minted.
    """
    for attempt in range(64):
        seed = name if attempt == 0 else f"{name}#{attempt}"
        h = int(hashlib.sha256(seed.encode("utf-8")).hexdigest(), 16)
        alias = f"{ADJECTIVES[h % len(ADJECTIVES)]}-{NOUNS[(h // len(ADJECTIVES)) % len(NOUNS)]}"
        if alias not in taken:
            return alias
    return f"vm-{hashlib.sha256(name.encode()).hexdigest()[:8]}"


def alias_for(name: str, table: dict[str, str], taken: set[str], path: Path) -> str:
    """The alias for ``name``, minting and RECORDING one when it is new.

    Recording is what makes an auto-alias permanent: once the pair is in the
    table it is read back, so a later change to the word lists (or to the hash)
    cannot rename a machine somebody has already learned.
    """
    if name in table:
        return table[name]
    alias = mint_alias(name, taken)
    table[name] = alias
    taken.add(alias)
    try:
        path.parent.mkdir(parents=True, exist_ok=True)
        new = not path.exists()
        with path.open("a", encoding="utf-8") as fh:
            if new:
                fh.write(
                    "# instance-name\talias\tnote\n"
                    "# Pinned rows are hand-written; `auto' rows were minted by gcp_status.py\n"
                    "# and are kept so the alias never changes underneath you.\n"
                )
            today = _dt.date.today().isoformat()
            fh.write(f"{name}\t{alias}\tauto {today}\n")
    except OSError as exc:
        print(f"gcp_status.py: could not record alias in {path}: {exc}", file=sys.stderr)
    return alias


##
def instances(project: str, owner: str, running_only: bool = True) -> list[dict]:
    flt = f"labels.owner={owner}"
    if running_only:
        flt += " AND status=RUNNING"
    raw = gcloud(project, "compute", "instances", "list", f"--filter={flt}", "--format=json")
    data = json.loads(raw or "[]")
    return sorted(data, key=lambda i: i["name"])


def external_ip(inst: dict) -> str:
    for ni in inst.get("networkInterfaces", []):
        for ac in ni.get("accessConfigs", []):
            if ac.get("natIP"):
                return ac["natIP"]
    return ""


def zone_of(inst: dict) -> str:
    return inst.get("zone", "").split("/")[-1]


def machine_of(inst: dict) -> str:
    return inst.get("machineType", "").split("/")[-1]


def model_of(inst: dict) -> str:
    return inst.get("scheduling", {}).get("provisioningModel") or "STANDARD"


def bucket_of(inst: dict) -> str:
    """Where this VM's runner syncs, read out of its own startup-script metadata.

    A known place that costs no extra API call, and it is the same string the
    tooling baked in at create time rather than a guess.
    """
    for item in inst.get("metadata", {}).get("items", []):
        if item.get("key") == "startup-script":
            m = re.search(r'GCP_GPU_BUCKET="?([^"\n]+)', item.get("value", ""))
            if m:
                return m.group(1).strip()
    return ""


##
def write_include(
    insts: list[dict],
    include: Path,
    alias_file: Path,
    user: str,
    owner: str,
    key: str,
    known_hosts: str,
    quiet: bool = False,
) -> dict[str, str]:
    table, taken = read_aliases(alias_file)
    aliases: dict[str, str] = {}
    blocks = []
    for inst in insts:
        name = inst["name"]
        ip = external_ip(inst)
        if not ip:
            #: No external address: `ssh <alias>' cannot reach it, and a block
            #: with an empty HostName silently resolves to the alias itself.
            continue
        alias = alias_for(name, table, taken, alias_file)
        aliases[name] = alias
        blocks.append(
            BLOCK.format(
                alias=alias, name=name, ip=ip, user=user, key=key,
                iid=inst["id"], known_hosts=known_hosts,
            )
        )

    body = INCLUDE_HEADER.format(
        when=_dt.datetime.now().astimezone().strftime("%Y-%m-%d %H:%M:%S %Z"),
        owner=owner,
        alias_file=alias_file,
    )
    body += "\n" + ("\n".join(blocks) if blocks else "# (nothing of ours is running)\n")

    include.parent.mkdir(parents=True, exist_ok=True)
    os.chmod(include.parent, 0o700)
    #: Written through a temporary file in the same directory and renamed, so a
    #: reader never sees a half-written config -- an ssh that read one would
    #: fail in a way that looks like a broken host rather than a race.
    fd, tmp = tempfile.mkstemp(dir=str(include.parent), prefix=".gcp.", suffix=".tmp")
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as fh:
            fh.write(body)
        os.chmod(tmp, 0o600)
        os.replace(tmp, include)
    except Exception:
        try:
            os.unlink(tmp)
        except OSError:
            pass
        raise

    if not quiet:
        n = len(blocks)
        print(f"{include}: {n} host{'' if n == 1 else 's'}"
              + (f" -- {', '.join(sorted(aliases.values()))}" if aliases else ""))
    return aliases


##
def ssh_probe(include: Path, alias: str, script: str, timeout: int) -> tuple[str, str]:
    """Run ``script`` on ``alias``.  Returns (stdout, error); error is truthy on failure.

    ``-F`` points at the generated include alone, so the probe proves the
    include is self-sufficient and keeps working even if the Include line in
    ~/.ssh/config was never added.  BatchMode means an instance that wants a
    password fails fast instead of hanging the whole listing.
    """
    cmd = [
        "ssh", "-F", str(include),
        "-o", "BatchMode=yes",
        "-o", f"ConnectTimeout={max(timeout // 2, 3)}",
        "-o", "LogLevel=ERROR",
        alias, "sh", "-s",
    ]
    try:
        out = subprocess.run(
            cmd, input=script, capture_output=True, text=True,
            timeout=timeout, check=False,
        )
    except subprocess.TimeoutExpired:
        return "", "timeout"
    except OSError as exc:
        return "", str(exc)
    if out.returncode != 0:
        return out.stdout, (out.stderr.strip().splitlines() or ["unreachable"])[-1]
    return out.stdout, ""


def parse_tsv(text: str) -> list[list[str]]:
    rows = []
    for line in text.splitlines():
        if not line.strip():
            continue
        rows.append(line.split("\t"))
    return rows


##
def cmd_vm(args: argparse.Namespace) -> int:
    project, owner = args.project, args.owner
    insts = instances(project, owner)
    include = Path(args.include).expanduser()
    alias_file = Path(args.alias_file).expanduser()

    aliases = write_include(
        insts, include, alias_file, args.user, owner,
        args.key, args.known_hosts, quiet=True,
    )

    st = Style(color_enabled(args.color))
    if not insts:
        print(f"no instances labelled owner={owner} are RUNNING anywhere in {project}.")
        print(f"{include}: rewritten, now empty.")
        return 0

    prices = json.loads(os.environ.get("GCP_STATUS_PRICES", "{}"))

    probes: dict[str, tuple[str, str]] = {}
    if not args.no_ssh:
        targets = [(i["name"], aliases.get(i["name"])) for i in insts]
        targets = [(n, a) for n, a in targets if a]
        with concurrent.futures.ThreadPoolExecutor(max_workers=max(len(targets), 1)) as pool:
            futures = {
                pool.submit(ssh_probe, include, a, PROBE_VM, args.timeout): n
                for n, a in targets
            }
            for fut in concurrent.futures.as_completed(futures):
                probes[futures[fut]] = fut.result()

    head = f"{'ALIAS':<15} {'INSTANCE':<16} {'ZONE':<16} {'MACHINE':<15} {'MODEL':<11} {'UPTIME':<14} {'EUR/HR':>7}  SSH"
    print(st(head, GRAY))

    total = 0.0
    for inst in insts:
        name = inst["name"]
        alias = aliases.get(name, "-")
        machine, model = machine_of(inst), model_of(inst)
        rate = price_of(prices, machine, model)
        total += rate
        up = uptime_seconds(inst.get("lastStartTimestamp", ""))
        print(
            f"{st(f'{alias:<15}', BLUE)} {name:<16} {zone_of(inst):<16} {machine:<15} "
            f"{st(f'{model:<11}', YELLOW if model in ('SPOT', 'FLEX_START') else None)} "
            f"{human_dur(up) if up is not None else '-':<14} {rate:>7.2f}  "
            f"{st(f'ssh {alias}', GRAY)}"
        )
        if args.no_ssh:
            continue
        render_vm_detail(st, inst, probes.get(name, ("", "not probed")))

    print()
    print(f"TOTAL {total:.2f} EUR/hr  ({total * 24:.2f} EUR/day if left running)")
    print(st("compute only; disks and buckets bill separately -- gcp-storage-status", GRAY))
    print(st(f"ssh include: {include} ({len(aliases)} host(s))", GRAY))
    return 0


def price_of(prices: dict, machine: str, model: str) -> float:
    key = {"SPOT": "spot", "FLEX_START": "flexstart"}.get(model, "ondemand")
    table = prices.get(key, {})
    val = table.get(machine)
    if val is None:
        #: The same direction the zsh price helper fails in: fall back to the
        #: HIGHEST rate, so an unknown machine over-reports rather than
        #: silently costing zero.
        val = prices.get("ondemand", {}).get(machine)
    try:
        return float(val)
    except (TypeError, ValueError):
        return 0.0


def render_vm_detail(st: Style, inst: dict, probe: tuple[str, str]) -> None:
    out, err = probe
    if err and not out:
        print("  " + st(f"unreachable ({err})", RED))
        return

    nproc = cpumodel = cpumhz = memkb = load = ""
    dfs: dict[str, tuple[str, int, int]] = {}
    gpus: list[list[str]] = []
    for row in parse_tsv(out):
        tag = row[0]
        if tag == "nproc" and len(row) > 1:
            nproc = row[1]
        elif tag == "cpumodel" and len(row) > 1:
            cpumodel = row[1]
        elif tag in ("cpumhz", "cpumhz_cur") and len(row) > 1 and not cpumhz:
            cpumhz = row[1]
        elif tag == "memkb" and len(row) > 1:
            memkb = row[1]
        elif tag == "load" and len(row) > 1:
            load = row[1]
        elif tag == "df" and len(row) > 4:
            try:
                dfs[row[1]] = (row[2], int(row[3]), int(row[4]))
            except ValueError:
                pass
        elif tag == "gpu" and len(row) > 1:
            #: `--format=csv' means commas, and the tag was glued on with a
            #: tab -- so the payload has to be split a second time. Parsing it
            #: as TSV silently produced a one-field row and dropped every GPU.
            gpus.append([f.strip() for f in row[1].split(",")])

    #: No cpufreq on a GCE guest, so `CPU max MHz' is usually absent; the model
    #: name carries the nominal clock and is the only figure available there.
    if not cpumhz and cpumodel:
        m = re.search(r"@\s*([\d.]+)\s*GHz", cpumodel)
        if m:
            cpumhz = str(float(m.group(1)) * 1000)
    clock = f"{float(cpumhz) / 1000:.2f}GHz" if cpumhz else "?GHz"
    ram = human_bytes(int(memkb) * 1024) + "iB" if memkb else "?"

    load_txt = load or "?"
    load_rgb = None
    if load and nproc:
        try:
            ratio = float(load.split()[0]) / max(float(nproc), 1.0)
            load_rgb = RED if ratio > 1.0 else YELLOW if ratio > 0.7 else GREEN
        except ValueError:
            pass
    cpu_line = (
        f"  cpu {nproc or '?':>4} core @ {clock:<8} "
        f"ram {ram:<8} load {st(load_txt.replace(' ', '/'), load_rgb)}"
    )
    if cpumodel:
        cpu_line += st(f"   [{cpumodel}]", GRAY)
    print(cpu_line)

    parts = []
    seen_devices: set[str] = set()
    for mount, label in (("/", "boot"), ("/mnt/scratch", "scratch"), ("/mnt/data", "data")):
        if mount not in dfs:
            continue
        device, size_kb, free_kb = dfs[mount]
        #: /mnt/data is a plain directory on the boot disk unless a data disk
        #: was asked for, and printing the same filesystem twice reads as two
        #: half-empty disks.
        if device in seen_devices:
            continue
        seen_devices.add(device)
        used_frac = 1.0 - (free_kb / size_kb if size_kb else 0.0)
        rgb = RED if used_frac > 0.9 else YELLOW if used_frac > 0.75 else None
        parts.append(
            f"{label} {human_bytes(size_kb * 1024)} "
            f"({st(human_bytes(free_kb * 1024) + ' free', rgb)})"
        )
    if parts:
        print("  disk " + "  ".join(parts))

    if err:
        print("  " + st(f"partial probe: {err}", YELLOW))
    if not gpus:
        print("  " + st("gpu  -- (no nvidia-smi on this machine)", GRAY))
        return
    for g in gpus:
        if len(g) < 5:
            continue
        idx, gname, used, tot, util = (x.strip() for x in g[:5])
        try:
            frac = float(used) / max(float(tot), 1.0)
            vram_rgb = RED if frac > 0.9 else YELLOW if frac > 0.7 else GREEN
        except ValueError:
            vram_rgb = None
        try:
            u = float(util)
            #: Zero is the expensive state, not the healthy one: an H100 at 0%
            #: is a machine that is up and not computing.
            util_rgb = GRAY if u == 0 else GREEN if u > 50 else YELLOW
        except ValueError:
            util_rgb = None
        print(
            f"  gpu{idx:>2}  {gname:<24} "
            f"{st(f'{used:>6}/{tot} MiB', vram_rgb)}  {st(f'{util:>3}%', util_rgb)}"
        )


##
def cmd_ssh_sync(args: argparse.Namespace) -> int:
    insts = instances(args.project, args.owner)
    write_include(
        insts,
        Path(args.include).expanduser(),
        Path(args.alias_file).expanduser(),
        args.user, args.owner, args.key, args.known_hosts,
        quiet=args.quiet,
    )
    return 0


##
def cmd_storage(args: argparse.Namespace) -> int:
    project, owner = args.project, args.owner
    st = Style(color_enabled(args.color))
    prices = json.loads(os.environ.get("GCP_STATUS_PRICES", "{}"))
    disk_prices = prices.get("disk", {})

    raw = gcloud(
        project, "compute", "disks", "list",
        f"--filter=labels.owner={owner}", "--format=json",
    )
    disks = sorted(json.loads(raw or "[]"), key=lambda d: d["name"])

    insts = instances(project, owner)
    include = Path(args.include).expanduser()
    alias_file = Path(args.alias_file).expanduser()
    aliases = write_include(
        insts, include, alias_file, args.user, owner,
        args.key, args.known_hosts, quiet=True,
    )

    #: Mount paths only exist inside a running guest, so this is the one part
    #: of the storage view that needs ssh.  Skipped entirely with --no-ssh.
    probes: dict[str, tuple[str, str]] = {}
    if not args.no_ssh and aliases:
        with concurrent.futures.ThreadPoolExecutor(max_workers=len(aliases)) as pool:
            futures = {
                pool.submit(ssh_probe, include, a, PROBE_STORAGE, args.timeout): n
                for n, a in aliases.items()
            }
            for fut in concurrent.futures.as_completed(futures):
                probes[futures[fut]] = fut.result()

    mounts = {name: parse_mounts(out) for name, (out, _err) in probes.items()}

    print(st("PERSISTENT DISKS", GRAY))
    if not disks:
        print(f"  no disks labelled owner={owner}.")
    else:
        print(st(f"  {'NAME':<18} {'ZONE':<16} {'GB':>6} {'TYPE':<18} "
                 f"{'ATTACHED-TO':<16} {'EUR/MONTH':>9}  MOUNT", GRAY))
        total = 0.0
        for d in disks:
            users = [u.split("/")[-1] for u in d.get("users", [])]
            attached = users[0] if users else "-"
            dtype = d.get("type", "").split("/")[-1]
            gb = int(d.get("sizeGb", 0))
            cost = float(disk_prices.get(dtype, 0.11)) * gb
            total += cost
            mount = ""
            if attached in mounts:
                mount = disk_mount(mounts[attached], d["name"], attached, insts)
                if mount:
                    mount = f"{mount}   ({st('ssh ' + aliases.get(attached, attached), GRAY)})"
            print(f"  {d['name']:<18} {d.get('zone','').split('/')[-1]:<16} {gb:>6} "
                  f"{dtype:<18} {st(f'{attached:<16}', GREEN if users else GRAY)} "
                  f"{cost:>9.2f}  {mount}")
        print(f"  TOTAL {total:.2f} EUR/month -- billed whether or not anything is running.")

    print()
    print(st("LOCAL SSD (scratch; DISCARDED on stop, billed with the instance)", GRAY))
    any_scratch = False
    for name, ms in mounts.items():
        for target, (source, fstype, size_kb, free_kb) in ms.items():
            if target in ("/", "/boot/efi") or not source:
                continue
            if not (source.startswith("/dev/md") or source.startswith("/dev/nvme")):
                continue
            any_scratch = True
            used_frac = 1.0 - (free_kb / size_kb if size_kb else 0.0)
            rgb = RED if used_frac > 0.9 else YELLOW if used_frac > 0.75 else None
            print(f"  {name:<18} {target:<16} {source:<14} {fstype:<8} "
                  f"{human_bytes(size_kb * 1024):>8} "
                  f"({st(human_bytes(free_kb * 1024) + ' free', rgb)})"
                  f"   ({st('ssh ' + aliases.get(name, name), GRAY)})")
    if not any_scratch:
        print(st("  none reported (no running instance with a scratch array, or --no-ssh)", GRAY))

    print()
    print(st("GCS BUCKETS", GRAY))
    render_buckets(st, project, args)

    if insts:
        print()
        print(st("WHERE RUNNING JOBS SYNC (from each instance's own startup-script metadata)", GRAY))
        for inst in insts:
            b = bucket_of(inst)
            if b:
                print(f"  {inst['name']:<18} -> {b}/runs/")
    return 0


def parse_mounts(out: str) -> dict:
    """target -> (source, fstype, size_kb, free_kb), plus '@bydev' and '@blk' maps."""
    res: dict = {"@bydev": {}, "@blk": {}}
    fstypes: dict[str, str] = {}
    for row in parse_tsv(out):
        tag = row[0]
        if tag == "bydev" and len(row) > 2:
            res["@bydev"][row[1]] = row[2]
        elif tag == "blk" and len(row) > 1:
            fields = row[1].split()
            if len(fields) >= 3:
                res["@blk"][fields[0]] = fields[2]
        elif tag == "mnt" and len(row) > 1:
            fields = row[1].split()
            if len(fields) >= 3:
                fstypes[fields[0]] = fields[2]
        elif tag == "df" and len(row) > 4:
            try:
                res[row[1]] = (row[2], "", int(row[3]), int(row[4]))
            except ValueError:
                pass
    for target, val in list(res.items()):
        if target.startswith("@"):
            continue
        source, _ft, size_kb, free_kb = val
        res[target] = (source, fstypes.get(target, ""), size_kb, free_kb)
    return res


def disk_mount(ms: dict, disk_name: str, instance: str, insts: list[dict]) -> str:
    """Where a persistent disk is mounted inside its guest.

    GCE names the guest symlink after the attachment's deviceName, not after
    the disk, so the disk is matched to its attachment first and only then to a
    block device and a mount point.
    """
    device = ""
    for inst in insts:
        if inst["name"] != instance:
            continue
        for d in inst.get("disks", []):
            if d.get("source", "").split("/")[-1] == disk_name:
                device = d.get("deviceName", "")
        break
    if not device:
        return ""
    real = ms.get("@bydev", {}).get(device, "")
    if not real:
        return ""
    base = real.split("/")[-1]
    blk = ms.get("@blk", {})
    #: The disk itself is rarely the mounted thing: its first partition is.
    for cand in sorted(blk, key=len):
        if cand.startswith(base) and blk[cand]:
            return blk[cand]
    return ""


def render_buckets(st: Style, project: str, args: argparse.Namespace) -> None:
    raw = gcloud(project, "storage", "buckets", "list", "--format=json", timeout=60)
    try:
        buckets = json.loads(raw or "[]")
    except json.JSONDecodeError:
        buckets = []
    if not buckets:
        print("  no buckets visible in this project.")
        return
    mine = os.environ.get("GCP_STATUS_BUCKET", "")
    print(st(f"  {'BUCKET':<30} {'LOCATION':<16} {'CLASS':<14} {'SIZE':>10}", GRAY))
    for b in buckets:
        name = b.get("name") or b.get("id", "")
        url = f"gs://{name}"
        size = "-"
        if not args.fast:
            size = bucket_size(url, args.bucket_timeout)
        tag = "  <- ours" if mine and url == mine.rstrip("/") else ""
        print(f"  {st(f'{name:<30}', GREEN if tag else None)} "
              f"{(b.get('location') or b.get('location_type') or '-'):<16} "
              f"{(b.get('default_storage_class') or b.get('defaultStorageClass') or b.get('storage_class') or b.get('storageClass') or '-'):<14} "
              f"{size:>10}{st(tag, GRAY)}")
    if args.fast:
        print(st("  --fast: sizes not measured (a `gcloud storage du' over a big bucket is slow).", GRAY))
    else:
        print(st(f"  sizes from `gcloud storage du --summarize' with a {args.bucket_timeout}s timeout;"
                 " `-' means it did not finish. Object counts need a full listing, so they are not shown.", GRAY))


def bucket_size(url: str, timeout: int) -> str:
    try:
        out = subprocess.run(
            ["gcloud", "storage", "du", "--summarize", "--readable-sizes", url],
            capture_output=True, text=True, timeout=timeout, check=False,
        )
    except (subprocess.TimeoutExpired, OSError):
        return "-"
    if out.returncode != 0 or not out.stdout.strip():
        return "-"
    return out.stdout.split()[0]


##
def main(argv: list[str]) -> int:
    p = argparse.ArgumentParser(prog="gcp_status.py", description=__doc__)
    p.add_argument("--project", default=os.environ.get("GCP_STATUS_PROJECT", ""))
    p.add_argument("--owner", default=os.environ.get("GCP_STATUS_OWNER", ""))
    p.add_argument("--user", default=os.environ.get("GCP_STATUS_SSH_USER", ""))
    p.add_argument("--include", default=os.environ.get(
        "GCP_STATUS_INCLUDE", "~/.ssh/config.d/gcp"))
    p.add_argument("--alias-file", default=os.environ.get("GCP_STATUS_ALIAS_FILE", ""))
    p.add_argument("--key", default=os.environ.get(
        "GCP_STATUS_SSH_KEY", "~/.ssh/google_compute_engine"))
    p.add_argument("--known-hosts", default=os.environ.get(
        "GCP_STATUS_KNOWN_HOSTS", "~/.ssh/google_compute_known_hosts"))
    p.add_argument("--color", choices=("auto", "always", "never"), default="auto")
    p.add_argument("--timeout", type=int, default=12,
                   help="seconds for each parallel ssh probe")

    subs = p.add_subparsers(dest="cmd", required=True)

    v = subs.add_parser("vm", help="every RUNNING instance of ours, with live machine stats")
    v.add_argument("--no-ssh", action="store_true", help="skip the ssh probes")
    v.set_defaults(func=cmd_vm)

    s = subs.add_parser("ssh-sync", help="regenerate the ssh include only")
    s.add_argument("--quiet", action="store_true")
    s.set_defaults(func=cmd_ssh_sync)

    d = subs.add_parser("storage", help="disks, scratch arrays and buckets")
    d.add_argument("--no-ssh", action="store_true")
    d.add_argument("--fast", action="store_true", help="skip bucket sizes")
    d.add_argument("--bucket-timeout", type=int, default=25)
    d.set_defaults(func=cmd_storage)

    args = p.parse_args(argv)
    if not args.project:
        die("no project; the zsh wrappers pass it from ~/.night-gcp/config.zsh")
    if not args.owner:
        die("no owner label")
    if not args.user:
        args.user = args.owner
    if not args.alias_file:
        die("no --alias-file; it belongs with the private config, not in ~/scripts")
    args.key = str(Path(args.key).expanduser())
    args.known_hosts = str(Path(args.known_hosts).expanduser())
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
