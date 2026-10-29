#!/usr/bin/env python3
"""On-demand tailnet battery dashboard. Remote probes run over existing SSH, without installation."""
from __future__ import annotations

import argparse
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone
import glob
import json
import os
from pathlib import Path
import platform
import plistlib
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
import time
import unicodedata


def now():
    return datetime.now(timezone.utc).isoformat(timespec="seconds")


def percentage(value):
    if isinstance(value, bool):
        return None
    match = re.fullmatch(r"\s*(\d{1,3})\s*%?\s*", str(value))
    return int(match[1]) if match and int(match[1]) <= 100 else None


def address(value):
    value = value.hex() if isinstance(value, bytes) else re.sub(r"[:-]", "", str(value or "")).lower()
    return value if re.fullmatch(r"[0-9a-f]{12}", value) else ""


def reading(name, level=None, *, identity="local:internal", kind="peripheral", part="battery",
            charging="unknown", detail=""):
    return dict(name=str(name), level_percent=percentage(level), identity=identity, kind=kind,
                part=part, charging=charging, detail=detail)


class Reader:
    """One overall probe budget, also bounding each individual subprocess."""
    def __init__(self, budget=12):
        self.deadline = time.monotonic() + budget
        self.warnings = []

    def get(self, label, argv, parse=lambda data: data.decode()):
        remaining = self.deadline - time.monotonic()
        try:
            if remaining <= 0:
                raise TimeoutError()
            result = subprocess.run(argv, capture_output=True, check=True, timeout=min(4, remaining))
            return parse(result.stdout)
        except (OSError, subprocess.SubprocessError, ValueError, TimeoutError):
            self.warnings.append(f"{label} unavailable")
            return None


def laptop_readings(text):
    rows = []
    for line in (text or "").splitlines():
        match = re.search(r"(\d+)%;\s*(.*)", line)
        if match:
            detail = re.sub(r"\s*present:\s*(true|false).*$", "", match[2]).strip(" ;")
            charging = "no" if "not charging" in detail or "discharging" in detail else (
                "complete" if "charged" in detail else "yes" if "charging" in detail else "unknown")
            rows.append(reading("Laptop", match[1], kind="computer", charging=charging, detail=detail))
    return rows


def connected_devices(bluetooth):
    connected = {}
    for controller in (bluetooth or {}).get("SPBluetoothDataType", []):
        for entry in controller.get("device_connected", []):
            for name, properties in entry.items():
                connected[address(properties.get("device_address")) or f"local:{name}"] = (name, properties)
    return connected


def registry_devices(nodes):
    for node in nodes or []:
        if "BatteryPercent" in node and not node.get("Built-In"):
            yield node
        yield from registry_devices(node.get("IORegistryEntryChildren", []))


def headphone_brand(name, properties):
    vendor = str(properties.get("device_vendorID", "")).lower()
    if "009e" in vendor or "bose" in name.lower():
        return "bose"
    if "054c" in vendor or re.search(r"\b(?:WF-|WH-|Sony)", name, re.I):
        return "sony"
    return None


def mac_peripherals(bluetooth, registry, headphones):
    hid = {}
    for device in registry_devices(registry):
        key = address(device.get("DeviceAddress") or device.get("BD_ADDR"))
        key = key or f"local:hid:{device.get('IORegistryEntryID', device.get('Product'))}"
        hid[key] = device
    rows = []
    for key, (name, properties) in connected_devices(bluetooth).items():
        identity = f"bluetooth:{key}" if address(key) else key
        parts = headphones.get(key, {}).get("batteries", [])
        if parts:
            for part in parts:
                rows.append(reading(name, part.get("level_percent"), identity=identity,
                                    part=part.get("part", "battery"), charging=part.get("charging", "unknown")))
        else:
            levels = [(field.removeprefix("device_batteryLevel").lower(), value)
                      for field, value in properties.items() if field.startswith("device_batteryLevel")]
            if levels:
                for part, level in levels:
                    rows.append(reading(name, level, identity=identity, part="battery" if part in ("", "main") else part))
            else:
                device = hid.get(key, {})
                charging = "yes" if device.get("BatteryIsCharging") or device.get("IsCharging") else "unknown"
                rows.append(reading(name, device.get("BatteryPercent"), identity=identity, charging=charging))
        hid.pop(key, None)
    for key, device in hid.items():
        if bluetooth is not None and str(device.get("Transport", "")).lower() == "bluetooth":
            continue
        identity = f"bluetooth:{key}" if address(key) else key
        charging = "yes" if device.get("BatteryIsCharging") or device.get("IsCharging") else "unknown"
        rows.append(reading(device.get("Product", "Peripheral"), device["BatteryPercent"],
                            identity=identity, charging=charging))
    return rows


def probe_mac(reader):
    laptop = reader.get("Laptop battery", ["/usr/bin/pmset", "-g", "batt"])
    registry = reader.get("HID batteries", ["/usr/sbin/ioreg", "-a", "-r", "-k", "BatteryPercent"], plistlib.loads)
    bluetooth = reader.get("Bluetooth inventory", ["/usr/sbin/system_profiler", "-json", "SPBluetoothDataType"], json.loads)
    if bluetooth is not None and not any(bluetooth.get("SPBluetoothDataType", [])):
        reader.warnings.append("Bluetooth inventory unavailable")
        bluetooth = None
    headphones = {}
    for key, (name, properties) in connected_devices(bluetooth).items():
        brand = headphone_brand(name, properties)
        tool = shutil.which(f"{brand}ctl") if brand else None
        if tool and address(key):
            mac = ":".join(key[i:i+2] for i in range(0, 12, 2))
            # Target the exact device: several headsets of one brand can be linked.
            # No --auto-connect: sleeping/disconnected headphones stay that way.
            argv = [tool, "--mac", mac, "--json", "--color=never", "battery"]
            if brand == "bose":
                argv = [tool, "battery", "--mac", mac, "--json", "--color=never"]
            headphones[key] = reader.get(f"{name} battery", argv, json.loads) or {}
    return laptop_readings(laptop) + mac_peripherals(bluetooth, registry, headphones)


def probe_android(reader):
    tool = shutil.which("termux-battery-status")
    if not tool:
        reader.warnings.append("Install Termux:API and the termux-api package for the phone battery")
        return []
    data = reader.get("Phone battery", [tool], json.loads)
    if not data:
        return []
    status = str(data.get("status", "")).lower()
    charging = {"charging": "yes", "full": "complete", "discharging": "no", "not_charging": "no"}.get(status, "unknown")
    return [reading("Phone", data.get("percentage"), kind="phone", charging=charging,
                    detail=f"{status}; {data.get('plugged', '').lower()}".strip(" ;"))]


def bluez_readings(listing, infos):
    rows = []
    for line in listing.splitlines():
        match = re.fullmatch(r"Device ([0-9A-Fa-f:]{17}) (.*)", line.strip())
        if not match:
            continue
        mac, name = match.groups()
        info = infos.get(mac, "")
        if not re.search(r"Connected:\s*yes\b", info):
            continue
        level = re.search(r"Battery Percentage:.*\((\d+)\)", info)
        rows.append(reading(name, level[1] if level else None, identity=f"bluetooth:{address(mac)}"))
    return rows


def probe_linux(reader):
    rows = []
    for supply in sorted(Path("/sys/class/power_supply").glob("*")):
        try:
            if (supply / "type").read_text().strip() != "Battery":
                continue
            level = (supply / "capacity").read_text().strip()
            status = (supply / "status").read_text().strip()
            scope = (supply / "scope").read_text().strip() if (supply / "scope").exists() else "System"
            kind = "peripheral" if scope == "Device" else "computer"
            mac = re.search(r"([0-9a-f]{2}(?::[0-9a-f]{2}){5})", supply.name, re.I)
            identity = f"bluetooth:{address(mac[1])}" if mac else f"local:power:{supply.name}"
            rows.append(reading(supply.name, level, identity=identity, kind=kind,
                                charging={"Charging": "yes", "Discharging": "no", "Full": "complete"}.get(status, "unknown"),
                                detail=status.lower()))
        except OSError:
            reader.warnings.append(f"{supply.name} battery unavailable")
    tool = shutil.which("bluetoothctl")
    if tool:
        listing = reader.get("Bluetooth inventory", [tool, "devices"]) or ""
        infos = {}
        for mac in re.findall(r"^Device ([0-9A-Fa-f:]{17}) ", listing, re.M):
            infos[mac] = reader.get("Bluetooth device", [tool, "info", mac]) or ""
        # BlueZ may expose the same device through sysfs and DBus.
        bluetooth = bluez_readings(listing, infos)
        seen = {row["identity"] for row in bluetooth if row["level_percent"] is not None}
        rows = [row for row in rows if row["identity"] not in seen] + bluetooth
    return rows


def cache_directory():
    return Path(os.environ.get("XDG_CACHE_HOME", str(Path.home() / ".cache"))) / "batteries" / "published"


def receive(host, stream):
    if not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_.-]{0,199}", host):
        raise ValueError("publisher host must be a tailnet name or a simple device identifier")
    raw = stream.read(65537)
    if len(raw) > 65536:
        raise ValueError("upload exceeds 64 KiB")
    data = json.loads(raw)
    level = percentage(data.get("level_percent"))
    charging = data.get("charging", "unknown")
    if level is None or charging not in ("yes", "no", "complete", "unknown"):
        raise ValueError("upload requires level_percent (0..100) and optional charging (yes/no/complete/unknown)")
    record = dict(host=host.lower().rstrip("."), received_at=now(),
                  battery=reading(data.get("name", host), level, kind="phone", charging=charging))
    folder = cache_directory()
    folder.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(mode="w", dir=folder, prefix=".incoming-", delete=False) as output:
        json.dump(record, output)
        temporary = output.name
    os.replace(temporary, folder / f"{record['host']}.json")
    return record


def published_snapshots():
    records = []
    for item in cache_directory().glob("*.json"):
        try:
            if item.stat().st_size <= 65536:
                value = json.loads(item.read_text())
                if isinstance(value, dict) and all(key in value for key in ("host", "received_at", "battery")):
                    records.append(value)
        except (OSError, ValueError):
            pass
    return records


def probe(os_hint="", budget=12):
    reader = Reader(budget)
    system = platform.system()
    android = os_hint.lower() == "android" or "/com.termux/" in os.environ.get("PREFIX", "")
    label = "android" if android else "macOS" if system == "Darwin" else system.lower()
    limitations = []
    if android:
        rows = probe_android(reader)
        limitations.append("Termux does not expose connected Bluetooth peripheral battery levels")
    elif system == "Darwin":
        rows = probe_mac(reader)
    elif system == "Linux":
        rows = probe_linux(reader)
    else:
        rows = []
        limitations.append(f"No collector for {system}")
    return dict(platform=label, status="partial" if reader.warnings else "ok", observed_at=now(),
                batteries=rows, warnings=reader.warnings, limitations=limitations, published=published_snapshots())


def ssh_aliases(config=None):
    """Read only SSH config metadata; credentials and private keys are never opened."""
    mapping, visited = {}, set()

    def visit(filename):
        filename = Path(filename).expanduser()
        if filename in visited or len(visited) >= 64:
            return
        visited.add(filename)
        try:
            lines = filename.read_text().splitlines()
        except OSError:
            return
        aliases = []
        for line in lines:
            try:
                words = shlex.split(line, comments=True)
            except ValueError:
                continue
            if not words:
                continue
            key = words[0].lower()
            if key == "host":
                aliases = [word for word in words[1:] if not re.search(r"[*?!%]", word)]
            elif key == "match":
                aliases = []
            elif key == "hostname" and len(words) == 2 and aliases and "%" not in words[1]:
                mapping.setdefault(words[1].lower().rstrip("."), aliases[0])
            elif key == "include":
                for pattern in words[1:]:
                    expanded = Path(os.path.expandvars(pattern)).expanduser()
                    if not expanded.is_absolute():
                        expanded = Path.home() / ".ssh" / expanded
                    for child in sorted(glob.glob(str(expanded))):
                        visit(child)
    visit(config or Path.home() / ".ssh" / "config")
    return mapping


def peer_record(peer, local=False, aliases=None):
    aliases = aliases or {}
    dns = str(peer.get("DNSName", "")).rstrip(".")
    short = dns.split(".")[0] if dns else str(peer.get("HostName", "unknown"))
    candidates = [dns, short, peer.get("HostName", "")] + peer.get("TailscaleIPs", [])
    target = next((aliases[str(candidate).lower()] for candidate in candidates if str(candidate).lower() in aliases), short)
    return dict(id=dns.lower() or short.lower(), name=short, platform=peer.get("OS", "unknown"),
                local=local, online=peer.get("Online", local), target=target)


def discover(timeout=5):
    result = subprocess.run(["tailscale", "status", "--json"], capture_output=True, check=True, timeout=timeout)
    data = json.loads(result.stdout)
    if not data.get("Self"):
        raise ValueError("tailscale did not return this device")
    aliases = ssh_aliases()
    return [peer_record(data["Self"], True, aliases)] + sorted(
        [peer_record(peer, aliases=aliases) for peer in data.get("Peer", {}).values()], key=lambda peer: peer["name"].lower())


def host_matches(host, value):
    value = value.lower().rstrip(".")
    return value in {str(host.get(key, "")).lower().rstrip(".") for key in ("id", "name", "target")}


def query_host(host, source, timeout):
    record = dict(host)
    record.pop("target", None)
    if host["platform"].lower() == "ios":
        record["limitations"] = ["Needs a phone-side battery publisher"]
    if not host["local"] and not host["online"]:
        return dict(record, status="offline", batteries=[], warnings=[], published=[])
    if host["platform"].lower() in ("ios", "tvos", "windows") and not host["local"]:
        return dict(record, status="unsupported", batteries=[], warnings=[], published=[],
                    limitations=["Needs a phone-side battery publisher" if host["platform"].lower() == "ios" else "No SSH collector"])
    try:
        if host["local"]:
            snapshot = probe(budget=max(1, timeout - 3))
        else:
            # Source goes over stdin and executes in memory; nothing is installed on peers.
            remote = shlex.join(["python3", "-", "--probe", "--platform", host["platform"],
                                 "--probe-budget", str(max(1, timeout - 3))])
            argv = ["ssh", "-T", "-o", "BatchMode=yes", "-o", "ConnectTimeout=4", "-o", "ConnectionAttempts=1",
                    "-o", "ServerAliveInterval=3", "-o", "ServerAliveCountMax=1", host["target"], remote]
            result = subprocess.run(argv, input=source, capture_output=True, text=True, check=True, timeout=timeout)
            snapshot = json.loads(result.stdout)
            if not isinstance(snapshot, dict) or not isinstance(snapshot.get("batteries"), list):
                raise ValueError("invalid collector response")
        record.update(snapshot)
    except subprocess.TimeoutExpired:
        record.update(status="unreachable", batteries=[], warnings=[f"query timed out after {timeout:g}s"], published=[])
    except subprocess.CalledProcessError as error:
        message = " ".join((error.stderr or "SSH query failed").split())[-250:]
        record.update(status="unreachable", batteries=[], warnings=[message], published=[])
    except (OSError, ValueError) as error:
        record.update(status="error", batteries=[], warnings=[str(error)], published=[])
    if host["platform"].lower() == "ios":
        record.setdefault("limitations", ["Needs a phone-side battery publisher"])
    return record


def apply_published(hosts, max_age):
    publications = [entry for host in hosts for entry in host.pop("published", [])]
    publications += published_snapshots()
    latest = {}
    for entry in publications:
        key = entry["host"]
        if key not in latest or entry["received_at"] > latest[key]["received_at"]:
            latest[key] = entry
    for key, entry in latest.items():
        host = next((host for host in hosts if host_matches(host, key)), None)
        if host is None:
            continue
        if host.get("batteries"):
            continue
        try:
            age = max(0, (datetime.now(timezone.utc) - datetime.fromisoformat(entry["received_at"])).total_seconds())
        except (TypeError, ValueError):
            continue
        host.update(status="cached", batteries=[entry["battery"]], observed_at=entry["received_at"],
                    age_seconds=round(age), stale=age > max_age,
                    limitations=["Published phone battery only; peripheral levels unavailable"])


def merge_devices(hosts):
    merged = {}
    for host in hosts:
        for sample in host.get("batteries", []):
            identity = sample.get("identity", "local:unknown")
            if not identity.startswith("bluetooth:") or not address(identity.removeprefix("bluetooth:")):
                identity = f"{host['id']}:{identity}"
            part = sample.get("part", "battery")
            key = (identity, part)
            row = merged.setdefault(key, dict(identity=identity, name=sample["name"], names=[],
                                              kind=sample["kind"], part=part, hosts=[], observations=[]))
            if sample["name"] not in row["names"]:
                row["names"].append(sample["name"])
            if host["name"] not in row["hosts"]:
                row["hosts"].append(host["name"])
            row["observations"].append(dict(host=host["name"], level_percent=sample.get("level_percent"),
                                             charging=sample.get("charging", "unknown"), detail=sample.get("detail", ""),
                                             observed_at=host.get("observed_at")))
    for row in merged.values():
        levels = sorted({sample["level_percent"] for sample in row["observations"] if sample["level_percent"] is not None})
        row.update(level_percent=levels[0] if len(levels) == 1 else None,
                   level_min=levels[0] if levels else None, level_max=levels[-1] if levels else None,
                   conflict=len(levels) > 1)
        states = {sample["charging"] for sample in row["observations"] if sample["charging"] != "unknown"}
        row["charging"] = next(iter(states)) if len(states) == 1 else "unknown"
        row["charging_conflict"] = len(states) > 1
    return sorted(merged.values(), key=lambda row: (row["kind"], row["name"].casefold(), row["identity"], row["part"]))


def clean(text):
    return "".join(c for c in str(text) if not unicodedata.category(c).startswith("C"))


def width(text):
    return sum(0 if unicodedata.combining(c) else 2 if unicodedata.east_asian_width(c) in "WF" else 1 for c in text)


def pad(text, size):
    text = clean(text)
    while width(text) > size:
        text = text[:-2] + "…" if len(text) > 1 else ""
    return text + " " * max(0, size - width(text))


def tint(text, code, color):
    return f"\033[{code}m{text}\033[0m" if color else text


def meter(low, high, color):
    if low is None:
        return tint("[··········]     ?%", 90, color)
    filled = (low + 5) // 10
    label = f"{low}-{high}%" if high != low else f"{low}%"
    return tint(f"[{'█' * filled}{'░' * (10-filled)}] {label:>6}", 31 if low <= 20 else 33 if low <= 40 else 32, color)


def render(report, color=False):
    hosts = [host for host in report["hosts"]
             if host.get("platform", "").lower() != "ios" or host.get("batteries")]
    devices = report["devices"]
    columns = max(50, min(shutil.get_terminal_size((100, 24)).columns, 110))
    label_width = max(16, min(32, columns - 48))
    available = sum(host["status"] in ("ok", "partial") for host in hosts)
    stamp = datetime.fromisoformat(report["generated_at"]).astimezone().strftime("%H:%M:%S %Z")
    print(tint("BATTERIES", "1;36", color) + f"  {available}/{len(hosts)} hosts live  ·  {stamp}")
    print(tint("─" * columns, 90, color))
    print(tint("COMPUTERS & PHONES", 1, color))
    for host in hosts:
        own = [row for row in devices if row["kind"] != "peripheral" and host["name"] in row["hosts"]]
        prefix = "  " + pad(host["name"], label_width)
        limitations = list(host.get("limitations", []))
        if own:
            for row in own:
                state = row["observations"][0]["detail"] or {"yes": "charging", "complete": "charged"}.get(row["charging"], "")
                if host["status"] == "cached":
                    age = host["age_seconds"]
                    state = f"{'stale · ' if host['stale'] else ''}reported {age//60}m {age%60}s ago"
                print(f"{prefix}  {meter(row['level_min'], row['level_max'], color)}  {clean(state)}")
        else:
            state = {"ok": "no internal battery", "partial": "reading unavailable", "offline": "offline",
                     "unsupported": "unsupported collector"}.get(host["status"], host["status"])
            if host["status"] == "unsupported" and limitations:
                state = limitations.pop(0)
            print(f"{prefix}  {tint(state, 90 if host['status'] in ('ok', 'offline', 'unsupported') else 33, color)}")
        for warning in host.get("warnings", []):
            print(tint(f"    ! {clean(warning)}", 33, color))
        for limitation in limitations:
            print(tint(f"    {clean(limitation)}", 90, color))
    peripherals = {}
    for row in devices:
        if row["kind"] == "peripheral":
            peripherals.setdefault(row["identity"], []).append(row)
    print()
    print(tint(f"PERIPHERALS  ·  {len(peripherals)} unique", 1, color))
    for parts in peripherals.values():
        host_names = sorted({host for part in parts for host in part["hosts"]})
        print("  " + tint(clean(parts[0]["name"]), 1, color) + tint("  ↔  " + ", ".join(map(clean, host_names)), 90, color))
        for row in parts:
            label = row["part"] if len(parts) > 1 or row["part"] != "battery" else "battery"
            state = {"yes": "charging", "complete": "charged"}.get(row["charging"], "")
            if row["conflict"] or row["charging_conflict"]:
                state = "conflicting readings: " + ", ".join(
                    f"{s['level_percent'] if s['level_percent'] is not None else '?'}% {s['charging']}@{s['host']}" for s in row["observations"])
            elif row["level_min"] is None:
                state = "not reported"
            print(f"    {pad(label, label_width-2)}  {meter(row['level_min'], row['level_max'], color)}  {clean(state)}".rstrip())
    if not peripherals:
        print(tint("  No peripheral readings available.", 90, color))
    for warning in report["warnings"]:
        print(tint(f"! {clean(warning)}", 33, color))
    print(tint("─" * columns, 90, color))


def collect(args):
    warnings = []
    fallback = dict(id=platform.node().lower(), name=platform.node().split(".")[0], platform=platform.system(),
                    local=True, online=True, target="")
    if args.local:
        hosts = [fallback]
    else:
        try:
            hosts = discover(min(5, args.timeout))
        except (OSError, ValueError, subprocess.SubprocessError):
            hosts = [fallback]
            warnings.append("Tailnet inventory unavailable; showing local and explicitly selected SSH hosts")
    if args.host:
        selected = []
        for name in args.host:
            match = next((host for host in hosts if host_matches(host, name)), None)
            if match is None:
                if not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_.@-]*", name):
                    raise ValueError(f"invalid SSH host: {name}")
                match = dict(id=name.lower(), name=name, platform="unknown", local=False, online=True, target=name)
            if match not in selected:
                selected.append(match)
        hosts = selected
    if args.online:
        hosts = [host for host in hosts if host["online"]]
    if args.exclude:
        hosts = [host for host in hosts if not any(host_matches(host, name) for name in args.exclude)]
    source = Path(__file__).read_text()
    with ThreadPoolExecutor(max_workers=8) as pool:
        results = list(pool.map(lambda host: query_host(host, source, args.timeout), hosts))
    apply_published(results, args.max_age)
    return dict(schema_version=1, generated_at=now(), hosts=results, devices=merge_devices(results), warnings=warnings)


def positive(value):
    number = float(value)
    if not 1 <= number <= 86400:
        raise argparse.ArgumentTypeError("must be between 1 and 86400 seconds")
    return number


def main():
    parser = argparse.ArgumentParser(prog="batteries", description=__doc__)
    scope = parser.add_mutually_exclusive_group()
    scope.add_argument("--local", action="store_true", help="query this machine only, without tailnet discovery")
    scope.add_argument("--host", action="append", metavar="HOST", help="query only this tailnet name or SSH alias; repeatable")
    parser.add_argument("--exclude", action="append", metavar="HOST", help="exclude a host; repeatable")
    parser.add_argument("--online", action="store_true", help="omit devices marked offline by Tailscale")
    parser.add_argument("--json", action="store_true", help="emit one JSON document, including errors and individual observations")
    parser.add_argument("--timeout", type=positive, default=18, metavar="SECONDS", help="per-host deadline (default: 18)")
    parser.add_argument("--max-age", type=positive, default=3600, metavar="SECONDS", help="mark phone uploads stale after this age (default: 3600)")
    parser.add_argument("--watch", nargs="?", type=positive, const=30, metavar="SECONDS", help="repeat explicitly (default interval: 30); Ctrl-C exits")
    parser.add_argument("--color", choices=("always", "never", "auto"), default="auto")
    parser.add_argument("--no-color", dest="color", action="store_const", const="never")
    parser.add_argument("--receive", metavar="HOST", help="store a phone battery upload from JSON stdin; no network listener")
    parser.add_argument("--probe", action="store_true", help=argparse.SUPPRESS)
    parser.add_argument("--platform", default="", help=argparse.SUPPRESS)
    parser.add_argument("--probe-budget", type=positive, default=12, help=argparse.SUPPRESS)
    args = parser.parse_args()
    if args.json and args.watch:
        parser.error("--json and --watch cannot be combined")
    if args.watch and args.watch < 5:
        parser.error("--watch must be at least 5 seconds")
    try:
        if args.receive:
            print(json.dumps(receive(args.receive, sys.stdin)))
            return 0
        if args.probe:
            print(json.dumps(probe(args.platform, args.probe_budget)))
            return 0
        color = args.color == "always" or (args.color == "auto" and sys.stdout.isatty() and "NO_COLOR" not in os.environ)
        while True:
            report = collect(args)
            if args.json:
                print(json.dumps(report, indent=2))
            else:
                if args.watch and sys.stdout.isatty():
                    print("\033[2J\033[H", end="")
                render(report, color)
            sys.stdout.flush()
            if not args.watch:
                return int(bool(report["warnings"]) or any(host["status"] in ("partial", "unreachable", "error") for host in report["hosts"]))
            time.sleep(args.watch)
    except KeyboardInterrupt:
        return 130
    except (OSError, ValueError) as error:
        parser.error(str(error))


if __name__ == "__main__":
    sys.exit(main())
