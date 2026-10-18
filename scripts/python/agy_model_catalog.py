#!/usr/bin/env python3
"""Cache account-specific `agy models` results and choose eligible quota."""

import argparse
import json
import math
import os
from pathlib import Path
import re
import signal
import subprocess
import sys
import tempfile
import time


ANSI = re.compile(r"\x1b\[[0-?]*[ -/]*[@-~]")
MODEL_ID = re.compile(r"[a-z0-9][a-z0-9._:@/-]*")
WINDOW = re.compile(r"-(?:weekly|5h)$")


def normalized(value):
    return re.sub(r"[^a-z0-9]+", "-", value.casefold()).strip("-")


def parse_models(output):
    models = {}
    for line in ANSI.sub("", output).splitlines():
        fields = line.strip().split("\t", 1)
        if len(fields) != 2 or not MODEL_ID.fullmatch(fields[0]):
            continue
        model_id, name = fields[0], fields[1].strip()
        if not name:
            continue
        if model_id in models and models[model_id] != name:
            raise ValueError("conflicting model IDs in models output")
        models[model_id] = name
    if not models:
        raise ValueError("models output contained no model ID/name rows")
    return [{"id": model_id, "name": name} for model_id, name in models.items()]


def read_cache(cache, account, ttl, now=None):
    data = json.loads(cache.read_text())
    now = time.time() if now is None else now
    captured = data.get("captured_at")
    if data.get("account") != account or data.get("version") != 1:
        raise ValueError("catalog belongs to another account or schema")
    if (not isinstance(captured, (int, float)) or isinstance(captured, bool)
            or not math.isfinite(captured) or captured > now + 60):
        raise ValueError("invalid catalog timestamp")
    if now - captured >= ttl:
        raise ValueError("catalog expired")
    models = data.get("models")
    if not isinstance(models, list) or not models:
        raise ValueError("catalog has no models")
    seen = set()
    for model in models:
        if (not isinstance(model, dict)
                or not isinstance(model.get("id"), str)
                or not MODEL_ID.fullmatch(model["id"])
                or not isinstance(model.get("name"), str) or not model["name"]
                or model["id"] in seen):
            raise ValueError("invalid catalog model entry")
        seen.add(model["id"])
    return data


def write_cache(cache, data):
    cache.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
    fd, temporary = tempfile.mkstemp(prefix=".models-", dir=cache.parent)
    try:
        with os.fdopen(fd, "w") as stream:
            json.dump(data, stream, indent=2, allow_nan=False)
            stream.write("\n")
        os.replace(temporary, cache)
    finally:
        if os.path.exists(temporary):
            os.unlink(temporary)


def refresh(cache, account, command, timeout):
    if not command:
        raise ValueError("models command is required to refresh the catalog")
    environment = os.environ.copy()
    environment["AGY_CLI_DISABLE_AUTO_UPDATE"] = "true"
    process = subprocess.Popen(command, stdin=subprocess.DEVNULL,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                               text=True, env=environment, start_new_session=True)
    try:
        output, _ = process.communicate(timeout=timeout)
    except (subprocess.TimeoutExpired, KeyboardInterrupt):
        # This is our own newly created group, never a pattern-based PID search.
        print(f"agy models: stopping timed-out process group {process.pid}", file=sys.stderr)
        try:
            os.killpg(process.pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
        try:
            process.communicate(timeout=1)
        except subprocess.TimeoutExpired:
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            process.communicate()
        raise ValueError(f"models command did not finish within {timeout:g}s") from None
    if process.returncode:
        # Raw stderr may contain account details; never put it in the cache.
        raise ValueError(f"models command exited {process.returncode}; run the account's models command to diagnose")
    data = {"version": 1, "account": account, "captured_at": time.time(),
            "models": parse_models(output)}
    write_cache(cache, data)
    return data


def resolve_model(models, requested):
    exact = [model for model in models if model["id"] == requested]
    if len(exact) == 1:
        return exact[0]
    query = normalized(requested)
    matches = [model for model in models
               if query in (normalized(model["id"]), normalized(model["name"]))]
    if len(matches) != 1:
        raise ValueError("requested model is unavailable or ambiguous in the account catalog")
    return matches[0]


def quota_for_model(buckets, model):
    slug = normalized(model["id"])
    if slug.startswith("gemini-"):
        family = "gemini"
    elif slug.startswith(("claude-", "gpt-", "openai-", "anthropic-")):
        family = "3p"
    else:
        raise ValueError("cannot determine quota family for model ID")
    rows = []
    for bucket in buckets:
        if not isinstance(bucket, dict):
            continue
        bucket_id = bucket.get("bucket_id", "")
        if not isinstance(bucket_id, str) or not WINDOW.search(bucket_id):
            continue
        base = normalized(WINDOW.sub("", bucket_id))
        if base == family or slug == base or slug.startswith(base + "-"):
            rows.append((base, bucket))
    # Every matching pool can constrain this model, including a broader
    # model group when a variant-specific window is also present.
    applicable = [row for _, row in rows]
    if not applicable:
        raise ValueError("no applicable quota measurements")
    remaining = []
    for row in applicable:
        value = row.get("remaining_percent")
        if (not isinstance(value, (int, float)) or isinstance(value, bool)
                or not math.isfinite(value) or not 0 <= value <= 100):
            raise ValueError("applicable quota measurement is unknown or invalid")
        remaining.append(value)
    return min(remaining), [row["bucket_id"] for row in applicable]


def select_account(statuses, catalogs, model, ttl):
    candidates = []
    for status in statuses:
        account = status.get("account")
        if account not in catalogs or status.get("error") or status.get("signed_in") is False:
            continue
        try:
            catalog = read_cache(catalogs[account], account, ttl)
            resolved = resolve_model(catalog["models"], model)
            remaining, buckets = quota_for_model(status.get("buckets", []), resolved)
        except (OSError, ValueError, TypeError, KeyError, AttributeError) as error:
            print(f"agy-best: skipping {account}: {error}", file=sys.stderr)
            continue
        if remaining > 0:
            candidates.append({"account": account, "model": resolved["id"],
                               "remaining_percent": remaining, "buckets": buckets})
    if not candidates:
        raise ValueError("no account has the requested model and known remaining applicable quota")
    # Stable ties retain the account registry order returned by agy-status.
    return max(candidates, key=lambda item: item["remaining_percent"])


def nonnegative(value):
    number = float(value)
    if not math.isfinite(number) or number < 0:
        raise argparse.ArgumentTypeError("must be a finite nonnegative number")
    return number


def positive(value):
    number = nonnegative(value)
    if number == 0:
        raise argparse.ArgumentTypeError("must be greater than zero")
    return number


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="action", required=True)
    catalog_parser = subparsers.add_parser("catalog")
    catalog_parser.add_argument("--cache", type=Path, required=True)
    catalog_parser.add_argument("--account", required=True)
    catalog_parser.add_argument("--ttl", type=positive, default=3600)
    catalog_parser.add_argument("--timeout", type=positive, default=20)
    catalog_parser.add_argument("--refresh", action="store_true")
    catalog_parser.add_argument("--model")
    catalog_parser.add_argument("command", nargs=argparse.REMAINDER)
    select_parser = subparsers.add_parser("select")
    select_parser.add_argument("--model", required=True)
    select_parser.add_argument("--ttl", type=positive, default=3600)
    select_parser.add_argument("--catalog", nargs=2, action="append", default=[],
                               metavar=("ACCOUNT", "FILE"))
    args = parser.parse_args()
    try:
        if args.action == "catalog":
            try:
                if args.refresh:
                    raise ValueError("refresh requested")
                result = read_cache(args.cache, args.account, args.ttl)
            except (OSError, ValueError, TypeError, AttributeError):
                command = args.command[1:] if args.command[:1] == ["--"] else args.command
                result = refresh(args.cache, args.account, command, args.timeout)
            if args.model:
                result = dict(result, resolved_model=resolve_model(result["models"], args.model))
        else:
            statuses = json.load(sys.stdin)
            if not isinstance(statuses, list) or any(not isinstance(item, dict) for item in statuses):
                raise ValueError("account status must be a JSON list of objects")
            result = select_account(statuses, {account: Path(filename) for account, filename in args.catalog},
                                    args.model, args.ttl)
        json.dump(result, sys.stdout, allow_nan=False)
        print()
        return 0
    except (OSError, ValueError, TypeError, AttributeError) as error:
        print(f"agy models: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
