#!/usr/bin/env python3
"""Human-only agy quota layout. Public JSON contracts belong to the shell."""
import argparse
from datetime import datetime
import json
import math
import os
from pathlib import Path
import re
import sys
import time
import unicodedata


def clean(value):
    return "".join(c for c in str(value or "") if not unicodedata.category(c).startswith("C"))


def width(value):
    return sum(0 if unicodedata.category(c).startswith("M") else
               2 if unicodedata.east_asian_width(c) in ("W", "F") else 1
               for c in value)


def clip(value, columns):
    value = clean(value)
    if width(value) <= columns:
        return value
    result = ""
    for char in value:
        if width(result + char) > max(0, columns - 1):
            break
        result += char
    return result + ("…" if columns > 0 else "")


def pad(value, columns, right=False):
    value = clip(value, columns)
    spaces = " " * max(0, columns - width(value))
    return spaces + value if right else value + spaces


def duration(seconds):
    seconds = max(0, int(seconds))
    cells = []
    for size, unit in ((86400, "d"), (3600, "h"), (60, "m")):
        amount, seconds = divmod(seconds, size)
        if amount:
            cells.append(f"{amount}{unit}")
        if len(cells) == 2:
            break
    return " ".join(cells) or "<1m"


def reset_text(epoch, now):
    if not isinstance(epoch, (float, int)) or not math.isfinite(epoch) or epoch <= 0:
        return "reset unknown", ""
    delta = epoch - now
    relative = f"resets in {duration(delta)}" if delta > 0 else f"reset {duration(-delta)} ago"
    try:
        absolute = datetime.fromtimestamp(epoch).astimezone().strftime("%a %d %b %H:%M %Z")
    except (ValueError, OverflowError, OSError):
        absolute = ""
    return relative, absolute


def bucket_names(bucket):
    identity = clean(bucket.get("bucket_id"))
    group = clean(bucket.get("group") or identity)
    label = clean(bucket.get("label"))
    if identity in ("gemini-5h", "gemini-weekly"):
        family = "Gemini"
    elif identity in ("3p-5h", "3p-weekly"):
        family = "Claude/GPT"
    else:
        # Keep each model-specific pool separate; only trim backend chrome.
        family = re.sub(r"\s+Models?$", "", group, flags=re.I)
        if family == identity:
            family = re.sub(r"-(?:5h|weekly)$", "", identity).replace("-", " ").title()
    if identity.endswith("-5h"):
        window = "5h"
    elif identity.endswith("-weekly") or "weekly" in label.lower():
        window = "weekly"
    elif re.search(r"5h|hour", label, re.I):
        window = "5h"
    else:
        window = re.sub(r"\b(?:Limit|Remaining)\b", "", label, flags=re.I).strip() or "quota"
    return family, window


def provenance(account, now):
    buckets = account.get("buckets", [])
    if not buckets:
        return "quota unavailable" if account.get("error") else ""
    sources = {b.get("source") for b in buckets}
    parts = ["live"] if "live" in sources else []
    if "cache" in sources:
        ages = []
        undated = False
        try:
            document = json.loads(Path(account["_display_cache_file"]).read_text())
            cached = document.get("buckets", {}) if isinstance(document, dict) else {}
            if not isinstance(cached, dict):
                cached = {}
        except (KeyError, OSError, ValueError, TypeError):
            cached = {}
        for bucket in buckets:
            if bucket.get("source") != "cache":
                continue
            identity = bucket.get("bucket_id")
            aliases = {"gemini-4-argon-weekly": "gemini-argon-wtu-weekly",
                       "gemini-4-argon-5h": "gemini-argon-5h"}
            candidates = [cached.get(identity), cached.get(aliases.get(identity))]
            captures = [value.get("captured_at") for value in candidates
                        if isinstance(value, dict)]
            valid = [value for value in captures if isinstance(value, (float, int))
                     and math.isfinite(value)]
            capture = max(valid) if valid else None
            if isinstance(capture, (float, int)) and math.isfinite(capture):
                ages.append(max(0, now - capture))
            else:
                undated = True
        age = max(ages) if ages else None
        stamp = "<1m" if age is not None and age < 60 else duration(age) if age is not None else "age unknown"
        parts.append(f"cache {stamp}" + (", some undated" if undated and ages else ""))
    return " + ".join(parts) or "source unknown"


def render(accounts, columns=80, color=False, absolute="auto", now=None):
    now = time.time() if now is None else now
    columns = max(40, columns)
    off = "\033[0m" if color else ""
    dim = "\033[2m" if color else ""
    bold = "\033[1m" if color else ""

    def paint(text, code):
        return f"\033[{code}m{text}{off}" if color else text

    rows = []
    prepared = []
    for account in accounts:
        entries = []
        for bucket in account.get("buckets", []):
            family, window = bucket_names(bucket)
            used = bucket.get("used_percent", 100 - bucket.get("remaining_percent", 100))
            used = max(0, min(100, float(used)))
            relative, stamp = reset_text(bucket.get("resets_at"), now)
            entry = (family, window, used, relative, stamp)
            entries.append(entry)
            rows.append(entry)
        entries.sort(key=lambda row: (0 if row[0] == "Gemini" else
                                     1 if row[0].lower().startswith("gemini") else
                                     2 if row[0] == "Claude/GPT" else 3,
                                     row[0].casefold(), 0 if row[1] == "5h" else 1, row[1]))
        prepared.append((account, entries))

    window_width = min(8, max([6] + [width(r[1]) for r in rows]))
    reset_width = min(23, max([12] + [width(r[3]) for r in rows]))
    bar_width = 10 if columns >= 70 else 6
    fixed_width = 2 + 2 + window_width + 2 + bar_width + 2 + 5 + 2 + reset_width
    family_width = min(max([10] + [width(r[0]) for r in rows]), max(8, columns - fixed_width))
    base_width = fixed_width + family_width
    stamp_width = max([0] + [width(r[4]) for r in rows])
    show_absolute = absolute != "n" and base_width + 3 + stamp_width <= columns
    if absolute == "auto":
        show_absolute = show_absolute and columns >= 100

    output = []
    for account, entries in prepared:
        if output:
            output.append("")
        label = clean(account.get("label") or account.get("account"))
        name = label if label == account.get("account") else f"{label} [{clean(account.get('account'))}]"
        state = ("not signed in" if account.get("signed_in") is False else
                 "signed in" if account.get("signed_in") is True else "auth stored")
        tier = clean(account.get("tier"))
        source = provenance(account, now)
        email = clean(account.get("email"))
        # Allocate header cells before ANSI styling. Provenance takes priority
        # over a long label/email; all header information stays on one line.
        tier_budget = max(0, columns - width(state) - width(source) - 16)
        tier = clip(tier, tier_budget)
        suffix = "  ".join(part for part in (state, tier, source) if part)
        name = clip(name, max(8, columns - width(suffix) - 2))
        header = f"{bold}{name}{off}  {suffix}"
        plain_length = width(name) + 2 + width(suffix)
        if email and columns - plain_length >= 6:
            header += f"  {dim}{clip(email, columns - plain_length - 2)}{off}"
        # Color only the source portion dimly, without dimming sign-in state.
        if color and source:
            header = header.replace(source, dim + source + off, 1)
        output.append(header)
        if account.get("error"):
            output.append(paint("  ! " + clip(account["error"], columns - 4), 31))
        if account.get("signed_in") is False:
            output.append("  " + clip(f"agy-acc-login {account['account']}", columns - 2))
        for family, window, used, relative, stamp in entries:
            level = 31 if used >= 90 else 33 if used >= 70 else 32
            filled = min(bar_width, int(used * bar_width / 100 + 0.5))
            bar = "█" * filled + "░" * (bar_width - filled)
            percent = f"{used:.1f}".rstrip("0").rstrip(".") + "%"
            line = (f"  {pad(family, family_width)}  {pad(window, window_width)}  "
                    f"{paint(bar, level)}  {paint(pad(percent, 5, right=True), level)}  "
                    f"{pad(relative, reset_width)}")
            if show_absolute and stamp:
                line += f"   {dim}{stamp}{off}"
            output.append(line.rstrip())
    return "\n".join(output) + "\n"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--columns", type=int, default=80)
    parser.add_argument("--color", choices=("auto", "always", "never"), default="auto")
    parser.add_argument("--absolute", choices=("auto", "y", "n"), default="auto")
    args = parser.parse_args()
    color = sys.stdout.isatty() and "NO_COLOR" not in os.environ and args.color != "never"
    print(render(json.load(sys.stdin), args.columns, color, args.absolute), end="")


if __name__ == "__main__":
    main()
