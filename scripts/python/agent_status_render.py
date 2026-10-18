#!/usr/bin/env python3
"""Render ordered provider JSON envelopes for the combined terminal report."""
import argparse
import json
import math
import os
import sys
import time

from agy_status_render import clean, clip, duration, render, width
from libs.codex_rate_limits import get_codex_limit


TITLES = {"claude": "Claude Code", "codex": "Codex", "agy": "Antigravity"}


def base_account(name, tier=None, email=None, error=None, source="live"):
    return {"account": name, "label": name, "tier": tier, "email": email,
            "error": error, "buckets": [], "_display_state": "unavailable" if error else "ready",
            "_display_source": "" if error else source, "_display_notes": []}


def add_window(account, family, window, used, reset):
    if (not isinstance(used, (int, float)) or isinstance(used, bool)
            or not math.isfinite(used) or not 0 <= used <= 100):
        account["_display_notes"].append(("warning", f"{family} {window}: usage unavailable"))
        return
    account["buckets"].append({"bucket_id": f"{family}-{window}", "group": family,
                               "used_percent": used, "resets_at": reset,
                               "_display_family": family, "_display_window": window})


def claude_accounts(report, now):
    accounts = []
    for row in report if isinstance(report, list) else [report]:
        cache = row.get("cache") or {}
        capture = cache.get("fetched_at")
        source = "live"
        if cache.get("from_cache"):
            source = "cache " + (duration(max(0, now - capture)) if isinstance(capture, (int, float)) else "age unknown")
        if cache.get("stale_reason"):
            source += " (stale)"
        account = base_account(row.get("profile") or "default", row.get("plan"),
                               error=row.get("error"), source=source)
        for window in row.get("windows", []):
            key, label = window.get("key", ""), window.get("label", "")
            family = "Claude" if key in ("session", "weekly_all") else label.removeprefix("7d ").strip() or key
            period = "5h" if key == "session" else "weekly" if key.startswith("weekly") else label
            add_window(account, family, period, window.get("utilization_percent"), window.get("resets_at"))
        for note in row.get("warnings") or []:
            account["_display_notes"].append(("warning", note))
        if row.get("token_expired"):
            account["_display_notes"].append(("warning", "OAuth token expired; run the profile's login command"))
        if cache.get("stale_reason"):
            account["_display_notes"].append(("warning", cache["stale_reason"]))
        if row.get("relogin"):
            account["_display_notes"].append(("dim", "Relogin: " + str(row["relogin"])))
        extra = row.get("extra_usage")
        if isinstance(extra, dict) and (extra.get("is_enabled") or extra.get("used_credits") or extra.get("spend_limit_reached")):
            detail = ", ".join(f"{key.replace('_', ' ')}: {value}" for key, value in extra.items() if value is not None)
            account["_display_notes"].append(("warning" if extra.get("spend_limit_reached") else "dim", "Extra usage: " + detail))
        if not account["buckets"] and not account["error"]:
            account["_display_notes"].append(("warning", "No usage windows reported"))
        accounts.append(account)
    return accounts


def codex_window(window):
    minutes = window.get("windowDurationMins")
    if minutes == 10080:
        return "weekly"
    if minutes == 300:
        return "5h"
    if isinstance(minutes, (int, float)) and minutes > 0:
        return duration(minutes * 60)
    return window.get("label") or window.get("key") or "quota"


def codex_accounts(report, now):
    accounts = []
    for row in report.get("authFiles", [report]):
        identity = row.get("identity") or {}
        quota = row.get("quota") or {}
        account = base_account(row.get("alias") or "default", identity.get("planType"),
                               identity.get("planOwnerEmail"), row.get("error"))
        if identity.get("workspaceName"):
            account["label"] = identity["workspaceName"]
        if row.get("ok") is False and not account["error"]:
            account.update(error="Status lookup failed", _display_state="unavailable", _display_source="")
        for window in quota.get("windows", []):
            add_window(account, "Codex", codex_window(window), window.get("usedPercent"), window.get("resetsAt"))
        reserve = quota.get("lunaReserve") or {}
        limits = quota.get("limits") or []
        if reserve:
            limits = [reserve] + [limit for limit in limits if limit.get("limitId") != reserve.get("limitId")]
            note = "Reserve: " + ("available" if reserve.get("available") else "spent")
            if reserve.get("model"):
                note += " via " + reserve["model"]
            account["_display_notes"].append(("dim" if reserve.get("available") else "warning", note))
        for limit in limits:
            family = limit.get("label") or limit.get("limitName") or limit.get("limitId") or "Model"
            for window in limit.get("windows", []):
                add_window(account, family, codex_window(window), window.get("usedPercent"), window.get("resetsAt"))
        if quota.get("blocked"):
            account["_display_notes"].append(("alarm", "Allowance blocked: " + "; ".join(quota.get("reasons") or ["quota exhausted"])))
        credits = quota.get("resetCredits") or {}
        if credits.get("availableCount", 0):
            note = f"Reset credits: {credits['availableCount']} available"
            grants = [grant for grant in credits.get("credits", []) if grant.get("status") == "available"]
            titles = list(dict.fromkeys(grant["title"] for grant in grants if grant.get("title")))
            expiries = [grant["expiresAt"] for grant in grants if isinstance(grant.get("expiresAt"), (int, float))]
            details = titles + (["expires in " + duration(max(0, min(expiries) - now))] if expiries else [])
            if details:
                note += " (" + ", ".join(details) + ")"
            account["_display_notes"].append(("dim", note))
        balance = (get_codex_limit(row) or {}).get("credits") or {}
        if balance.get("unlimited") or balance.get("hasCredits"):
            note = "Credits: " + ("unlimited" if balance.get("unlimited") else "available")
            if balance.get("balance") is not None:
                note += f" (balance: {balance['balance']})"
            account["_display_notes"].append(("dim", note))
        if not account["buckets"] and not account["error"]:
            account["_display_notes"].append(("warning", "No usage windows reported"))
        accounts.append(account)
    average = report.get("averageUsage") or {}
    if len(accounts) > 1 and average:
        notes = accounts[-1]["_display_notes"]
        usable, blocked = average.get("usableCount", 0), average.get("blockedCount", 0)
        notes.append(("dim", f"All accounts: {usable}/{usable + blocked} usable"))
        windows = average.get("windows") or []
        cells = [f"{codex_window(window)} {window['usedPercent']:g}%" for window in windows
                 if isinstance(window.get("usedPercent"), (int, float))]
        if cells:
            notes.append(("dim", "Average usage: " + ", ".join(cells)))
        reset = average.get("firstTimeToReset")
        if isinstance(reset, (int, float)):
            notes.append(("dim", f"First reset: {average.get('firstTimeToResetAlias', 'account')} in {duration(max(0, reset - now))}"))
    return accounts


def render_section(envelope, columns=80, color=False, absolute="auto", now=None):
    now = time.time() if now is None else now
    agent = envelope.get("agent", "unknown")
    title = TITLES.get(agent, clean(agent))
    head = clip(title, columns)
    rule = "─" * max(0, columns - width(head) - 2)
    heading = f"\033[1;34m{head}\033[0m  \033[2m{rule}\033[0m" if color else head + "  " + rule
    report = envelope.get("report")
    try:
        if agent not in TITLES:
            raise ValueError("Unknown provider")
        if not isinstance(report, (dict, list)) or not report:
            raise ValueError(f"Status lookup failed (exit {envelope.get('exit', '?')})")
        if agent == "claude":
            accounts = claude_accounts(report, now)
        elif agent == "codex":
            accounts = codex_accounts(report, now)
        else:
            accounts = [dict(row, _display_cache_file=envelope.get("cache_files", {}).get(row.get("account"), "")) for row in report]
        body = render(accounts, columns, color, absolute, now)
    except (ValueError, TypeError, KeyError, AttributeError) as error:
        message = "  ! " + clip(str(error) or "Invalid status response", columns - 4)
        body = ("\033[31m" + message + "\033[0m" if color else message) + "\n"
    return heading.rstrip() + "\n\n" + body.rstrip() + "\n\n"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--columns", type=int, default=80)
    parser.add_argument("--color", choices=("auto", "always", "never"), default="auto")
    parser.add_argument("--absolute", choices=("auto", "y", "n"), default="auto")
    args = parser.parse_args()
    color = "NO_COLOR" not in os.environ and (args.color == "always" or (args.color == "auto" and sys.stdout.isatty()))
    for line in sys.stdin:
        if not line.strip():
            continue
        try:
            envelope = json.loads(line)
            if not isinstance(envelope, dict):
                raise ValueError()
        except ValueError:
            envelope = {"agent": "Status", "report": None}
        print(render_section(envelope, max(40, args.columns), color, args.absolute), end="", flush=True)


if __name__ == "__main__":
    main()
