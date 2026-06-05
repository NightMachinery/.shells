#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import os
import sys
import urllib.error
import urllib.parse
import urllib.request
from datetime import datetime, timezone
from decimal import Decimal, ROUND_HALF_UP
from typing import Any

API_URL = "https://api.pioneer.ai/billing/usage/requests"
PAGE_SIZE = 100


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Summarize Pioneer request usage since today's 00:00 UTC."
    )
    human = parser.add_mutually_exclusive_group()
    human.add_argument(
        "--human",
        dest="human",
        action="store_true",
        default=True,
        help="print a compact human-readable summary (default)",
    )
    human.add_argument(
        "--no-human",
        dest="human",
        action="store_false",
        help="print pretty JSON",
    )
    parser.add_argument(
        "--api-key",
        default=os.environ.get("PIONEER_API_KEY") or os.environ.get("pioneer_api_key"),
        help=argparse.SUPPRESS,
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=30.0,
        help="HTTP timeout in seconds (default: 30)",
    )
    return parser.parse_args()


def decimal_from_value(value: Any) -> Decimal:
    if value is None:
        return Decimal("0")
    return Decimal(str(value))


def get_json(url: str, api_key: str, timeout: float) -> dict[str, Any]:
    request = urllib.request.Request(url, headers={"X-API-Key": api_key})
    try:
        with urllib.request.urlopen(request, timeout=timeout) as response:
            body = response.read().decode("utf-8")
    except urllib.error.HTTPError as exc:
        detail = exc.read().decode("utf-8", errors="replace").strip()
        if detail:
            raise RuntimeError(f"HTTP {exc.code}: {detail}") from exc
        raise RuntimeError(f"HTTP {exc.code}: {exc.reason}") from exc
    except urllib.error.URLError as exc:
        raise RuntimeError(str(exc.reason)) from exc

    parsed = json.loads(body)
    if not isinstance(parsed, dict):
        raise RuntimeError("unexpected non-object API response")
    return parsed


def fetch_items(api_key: str, since_utc: str, timeout: float) -> list[dict[str, Any]]:
    items: list[dict[str, Any]] = []
    page = 1
    total_count: int | None = None

    while total_count is None or len(items) < total_count:
        query = urllib.parse.urlencode(
            {"start_date": since_utc, "page_size": PAGE_SIZE, "page": page}
        )
        data = get_json(f"{API_URL}?{query}", api_key=api_key, timeout=timeout)

        page_items = data.get("items", [])
        if not isinstance(page_items, list):
            raise RuntimeError("unexpected API response: items is not a list")
        if not page_items:
            break

        for item in page_items:
            if isinstance(item, dict):
                items.append(item)

        if total_count is None:
            total_count_raw = data.get("total_count", len(items))
            try:
                total_count = int(total_count_raw)
            except (TypeError, ValueError):
                total_count = len(items)

        page += 1

    return items


def rounded_float(value: Decimal, places: str) -> float:
    return float(value.quantize(Decimal(places), rounding=ROUND_HALF_UP))


def build_summary(items: list[dict[str, Any]], since_utc: str) -> dict[str, Any]:
    usd = sum((decimal_from_value(item.get("cost")) for item in items), Decimal("0"))
    credits = sum(
        (decimal_from_value(item.get("credit_usage")) for item in items), Decimal("0")
    )
    tokens = sum(int(decimal_from_value(item.get("token_usage"))) for item in items)

    return {
        "since_utc": since_utc,
        "usd": rounded_float(usd, "0.000001"),
        "credits": rounded_float(credits, "0.0001"),
        "tokens": tokens,
    }


def print_human(summary: dict[str, Any]) -> None:
    print(f"Since UTC: {summary['since_utc']}")
    print(f"USD: ${summary['usd']:,.6f}")
    print(f"Credits: {summary['credits']:,.4f}")
    print(f"Tokens: {summary['tokens']:,}")


def main() -> int:
    args = parse_args()
    if not args.api_key:
        print(
            "pioneer-used-today: no API key (set $PIONEER_API_KEY or $pioneer_api_key)",
            file=sys.stderr,
        )
        return 2

    since_utc = datetime.now(timezone.utc).strftime("%Y-%m-%dT00:00:00Z")
    try:
        summary = build_summary(
            fetch_items(api_key=args.api_key, since_utc=since_utc, timeout=args.timeout),
            since_utc=since_utc,
        )
    except Exception as exc:
        print(f"pioneer-used-today: {exc}", file=sys.stderr)
        return 1

    if args.human:
        print_human(summary)
    else:
        print(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
