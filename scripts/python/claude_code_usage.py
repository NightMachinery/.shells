#!/usr/bin/env python3
"""Show the current Claude Code plan usage (like the in-app ``/usage``).

Reads the Claude Code OAuth token (env, macOS Keychain, or credentials
file) and queries the undocumented ``/api/oauth/usage`` endpoint that
Claude Code itself uses. Responses are cached locally because the
endpoint rate-limits aggressively.
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
import tempfile
import time
import urllib.error
import urllib.request
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path

from libs.common_sub_status import (
    DARK_THEME_DEFAULT,
    DARK_THEMES,
    LIGHT_THEME_DEFAULT,
    LIGHT_THEMES,
    Style,
    build_style,
    env_first,
    format_relative,
    format_timestamp,
    format_used_percent,
    get_path,
    nonnegative_int,
)

USAGE_URL = "https://api.anthropic.com/api/oauth/usage"
OAUTH_BETA = "oauth-2025-04-20"
KEYCHAIN_SERVICE = "Claude Code-credentials"
CREDENTIALS_FILE = Path(os.path.expanduser("~/.claude/.credentials.json"))
CACHE_FILE_NAME = "usage.json"
LOGIN_HINT = "open `claude` (or run `/login` inside it) to refresh the token"

WINDOW_LABELS = {
    "five_hour": "5h session",
    "seven_day": "7d (all models)",
    "seven_day_opus": "7d Opus",
    "seven_day_sonnet": "7d Sonnet",
}
WINDOW_ORDER = tuple(WINDOW_LABELS)

LIMIT_KIND_LABELS = {
    "session": "5h session",
    "weekly_all": "7d (all models)",
}


class UsageError(RuntimeError):
    def __init__(self, message: str, *, http_status: int | None = None) -> None:
        super().__init__(message)
        self.http_status = http_status


@dataclass(frozen=True)
class TokenInfo:
    token: str
    source: str
    expires_at_s: float | None = None
    subscription_type: str | None = None

    @property
    def expired(self) -> bool | None:
        if self.expires_at_s is None:
            return None
        return self.expires_at_s < time.time()


@dataclass(frozen=True)
class UsageWindow:
    key: str
    label: str
    utilization_pct: float | None
    resets_at_s: float | None
    raw: dict
    severity: str | None = None
    is_active: bool | None = None


@dataclass(frozen=True)
class FetchResult:
    payload: dict
    fetched_at_s: float
    from_cache: bool = False
    stale_reason: str | None = None


def read_keychain_credentials(*, timeout: float) -> dict | None:
    if sys.platform != "darwin":
        return None

    try:
        proc = subprocess.run(
            ["security", "find-generic-password", "-s", KEYCHAIN_SERVICE, "-w"],
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
            timeout=timeout,
            check=False,
        )
    except (OSError, subprocess.TimeoutExpired):
        return None

    if proc.returncode != 0:
        return None

    try:
        data = json.loads(proc.stdout.strip())
    except json.JSONDecodeError:
        return None

    return data if isinstance(data, dict) else None


def read_credentials_file() -> dict | None:
    try:
        with CREDENTIALS_FILE.open(encoding="utf-8") as handle:
            data = json.load(handle)
    except (OSError, json.JSONDecodeError):
        return None

    return data if isinstance(data, dict) else None


def token_info_from_oauth(data: dict, *, source: str) -> TokenInfo | None:
    token = get_path(data, ("claudeAiOauth", "accessToken"))
    if not isinstance(token, str) or not token:
        return None

    expires_at_s: float | None = None
    expires_at = get_path(data, ("claudeAiOauth", "expiresAt"))
    if isinstance(expires_at, (int, float)):
        # Stored as epoch milliseconds.
        expires_at_s = float(expires_at) / 1000.0

    subscription = get_path(data, ("claudeAiOauth", "subscriptionType"))
    subscription_type = subscription if isinstance(subscription, str) else None

    return TokenInfo(
        token=token,
        source=source,
        expires_at_s=expires_at_s,
        subscription_type=subscription_type,
    )


def get_token(*, timeout: float) -> TokenInfo:
    env_token = os.environ.get("CLAUDE_CODE_OAUTH_TOKEN")
    if env_token:
        return TokenInfo(token=env_token, source="env")

    keychain = read_keychain_credentials(timeout=timeout)
    if keychain is not None:
        info = token_info_from_oauth(keychain, source="keychain")
        if info is not None:
            return info

    file_creds = read_credentials_file()
    if file_creds is not None:
        info = token_info_from_oauth(file_creds, source="credentials-file")
        if info is not None:
            return info

    raise UsageError(
        "no OAuth token found (tried CLAUDE_CODE_OAUTH_TOKEN, "
        f"Keychain item {KEYCHAIN_SERVICE!r}, and {CREDENTIALS_FILE}); "
        + LOGIN_HINT
    )


def cache_path(cache_dir: str) -> Path:
    return Path(os.path.expanduser(cache_dir)) / CACHE_FILE_NAME


def read_cache(path: Path) -> FetchResult | None:
    try:
        with path.open(encoding="utf-8") as handle:
            data = json.load(handle)
    except (OSError, json.JSONDecodeError):
        return None

    if not isinstance(data, dict):
        return None

    fetched_at = data.get("fetched_at")
    payload = data.get("payload")
    if not isinstance(fetched_at, (int, float)) or not isinstance(payload, dict):
        return None

    return FetchResult(payload=payload, fetched_at_s=float(fetched_at), from_cache=True)


def write_cache(path: Path, payload: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    data = {"fetched_at": time.time(), "payload": payload}

    fd, tmp_name = tempfile.mkstemp(dir=path.parent, prefix=".usage-", suffix=".tmp")
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as handle:
            json.dump(data, handle)
        os.replace(tmp_name, path)
    except OSError:
        try:
            os.unlink(tmp_name)
        except OSError:
            pass


def fetch_usage(token: str, *, timeout: float, user_agent: str) -> dict:
    request = urllib.request.Request(
        USAGE_URL,
        headers={
            "Authorization": f"Bearer {token}",
            "anthropic-beta": OAUTH_BETA,
            "Content-Type": "application/json",
            "User-Agent": user_agent,
        },
    )

    try:
        with urllib.request.urlopen(request, timeout=timeout) as response:
            body = response.read().decode("utf-8", errors="replace")
    except urllib.error.HTTPError as exc:
        detail = ""
        try:
            detail = exc.read().decode("utf-8", errors="replace")[:200]
        except OSError:
            pass

        message = f"usage endpoint returned HTTP {exc.code}"
        if exc.code == 401:
            message += f" (token rejected); {LOGIN_HINT}"
        elif exc.code == 429:
            message += (
                " (rate limited; the endpoint recovers slowly -- "
                "rely on the cache and avoid --refresh loops)"
            )
        if detail:
            message += f": {detail}"
        raise UsageError(message, http_status=exc.code) from exc
    except (urllib.error.URLError, TimeoutError, OSError) as exc:
        raise UsageError(f"usage endpoint unreachable: {exc}") from exc

    try:
        payload = json.loads(body)
    except json.JSONDecodeError as exc:
        raise UsageError(f"usage endpoint returned invalid JSON: {body[:200]}") from exc

    if not isinstance(payload, dict):
        raise UsageError(f"usage endpoint returned non-object JSON: {body[:200]}")

    return payload


def get_usage(
    token_info: TokenInfo,
    *,
    cache_file: Path,
    ttl_s: int,
    refresh: bool,
    timeout: float,
    user_agent: str,
) -> FetchResult:
    cached = read_cache(cache_file)

    if not refresh and cached is not None and ttl_s > 0:
        age = time.time() - cached.fetched_at_s
        if age < ttl_s:
            return cached

    try:
        payload = fetch_usage(token_info.token, timeout=timeout, user_agent=user_agent)
    except UsageError as exc:
        if cached is not None:
            print(
                f"claude_code_usage: fetch failed ({exc}); showing cached data",
                file=sys.stderr,
            )
            return FetchResult(
                payload=cached.payload,
                fetched_at_s=cached.fetched_at_s,
                from_cache=True,
                stale_reason=str(exc),
            )
        raise

    write_cache(cache_file, payload)
    return FetchResult(payload=payload, fetched_at_s=time.time())


def parse_utilization(value: object) -> float | None:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        return None

    number = float(value)
    # The endpoint has been observed returning both 0-1 fractions and 0-100
    # percentages. Treat fractional floats as fractions; a true float 1.0%
    # would be misread as 100%, but integer percentages pass through intact.
    if isinstance(value, float) and 0.0 <= number <= 1.0:
        return number * 100.0
    return number


def parse_resets_at(value: object) -> float | None:
    if isinstance(value, bool):
        return None

    if isinstance(value, (int, float)):
        number = float(value)
        if number > 1e12:  # epoch milliseconds
            number /= 1000.0
        return number

    if isinstance(value, str):
        try:
            return datetime.fromisoformat(value.replace("Z", "+00:00")).timestamp()
        except ValueError:
            return None

    return None


def looks_like_window(value: object) -> bool:
    return isinstance(value, dict) and ("utilization" in value or "resets_at" in value)


def window_label(key: str) -> str:
    return WINDOW_LABELS.get(key, key.replace("_", " "))


def build_window(key: str, raw: dict) -> UsageWindow:
    return UsageWindow(
        key=key,
        label=window_label(key),
        utilization_pct=parse_utilization(raw.get("utilization")),
        resets_at_s=parse_resets_at(raw.get("resets_at")),
        raw=raw,
    )


def limit_label(limit: dict) -> str:
    kind = limit.get("kind")
    base = LIMIT_KIND_LABELS.get(kind)
    if base is not None:
        return base

    scope_name = get_path(limit, ("scope", "model", "display_name"))
    group = limit.get("group")
    if group == "weekly":
        return f"7d {scope_name}" if scope_name else "7d (scoped)"

    text = str(kind or group or "limit").replace("_", " ")
    return f"{text} {scope_name}" if scope_name else text


def build_limit_window(limit: dict) -> UsageWindow:
    severity = limit.get("severity")
    is_active = limit.get("is_active")
    return UsageWindow(
        key=str(limit.get("kind") or limit.get("group") or "limit"),
        label=limit_label(limit),
        utilization_pct=parse_utilization(limit.get("percent")),
        resets_at_s=parse_resets_at(limit.get("resets_at")),
        raw=limit,
        severity=severity if isinstance(severity, str) else None,
        is_active=is_active if isinstance(is_active, bool) else None,
    )


def extract_windows(payload: dict) -> list[UsageWindow]:
    # Newer responses carry an authoritative `limits` array (integer
    # percents, severity, model-scoped weekly windows); prefer it.
    limits = payload.get("limits")
    if isinstance(limits, list):
        windows = [
            build_limit_window(limit) for limit in limits if isinstance(limit, dict)
        ]
        if windows:
            return windows

    windows = []
    for key in WINDOW_ORDER:
        value = payload.get(key)
        if looks_like_window(value):
            windows.append(build_window(key, value))

    for key in sorted(payload):
        if key in WINDOW_ORDER or key == "extra_usage":
            continue
        value = payload.get(key)
        if looks_like_window(value):
            windows.append(build_window(key, value))

    return windows


def render_window(style: Style, window: UsageWindow) -> str:
    if window.utilization_pct is None:
        used = style.dim("n/a")
    else:
        used = format_used_percent(style, round(window.utilization_pct, 1))

    bits = [f"{style.bold(window.label)}: {used} used"]
    if window.resets_at_s is None:
        bits.append(f"resets {style.dim('n/a')}")
    else:
        bits.append(
            f"resets {format_relative(window.resets_at_s)} "
            f"({format_timestamp(window.resets_at_s)})"
        )

    if window.severity is not None and window.severity != "normal":
        bits.append(style.red(window.severity))

    return " | ".join(bits)


def render_extra_usage(style: Style, extra: object) -> str | None:
    if not isinstance(extra, dict):
        return None

    if not (
        extra.get("is_enabled")
        or extra.get("used_credits")
        or extra.get("spend_limit_reached")
    ):
        return None

    bits = [
        f"{key}: {value}"
        for key, value in sorted(extra.items())
        if value is not None
    ]
    return f"{style.bold('Extra usage')}: " + ", ".join(bits)


def render_human(style: Style, *, token_info: TokenInfo, result: FetchResult) -> str:
    lines = [style.bold(style.cyan("Claude Code plan usage"))]

    plan = token_info.subscription_type or "unknown"
    lines.append(f"Plan: {style.magenta(plan)} ({token_info.source})")

    if token_info.expired:
        lines.append(
            style.yellow(
                "OAuth token expired "
                f"({format_relative(token_info.expires_at_s)}); {LOGIN_HINT}"
            )
        )

    windows = extract_windows(result.payload)
    if windows:
        lines.extend(render_window(style, window) for window in windows)
    else:
        lines.append(style.dim("No usage windows in response."))

    extra_line = render_extra_usage(style, result.payload.get("extra_usage"))
    if extra_line is not None:
        lines.append(extra_line)

    fetched = f"Fetched: {format_timestamp(result.fetched_at_s)}"
    if result.stale_reason is not None:
        reason = result.stale_reason
        if len(reason) > 80:
            reason = reason[:77] + "..."
        fetched += " " + style.red(f"[stale cache: {reason}]")
    elif result.from_cache:
        fetched += " " + style.dim("(cached)")
    lines.append(style.dim(fetched) if result.stale_reason is None else fetched)

    return "\n".join(lines)


def render_json(*, token_info: TokenInfo, result: FetchResult) -> str:
    windows = [
        {
            "key": window.key,
            "label": window.label,
            "utilization_percent": window.utilization_pct,
            "resets_at": window.resets_at_s,
            "resets_at_iso": (
                datetime.fromtimestamp(window.resets_at_s).astimezone().isoformat()
                if window.resets_at_s is not None
                else None
            ),
            "severity": window.severity,
            "is_active": window.is_active,
        }
        for window in extract_windows(result.payload)
    ]

    return json.dumps(
        {
            "plan": token_info.subscription_type,
            "token_source": token_info.source,
            "token_expired": token_info.expired,
            "windows": windows,
            "extra_usage": result.payload.get("extra_usage"),
            "cache": {
                "from_cache": result.from_cache,
                "stale_reason": result.stale_reason,
                "fetched_at": result.fetched_at_s,
            },
            "raw": result.payload,
        },
        indent=2,
    )


def parse_timeout_default() -> float:
    raw = env_first(
        "claude_code_usage_timeout_s", "CLAUDE_CODE_USAGE_TIMEOUT_S", default="10"
    )
    assert raw is not None

    try:
        return float(raw)
    except ValueError:
        return 10.0


def parse_cache_ttl_default() -> int:
    raw = env_first(
        "claude_code_usage_cache_ttl_s", "CLAUDE_CODE_USAGE_CACHE_TTL_S", default="300"
    )
    assert raw is not None

    try:
        value = int(raw)
    except ValueError:
        return 300

    return max(0, value)


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Show Claude Code plan usage (like the in-app /usage)."
    )
    parser.add_argument(
        "--json",
        action=argparse.BooleanOptionalAction,
        default=False,
        help="Output JSON instead of human-readable text (default: %(default)s).",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=parse_timeout_default(),
        help="Timeout in seconds (default: %(default)s).",
    )
    parser.add_argument(
        "--cache-ttl",
        type=nonnegative_int,
        default=parse_cache_ttl_default(),
        help=(
            "Reuse cached responses younger than this many seconds; "
            "0 always refetches (default: %(default)s)."
        ),
    )
    parser.add_argument(
        "--refresh",
        action=argparse.BooleanOptionalAction,
        default=False,
        help=(
            "Skip the cache and fetch fresh data; the cache is still "
            "updated afterwards (default: %(default)s)."
        ),
    )
    parser.add_argument(
        "--cache-dir",
        default=env_first(
            "claude_code_usage_cache_dir",
            "CLAUDE_CODE_USAGE_CACHE_DIR",
            default="~/tmp/.claude-usage",
        ),
        help="Directory for the response cache (default: %(default)s).",
    )
    parser.add_argument(
        "--user-agent",
        default=env_first(
            "claude_code_usage_user_agent",
            "CLAUDE_CODE_USAGE_USER_AGENT",
            default="claude-code/2.1.220",
        ),
        help=(
            "User-Agent header; without a claude-code one the endpoint "
            "rate-limits hard (default: %(default)s)."
        ),
    )
    parser.add_argument(
        "--color",
        choices=("auto", "always", "never"),
        default="auto",
        help="Color mode for human-readable output (default: %(default)s).",
    )
    parser.add_argument(
        "--true-color",
        choices=("on", "off", "auto"),
        default="auto",
        help="True-color mode for human-readable output (default: %(default)s).",
    )
    parser.add_argument(
        "--dark-mode",
        choices=("on", "off", "auto"),
        default="auto",
        help="Theme brightness mode for human-readable output (default: %(default)s).",
    )
    parser.add_argument(
        "--dark-theme",
        choices=tuple(DARK_THEMES),
        default=DARK_THEME_DEFAULT,
        help="Dark true-color theme name (default: %(default)s).",
    )
    parser.add_argument(
        "--light-theme",
        choices=tuple(LIGHT_THEMES),
        default=LIGHT_THEME_DEFAULT,
        help="Light true-color theme name (default: %(default)s).",
    )

    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    style = build_style(args)

    try:
        token_info = get_token(timeout=args.timeout)
        result = get_usage(
            token_info,
            cache_file=cache_path(args.cache_dir),
            ttl_s=args.cache_ttl,
            refresh=args.refresh,
            timeout=args.timeout,
            user_agent=args.user_agent,
        )
    except UsageError as exc:
        print(f"claude_code_usage: {exc}", file=sys.stderr)
        return 1

    if args.json:
        print(render_json(token_info=token_info, result=result))
    else:
        print(render_human(style, token_info=token_info, result=result))

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
