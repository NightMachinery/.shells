#!/usr/bin/env python3
"""Show the current Claude Code plan usage (like the in-app ``/usage``).

Reads the Claude Code OAuth token (env, macOS Keychain, or credentials
file) and queries the undocumented ``/api/oauth/usage`` endpoint that
Claude Code itself uses. Responses are cached locally because the
endpoint rate-limits aggressively.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import getpass
import hashlib
import json
import os
import re
import subprocess
import sys
import tempfile
import time
import unicodedata
import urllib.error
import urllib.request
from dataclasses import dataclass, field, replace
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
    positive_int,
)

USAGE_URL = "https://api.anthropic.com/api/oauth/usage"
OAUTH_BETA = "oauth-2025-04-20"
KEYCHAIN_SERVICE_BASE = "Claude Code-credentials"
KEYCHAIN_ACCOUNT_FALLBACK = "claude-code-user"
#: Claude Code rejects any account name outside this set and falls back.
KEYCHAIN_ACCOUNT_RE = re.compile(r"^[a-zA-Z0-9._-]+$")
CREDENTIALS_FILE = Path(os.path.expanduser("~/.claude/.credentials.json"))
DEFAULT_PROFILE_CONFIG = Path(os.path.expanduser("~/.claude.json"))
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


@dataclass
class ProfileReport:
    profile: Profile
    token_info: "TokenInfo | None" = None
    result: "FetchResult | None" = None
    #: Set only when there is nothing to show at all -- not when a fallback
    #: succeeded, which is a degraded report rather than a failed one.
    error: str | None = None
    warnings: list[str] = field(default_factory=list)


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
class Profile:
    label: str
    #: The profile's CLAUDE_CONFIG_DIR, or None for the default profile.
    config_dir: str | None = None


@dataclass(frozen=True)
class FetchResult:
    payload: dict
    fetched_at_s: float
    from_cache: bool = False
    stale_reason: str | None = None
    #: "api" (fresh fetch), "api-cache" (our own response cache), or
    #: "config-cache" (the profile's own cachedUsageUtilization).
    source: str = "api"


def keychain_service_for(config_dir: str | None) -> str:
    """The Keychain service name Claude Code itself would use.

    Mirrors the derivation in the Claude Code bundle: the service is
    ``Claude Code-credentials`` plus, for any non-default config dir, ``-`` and
    the first 8 hex digits of sha256 over the NFC-normalized config dir path.
    ``CLAUDE_SECURESTORAGE_CONFIG_DIR`` wins over the config dir when it is set.
    The suffix goes at the *end*, after ``-credentials``.
    """
    secure_dir = os.environ.get("CLAUDE_SECURESTORAGE_CONFIG_DIR")
    if secure_dir is not None:
        hashed = secure_dir
    else:
        hashed = config_dir or ""

    if not hashed:
        return KEYCHAIN_SERVICE_BASE

    digest = hashlib.sha256(
        unicodedata.normalize("NFC", hashed).encode("utf-8")
    ).hexdigest()[:8]
    return f"{KEYCHAIN_SERVICE_BASE}-{digest}"


def keychain_account_default() -> str:
    """The Keychain account name Claude Code itself would use ($USER)."""
    try:
        name = os.environ.get("USER") or getpass.getuser()
    except OSError:
        return KEYCHAIN_ACCOUNT_FALLBACK

    if not name or not KEYCHAIN_ACCOUNT_RE.match(name):
        return KEYCHAIN_ACCOUNT_FALLBACK

    return name


def profile_config_path(config_dir: str | None) -> Path:
    # The default profile keeps its config at ~/.claude.json -- home root, not
    # inside ~/.claude/, which has no .claude.json at all. A custom config dir
    # keeps it inside that dir.
    if config_dir:
        return Path(os.path.expanduser(config_dir)) / ".claude.json"

    return DEFAULT_PROFILE_CONFIG


def credentials_file_paths(config_dir: str | None) -> list[Path]:
    paths: list[Path] = []
    if config_dir:
        paths.append(Path(os.path.expanduser(config_dir)) / ".credentials.json")
    if CREDENTIALS_FILE not in paths:
        paths.append(CREDENTIALS_FILE)

    return paths


def read_json_file(path: Path) -> dict | None:
    try:
        with path.open(encoding="utf-8") as handle:
            data = json.load(handle)
    except (OSError, json.JSONDecodeError):
        return None

    return data if isinstance(data, dict) else None


def read_keychain_item(
    *, service: str, account: str | None, timeout: float
) -> dict | None:
    command = ["security", "find-generic-password", "-s", service]
    if account is not None:
        command.extend(["-a", account])
    command.append("-w")

    try:
        proc = subprocess.run(
            command,
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


def keychain_token_infos(
    *, service: str, accounts: list[str | None], timeout: float
) -> list[TokenInfo]:
    if sys.platform != "darwin":
        return []

    infos: list[TokenInfo] = []
    seen_tokens: set[str] = set()
    seen_accounts: list[str | None] = []
    for account in accounts:
        if account in seen_accounts:
            continue
        seen_accounts.append(account)

        data = read_keychain_item(service=service, account=account, timeout=timeout)
        if data is None:
            continue

        source = "keychain" if account is None else f"keychain:{account}"
        info = token_info_from_oauth(data, source=source)
        if info is None or info.token in seen_tokens:
            continue

        seen_tokens.add(info.token)
        infos.append(info)

    return infos


def best_token_info(infos: list[TokenInfo]) -> TokenInfo | None:
    def freshness(info: TokenInfo) -> tuple[bool, float]:
        return (info.expired is not True, info.expires_at_s or 0.0)

    return max(infos, key=freshness, default=None)


def read_credentials_file(config_dir: str | None = None) -> tuple[dict, Path] | None:
    for path in credentials_file_paths(config_dir):
        data = read_json_file(path)
        if data is not None:
            return data, path

    return None


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


def get_token(
    *,
    timeout: float,
    config_dir: str | None = None,
    keychain_service: str | None = None,
    keychain_account: str | None = None,
) -> TokenInfo:
    env_token = os.environ.get("CLAUDE_CODE_OAUTH_TOKEN")
    if env_token:
        return TokenInfo(token=env_token, source="env")

    service = keychain_service or keychain_service_for(config_dir)
    account = keychain_account or keychain_account_default()

    # The derived (service, account) pair is what current Claude Code writes,
    # so it is authoritative and unambiguous even with several profiles logged
    # in. Older versions stored the *default* profile under other account names
    # ("unknown", or no filter at all), so probe those as a fallback -- but only
    # for the default profile and only when the account was not pinned: a
    # hashed service name can only have been written by a version that already
    # used $USER, and probing without a filter there would just pick whichever
    # orphan `security` returns first.
    infos = keychain_token_infos(
        service=service, accounts=[account], timeout=timeout
    )
    if not infos and keychain_account is None and not config_dir:
        infos = keychain_token_infos(
            service=service,
            accounts=[None, account, "unknown"],
            timeout=timeout,
        )

    keychain_info = best_token_info(infos)
    if keychain_info is not None:
        return keychain_info

    file_creds = read_credentials_file(config_dir)
    if file_creds is not None:
        data, path = file_creds
        info = token_info_from_oauth(data, source=f"credentials-file:{path}")
        if info is not None:
            return info

    tried = ", ".join(str(path) for path in credentials_file_paths(config_dir))
    raise UsageError(
        "no OAuth token found (tried CLAUDE_CODE_OAUTH_TOKEN, "
        f"Keychain item {service!r} account {account!r}, and {tried}); "
        + LOGIN_HINT
    )


def cache_path(cache_dir: str, label: str) -> Path:
    # Keyed by profile: profiles share the endpoint but not the account, so one
    # shared cache file would have them overwrite each other's response.
    return Path(os.path.expanduser(cache_dir)) / label / CACHE_FILE_NAME


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

    return FetchResult(
        payload=payload,
        fetched_at_s=float(fetched_at),
        from_cache=True,
        source="api-cache",
    )


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
                source="api-cache",
            )
        raise

    write_cache(cache_file, payload)
    return FetchResult(payload=payload, fetched_at_s=time.time(), source="api")


def config_cache_result(config_path: Path) -> FetchResult | None:
    """The profile's own usage cache, as written by Claude Code itself.

    Claude Code stores the whole usage payload per profile under
    ``cachedUsageUtilization`` in that profile's ``.claude.json``, in exactly
    the shape the endpoint returns. That makes a credential-free, network-free
    fallback possible for any profile. It is only as fresh as the last Claude
    Code session in that profile, which is why callers annotate its age.
    """
    data = read_json_file(config_path)
    if data is None:
        return None

    cached = data.get("cachedUsageUtilization")
    if not isinstance(cached, dict):
        return None

    payload = cached.get("utilization")
    if not isinstance(payload, dict):
        return None

    fetched_at_ms = cached.get("fetchedAtMs")
    if isinstance(fetched_at_ms, bool) or not isinstance(fetched_at_ms, (int, float)):
        fetched_at_s = 0.0
    else:
        fetched_at_s = float(fetched_at_ms) / 1000.0

    return FetchResult(
        payload=payload,
        fetched_at_s=fetched_at_s,
        from_cache=True,
        source="config-cache",
    )


def gather_report(profile: Profile, *, args: argparse.Namespace) -> ProfileReport:
    """Everything needed to render one profile, with no rendering and no I/O
    on stdout -- so several of these can run at once."""
    report = ProfileReport(profile=profile)
    config_path = profile_config_path(profile.config_dir)

    failure: str | None = None
    try:
        report.token_info = get_token(
            timeout=args.timeout,
            config_dir=profile.config_dir,
            keychain_service=args.keychain_service,
            keychain_account=args.keychain_account,
        )
    except UsageError as exc:
        failure = str(exc)

    if report.token_info is not None:
        try:
            report.result = get_usage(
                report.token_info,
                cache_file=cache_path(args.cache_dir, profile.label),
                ttl_s=args.cache_ttl,
                refresh=args.refresh,
                timeout=args.timeout,
                user_agent=args.user_agent,
            )
        except UsageError as exc:
            failure = str(exc)

    if report.result is None:
        # No token at all, or a fetch failure with no response cache to fall
        # back on. The profile's own cache is written by Claude Code itself, so
        # it is the right last resort before giving up.
        fallback = config_cache_result(config_path)
        if fallback is None:
            report.error = failure or "no usage data available"
            return report

        report.result = replace(fallback, stale_reason=failure)

    return report


def gather_reports(
    profiles: list[Profile], *, args: argparse.Namespace
) -> list[ProfileReport]:
    if len(profiles) == 1:
        return [gather_report(profiles[0], args=args)]

    # Threads, not processes: every profile is a separate account doing its own
    # network round trip, so this is all I/O wait and the GIL is irrelevant.
    # Results are written by index and never appended, so the report order is
    # the order the profiles were given no matter which one finishes first.
    reports: list[ProfileReport | None] = [None] * len(profiles)
    workers = max(1, min(args.workers, len(profiles)))

    with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as executor:
        future_to_index = {
            executor.submit(gather_report, profile, args=args): index
            for index, profile in enumerate(profiles)
        }

        for future in concurrent.futures.as_completed(future_to_index):
            index = future_to_index[future]
            try:
                reports[index] = future.result()
            except Exception as exc:
                # One profile blowing up must not cost the others their report.
                reports[index] = ProfileReport(
                    profile=profiles[index],
                    error=f"{type(exc).__name__}: {exc}",
                )

    return [report for report in reports if report is not None]


def resolve_profiles(args: argparse.Namespace) -> list[Profile]:
    if not args.all:
        return [
            Profile(
                label=args.profile_label or "default",
                config_dir=args.config_dir or None,
            )
        ]

    if not args.profile:
        raise ValueError("--all needs at least one --profile NAME=CONFIG_DIR")

    profiles = []
    for spec in args.profile:
        label, sep, config_dir = spec.partition("=")
        if not sep or not label:
            raise ValueError(f"--profile wants NAME=CONFIG_DIR, got {spec!r}")

        profiles.append(Profile(label=label, config_dir=config_dir or None))

    return profiles


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
        if window.resets_at_s < time.time():
            # Only reachable with cached data. The percent is no longer
            # meaningful, but do not rewrite it to 0 -- that would fabricate a
            # reading we never took.
            bits.append(style.dim("(rolled over)"))

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


def render_human(style: Style, report: ProfileReport) -> str:
    heading = "Claude Code plan usage"
    if report.profile.label:
        heading += f" [{report.profile.label}]"
    lines = [style.bold(style.cyan(heading))]

    if report.error is not None:
        # Rendered as this profile's own section rather than dumped on stderr,
        # so a multi-profile report still shows which one is missing and why.
        lines.append(style.red(report.error))
        return "\n".join(lines)

    token_info = report.token_info
    result = report.result
    assert result is not None
    warnings = report.warnings

    plan = (token_info.subscription_type if token_info else None) or "unknown"
    token_source = token_info.source if token_info else result.source
    lines.append(f"Plan: {style.magenta(plan)} ({token_source})")

    if token_info is not None and token_info.expired:
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

    lines.extend(style.yellow(warning) for warning in warnings or ())

    fetched = f"Fetched: {format_timestamp(result.fetched_at_s)}"
    annotated = False

    if result.source == "config-cache":
        age = (
            format_relative(result.fetched_at_s)
            if result.fetched_at_s > 0
            else "age unknown"
        )
        fetched += " " + style.yellow(f"[local cache: {age}]")
        annotated = True
    elif result.from_cache and result.stale_reason is None:
        fetched += " " + style.dim("(cached)")

    if result.stale_reason is not None:
        reason = result.stale_reason
        if len(reason) > 80:
            reason = reason[:77] + "..."
        label = "no live data" if result.source == "config-cache" else "stale cache"
        fetched += " " + style.red(f"[{label}: {reason}]")
        annotated = True

    lines.append(fetched if annotated else style.dim(fetched))

    return "\n".join(lines)


def build_json(report: ProfileReport, *, keychain: dict | None = None) -> dict:
    # A dict rather than a string, so --all can put several of these in one
    # array: two bare objects in a row are not JSON.
    if report.error is not None:
        return {
            "profile": report.profile.label or None,
            "error": report.error,
            "keychain": keychain,
        }

    token_info = report.token_info
    result = report.result
    assert result is not None
    warnings = report.warnings
    profile = report.profile.label

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

    return {
        "profile": profile or None,
        "error": None,
        "source": result.source,
        "keychain": keychain,
        "warnings": list(warnings or ()),
        "plan": token_info.subscription_type if token_info else None,
        "token_source": token_info.source if token_info else None,
        "token_expired": token_info.expired if token_info else None,
        "windows": windows,
        "extra_usage": result.payload.get("extra_usage"),
        "cache": {
            "from_cache": result.from_cache,
            "stale_reason": result.stale_reason,
            "fetched_at": result.fetched_at_s,
        },
        "raw": result.payload,
    }


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
        "--config-dir",
        default=env_first(
            "claude_code_usage_config_dir",
            "CLAUDE_CODE_USAGE_CONFIG_DIR",
            default="",
        ),
        help=(
            "The profile's CLAUDE_CONFIG_DIR. Empty means the default profile: "
            "config at ~/.claude.json and a Keychain service with no hash "
            "suffix. Deliberately NOT defaulted from CLAUDE_CONFIG_DIR itself, "
            "so running inside a Claude Code session cannot silently switch "
            "which profile is reported (default: %(default)r)."
        ),
    )
    parser.add_argument(
        "--all",
        action=argparse.BooleanOptionalAction,
        default=False,
        help=(
            "Report every profile given with --profile, fetched concurrently "
            "and printed in the order given (default: %(default)s)."
        ),
    )
    parser.add_argument(
        "--profile",
        action="append",
        default=[],
        metavar="NAME=CONFIG_DIR",
        help=(
            "A profile for --all, as NAME=CONFIG_DIR; an empty CONFIG_DIR "
            "means the default profile. Repeatable."
        ),
    )
    parser.add_argument(
        "--workers",
        type=positive_int,
        default=8,
        help="Maximum concurrent profile fetches for --all (default: %(default)s).",
    )
    parser.add_argument(
        "--profile-label",
        default=env_first(
            "claude_code_usage_profile_label",
            "CLAUDE_CODE_USAGE_PROFILE_LABEL",
            default="",
        ),
        help="Profile name shown in the header (default: %(default)r).",
    )
    parser.add_argument(
        "--keychain-service",
        default=None,
        help=(
            "Override the derived Keychain service name. Only needed if a "
            "Claude Code build changes the derivation (default: derived from "
            "--config-dir)."
        ),
    )
    parser.add_argument(
        "--keychain-account",
        default=None,
        help=(
            "Override the derived Keychain account name; also disables the "
            "legacy account probe (default: $USER)."
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
        profiles = resolve_profiles(args)
    except ValueError as exc:
        print(f"claude_code_usage: {exc}", file=sys.stderr)
        return 2

    reports = gather_reports(profiles, args=args)

    def keychain_of(report: ProfileReport) -> dict:
        # No usable cross-check exists that a token belongs to this profile:
        # the payload carries no account id, the tokens are opaque, and
        # resets_at is recomputed per response (so it is not a fingerprint) and
        # re-anchors on each new session. Report the item that was actually
        # used and let a human judge, instead of emitting a guess that cries
        # wolf on a correct setup.
        return {
            "service": args.keychain_service
            or keychain_service_for(report.profile.config_dir),
            "account": args.keychain_account or keychain_account_default(),
        }

    if args.json:
        objects = [build_json(report, keychain=keychain_of(report)) for report in reports]
        # An array only for --all: a single report stays a bare object, which is
        # what every existing caller parses.
        print(json.dumps(objects if args.all else objects[0], indent=2))
    else:
        print("\n\n".join(render_human(style, report) for report in reports))

    return 1 if any(report.error is not None for report in reports) else 0


if __name__ == "__main__":
    raise SystemExit(main())
