"""Interpretation of Codex's ``account/rateLimits/read`` payload.

Shared by ``codex_status.py`` (which formats it) and ``codex_loop.py`` (which
sleeps on it). Consumers live directly in ``~/scripts/python/`` (which Python
puts on ``sys.path`` when running them by basename), so they import this module
as ``from libs.codex_rate_limits import ...``.

Pure data: stdlib only, and deliberately no dependency on
``libs.common_sub_status``, so nothing here drags in ``Style``/argparse and
``codex_loop.py`` can use it as-is.

The payload's shape is *not* fixed. It used to report exactly two windows per
limit -- ``primary`` (5 hours) and ``secondary`` (weekly) -- and older code
assumed both were always present. They are not: on the ``prolite`` plan
``primary`` *is* the weekly window and ``secondary`` is ``null``. So everything
here discovers whatever windows a limit happens to carry and labels them by
their duration, never by their slot.

The rule that matters, and the one the old code got backwards: a window
constrains the account only when it is **present and** at/above the full
threshold. A missing window is not an exhausted one.
"""

from __future__ import annotations

from dataclasses import dataclass, field

#: Keys that are dicts but are never usage windows, so window discovery does
#: not mistake them for one. ``credits`` is the dangerous one: it sits beside
#: the windows inside a limit object.
NON_WINDOW_KEYS = frozenset({"credits", "rateLimitUpsell", "account", "individualLimit"})

#: The fields that make a dict window-shaped. Any one is enough; a window that
#: has rolled over can report a percent with no reset time, and vice versa.
WINDOW_FIELDS = ("usedPercent", "resetsAt", "windowDurationMins")

#: Slots that existed before the shape went variable, listed first so output
#: ordering stays stable for accounts that still report them.
LEGACY_WINDOW_ORDER = ("primary", "secondary")

#: The limit id governing ordinary Codex usage. Other ids in
#: ``rateLimitsByLimitId`` are per-model-family (e.g. ``codex_bengalfox`` /
#: GPT-5.3-Codex-Spark) and constrain only that model, so they are reported but
#: never allowed to decide whether an account is usable.
DEFAULT_LIMIT_ID = "codex"

DEFAULT_FULL_PCT = 100.0


def as_dict(value: object) -> dict:
    return value if isinstance(value, dict) else {}


def numeric(value: object) -> float | None:
    #: bool is an int subclass, and a stray `true` here must not read as 1.
    if isinstance(value, bool):
        return None
    if isinstance(value, (int, float)):
        return float(value)
    return None


def used_percent(window: object) -> float | None:
    return numeric(as_dict(window).get("usedPercent"))


def reset_time_for_window(window: object) -> float | None:
    return numeric(as_dict(window).get("resetsAt"))


def format_window_duration(mins: object) -> str:
    """A window's duration as a label: ``5h``, ``Weekly``, ``30m``.

    Named for the common periods, because "Weekly" is what the plan pages call
    it and a 7-day budget misread as a 5-hour one is the whole reason this
    function exists.
    """
    value = numeric(mins)
    if value is None or value <= 0:
        return "?"

    total = int(round(value))
    if total == 1440:
        return "Daily"
    if total == 10080:
        return "Weekly"
    if total in (43200, 44640):
        return "Monthly"
    if total < 60:
        return f"{total}m"
    if total < 1440:
        return f"{total // 60}h" if total % 60 == 0 else f"{total / 60:g}h"
    if total % 1440 == 0:
        return f"{total // 1440}d"
    return f"{total}m"


def is_window_shaped(key: str, value: object) -> bool:
    if key in NON_WINDOW_KEYS or not isinstance(value, dict):
        return False
    return any(k in value for k in WINDOW_FIELDS)


@dataclass(frozen=True)
class Window:
    key: str
    used_percent: float | None
    resets_at: float | None
    duration_mins: float | None
    label: str

    @property
    def idle(self) -> bool:
        return self.used_percent is not None and self.used_percent <= 0

    def blocked(self, full_pct: float = DEFAULT_FULL_PCT) -> bool:
        return self.used_percent is not None and self.used_percent >= full_pct


def build_window(key: str, raw: object) -> Window:
    data = as_dict(raw)
    duration = numeric(data.get("windowDurationMins"))
    return Window(
        key=key,
        used_percent=used_percent(data),
        resets_at=reset_time_for_window(data),
        duration_mins=duration,
        label=format_window_duration(duration),
    )


def iter_windows(limit: object) -> list[Window]:
    """Every usage window a limit reports, shortest slot-order first.

    Returns ``[]`` when a limit carries none, which callers must read as
    "unconstrained", not "exhausted".
    """
    data = as_dict(limit)
    keys = [k for k in LEGACY_WINDOW_ORDER if is_window_shaped(k, data.get(k))]
    keys += sorted(
        k for k, v in data.items() if k not in LEGACY_WINDOW_ORDER and is_window_shaped(k, v)
    )
    return [build_window(k, data[k]) for k in keys]


@dataclass(frozen=True)
class LimitView:
    limit_id: str | None
    limit_name: str | None
    normal_model_slug: str | None
    windows: list[Window]
    raw: dict

    @property
    def display_name(self) -> str:
        if self.limit_name and self.limit_id:
            return f"{self.limit_name} [{self.limit_id}]"
        return self.limit_name or self.limit_id or "?"


def limit_view(limit: object) -> LimitView:
    data = as_dict(limit)
    return LimitView(
        limit_id=data.get("limitId") if isinstance(data.get("limitId"), str) else None,
        limit_name=data.get("limitName") if isinstance(data.get("limitName"), str) else None,
        normal_model_slug=(
            data.get("normalModelSlug") if isinstance(data.get("normalModelSlug"), str) else None
        ),
        windows=iter_windows(data),
        raw=data,
    )


def unwrap_auth_envelope(status: object, *, alias: str | None = None) -> dict:
    """Pick one auth entry out of a ``{"authFiles": [...]}`` report.

    ``codex_status.py --json`` defaults to ``--all``, so its output is an
    envelope rather than a bare rate-limit object. Callers handed that envelope
    (``codex_loop.py`` is one) would otherwise find no limits at all and
    conclude, wrongly, that there is nothing to wait for.
    """
    entries = as_dict(status).get("authFiles")
    if not isinstance(entries, list):
        return {}

    ok_entries = [e for e in entries if isinstance(e, dict) and e.get("ok")]
    if not ok_entries:
        return {}

    if alias is not None:
        for entry in ok_entries:
            if entry.get("alias") == alias:
                return entry

    for entry in ok_entries:
        if entry.get("active"):
            return entry

    return ok_entries[0]


def get_codex_limit(status: object, *, alias: str | None = None) -> dict | None:
    """The limit object governing ordinary usage, or ``None``.

    Prefers the explicit ``rateLimitsByLimitId.codex`` entry, falls back to the
    top-level ``rateLimits``, and finally unwraps an ``authFiles`` envelope.
    """
    data = as_dict(status)

    by_id = as_dict(data.get("rateLimitsByLimitId"))
    default = by_id.get(DEFAULT_LIMIT_ID)
    if isinstance(default, dict):
        return default

    top = data.get("rateLimits")
    if isinstance(top, dict):
        return top

    entry = unwrap_auth_envelope(data, alias=alias)
    if entry:
        return get_codex_limit(entry, alias=alias)

    return None


def limit_source(status: object) -> str | None:
    """Where :func:`get_codex_limit` found its answer, for the report."""
    data = as_dict(status)
    if isinstance(as_dict(data.get("rateLimitsByLimitId")).get(DEFAULT_LIMIT_ID), dict):
        return f"rateLimitsByLimitId.{DEFAULT_LIMIT_ID}"
    if isinstance(data.get("rateLimits"), dict):
        return "rateLimits"
    if unwrap_auth_envelope(data):
        return "authFiles"
    return None


def limits_by_id(status: object) -> list[LimitView]:
    """Every per-model-family limit, sorted by id."""
    by_id = as_dict(as_dict(status).get("rateLimitsByLimitId"))
    return [limit_view(by_id[key]) for key in sorted(by_id) if isinstance(by_id[key], dict)]


def blocking_signals(limit: object, envelope: object = None) -> list[str]:
    """Explicit "you are blocked" flags, independent of any percentage.

    Trusted over the percentages because a limit can be reached while
    ``usedPercent`` is missing or rounds below 100.

    ``ordinaryUsageAllowed`` lives at the top level of the response rather than
    inside the limit, hence the separate envelope.
    """
    data = as_dict(limit)
    outer = as_dict(envelope)
    reasons: list[str] = []

    for source in (data, outer):
        if source.get("ordinaryUsageAllowed") is False:
            reasons.append("ordinary usage not allowed")
            break

    for source in (data, outer):
        if source.get("spendControlReached") is True:
            reasons.append("spend control reached")
            break

    reached = data.get("rateLimitReachedType") or outer.get("rateLimitReachedType")
    if reached:
        reasons.append(f"rate limit reached ({reached})")

    return reasons


@dataclass(frozen=True)
class BlockState:
    blocked: bool
    reasons: list[str] = field(default_factory=list)
    blocked_windows: list[Window] = field(default_factory=list)
    resets_at: float | None = None


def block_state(
    limit: object,
    *,
    envelope: object = None,
    full_pct: float = DEFAULT_FULL_PCT,
) -> BlockState:
    """Whether a limit currently blocks usage, and when that ends.

    ``resets_at`` is the **latest** reset among the blocked windows, not the
    earliest: a short window rolling over buys nothing while a longer one is
    still spent. (Mirrors ``h-codex-status-arm-auth`` in ``codex.zsh``.)
    """
    windows = iter_windows(limit)
    blocked_windows = [w for w in windows if w.blocked(full_pct)]

    reasons = [
        f"{w.label} window at {w.used_percent:g}%"
        + (f" (>= {full_pct:g}%)" if full_pct != DEFAULT_FULL_PCT else "")
        for w in blocked_windows
    ]
    signal_reasons = blocking_signals(limit, envelope)
    reasons.extend(signal_reasons)

    if not reasons:
        return BlockState(blocked=False)

    resets = [w.resets_at for w in blocked_windows if w.resets_at is not None]
    if not resets:
        #: Blocked by a signal alone: no window is over the line, so fall back
        #: to the furthest reset the limit reports at all.
        resets = [w.resets_at for w in windows if w.resets_at is not None]

    return BlockState(
        blocked=True,
        reasons=reasons,
        blocked_windows=blocked_windows,
        resets_at=max(resets) if resets else None,
    )


def longest_window(limit: object) -> Window | None:
    windows = [w for w in iter_windows(limit) if w.duration_mins is not None]
    if not windows:
        return None
    return max(windows, key=lambda w: w.duration_mins or 0)


def shortest_window(limit: object) -> Window | None:
    windows = [w for w in iter_windows(limit) if w.duration_mins is not None]
    if not windows:
        return None
    return min(windows, key=lambda w: w.duration_mins or 0)


def usage_sort_key(limit: object) -> tuple[float, float]:
    """Ordering for "which auth has the most headroom".

    Longest window first, because that is the budget that takes longest to come
    back. Duration-derived rather than slot-derived, so it keeps meaning the
    same thing when a plan reports one window or three.
    """
    def pct(window: Window | None) -> float:
        if window is None or window.used_percent is None:
            return float("inf")
        return window.used_percent

    return (pct(longest_window(limit)), pct(shortest_window(limit)))


@dataclass(frozen=True)
class ResetCredit:
    id: str | None
    title: str | None
    description: str | None
    status: str | None
    reset_type: str | None
    granted_at: float | None
    expires_at: float | None

    @property
    def available(self) -> bool:
        return self.status == "available"


@dataclass(frozen=True)
class ResetCredits:
    available_count: int
    credits: list[ResetCredit] = field(default_factory=list)

    @property
    def available_credits(self) -> list[ResetCredit]:
        return [c for c in self.credits if c.available]


def reset_credits(status: object) -> ResetCredits:
    """One-off "Full reset" grants, which can end a wait early."""
    data = as_dict(as_dict(status).get("rateLimitResetCredits"))
    raw = data.get("credits")
    credits = [
        ResetCredit(
            id=c.get("id"),
            title=c.get("title"),
            description=c.get("description"),
            status=c.get("status"),
            reset_type=c.get("resetType"),
            granted_at=numeric(c.get("grantedAt")),
            expires_at=numeric(c.get("expiresAt")),
        )
        for c in (raw if isinstance(raw, list) else [])
        if isinstance(c, dict)
    ]

    count = numeric(data.get("availableCount"))
    if count is None:
        count = float(len([c for c in credits if c.available]))

    return ResetCredits(available_count=int(count), credits=credits)


def json_number(value: float | None) -> float | int | None:
    """Keep integral values integral in the report.

    Everything here is parsed through `float`, but emitting `10080.0` where the
    payload said `10080` -- and epoch seconds with a fractional part -- just
    makes the JSON noisier to read and to consume.
    """
    if value is None:
        return None
    return int(value) if float(value).is_integer() else value


def window_json(window: Window, *, full_pct: float = DEFAULT_FULL_PCT) -> dict:
    return {
        "key": window.key,
        "label": window.label,
        "windowDurationMins": json_number(window.duration_mins),
        "usedPercent": json_number(window.used_percent),
        "resetsAt": json_number(window.resets_at),
        "blocked": window.blocked(full_pct),
    }


def limit_json(view: LimitView, *, full_pct: float = DEFAULT_FULL_PCT) -> dict:
    state = block_state(view.raw, full_pct=full_pct)
    return {
        "limitId": view.limit_id,
        "limitName": view.limit_name,
        "normalModelSlug": view.normal_model_slug,
        "blocked": state.blocked,
        "reasons": state.reasons,
        "resetsAt": json_number(state.resets_at),
        "windows": [window_json(w, full_pct=full_pct) for w in view.windows],
    }


def reset_credits_json(credits: ResetCredits) -> dict:
    return {
        "availableCount": credits.available_count,
        "credits": [
            {
                "id": c.id,
                "title": c.title,
                "resetType": c.reset_type,
                "status": c.status,
                "grantedAt": json_number(c.granted_at),
                "expiresAt": json_number(c.expires_at),
            }
            for c in credits.credits
        ],
    }


def signals_json(limit: object, envelope: object = None) -> dict:
    data = as_dict(limit)
    outer = as_dict(envelope)

    def pick(key: str):
        if key in data:
            return data.get(key)
        return outer.get(key)

    return {
        "ordinaryUsageAllowed": pick("ordinaryUsageAllowed"),
        "spendControlReached": pick("spendControlReached"),
        "rateLimitReachedType": pick("rateLimitReachedType"),
        "individualLimit": pick("individualLimit"),
        "rateLimitUpsell": pick("rateLimitUpsell"),
    }
