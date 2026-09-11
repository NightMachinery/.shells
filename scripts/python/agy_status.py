#!/usr/bin/env python3
"""Show Antigravity's remaining quota, read straight from its own backend.

The other way to this number is `agy -p /usage`, which is what
[agfi:agy-status-slow] runs: correct, but it pays a full CLI start-up and
several sequential backend round trips for it. This asks the one endpoint
behind those numbers directly -- ``v1internal:retrieveUserQuotaSummary`` --
with the OAuth token `agy` keeps in the login Keychain.

The token is `agy`'s to manage: it refreshes it on startup and rewrites the
Keychain item. We only ever read it, and when it has expired we ask `agy` to
do the refresh for us (see `Relogin`) rather than touching the refresh token
ourselves. See docs/agy_status.md.
"""

from __future__ import annotations

import argparse
import base64
import binascii
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time
import urllib.error
import urllib.request
from dataclasses import dataclass, field
from datetime import datetime, timezone
from typing import Callable

from libs.common_sub_status import (
    DARK_THEME_DEFAULT,
    DARK_THEMES,
    LIGHT_THEME_DEFAULT,
    LIGHT_THEMES,
    Style,
    as_nonempty_str,
    build_style,
    env_first,
    format_relative,
    format_timestamp,
)

#: The hosts that answer the internal Code Assist API, in the order they are
#: tried. `agy` is reported to talk to the `daily-` one, but the plain host has
#: answered the same calls, so it leads -- reorder this tuple (or pass
#: `--host`) if that ever stops being true. A constant rather than a flag
#: default, so the fallback order is one edit in one place.
API_HOSTS = (
    "cloudcode-pa.googleapis.com",
    "daily-cloudcode-pa.googleapis.com",
)
QUOTA_PATH = "/v1internal:retrieveUserQuotaSummary"
LOAD_ASSIST_PATH = "/v1internal:loadCodeAssist"

#: The Keychain generic password `agy` writes its OAuth credential into.
KEYCHAIN_SERVICE = "gemini"
KEYCHAIN_ACCOUNT = "antigravity"
#: zalando/go-keyring (which the CLI uses) base64s any value that is not plain
#: ASCII and marks it with this prefix. Keychain hands the marker back too, so
#: it has to be stripped before the JSON underneath can be parsed.
KEYRING_BASE64_PREFIX = "go-keyring-base64:"

#: `security` exits with the low byte of the OSStatus, which is where these
#: otherwise baffling numbers come from: errSecItemNotFound is -25300, whose
#: low byte is 44; errSecInteractionNotAllowed is -25308, low byte 36;
#: userCanceled is -128, so 128. Spelled out the same way for
#: [agfi:wifi-password-get-darwin] and in claude_code_usage.py.
KEYCHAIN_NOT_FOUND = 44
KEYCHAIN_NO_GUI = 36
KEYCHAIN_USER_CANCELED = 128

#: What a GUI-detached process (anything over ssh) can do about a keychain it
#: is not allowed to see. [agfi:h-agy-status-run-direct] takes the first of
#: these automatically.
GUI_LESS_HINT = (
    "run the report through `brishz`, whose shells are attached to the GUI "
    "session (see docs/agy_status.md)"
)
LOGIN_HINT = "run `agy` once (or `/login` inside it) to refresh the credential"
#: Named in every fatal error: the slow path asks `agy` itself and so does not
#: depend on any of this script's guesses about the backend.
FALLBACK_HINT = "the `agy -p /usage` path is still there as `agy-status-slow`"

#: How an expired credential is refreshed without a human: print mode
#: `/usage` is a built-in `agy` answers locally, so it starts no agent turn and
#: spends no quota, while still doing the OAuth refresh every start-up does.
#: We never see the refresh token; `agy` rewrites the Keychain item and we read
#: it again. Deliberately NOT an oauth2.googleapis.com refresh grant: the
#: client id/secret the CLI registers is not something we know, and a wrong one
#: buys an `invalid_client` rather than a token.
RELOGIN_COMMAND = "agy"
RELOGIN_ARGV = ("-p", "/usage")
#: Generous: a cold start loads the whole CLI before it gets to the refresh.
RELOGIN_TIMEOUT_S = 90.0

#: What the CLI itself sends. The version is pinned rather than read from
#: `agy --version`, because shelling out to `agy` is the very cost this path
#: exists to avoid; bump it here if the backend ever starts caring.
ANTIGRAVITY_VERSION = "1.2.1"
CLIENT_METADATA = {
    "ideType": "ANTIGRAVITY",
    "platform": "MACOS",
    "pluginType": "GEMINI",
}
X_GOOG_API_CLIENT = "google-cloud-sdk vscode_cloudshelleditor/0.1"

#: The credential's field names, most specific first. Spelled out as lists
#: rather than as one name each because the item is `agy`'s private format: it
#: has been seen under snake_case, and a Go client marshalling the same struct
#: with different tags would hand us camelCase without warning. Cheap
#: insurance, and the alternative is a report that dies on a rename.
ACCESS_TOKEN_KEYS = ("access_token", "accessToken", "token")
EXPIRY_KEYS = ("expiry", "expiry_date", "expiryDate", "expires_at", "expiresAt")
EMAIL_KEYS = ("email", "account")
#: `quota_project_id` is a fallback rather than an equal: it is the project
#: quota is billed against, which is usually but not always the project the
#: summary is asked for.
PROJECT_KEYS = (
    "project_id",
    "projectId",
    "quota_project_id",
    "quotaProjectId",
)
TIER_KEYS = ("tier_display_name", "tierDisplayName", "tier")

#: Above this, a numeric timestamp cannot be seconds (it would be year 5138),
#: so it is the milliseconds a Google OAuth `expiry_date` is measured in.
EPOCH_MS_THRESHOLD = 1e11

#: Percent-remaining thresholds for the colour of the number, matching the
#: prose report of [agfi:h-agy-status-slow]. Presentation only: nothing
#: branches on them but the escape codes.
REMAINING_LOW_PCT = 10.0
REMAINING_MID_PCT = 33.0


class UsageError(RuntimeError):
    """Anything that stops us from printing a report, with the HTTP status if
    the backend is the one that said no."""

    def __init__(self, message: str, *, http_status: int | None = None) -> None:
        super().__init__(message)
        self.http_status = http_status


@dataclass
class TokenInfo:
    token: str
    expiry_s: float | None = None
    email: str | None = None
    project_id: str | None = None
    tier: str | None = None
    #: Where it came from, so an error can say which item is the stale one.
    source: str = "keychain"

    @property
    def expired(self) -> bool | None:
        """True, False, or None when the item carries no usable expiry."""
        if self.expiry_s is None:
            return None

        return self.expiry_s <= time.time()


@dataclass
class KeychainRead:
    #: The decoded credential, or the reason there is none. Absent item and
    #: failed read are deliberately different states: a locked keychain must
    #: not be reported as "you are not logged in".
    data: dict | None = None
    error: str | None = None


@dataclass
class QuotaRow:
    """One bucket, normalized the way the report and the JSON both want it."""

    group: str
    label: str
    bucket_id: str
    #: What is LEFT, not what is spent -- the opposite sense from Claude Code's
    #: utilization -- and the name says so, so nobody reads 98 as "nearly out".
    remaining_percent: float
    #: A count, when the bucket reports one. Beside the percent rather than
    #: instead of it, because no total comes with it to divide by.
    remaining_amount: float | None = None
    #: False when the percent is the proto3 default we assumed rather than a
    #: number the backend sent; the human report then shows the amount.
    remaining_percent_known: bool = True
    resets_at_s: float | None = None
    resets_at_iso: str = ""


@dataclass
class Report:
    rows: list[QuotaRow] = field(default_factory=list)
    email: str | None = None
    tier: str | None = None
    project_id: str | None = None
    host: str | None = None
    #: None, "refreshed" or "failed: <reason>".
    relogin: str | None = None


##
#: Reading the credential
##
def decode_keychain_value(raw: str) -> str:
    """Undoes go-keyring's base64 wrapper, if the value carries it."""
    text = raw.strip()
    if not text.startswith(KEYRING_BASE64_PREFIX):
        return text

    encoded = text[len(KEYRING_BASE64_PREFIX) :]
    try:
        return base64.b64decode(encoded).decode("utf-8", errors="replace")
    except (binascii.Error, ValueError) as exc:
        raise UsageError(
            f"Keychain item {KEYCHAIN_SERVICE!r} is marked base64 but does not "
            f"decode ({exc})"
        ) from exc


def read_keychain_item(*, service: str, account: str, timeout: float) -> KeychainRead:
    """The credential `agy` last wrote, or why we could not have it.

    The default keychain search list is used as-is: over ssh it collapses to
    the System keychain and this read cannot succeed at all, and the answer to
    that is to run the whole report in the garden rather than to hunt for the
    login keychain by path. [agfi:h-agy-status-run-direct] does exactly that.
    """
    command = [
        "security",
        "find-generic-password",
        "-s",
        service,
        "-a",
        account,
        "-w",
    ]

    try:
        proc = subprocess.run(
            command,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            timeout=timeout,
            check=False,
        )
    except subprocess.TimeoutExpired:
        #: Almost always an authorization dialog nobody answered: `security` is
        #: not the application that created the item, so the first read of it
        #: can prompt.
        return KeychainRead(
            error=(
                f"timed out after {timeout:g}s reading Keychain item {service!r} "
                "(an authorization prompt may be waiting for you)"
            )
        )
    except OSError as exc:
        return KeychainRead(error=f"could not run security: {exc}")

    if proc.returncode == KEYCHAIN_NOT_FOUND:
        return KeychainRead(
            error=(
                f"no Keychain item {service!r}/{account!r}; {LOGIN_HINT}"
            )
        )

    if proc.returncode == KEYCHAIN_NO_GUI:
        return KeychainRead(
            error=(
                f"Keychain item {service!r} needs authorization and macOS would "
                "not show a prompt, because this process is not attached to the "
                f"GUI session (security exit 36); {GUI_LESS_HINT}"
            )
        )

    if proc.returncode == KEYCHAIN_USER_CANCELED:
        return KeychainRead(
            error=(
                f"Keychain authorization for item {service!r} was cancelled "
                "(security exit 128)"
            )
        )

    if proc.returncode != 0:
        #: stderr only; stdout would be the secret.
        detail = [line for line in (proc.stderr or "").splitlines() if line.strip()]
        return KeychainRead(
            error=(
                f"Keychain item {service!r} could not be read "
                f"(security exit {proc.returncode})"
                + (f": {detail[-1].strip()}" if detail else "")
            )
        )

    try:
        payload = json.loads(decode_keychain_value(proc.stdout))
    except json.JSONDecodeError:
        return KeychainRead(error=f"Keychain item {service!r} does not contain JSON")

    if not isinstance(payload, dict):
        return KeychainRead(
            error=f"Keychain item {service!r} does not contain a JSON object"
        )

    return KeychainRead(data=payload)


def find_field(data: dict, keys: tuple[str, ...]) -> object | None:
    """The first of `keys` the credential carries, top level or one level in.

    One level in, because a keyring item that holds more than one credential
    wraps each in an object of its own, and there is no reason for us to care
    which of the two shapes we were handed.
    """
    for key in keys:
        value = data.get(key)
        if value not in (None, ""):
            return value

    for nested in data.values():
        if not isinstance(nested, dict):
            continue

        for key in keys:
            value = nested.get(key)
            if value not in (None, ""):
                return value

    return None


def token_info_from_payload(data: dict, *, source: str = "keychain") -> TokenInfo:
    """The fields we need out of `agy`'s credential blob.

    Pure, so it can be exercised on a fixture without a Keychain.
    """
    token = as_nonempty_str(find_field(data, ACCESS_TOKEN_KEYS))
    if token is None:
        #: The field NAMES, never a value: enough to see that the format has
        #: moved on, and nothing that should not be on a terminal.
        names = ", ".join(sorted(str(key) for key in data)) or "none"
        raise UsageError(
            f"the credential in {source} carries no access token "
            f"(fields present: {names}); {LOGIN_HINT}"
        )

    return TokenInfo(
        token=token,
        expiry_s=parse_time(find_field(data, EXPIRY_KEYS)),
        email=as_nonempty_str(find_field(data, EMAIL_KEYS)),
        project_id=as_nonempty_str(find_field(data, PROJECT_KEYS)),
        tier=as_nonempty_str(find_field(data, TIER_KEYS)),
        source=source,
    )


def get_token(*, service: str, account: str, timeout: float) -> TokenInfo:
    read = read_keychain_item(service=service, account=account, timeout=timeout)
    if read.error is not None:
        raise UsageError(read.error)

    assert read.data is not None
    return token_info_from_payload(read.data, source=f"keychain:{service}/{account}")


class Relogin:
    """Refreshes an expired Keychain credential by running `agy -p /usage`.

    The same shape as claude_code_usage.py's, and for the same reason: we never
    see the refresh token, but the CLI refreshes on every start-up, and print
    mode `/usage` is a built-in it answers without a model turn. So one such
    run costs nothing, leaves no conversation behind, and hands the next
    request a live credential.

    At most one attempt, ever: `run` is a no-op after the first call, so the
    401 retry in `fetch_quota_summary`'s caller cannot become a loop.
    """

    def __init__(
        self,
        *,
        enabled: bool,
        reread: Callable[[], TokenInfo],
        timeout_s: float = RELOGIN_TIMEOUT_S,
    ) -> None:
        self.enabled = enabled
        self.timeout_s = timeout_s
        self._reread = reread
        self.attempted = False
        #: None, "refreshed" or "failed: <reason>".
        self.status: str | None = None
        self.token_info: TokenInfo | None = None

    def refresh_if_expired(self, token_info: TokenInfo) -> TokenInfo | None:
        """Called before a fetch: spend the refresh rather than a certain 401."""
        if token_info.expired is not True:
            return None

        return self.run(token_info)

    def run(self, token_info: TokenInfo) -> TokenInfo | None:
        if self.attempted:
            return None
        self.attempted = True

        if not self.enabled:
            #: Nothing was tried, so there is nothing to report: LOGIN_HINT
            #: already says what to do by hand.
            return None

        binary = shutil.which(RELOGIN_COMMAND)
        if binary is None:
            self.status = f"failed: {RELOGIN_COMMAND!r} is not on PATH"
            return None

        ok, reason = self._invoke(binary)
        if not ok:
            self.status = f"failed: {reason}"
            return None

        try:
            info = self._reread()
        except UsageError as exc:
            self.status = f"failed: no credential after the refresh ({exc})"
            return None

        if info.expired is True:
            #: The command ran and the credential is still stale, which is a
            #: different problem from the command failing -- say which.
            self.status = "failed: the credential is still expired afterwards"
            return None

        self.status = "refreshed"
        self.token_info = info
        return info

    def _invoke(self, binary: str) -> tuple[bool, str]:
        env = os.environ.copy()
        #: Agent CLIs rename the tmux session they find themselves in, and this
        #: child is not the caller's session.
        env.pop("TMUX", None)
        env.pop("TMUX_PANE", None)

        #: A fresh directory: Antigravity pairs a conversation to its working
        #: directory, so the child neither picks up the configuration of
        #: whatever project we are standing in nor leaves anything against it.
        workdir = tempfile.mkdtemp(prefix="agy-status-relogin-")
        command = f"{RELOGIN_COMMAND} {' '.join(RELOGIN_ARGV)}"
        try:
            proc = subprocess.run(
                [binary, *RELOGIN_ARGV],
                cwd=workdir,
                env=env,
                stdin=subprocess.DEVNULL,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                timeout=self.timeout_s,
                check=False,
            )
        except subprocess.TimeoutExpired:
            return False, f"`{command}` timed out after {self.timeout_s:g}s"
        except OSError as exc:
            return False, f"could not run {RELOGIN_COMMAND!r}: {exc}"
        finally:
            shutil.rmtree(workdir, ignore_errors=True)

        #: Captured rather than shown: the report *is* the output here, and a
        #: second rendering of `/usage` in the middle of it is noise. DEBUGME
        #: is how the rest of this repo asks to see the details anyway.
        if os.environ.get("DEBUGME"):
            for stream, text in (("stdout", proc.stdout), ("stderr", proc.stderr)):
                for line in (text or "").splitlines():
                    print(f"agy_status: relogin {stream}: {line}", file=sys.stderr)

        if proc.returncode != 0:
            lines = [ln.strip() for ln in (proc.stderr or "").splitlines() if ln.strip()]
            if not lines:
                lines = [
                    ln.strip() for ln in (proc.stdout or "").splitlines() if ln.strip()
                ]
            detail = f": {lines[-1][:200]}" if lines else ""
            return False, f"`{command}` exited {proc.returncode}{detail}"

        return True, ""


##
#: Talking to the backend
##
def request_headers(token: str) -> dict[str, str]:
    return {
        "Authorization": f"Bearer {token}",
        "Content-Type": "application/json",
        "User-Agent": f"Antigravity/{ANTIGRAVITY_VERSION} darwin/arm64",
        "X-Goog-Api-Client": X_GOOG_API_CLIENT,
        "Client-Metadata": json.dumps(CLIENT_METADATA, separators=(",", ":")),
    }


def post_json(
    *, host: str, api_path: str, body: dict, token: str, timeout: float
) -> dict:
    url = f"https://{host}{api_path}"
    request = urllib.request.Request(
        url,
        data=json.dumps(body).encode("utf-8"),
        headers=request_headers(token),
        method="POST",
    )

    try:
        with urllib.request.urlopen(request, timeout=timeout) as response:
            raw = response.read().decode("utf-8", errors="replace")
    except urllib.error.HTTPError as exc:
        detail = ""
        try:
            detail = exc.read().decode("utf-8", errors="replace")[:200]
        except OSError:
            pass

        message = f"{host}{api_path} returned HTTP {exc.code}"
        if exc.code == 401:
            message += f" (credential rejected); {LOGIN_HINT}"
        if detail:
            message += f": {detail}"
        raise UsageError(message, http_status=exc.code) from exc
    except (urllib.error.URLError, TimeoutError, OSError) as exc:
        raise UsageError(f"{host} unreachable: {exc}") from exc

    return parse_json_object(raw, where=f"{host}{api_path}")


def parse_json_object(raw: str, *, where: str) -> dict:
    """A JSON object, or an error that quotes what came back instead.

    An HTML error page or a proxy's login form is the common way this fails,
    and the first 200 characters of it say far more than "invalid JSON" does.
    """
    try:
        payload = json.loads(raw)
    except json.JSONDecodeError:
        raise UsageError(f"{where} returned invalid JSON: {raw[:200]!r}") from None

    if not isinstance(payload, dict):
        raise UsageError(f"{where} returned non-object JSON: {raw[:200]!r}")

    return payload


def hosts_to_try(host: str | None) -> tuple[str, ...]:
    return (host,) if host else API_HOSTS


def post_json_over_hosts(
    *, hosts: tuple[str, ...], api_path: str, body: dict, token: str, timeout: float
) -> tuple[dict, str]:
    """The first host that answers, and which one it was.

    A 401 stops the walk: that is the credential being refused, and asking a
    second host with the same token only turns one clear error into two.
    """
    last: UsageError | None = None
    for host in hosts:
        try:
            return post_json(
                host=host, api_path=api_path, body=body, token=token, timeout=timeout
            ), host
        except UsageError as exc:
            if exc.http_status == 401:
                raise

            last = exc

    assert last is not None
    raise last


def load_project_id(
    *, hosts: tuple[str, ...], token: str, timeout: float
) -> tuple[str | None, str | None]:
    """The project id from `loadCodeAssist`, for a credential that lacks one."""
    payload, host = post_json_over_hosts(
        hosts=hosts,
        api_path=LOAD_ASSIST_PATH,
        body={"metadata": dict(CLIENT_METADATA)},
        token=token,
        timeout=timeout,
    )

    project = payload.get("cloudaicompanionProject")
    #: The field has been seen both as a bare name and as a resource object.
    if isinstance(project, dict):
        project = project.get("id") or project.get("name")

    return as_nonempty_str(project), host


##
#: Response -> rows. Pure from here down: everything below takes a parsed dict
#: and returns data, so it is testable with no network and no Keychain.
##
def parse_time(value: object) -> float | None:
    """Epoch seconds from an RFC-3339 string (or a number that already is one)."""
    if isinstance(value, (int, float)) and not isinstance(value, bool):
        number = float(value)
        #: A Google OAuth `expiry_date` is in milliseconds, an `expiry` in
        #: seconds, and both arrive as bare numbers. Scale is the only thing
        #: that tells them apart.
        return number / 1000.0 if number >= EPOCH_MS_THRESHOLD else number

    text = as_nonempty_str(value)
    if text is None:
        return None

    candidate = text.strip()
    if candidate.endswith(("Z", "z")):
        candidate = f"{candidate[:-1]}+00:00"

    #: Protobuf timestamps carry up to nine fractional digits; `fromisoformat`
    #: takes at most six.
    trimmed = re.match(r"^(.*\.\d{6})\d+(.*)$", candidate)
    if trimmed is not None:
        candidate = f"{trimmed.group(1)}{trimmed.group(2)}"

    try:
        parsed = datetime.fromisoformat(candidate)
    except ValueError:
        return None

    if parsed.tzinfo is None:
        #: The API answers in UTC; a naive string is that, not local time.
        parsed = parsed.replace(tzinfo=timezone.utc)

    return parsed.timestamp()


def as_number(value: object) -> float | None:
    if isinstance(value, bool) or value is None:
        return None

    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def row_from_bucket(bucket: dict, *, group_name: str | None = None) -> QuotaRow | None:
    """One bucket, or None when it is disabled and should not be reported."""
    if bucket.get("disabled"):
        return None

    bucket_id = as_nonempty_str(bucket.get("bucketId")) or ""
    bucket_label = as_nonempty_str(bucket.get("displayName")) or bucket_id or "quota"
    #: A legacy flat bucket has no group above it, so it is its own group. Then
    #: the line reads the same as a grouped one rather than losing a column.
    group = group_name or bucket_label

    fraction = as_number(bucket.get("remainingFraction"))
    amount = as_number(bucket.get("remainingAmount"))

    if fraction is not None:
        remaining_pct = fraction * 100.0
        known = True
    else:
        #: proto3 omits default values, and a full bucket comes back with
        #: neither field. So an absent fraction is "nothing spent", NOT zero --
        #: reading it as zero would report a full quota as exhausted and arm a
        #: notifier for a reset that changes nothing.
        remaining_pct = 100.0
        #: With an amount but no total to divide it by there is no percentage
        #: to be had, so the report shows the count and says nothing it cannot
        #: back up.
        known = amount is None

    reset_iso = as_nonempty_str(bucket.get("resetTime")) or ""

    return QuotaRow(
        group=group,
        label=bucket_label,
        bucket_id=bucket_id,
        remaining_percent=remaining_pct,
        remaining_amount=amount,
        remaining_percent_known=known,
        resets_at_s=parse_time(reset_iso),
        resets_at_iso=reset_iso,
    )


def rows_from_payload(payload: dict) -> list[QuotaRow]:
    """Every reportable bucket in a `retrieveUserQuotaSummary` response.

    Grouped buckets first, then the legacy flat `buckets` list for anything
    the groups did not already carry -- the two have overlapped during
    upstream's migration, and a bucket id shown twice is worse than a field
    read from the older shape.
    """
    rows: list[QuotaRow] = []
    seen: set[str] = set()

    def add(row: QuotaRow | None) -> None:
        if row is None:
            return
        if row.bucket_id and row.bucket_id in seen:
            return
        if row.bucket_id:
            seen.add(row.bucket_id)
        rows.append(row)

    groups = payload.get("groups")
    if isinstance(groups, list):
        for group in groups:
            if not isinstance(group, dict):
                continue

            group_name = as_nonempty_str(group.get("displayName"))
            buckets = group.get("buckets")
            if not isinstance(buckets, list):
                continue

            for bucket in buckets:
                if isinstance(bucket, dict):
                    add(row_from_bucket(bucket, group_name=group_name))

    flat = payload.get("buckets")
    if isinstance(flat, list):
        for bucket in flat:
            if isinstance(bucket, dict):
                add(row_from_bucket(bucket))

    return rows


def row_to_json(row: QuotaRow) -> dict:
    """The object shape [agfi:h-agy-status-arm-deadline] parses.

    `group`, `label`, `remaining_percent`, `resets_at` and `resets_at_iso` are
    the contract, identical to what the slow path emits; `bucket_id` and
    `remaining_amount` are extra and every existing consumer ignores them.
    """
    return {
        "group": row.group,
        "label": row.label,
        "remaining_percent": round(row.remaining_percent, 4),
        "resets_at": row.resets_at_s,
        "resets_at_iso": row.resets_at_iso,
        "bucket_id": row.bucket_id,
        "remaining_amount": row.remaining_amount,
    }


##
#: Rendering
##
def format_remaining(style: Style, row: QuotaRow) -> str:
    if not row.remaining_percent_known and row.remaining_amount is not None:
        return f"{style.green(f'{row.remaining_amount:g}')} remaining"

    pct = row.remaining_percent
    text = f"{pct:g}%"
    if pct <= REMAINING_LOW_PCT:
        colored = style.red(text)
    elif pct <= REMAINING_MID_PCT:
        colored = style.yellow(text)
    else:
        colored = style.green(text)

    return f"{colored} remaining"


def render_row(style: Style, row: QuotaRow) -> str:
    head = style.bold(row.group)
    if row.label and row.label != row.group:
        head = f"{head} ({row.label})"

    if row.resets_at_s is None:
        when = style.dim("reset time unknown")
    elif row.resets_at_s > time.time():
        when = (
            f"resets {style.reset_time(format_timestamp(row.resets_at_s))} "
            f"({format_relative(row.resets_at_s)})"
        )
    else:
        #: The backend keeps reporting a window's end after it has passed,
        #: until the next roll-over moves it.
        when = f"reset {format_relative(row.resets_at_s)}"

    return f"{head}: {format_remaining(style, row)}, {when}"


def render_human(style: Style, report: Report) -> str:
    return "\n".join(render_row(style, row) for row in report.rows)


##
#: Wiring
##
def gather(args: argparse.Namespace) -> Report:
    timeout = args.timeout

    def read() -> TokenInfo:
        return get_token(
            service=args.keychain_service,
            account=args.keychain_account,
            timeout=timeout,
        )

    token_info = read()
    relogin = Relogin(enabled=args.relogin, reread=read)

    refreshed = relogin.refresh_if_expired(token_info)
    if refreshed is not None:
        token_info = refreshed

    hosts = hosts_to_try(args.host)
    report = Report(email=token_info.email, tier=token_info.tier)

    while True:
        try:
            project_id = token_info.project_id
            host: str | None = None
            if project_id is None:
                #: Only when the credential does not name one: this is a second
                #: round trip, and the whole point of the direct path is to
                #: make as few as possible.
                project_id, host = load_project_id(
                    hosts=hosts, token=token_info.token, timeout=timeout
                )

            if project_id is None:
                raise UsageError(
                    "no project id: the credential names none and "
                    f"loadCodeAssist returned none either; {FALLBACK_HINT}"
                )

            payload, host = post_json_over_hosts(
                hosts=hosts,
                api_path=QUOTA_PATH,
                body={"project": project_id},
                token=token_info.token,
                timeout=timeout,
            )
        except UsageError as exc:
            #: A 401 on a credential that looked live is the other way a stale
            #: token shows up -- the expiry is only what the item claims. The
            #: relogin refuses after one attempt, so this retries at most once
            #: and cannot loop.
            if exc.http_status == 401:
                retried = relogin.run(token_info)
                if retried is not None:
                    token_info = retried
                    continue

            raise

        break

    report.project_id = project_id
    report.host = host
    report.relogin = relogin.status
    report.rows = rows_from_payload(payload)
    if not report.rows:
        raise UsageError(
            f"no quota buckets in the {QUOTA_PATH} response; {FALLBACK_HINT}"
        )

    return report


def parse_timeout_default() -> float:
    raw = env_first(
        "agy_status_direct_timeout_s", "AGY_STATUS_DIRECT_TIMEOUT_S", default="20"
    )
    assert raw is not None

    try:
        return float(raw)
    except ValueError:
        return 20.0


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Show Antigravity's remaining quota per bucket, read directly from "
            "the Code Assist backend rather than by starting `agy`."
        )
    )
    parser.add_argument(
        "--json",
        action=argparse.BooleanOptionalAction,
        default=False,
        help="Output a JSON array of buckets (default: %(default)s).",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=parse_timeout_default(),
        help="Timeout in seconds for each request (default: %(default)s).",
    )
    parser.add_argument(
        "--host",
        default=None,
        help=(
            "Force one API host instead of trying "
            f"{', '.join(API_HOSTS)} in order (default: %(default)s)."
        ),
    )
    parser.add_argument(
        "--relogin",
        action=argparse.BooleanOptionalAction,
        default=True,
        help=(
            "On an expired credential, run `agy -p /usage` once to refresh it "
            "(default: %(default)s)."
        ),
    )
    parser.add_argument(
        "--keychain-service",
        default=KEYCHAIN_SERVICE,
        help="Keychain generic-password service (default: %(default)s).",
    )
    parser.add_argument(
        "--keychain-account",
        default=KEYCHAIN_ACCOUNT,
        help="Keychain generic-password account (default: %(default)s).",
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
        report = gather(args)
    except UsageError as exc:
        #: Never a silent fall back to the slow path: a direct path that
        #: quietly becomes the slow one hides its own breakage for months. Say
        #: what broke and name the way round it.
        print(f"agy_status: {exc}", file=sys.stderr)
        if FALLBACK_HINT not in str(exc):
            print(f"agy_status: {FALLBACK_HINT}", file=sys.stderr)
        return 1

    if report.relogin is not None:
        print(f"agy_status: relogin {report.relogin}", file=sys.stderr)

    if args.json:
        print(json.dumps([row_to_json(row) for row in report.rows], indent=2))
    else:
        print(render_human(style, report))

    return 0


##
#: @tests -- everything from `parse_time` down is pure, so the mapping,
#: the JSON shape and the rendering all run on a fixture with no network and
#: no Keychain:
#:
#:   cd "${NIGHTDIR}/python" && python3 -c '
#:   import agy_status as a
#:   rows = a.rows_from_payload({"groups": [{"displayName": "Gemini Models", "buckets": [
#:       {"bucketId": "gemini-5h", "resetTime": "2026-09-17T11:27:33Z"},
#:       {"bucketId": "gemini-weekly", "remainingFraction": 0.5, "disabled": True}]}]})
#:   print([(r.group, r.bucket_id, r.remaining_percent) for r in rows])'
#:
#: prints one row at 100.0 -- proto3 omits a full bucket's fields, so an
#: absent remaining is "nothing spent" -- and drops the disabled bucket.
#: `python3 -m py_compile agy_status.py` and `agy_status.py --help` need no
#: credential either.
##
if __name__ == "__main__":
    raise SystemExit(main())
