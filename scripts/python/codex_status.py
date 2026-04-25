#!/usr/bin/env python3
##
from __future__ import annotations

import argparse
import base64
import json
import os
import select
import subprocess
import sys
import time
from datetime import datetime
from pathlib import Path


AUTH_CLAIM = "https://api.openai.com/auth"


class CodexStatusError(RuntimeError):
    pass


class Style:
    def __init__(self, enabled: bool):
        self.enabled = enabled

    def _wrap(self, text: str, code: str) -> str:
        if not self.enabled:
            return text
        return f"\033[{code}m{text}\033[0m"

    def bold(self, text: str) -> str:
        return self._wrap(text, "1")

    def cyan(self, text: str) -> str:
        return self._wrap(text, "36")

    def magenta(self, text: str) -> str:
        return self._wrap(text, "35")

    def green(self, text: str) -> str:
        return self._wrap(text, "32")

    def yellow(self, text: str) -> str:
        return self._wrap(text, "33")

    def red(self, text: str) -> str:
        return self._wrap(text, "31")

    def dim(self, text: str) -> str:
        return self._wrap(text, "2")


def env_first(*names: str, default: str | None = None) -> str | None:
    for name in names:
        value = os.environ.get(name)
        if value not in (None, ""):
            return value
    return default


def parse_timeout() -> float:
    raw = env_first("codex_status_timeout_s", "CODEX_STATUS_TIMEOUT_S", default="20")
    assert raw is not None
    try:
        return float(raw)
    except ValueError:
        return 20.0


def parse_args() -> tuple[argparse.Namespace, list[str]]:
    parser = argparse.ArgumentParser(
        description="Read Codex account identity and rate limits via codex app-server."
    )
    parser.add_argument("--json", action="store_true", help="Output JSON response.")
    parser.add_argument(
        "--timeout",
        type=float,
        default=parse_timeout(),
        help="Timeout in seconds (default: env codex_status_timeout_s or 20).",
    )
    parser.add_argument(
        "--profile",
        default=env_first("codex_status_profile", "CODEX_STATUS_PROFILE", default=""),
        help="Codex profile name.",
    )
    parser.add_argument(
        "--cd",
        dest="cd_dir",
        default=env_first(
            "codex_status_cd",
            "CODEX_STATUS_CD",
            default=os.path.join(os.path.expanduser("~"), "tmp"),
        ),
        help="Working directory for codex.",
    )
    parser.add_argument(
        "--color",
        choices=("auto", "always", "never"),
        default="auto",
        help="Color mode for human-readable output.",
    )
    parser.add_argument(
        "--codex-arg",
        action="append",
        default=[],
        metavar="ARG",
        help="Extra argument forwarded to codex before app-server.",
    )

    args, unknown = parser.parse_known_args()
    return args, unknown


def build_codex_cmd(args: argparse.Namespace, passthrough: list[str]) -> list[str]:
    cmd = ["codex"]
    if args.profile:
        cmd.extend(["--profile", args.profile])
    if args.cd_dir:
        cmd.extend(["--cd", args.cd_dir])
    cmd.extend(args.codex_arg)
    cmd.extend(passthrough)
    cmd.append("app-server")
    return cmd


def send_json(proc: subprocess.Popen[str], obj: dict) -> None:
    assert proc.stdin is not None
    payload = json.dumps(obj, separators=(",", ":"))
    proc.stdin.write(payload + "\n")
    proc.stdin.flush()


def recv_json_response(
    proc: subprocess.Popen[str], *, wanted_id: str, deadline: float
) -> dict | None:
    assert proc.stdout is not None
    wanted_id = str(wanted_id)

    while time.monotonic() < deadline:
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            break

        readable, _, _ = select.select([proc.stdout], [], [], min(0.25, remaining))
        if not readable:
            if proc.poll() is not None:
                break
            continue

        line = proc.stdout.readline()
        if line == "":
            if proc.poll() is not None:
                break
            continue

        line = line.strip()
        if not line:
            continue

        try:
            msg = json.loads(line)
        except json.JSONDecodeError:
            continue

        if str(msg.get("id")) == wanted_id:
            return msg

    return None


def rpc_request(
    proc: subprocess.Popen[str],
    *,
    method: str,
    request_id: str,
    deadline: float,
    params: object | None = None,
    include_params: bool = True,
) -> dict:
    req: dict[str, object] = {
        "method": method,
        "id": request_id,
    }
    if include_params:
        req["params"] = params

    send_json(proc, req)

    resp = recv_json_response(proc, wanted_id=request_id, deadline=deadline)
    if resp is None:
        raise CodexStatusError(f"codex-status: {method} timed out")

    if "error" in resp:
        raise CodexStatusError(
            f"codex-status: {method} failed: "
            + json.dumps(resp["error"], ensure_ascii=False)
        )

    result = resp.get("result", {})
    return result if isinstance(result, dict) else {}


def try_rpc_request(
    proc: subprocess.Popen[str],
    *,
    method: str,
    request_id: str,
    deadline: float,
    params: object | None = None,
    include_params: bool = True,
) -> dict:
    try:
        return rpc_request(
            proc,
            method=method,
            request_id=request_id,
            deadline=deadline,
            params=params,
            include_params=include_params,
        )
    except CodexStatusError:
        return {}


def get_path(obj: object, path: tuple[str, ...]) -> object | None:
    cur: object = obj

    for key in path:
        if not isinstance(cur, dict):
            return None

        cur = cur.get(key)
        if cur in (None, ""):
            return None

    return cur


def first_path(obj: object, *paths: tuple[str, ...]) -> object | None:
    for path in paths:
        value = get_path(obj, path)
        if value not in (None, ""):
            return value

    return None


def as_nonempty_str(value: object | None) -> str | None:
    if value in (None, ""):
        return None

    text = str(value).strip()
    return text or None


def format_timestamp(epoch_s: int | float | None) -> str:
    if epoch_s is None:
        return "n/a"

    try:
        dt = datetime.fromtimestamp(float(epoch_s)).astimezone()
    except (OverflowError, OSError, ValueError, TypeError):
        return f"{epoch_s}"

    return dt.strftime("%Y-%m-%d %H:%M:%S %Z")


def format_relative(epoch_s: int | float | None) -> str:
    if epoch_s is None:
        return "n/a"

    try:
        delta = int(round(float(epoch_s) - time.time()))
    except (TypeError, ValueError):
        return "n/a"

    past = delta < 0
    delta = abs(delta)

    weeks, rem = divmod(delta, 7 * 24 * 3600)
    days, rem = divmod(rem, 24 * 3600)
    hours, rem = divmod(rem, 3600)
    mins, secs = divmod(rem, 60)

    parts: list[str] = []
    if weeks:
        parts.append(f"{weeks}w")
    if days:
        parts.append(f"{days}d")
    if hours:
        parts.append(f"{hours}h")
    if mins:
        parts.append(f"{mins}m")
    if secs and not parts:
        parts.append(f"{secs}s")
    if not parts:
        parts.append("0s")

    text = " ".join(parts)
    return f"{text} ago" if past else f"in {text}"


def format_used_percent(style: Style, used: object | None) -> str:
    if used is None:
        return "n/a"

    try:
        pct = float(used)
    except (TypeError, ValueError):
        return "n/a"

    text = f"{pct:g}%"
    if pct >= 90:
        return style.red(text)
    if pct >= 75:
        return style.yellow(text)
    return style.green(text)


def format_window(style: Style, label: str, window: dict | None) -> str:
    if not isinstance(window, dict):
        return f"{label}: {style.dim('n/a')}"

    used = window.get("usedPercent")
    resets_at = window.get("resetsAt")
    duration_mins = window.get("windowDurationMins")

    bits = [f"{label}: {format_used_percent(style, used)} used"]
    if resets_at is None:
        bits.append(f"resets {style.dim('n/a')}")
    else:
        bits.append(f"resets {format_relative(resets_at)} ({format_timestamp(resets_at)})")
    if duration_mins is not None:
        bits.append(f"window {duration_mins}m")

    return " | ".join(bits)


def format_credits(style: Style, credits: dict | None) -> str:
    if not isinstance(credits, dict):
        return style.dim("n/a")

    unlimited = credits.get("unlimited")
    has_credits = credits.get("hasCredits")
    balance = credits.get("balance")

    if unlimited:
        base = style.green("unlimited")
    elif has_credits:
        base = style.green("available")
    else:
        base = style.red("none")

    if balance not in (None, ""):
        return f"{base} (balance: {balance})"
    return base


def decode_jwt_payload(token: str) -> dict | None:
    parts = token.split(".")
    if len(parts) < 2:
        return None

    payload = parts[1]
    payload += "=" * (-len(payload) % 4)

    try:
        raw = base64.urlsafe_b64decode(payload.encode("ascii"))
        data = json.loads(raw.decode("utf-8"))
    except (ValueError, json.JSONDecodeError):
        return None

    return data if isinstance(data, dict) else None


def read_json_file(path: Path) -> dict | None:
    try:
        with path.open("r", encoding="utf-8") as f:
            data = json.load(f)
    except (OSError, json.JSONDecodeError):
        return None

    return data if isinstance(data, dict) else None


def collect_tokens(obj: object) -> list[str]:
    tokens: list[str] = []
    seen: set[str] = set()

    def add(value: object | None) -> None:
        if not isinstance(value, str) or not value or value in seen:
            return
        seen.add(value)
        tokens.append(value)

    # Prefer ID tokens because they normally contain identity claims.
    explicit_paths = [
        ("tokens", "id_token"),
        ("id_token",),
        ("tokens", "access_token"),
        ("access_token",),
    ]
    for path in explicit_paths:
        add(get_path(obj, path))

    def walk(value: object) -> None:
        if isinstance(value, dict):
            for key, child in value.items():
                if key in {"id_token", "access_token"}:
                    add(child)
                walk(child)
        elif isinstance(value, list):
            for child in value:
                walk(child)

    walk(obj)
    return tokens


def auth_file_candidates(codex_home: object | None) -> list[Path]:
    homes = [
        as_nonempty_str(codex_home),
        env_first("CODEX_HOME", "codex_home"),
        os.path.join(os.path.expanduser("~"), ".codex"),
    ]

    out: list[Path] = []
    seen: set[str] = set()

    for home in homes:
        if not home:
            continue

        path = Path(home).expanduser() / "auth.json"
        key = str(path)
        if key in seen:
            continue
        seen.add(key)
        out.append(path)

    return out


def auth_file_bytes(path: Path) -> bytes | None:
    try:
        return path.read_bytes()
    except OSError:
        return None


def active_auth_file(codex_home: object | None) -> Path | None:
    for path in auth_file_candidates(codex_home):
        if path.exists():
            return path

    return None


def workspace_name_from_auth_file_alias(codex_home: object | None) -> str | None:
    """Infer a local workspace alias from auth_<alias>.json backups.

    Codex's app-server currently exposes the ChatGPT workspace/account ID but
    not its display name. Some local setups keep per-workspace auth snapshots
    named like auth_<workspace>.json; when auth.json is byte-identical to one
    of those snapshots, use that alias as a local fallback. This avoids
    presenting token organization titles such as "Personal" as the active
    Codex workspace name for Team/Enterprise accounts.
    """

    active = active_auth_file(codex_home)
    if active is None:
        return None

    active_bytes = auth_file_bytes(active)
    if active_bytes is None:
        return None

    matches: list[str] = []
    for path in sorted(active.parent.glob("auth_*.json")):
        if path.name == "auth.json":
            continue
        if auth_file_bytes(path) != active_bytes:
            continue

        alias = path.stem.removeprefix("auth_")
        alias = alias.strip()
        if alias:
            matches.append(alias)

    if not matches:
        return None

    # Multiple aliases for the exact same auth state are possible. Pick a
    # stable, deterministic value rather than guessing from non-identical files.
    return matches[0]


def read_local_auth_claims(codex_home: object | None) -> dict:
    for path in auth_file_candidates(codex_home):
        data = read_json_file(path)
        if not data:
            continue

        for token in collect_tokens(data):
            claims = decode_jwt_payload(token)
            if claims:
                return claims

    return {}


def workspace_name_from_claims(claims: dict) -> str | None:
    auth_claim = claims.get(AUTH_CLAIM)
    auth = auth_claim if isinstance(auth_claim, dict) else {}

    direct = first_path(
        auth,
        ("workspaceName",),
        ("workspace_name",),
        ("workspace", "name"),
        ("organization", "name"),
        ("organization", "title"),
        ("default_organization", "name"),
        ("default_organization", "title"),
    )
    direct_text = as_nonempty_str(direct)
    if direct_text and direct_text.lower() != "personal":
        return direct_text

    orgs = auth.get("organizations")
    if not isinstance(orgs, list):
        orgs = claims.get("organizations")

    if not isinstance(orgs, list):
        return None

    selected: dict | None = None
    for org in orgs:
        if isinstance(org, dict) and org.get("is_default"):
            selected = org
            break

    if selected is None:
        for org in orgs:
            if isinstance(org, dict):
                selected = org
                break

    if selected is None:
        return None

    name = as_nonempty_str(
        selected.get("title")
        or selected.get("name")
        or selected.get("display_name")
        or selected.get("displayName")
    )

    if name and name.lower() != "personal":
        return name

    return None


def build_identity(
    rate_limits: dict,
    account_result: dict,
    rate_result: dict,
    auth_claims: dict,
    codex_home: object | None = None,
) -> dict[str, str]:
    account_obj = account_result.get("account")
    account = account_obj if isinstance(account_obj, dict) else {}

    plan = as_nonempty_str(
        rate_limits.get("planType")
        or rate_limits.get("plan_type")
        or first_path(account, ("planType",), ("plan_type",))
        or first_path(auth_claims, (AUTH_CLAIM, "chatgpt_plan_type"))
    )

    # There is no documented separate "workspace owner email" in the app-server
    # rate-limit response. account/read exposes the authenticated ChatGPT email.
    plan_owner_email = as_nonempty_str(
        first_path(account, ("email",))
        or first_path(account_result, ("email",))
        or first_path(rate_result, ("account", "email"))
        or first_path(auth_claims, ("email",), (AUTH_CLAIM, "email"))
    )

    workspace_name = as_nonempty_str(
        first_path(
            account,
            ("workspaceName",),
            ("workspace_name",),
            ("workspace", "name"),
            ("organization", "name"),
            ("organization", "title"),
        )
        or first_path(
            rate_result,
            ("workspaceName",),
            ("workspace_name",),
            ("workspace", "name"),
            ("organization", "name"),
            ("organization", "title"),
        )
        or workspace_name_from_claims(auth_claims)
        or workspace_name_from_auth_file_alias(codex_home)
    )

    identity: dict[str, str] = {}
    if plan:
        identity["planType"] = plan
    if plan_owner_email:
        identity["planOwnerEmail"] = plan_owner_email
    if workspace_name:
        identity["workspaceName"] = workspace_name

    return identity


def print_human(
    rate_limits: dict,
    color_mode: str,
    *,
    account_result: dict,
    rate_result: dict,
    auth_claims: dict,
    codex_home: object | None = None,
) -> None:
    use_color = color_mode == "always" or (
        color_mode == "auto" and sys.stdout.isatty()
    )
    style = Style(use_color)

    identity = build_identity(
        rate_limits, account_result, rate_result, auth_claims, codex_home
    )

    plan = identity.get("planType") or "unknown"
    plan_owner_email = identity.get("planOwnerEmail")
    workspace_name = identity.get("workspaceName")

    primary = rate_limits.get("primary")
    secondary = rate_limits.get("secondary")
    credits = rate_limits.get("credits")

    print(style.bold(style.cyan("Codex rate limits")))
    print(f"Plan: {style.magenta(str(plan)) if plan != 'unknown' else plan}")
    print(f"Plan owner: {plan_owner_email if plan_owner_email else style.dim('n/a')}")
    print(
        f"Workspace: {style.magenta(workspace_name) if workspace_name else style.dim('n/a')}"
    )
    print(format_window(style, "Primary", primary))
    print(format_window(style, "Secondary", secondary))
    print(f"Credits: {format_credits(style, credits)}")


def run() -> int:
    args, passthrough = parse_args()
    cmd = build_codex_cmd(args, passthrough)
    deadline = time.monotonic() + max(1.0, args.timeout)

    try:
        proc = subprocess.Popen(
            cmd,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=1,
        )
    except FileNotFoundError:
        print("codex-status: codex not found in PATH", file=sys.stderr)
        return 127

    stderr_text = ""
    failed = False

    try:
        send_json(
            proc,
            {
                "method": "initialize",
                "id": "1",
                "params": {
                    "clientInfo": {
                        "name": "codex-status",
                        "version": "1",
                    },
                    "capabilities": {
                        "experimentalApi": True,
                    },
                },
            },
        )

        init_resp = recv_json_response(proc, wanted_id="1", deadline=deadline)
        if init_resp is None:
            raise CodexStatusError("codex-status: initialize timed out")
        if "error" in init_resp:
            raise CodexStatusError(
                "codex-status: initialize failed: "
                + json.dumps(init_resp["error"], ensure_ascii=False)
            )

        init_result = init_resp.get("result", {})
        if not isinstance(init_result, dict):
            init_result = {}

        send_json(proc, {"method": "initialized"})

        # Identity lives here, not in account/rateLimits/read.
        # Keep it nonfatal so rate limits still print on older/broken builds.
        account_deadline = min(deadline, time.monotonic() + max(1.0, min(5.0, args.timeout / 3)))
        account_result = try_rpc_request(
            proc,
            method="account/read",
            request_id="2",
            deadline=account_deadline,
            params={"refreshToken": False},
        )

        rate_result = rpc_request(
            proc,
            method="account/rateLimits/read",
            request_id="3",
            deadline=deadline,
            params=None,
        )

        rate_limits = rate_result.get("rateLimits", rate_result)
        if not isinstance(rate_limits, dict):
            rate_limits = {}

        codex_home = init_result.get("codexHome")
        auth_claims = read_local_auth_claims(codex_home)

        if args.json:
            output = dict(rate_result)
            if account_result.get("account") is not None:
                output["account"] = account_result.get("account")

            identity = build_identity(
                rate_limits, account_result, rate_result, auth_claims, codex_home
            )
            if identity:
                output["identity"] = identity

            print(json.dumps(output, indent=2, ensure_ascii=False))
        else:
            print_human(
                rate_limits,
                args.color,
                account_result=account_result,
                rate_result=rate_result,
                auth_claims=auth_claims,
                codex_home=codex_home,
            )

    except CodexStatusError as exc:
        failed = True
        print(str(exc), file=sys.stderr)
    finally:
        try:
            proc.terminate()
        except Exception:
            pass

        try:
            _stdout_tail, stderr_text = proc.communicate(timeout=1.5)
        except subprocess.TimeoutExpired:
            try:
                proc.kill()
            except Exception:
                pass
            try:
                _stdout_tail, stderr_text = proc.communicate(timeout=1.0)
            except Exception:
                stderr_text = ""

    if failed:
        if stderr_text.strip():
            print(stderr_text.strip(), file=sys.stderr)
        return 1

    return 0


def main() -> int:
    return run()


if __name__ == "__main__":
    raise SystemExit(main())
