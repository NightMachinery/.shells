#!/usr/bin/env python3
##
from __future__ import annotations

import argparse
import json
import os
import select
import subprocess
import sys
import time
from datetime import datetime


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
        description="Read Codex account rate limits via codex app-server."
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


def format_timestamp(epoch_s: int | None) -> str:
    if epoch_s is None:
        return "n/a"

    try:
        dt = datetime.fromtimestamp(epoch_s).astimezone()
    except (OverflowError, OSError, ValueError):
        return f"{epoch_s}"

    return dt.strftime("%Y-%m-%d %H:%M:%S %Z")


def format_relative(epoch_s: int | None) -> str:
    if epoch_s is None:
        return "n/a"

    now = time.time()
    delta = int(round(epoch_s - now))
    past = delta < 0
    delta = abs(delta)

    hours, rem = divmod(delta, 3600)
    mins, secs = divmod(rem, 60)

    parts: list[str] = []
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


def format_used_percent(style: Style, used: int | None) -> str:
    if used is None:
        return "n/a"

    text = f"{used}%"
    if used >= 90:
        return style.red(text)
    if used >= 75:
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


def print_human(rate_limits: dict, color_mode: str) -> None:
    use_color = (
        color_mode == "always"
        or (color_mode == "auto" and sys.stdout.isatty())
    )
    style = Style(use_color)

    plan = rate_limits.get("planType") or "unknown"
    primary = rate_limits.get("primary")
    secondary = rate_limits.get("secondary")
    credits = rate_limits.get("credits")

    print(style.bold(style.cyan("Codex rate limits")))
    print(f"Plan: {style.magenta(str(plan)) if plan != 'unknown' else plan}")
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

        send_json(proc, {"method": "initialized"})
        send_json(
            proc,
            {
                "method": "account/rateLimits/read",
                "id": "2",
                "params": None,
            },
        )
        status_resp = recv_json_response(proc, wanted_id="2", deadline=deadline)
        if status_resp is None:
            raise CodexStatusError("codex-status: account/rateLimits/read timed out")
        if "error" in status_resp:
            raise CodexStatusError(
                "codex-status: account/rateLimits/read failed: "
                + json.dumps(status_resp["error"], ensure_ascii=False)
            )

        result = status_resp.get("result", {})
        if args.json:
            print(json.dumps(result, indent=2, ensure_ascii=False))
        else:
            rate_limits = result.get("rateLimits", result)
            if not isinstance(rate_limits, dict):
                rate_limits = {}
            print_human(rate_limits, args.color)
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
