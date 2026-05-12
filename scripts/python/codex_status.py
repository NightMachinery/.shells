#!/usr/bin/env python3
from __future__ import annotations

import argparse
import base64
import concurrent.futures
import json
import os
import re
import select
import shutil
import subprocess
import sys
import tempfile
import termios
import time
import tty
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path


AUTH_CLAIM = "https://api.openai.com/auth"
AUTH_FILE_RE = re.compile(r"^auth(?:\.|_|-)?(?P<alias>.+)\.json$")
AUTH_RECOVERY_DIR = Path(os.path.expanduser("~/tmp/.codex-auths"))
RGB = tuple[int, int, int]


@dataclass(frozen=True)
class ColorTheme:
    heading: RGB
    identity: RGB
    ok: RGB
    warn: RGB
    error: RGB
    dim: RGB
    reset_time: RGB


DARK_THEMES: dict[str, ColorTheme] = {
    "neon": ColorTheme(
        heading=(80, 220, 255),
        identity=(255, 110, 210),
        ok=(80, 220, 135),
        warn=(245, 195, 80),
        error=(255, 95, 95),
        dim=(135, 145, 160),
        reset_time=(125, 185, 255),
    ),
    "ember": ColorTheme(
        heading=(255, 175, 95),
        identity=(230, 130, 255),
        ok=(110, 220, 140),
        warn=(245, 210, 95),
        error=(255, 105, 115),
        dim=(145, 135, 125),
        reset_time=(255, 145, 95),
    ),
    "ocean": ColorTheme(
        heading=(95, 200, 230),
        identity=(145, 165, 255),
        ok=(85, 210, 175),
        warn=(235, 200, 90),
        error=(255, 100, 120),
        dim=(130, 150, 165),
        reset_time=(80, 185, 255),
    ),
}
DARK_THEME_DEFAULT = "neon"

LIGHT_THEMES: dict[str, ColorTheme] = {
    "day": ColorTheme(
        heading=(0, 110, 150),
        identity=(165, 55, 145),
        ok=(20, 130, 75),
        warn=(165, 105, 0),
        error=(195, 45, 55),
        dim=(105, 110, 120),
        reset_time=(20, 105, 190),
    ),
    "paper": ColorTheme(
        heading=(20, 95, 125),
        identity=(140, 65, 120),
        ok=(40, 125, 80),
        warn=(150, 105, 25),
        error=(180, 55, 60),
        dim=(115, 105, 95),
        reset_time=(30, 95, 165),
    ),
    "mint": ColorTheme(
        heading=(0, 120, 115),
        identity=(150, 65, 150),
        ok=(20, 135, 90),
        warn=(155, 115, 15),
        error=(185, 55, 70),
        dim=(100, 115, 110),
        reset_time=(0, 115, 170),
    ),
}
LIGHT_THEME_DEFAULT = "day"


class CodexStatusError(RuntimeError):
    def __init__(self, message: str, *, stderr_text: str = "") -> None:
        super().__init__(message)
        self.stderr_text = stderr_text


@dataclass(frozen=True)
class ParsedArgs:
    args: argparse.Namespace
    passthrough: list[str]


@dataclass(frozen=True)
class AuthSource:
    path: Path | None
    label: str
    display_path: str | None


@dataclass(frozen=True)
class CodexSessionResult:
    init_result: dict
    account_result: dict
    rate_result: dict


@dataclass(frozen=True)
class AuthStatus:
    source: AuthSource
    ok: bool
    rate_result: dict = field(default_factory=dict)
    account_result: dict = field(default_factory=dict)
    rate_limits: dict = field(default_factory=dict)
    auth_claims: dict = field(default_factory=dict)
    identity: dict[str, str] = field(default_factory=dict)
    error: str | None = None
    stderr: str = ""
    recovered_from: AuthSource | None = None


@dataclass(frozen=True)
class SwapResult:
    selected: AuthStatus | None
    previously_active: AuthStatus | None
    statuses: list[AuthStatus]
    eligible: list[AuthStatus]
    active_alias: str | None
    active_auth_file: Path
    swapped: bool
    dry_run: bool
    reason: str


class Style:
    def __init__(
        self,
        enabled: bool,
        *,
        true_color: bool = False,
        theme: ColorTheme | None = None,
    ):
        self.enabled = enabled
        self.true_color = true_color
        self.theme = theme

    def _wrap(self, text: str, code: str) -> str:
        if not self.enabled:
            return text
        return f"\033[{code}m{text}\033[0m"

    def _rgb(self, text: str, color: RGB, fallback_code: str) -> str:
        if not self.enabled:
            return text
        if self.true_color:
            r, g, b = color
            return self._wrap(text, f"38;2;{r};{g};{b}")
        return self._wrap(text, fallback_code)

    def bold(self, text: str) -> str:
        return self._wrap(text, "1")

    def cyan(self, text: str) -> str:
        if self.theme is None:
            return self._wrap(text, "36")
        return self._rgb(text, self.theme.heading, "36")

    def magenta(self, text: str) -> str:
        if self.theme is None:
            return self._wrap(text, "35")
        return self._rgb(text, self.theme.identity, "35")

    def green(self, text: str) -> str:
        if self.theme is None:
            return self._wrap(text, "32")
        return self._rgb(text, self.theme.ok, "32")

    def yellow(self, text: str) -> str:
        if self.theme is None:
            return self._wrap(text, "33")
        return self._rgb(text, self.theme.warn, "33")

    def red(self, text: str) -> str:
        if self.theme is None:
            return self._wrap(text, "31")
        return self._rgb(text, self.theme.error, "31")

    def dim(self, text: str) -> str:
        if self.theme is None or not self.true_color:
            return self._wrap(text, "2")
        return self._rgb(text, self.theme.dim, "2")

    def reset_time(self, text: str) -> str:
        if self.theme is None:
            return self.cyan(text)
        return self._rgb(text, self.theme.reset_time, "36")


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


def parse_worker_default() -> int:
    raw = env_first("codex_status_workers", "CODEX_STATUS_WORKERS")
    if raw is not None:
        try:
            value = int(raw)
        except ValueError:
            value = 0

        if value > 0:
            return value

    return min(8, max(1, os.cpu_count() or 1))


def positive_int(value: str) -> int:
    try:
        number = int(value)
    except ValueError as exc:
        raise argparse.ArgumentTypeError("must be an integer") from exc

    if number <= 0:
        raise argparse.ArgumentTypeError("must be greater than zero")

    return number


def nonnegative_int(value: str) -> int:
    try:
        number = int(value)
    except ValueError as exc:
        raise argparse.ArgumentTypeError("must be an integer") from exc

    if number < 0:
        raise argparse.ArgumentTypeError("must be zero or greater")

    return number


def parse_args(argv: list[str] | None = None) -> ParsedArgs:
    raw_argv = list(sys.argv[1:] if argv is None else argv)
    command = "status"
    if raw_argv and raw_argv[0] in {"status", "swap"}:
        command = raw_argv.pop(0)

    parser = argparse.ArgumentParser(
        description=(
            "Read Codex account identity/rate limits, or swap to the least-used auth."
        )
    )
    parser.add_argument(
        "--json",
        action=argparse.BooleanOptionalAction,
        default=False,
        help="Output JSON response (default: %(default)s).",
    )
    parser.add_argument(
        "--all",
        action=argparse.BooleanOptionalAction,
        default=True,
        help="Show status for every ~/.codex/auth*.json file (default: %(default)s).",
    )
    parser.add_argument(
        "--workers",
        type=positive_int,
        default=parse_worker_default(),
        help="Maximum parallel auth files to check (default: %(default)s).",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=parse_timeout(),
        help="Timeout in seconds (default: %(default)s).",
    )
    parser.add_argument(
        "--retries",
        type=nonnegative_int,
        default=0,
        help=(
            "Retry failed status checks this many times after the first attempt "
            "(default: %(default)s)."
        ),
    )
    parser.add_argument(
        "--profile",
        default=env_first("codex_status_profile", "CODEX_STATUS_PROFILE", default=""),
        help="Codex profile name (default: %(default)r).",
    )
    parser.add_argument(
        "--cd",
        dest="cd_dir",
        default=env_first(
            "codex_status_cd",
            "CODEX_STATUS_CD",
            default=os.path.join(os.path.expanduser("~"), "tmp"),
        ),
        help="Working directory for codex (default: %(default)s).",
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
    parser.add_argument(
        "--codex-arg",
        action="append",
        default=[],
        metavar="ARG",
        help="Extra argument forwarded to codex before app-server.",
    )
    parser.add_argument(
        "--dry-run",
        action=argparse.BooleanOptionalAction,
        default=False,
        help="For swap, select an auth without replacing auth.json.",
    )

    args, unknown = parser.parse_known_args(raw_argv)
    args.command = command
    return ParsedArgs(args=args, passthrough=unknown)


def color_enabled(color_mode: str) -> bool:
    if color_mode == "always":
        return True
    if color_mode == "never":
        return False
    if color_mode == "auto":
        return sys.stdout.isatty()

    raise ValueError(f"unknown color mode: {color_mode}")


def terminal_looks_like_kitty() -> bool:
    return (
        os.environ.get("TERM_PROGRAM") == "kitty"
        or bool(os.environ.get("KITTY_WINDOW_ID"))
        or "kitty" in os.environ.get("TERM", "").lower()
    )


def true_color_enabled(true_color_mode: str) -> bool:
    if true_color_mode == "on":
        return True
    if true_color_mode == "off":
        return False
    if true_color_mode != "auto":
        raise ValueError(f"unknown true-color mode: {true_color_mode}")

    if terminal_looks_like_kitty():
        return True

    colorterm = os.environ.get("COLORTERM", "").lower()
    if "truecolor" in colorterm or "24bit" in colorterm:
        return True

    try:
        proc = subprocess.run(
            ["infocmp"],
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
            timeout=0.5,
            check=False,
        )
    except (OSError, subprocess.TimeoutExpired):
        return False

    return bool(re.search(r"\b(?:RGB|Tc)\b", proc.stdout))


def parse_terminal_rgb_response(response: bytes) -> RGB | None:
    text = response.decode("ascii", errors="ignore")
    match = re.search(
        r"(?:\]11;|\]10;)?rgb:([0-9a-fA-F]{1,4})/([0-9a-fA-F]{1,4})/([0-9a-fA-F]{1,4})",
        text,
    )
    if match:
        values: list[int] = []
        for raw in match.groups():
            value = int(raw, 16)
            max_value = (16 ** len(raw)) - 1
            values.append(round(value * 255 / max_value))
        return values[0], values[1], values[2]

    match = re.search(r"#([0-9a-fA-F]{6})", text)
    if match:
        raw = match.group(1)
        return int(raw[0:2], 16), int(raw[2:4], 16), int(raw[4:6], 16)

    return None


def query_terminal_background_rgb(timeout_s: float = 0.2) -> RGB | None:
    if not sys.stdout.isatty():
        return None

    try:
        fd = os.open("/dev/tty", os.O_RDWR | os.O_NOCTTY)
    except OSError:
        return None

    old_attrs = None
    try:
        old_attrs = termios.tcgetattr(fd)
        tty.setcbreak(fd)
        os.write(fd, b"\033]11;?\033\\")

        deadline = time.monotonic() + timeout_s
        chunks: list[bytes] = []
        while time.monotonic() < deadline:
            remaining = max(0.0, deadline - time.monotonic())
            readable, _, _ = select.select([fd], [], [], min(0.05, remaining))
            if not readable:
                continue

            chunk = os.read(fd, 128)
            if not chunk:
                break
            chunks.append(chunk)
            data = b"".join(chunks)
            if b"\a" in data or b"\033\\" in data:
                return parse_terminal_rgb_response(data)
    except OSError:
        return None
    finally:
        if old_attrs is not None:
            try:
                termios.tcsetattr(fd, termios.TCSADRAIN, old_attrs)
            except OSError:
                pass
        try:
            os.close(fd)
        except OSError:
            pass

    return None


def rgb_is_dark(rgb: RGB) -> bool:
    r, g, b = rgb
    luminance = (0.2126 * r) + (0.7152 * g) + (0.0722 * b)
    return luminance < 128


def dark_mode_enabled(dark_mode: str) -> bool:
    if dark_mode == "on":
        return True
    if dark_mode == "off":
        return False
    if dark_mode != "auto":
        raise ValueError(f"unknown dark mode: {dark_mode}")

    background = query_terminal_background_rgb()
    if background is not None:
        return rgb_is_dark(background)

    colorfgbg = os.environ.get("COLORFGBG", "")
    if colorfgbg:
        try:
            background_code = int(colorfgbg.split(";")[-1])
        except ValueError:
            background_code = -1
        if background_code >= 0:
            return background_code in {0, 1, 2, 3, 4, 5, 6, 8}

    return True


def build_style(args: argparse.Namespace) -> Style:
    enabled = color_enabled(args.color)
    true_color = enabled and true_color_enabled(args.true_color)
    theme: ColorTheme | None = None
    if true_color:
        if dark_mode_enabled(args.dark_mode):
            theme = DARK_THEMES[args.dark_theme]
        else:
            theme = LIGHT_THEMES[args.light_theme]

    return Style(enabled, true_color=true_color, theme=theme)


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


def run_codex_session(
    args: argparse.Namespace,
    passthrough: list[str],
    *,
    env: dict[str, str] | None = None,
) -> CodexSessionResult:
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
            env=env,
        )
    except FileNotFoundError as exc:
        raise CodexStatusError("codex-status: codex not found in PATH") from exc

    error: CodexStatusError | None = None
    session: CodexSessionResult | None = None
    stderr_text = ""

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

        account_deadline = min(
            deadline,
            time.monotonic() + max(1.0, min(5.0, args.timeout / 3)),
        )
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

        session = CodexSessionResult(
            init_result=init_result,
            account_result=account_result,
            rate_result=rate_result,
        )
    except CodexStatusError as exc:
        error = exc
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

    if error is not None:
        error.stderr_text = stderr_text.strip()
        raise error

    assert session is not None
    return session


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

    return dt.strftime("%Y-%m-%d %H:%M:%S %z")


def relative_parts(epoch_s: int | float | None) -> tuple[str, bool] | None:
    if epoch_s is None:
        return None

    try:
        delta = int(round(float(epoch_s) - time.time()))
    except (TypeError, ValueError):
        return None

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
    return text, past


def format_relative(epoch_s: int | float | None) -> str:
    parts = relative_parts(epoch_s)
    if parts is None:
        return "n/a"

    text, past = parts
    return f"{text} ago" if past else f"in {text}"


def format_relative_colored(style: Style, epoch_s: int | float | None) -> str:
    parts = relative_parts(epoch_s)
    if parts is None:
        return style.dim("n/a")

    text, past = parts
    colored = style.reset_time(text)
    return f"{colored} ago" if past else f"in {colored}"


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


def format_average_percent(style: Style, used: object | None) -> str:
    if used is None:
        return style.dim("n/a")

    try:
        pct = float(used)
    except (TypeError, ValueError):
        return style.dim("n/a")

    return format_used_percent(style, round(pct, 1))


def numeric_used_percent(window: object) -> float | None:
    if not isinstance(window, dict):
        return None

    value = window.get("usedPercent")
    if not isinstance(value, (int, float)):
        return None

    return float(value)


def numeric_reset_time(window: object) -> float | None:
    if not isinstance(window, dict):
        return None

    value = window.get("resetsAt")
    if not isinstance(value, (int, float)):
        return None

    return float(value)


def format_window(style: Style, label: str, *, window: dict | None) -> str:
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


def format_credits(style: Style, *, credits: dict | None) -> str:
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


def read_auth_claims_from_file(path: Path) -> dict:
    data = read_json_file(path)
    if not data:
        return {}

    for token in collect_tokens(data):
        claims = decode_jwt_payload(token)
        if claims:
            return claims

    return {}


def auth_refresh_token(data: dict | None) -> str | None:
    if not isinstance(data, dict):
        return None

    return as_nonempty_str(
        first_path(data, ("tokens", "refresh_token"), ("refresh_token",))
    )


def auth_id_token(data: dict | None) -> str | None:
    if not isinstance(data, dict):
        return None

    return as_nonempty_str(first_path(data, ("tokens", "id_token"), ("id_token",)))


def auth_account_id(data: dict | None) -> str | None:
    if not isinstance(data, dict):
        return None

    return as_nonempty_str(first_path(data, ("tokens", "account_id"), ("account_id",)))


def auth_identity_key(data: dict | None) -> tuple[str, ...]:
    if not isinstance(data, dict):
        return ()

    id_token = auth_id_token(data)
    claims = decode_jwt_payload(id_token) if id_token else None
    if not isinstance(claims, dict):
        claims = {}

    auth_claim = claims.get(AUTH_CLAIM)
    auth = auth_claim if isinstance(auth_claim, dict) else {}

    parts: list[str] = []
    for value in (
        auth_account_id(data),
        as_nonempty_str(first_path(claims, ("sub",))),
        as_nonempty_str(first_path(claims, ("email",), (AUTH_CLAIM, "email"))),
        as_nonempty_str(
            first_path(
                auth,
                ("workspaceId",),
                ("workspace_id",),
                ("workspace", "id"),
                ("organization", "id"),
                ("default_organization", "id"),
            )
        ),
    ):
        if value and value not in parts:
            parts.append(value)

    return tuple(parts)


def auth_files_in_directory(directory: Path) -> list[Path]:
    try:
        entries = list(directory.iterdir())
    except OSError:
        return []

    return sorted(
        entry
        for entry in entries
        if entry.is_file() and entry.name.startswith("auth") and entry.suffix == ".json"
    )


def auth_last_refresh_sort_key(path: Path, data: dict | None) -> tuple[str, float, str]:
    last_refresh = ""
    if isinstance(data, dict):
        last_refresh = as_nonempty_str(data.get("last_refresh")) or ""

    try:
        mtime = path.stat().st_mtime
    except OSError:
        mtime = 0.0

    return last_refresh, mtime, path.name


def find_recovered_auth_file(stale_auth_file: Path) -> Path | None:
    stale_data = read_json_file(stale_auth_file)
    stale_key = auth_identity_key(stale_data)
    stale_refresh_token = auth_refresh_token(stale_data)
    if not stale_key:
        return None

    candidates: list[tuple[tuple[str, float, str], Path]] = []
    for candidate in auth_files_in_directory(AUTH_RECOVERY_DIR):
        if candidate.resolve() == stale_auth_file.resolve():
            continue

        candidate_data = read_json_file(candidate)
        if auth_identity_key(candidate_data) != stale_key:
            continue

        candidate_refresh_token = auth_refresh_token(candidate_data)
        if (
            stale_refresh_token
            and candidate_refresh_token
            and candidate_refresh_token == stale_refresh_token
        ):
            continue

        candidates.append((auth_last_refresh_sort_key(candidate, candidate_data), candidate))

    if not candidates:
        return None

    return max(candidates, key=lambda item: item[0])[1]


def refresh_token_already_used_error(exc: CodexStatusError) -> bool:
    text = f"{exc}\n{exc.stderr_text}".lower()
    return (
        "refresh token" in text
        and (
            ("already" in text and "used" in text)
            or "reuse" in text
            or "invalid_grant" in text
        )
    )


def read_local_auth_claims(codex_home: object | None) -> dict:
    for path in auth_file_candidates(codex_home):
        claims = read_auth_claims_from_file(path)
        if claims:
            return claims

    return {}


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


def alias_from_auth_path(path: Path) -> str | None:
    match = AUTH_FILE_RE.fullmatch(path.name)
    if match:
        alias = match.group("alias").strip()
        return alias or None

    alias = path.stem.strip()
    return alias or None


def iter_matching_files(directory: Path, pattern: re.Pattern[str]) -> list[Path]:
    try:
        entries = list(directory.iterdir())
    except OSError:
        return []

    paths: list[Path] = []
    for entry in entries:
        if not entry.is_file():
            continue
        if pattern.fullmatch(entry.name):
            paths.append(entry)

    return sorted(paths, key=lambda path: path.name)


def workspace_name_from_matching_auth_alias(codex_home: object | None) -> str | None:
    active = active_auth_file(codex_home)
    if active is None:
        return None

    active_bytes = auth_file_bytes(active)
    if active_bytes is None:
        return None

    matches: list[str] = []
    for path in iter_matching_files(active.parent, AUTH_FILE_RE):
        if path.name == "auth.json":
            continue
        if auth_file_bytes(path) != active_bytes:
            continue

        alias = alias_from_auth_path(path)
        if alias:
            matches.append(alias)

    return matches[0] if matches else None


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
    *,
    rate_limits: dict,
    account_result: dict,
    rate_result: dict,
    auth_claims: dict,
    auth_file: Path | None = None,
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

    plan_owner_email = as_nonempty_str(
        first_path(account, ("email",))
        or first_path(account_result, ("email",))
        or first_path(rate_result, ("account", "email"))
        or first_path(auth_claims, ("email",), (AUTH_CLAIM, "email"))
    )

    auth_file_alias = alias_from_auth_path(auth_file) if auth_file else None
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
        or auth_file_alias
        or workspace_name_from_matching_auth_alias(codex_home)
    )

    identity: dict[str, str] = {}
    if plan:
        identity["planType"] = plan
    if plan_owner_email:
        identity["planOwnerEmail"] = plan_owner_email
    if workspace_name:
        identity["workspaceName"] = workspace_name

    return identity


def home_relative(path: Path) -> str:
    expanded = path.expanduser()
    home = Path.home()

    try:
        return "~/" + str(expanded.resolve().relative_to(home.resolve()))
    except (OSError, ValueError):
        return str(expanded)


def all_auth_sources() -> list[AuthSource]:
    codex_home = Path(os.path.expanduser("~")) / ".codex"
    sources: list[AuthSource] = []

    for path in iter_matching_files(codex_home, AUTH_FILE_RE):
        label = alias_from_auth_path(path) or path.name
        sources.append(
            AuthSource(
                path=path,
                label=label,
                display_path=home_relative(path),
            )
        )

    return sources


def active_auth_source() -> AuthSource:
    path = active_auth_file(None)
    return AuthSource(
        path=None,
        label="active",
        display_path=home_relative(path) if path else None,
    )


def copy_config_file(source_home: Path, temp_home: Path, *, name: str) -> None:
    source = source_home / name
    target = temp_home / name

    if not source.is_file():
        return

    try:
        shutil.copy2(source, target)
    except OSError:
        return


def prepare_codex_home(source_home: Path, temp_home: Path, *, auth_file: Path) -> None:
    temp_home.mkdir(parents=True, exist_ok=True)
    shutil.copy2(auth_file, temp_home / "auth.json")

    for name in ("config.toml", "config.json", "AGENTS.md"):
        copy_config_file(source_home, temp_home, name=name)


def env_for_codex_home(codex_home: Path) -> dict[str, str]:
    env = os.environ.copy()
    env["CODEX_HOME"] = str(codex_home)
    env["codex_home"] = str(codex_home)
    return env


def build_success_status(
    source: AuthSource,
    session: CodexSessionResult,
    *,
    auth_file: Path | None,
    recovered_from: AuthSource | None = None,
) -> AuthStatus:
    rate_limits_obj = session.rate_result.get("rateLimits", session.rate_result)
    rate_limits = rate_limits_obj if isinstance(rate_limits_obj, dict) else {}

    codex_home = session.init_result.get("codexHome")
    auth_claims = (
        read_auth_claims_from_file(auth_file)
        if auth_file is not None
        else read_local_auth_claims(codex_home)
    )

    identity = build_identity(
        rate_limits=rate_limits,
        account_result=session.account_result,
        rate_result=session.rate_result,
        auth_claims=auth_claims,
        auth_file=auth_file,
        codex_home=codex_home,
    )

    return AuthStatus(
        source=source,
        ok=True,
        rate_result=session.rate_result,
        account_result=session.account_result,
        rate_limits=rate_limits,
        auth_claims=auth_claims,
        identity=identity,
        recovered_from=recovered_from,
    )


def gather_status_for_auth_file(
    source: AuthSource,
    *,
    parsed: ParsedArgs,
    auth_file: Path,
    config_home: Path | None = None,
    recovered_from: AuthSource | None = None,
) -> AuthStatus:
    with tempfile.TemporaryDirectory(prefix="codex-status-") as temp_dir:
        temp_home = Path(temp_dir)
        prepare_codex_home(
            config_home or auth_file.parent,
            temp_home,
            auth_file=auth_file,
        )
        session = run_codex_session(
            parsed.args,
            parsed.passthrough,
            env=env_for_codex_home(temp_home),
        )
        return build_success_status(
            source,
            session,
            auth_file=auth_file,
            recovered_from=recovered_from,
        )


def gather_status_once(source: AuthSource, *, parsed: ParsedArgs) -> AuthStatus:
    if source.path is None:
        session = run_codex_session(parsed.args, parsed.passthrough)
        return build_success_status(source, session, auth_file=None)

    try:
        return gather_status_for_auth_file(
            source,
            parsed=parsed,
            auth_file=source.path,
        )
    except CodexStatusError as exc:
        if not refresh_token_already_used_error(exc):
            raise

        recovered_path = find_recovered_auth_file(source.path)
        if recovered_path is None:
            raise

        recovered_source = AuthSource(
            path=recovered_path,
            label=alias_from_auth_path(recovered_path) or recovered_path.name,
            display_path=home_relative(recovered_path),
        )
        recovered_status = gather_status_for_auth_file(
            source,
            parsed=parsed,
            auth_file=recovered_path,
            config_home=source.path.parent,
            recovered_from=recovered_source,
        )
        replace_auth_file(source_path=recovered_path, target_path=source.path)
        return recovered_status


def error_status_from_exception(source: AuthSource, exc: BaseException) -> AuthStatus:
    if isinstance(exc, CodexStatusError):
        return AuthStatus(
            source=source,
            ok=False,
            error=str(exc),
            stderr=exc.stderr_text,
        )

    return AuthStatus(
        source=source,
        ok=False,
        error=f"codex-status: {type(exc).__name__}: {exc}",
    )


def gather_status(source: AuthSource, *, parsed: ParsedArgs) -> AuthStatus:
    attempts = parsed.args.retries + 1
    last_exc: BaseException | None = None

    for attempt in range(attempts):
        try:
            return gather_status_once(source, parsed=parsed)
        except Exception as exc:
            last_exc = exc
            if attempt + 1 < attempts:
                continue

    assert last_exc is not None
    return error_status_from_exception(source, last_exc)


def gather_statuses(parsed: ParsedArgs, sources: list[AuthSource]) -> list[AuthStatus]:
    if len(sources) <= 1:
        return [gather_status(sources[0], parsed=parsed)] if sources else []

    max_workers = min(parsed.args.workers, len(sources))
    statuses: list[AuthStatus | None] = [None] * len(sources)

    with concurrent.futures.ThreadPoolExecutor(max_workers=max_workers) as executor:
        future_to_index = {
            executor.submit(gather_status, source, parsed=parsed): index
            for index, source in enumerate(sources)
        }

        for future in concurrent.futures.as_completed(future_to_index):
            index = future_to_index[future]
            try:
                statuses[index] = future.result()
            except Exception as exc:
                statuses[index] = AuthStatus(
                    source=sources[index],
                    ok=False,
                    error=f"codex-status: {type(exc).__name__}: {exc}",
                )

    return [status for status in statuses if status is not None]


def print_rate_details(
    rate_limits: dict,
    style: Style,
    *,
    identity: dict[str, str],
) -> None:
    plan = identity.get("planType") or "unknown"
    plan_owner_email = identity.get("planOwnerEmail")
    workspace_name = identity.get("workspaceName")

    primary_obj = rate_limits.get("primary")
    secondary_obj = rate_limits.get("secondary")
    credits_obj = rate_limits.get("credits")

    primary = primary_obj if isinstance(primary_obj, dict) else None
    secondary = secondary_obj if isinstance(secondary_obj, dict) else None
    credits = credits_obj if isinstance(credits_obj, dict) else None

    print(f"Plan: {style.magenta(str(plan)) if plan != 'unknown' else plan}")
    print(f"Plan owner: {plan_owner_email if plan_owner_email else style.dim('n/a')}")
    print(
        f"Workspace: {style.magenta(workspace_name) if workspace_name else style.dim('n/a')}"
    )
    print(format_window(style, "Primary", window=primary))
    print(format_window(style, "Secondary", window=secondary))
    print(f"Credits: {format_credits(style, credits=credits)}")


def print_human_status(
    status: AuthStatus,
    style: Style,
    *,
    show_auth_header: bool,
    heading_tags: list[str] | None = None,
    extra_lines: list[str] | None = None,
) -> None:
    title = "Codex rate limits"
    if show_auth_header and status.source.display_path:
        title = f"* {status.source.display_path}"
        if heading_tags is None:
            heading_tags = ["Active"] if status_is_active(status) else []

        for tag in heading_tags:
            title = f"{title} {style.magenta(f'[{tag}]')}"

    print(style.bold(style.cyan(title)))

    if not status.ok:
        print(f"Status: {style.red('failed')}")
        print(f"Error: {status.error or 'unknown error'}")
        if status.stderr.strip():
            print(style.dim(status.stderr.strip()))
        return

    print_rate_details(status.rate_limits, style, identity=status.identity)
    if status.recovered_from is not None and status.recovered_from.display_path:
        print(f"Recovered auth: {style.dim(status.recovered_from.display_path)}")
    for line in extra_lines or []:
        print(line)


def average_used_percent(statuses: list[AuthStatus], window_name: str) -> float | None:
    values: list[float] = []
    for status in statuses:
        if not status.ok:
            continue

        pct = numeric_used_percent(status.rate_limits.get(window_name))
        if pct is not None:
            values.append(pct)

    if not values:
        return None

    return sum(values) / len(values)


def status_is_active(status: AuthStatus) -> bool:
    active_path = active_auth_file(None)
    if active_path is None:
        return False

    return any(auth_bytes_equal(active_path, path) for path in status_auth_paths(status))


def status_matches_auth_path(status: AuthStatus, path: Path) -> bool:
    return any(auth_bytes_equal(path, auth_path) for auth_path in status_auth_paths(status))


def status_auth_paths(status: AuthStatus) -> list[Path]:
    paths: list[Path] = []
    if status.source.path is not None:
        paths.append(status.source.path)
    if status.recovered_from is not None and status.recovered_from.path is not None:
        paths.append(status.recovered_from.path)
    return paths


def status_effective_auth_path(status: AuthStatus) -> Path | None:
    if status.recovered_from is not None and status.recovered_from.path is not None:
        return status.recovered_from.path
    return status.source.path


def active_last_statuses(statuses: list[AuthStatus]) -> list[AuthStatus]:
    return sorted(statuses, key=lambda status: status_is_active(status))


def has_usable_quota(status: AuthStatus) -> bool:
    primary = numeric_used_percent(status.rate_limits.get("primary"))
    secondary = numeric_used_percent(status.rate_limits.get("secondary"))
    return (
        primary is not None
        and primary < 100
        and secondary is not None
        and secondary < 100
    )


def reset_time_for_exhausted_auth(status: AuthStatus) -> float | None:
    secondary = numeric_used_percent(status.rate_limits.get("secondary"))
    if secondary is not None and secondary < 100:
        return numeric_reset_time(status.rate_limits.get("primary"))

    return numeric_reset_time(status.rate_limits.get("secondary"))


@dataclass(frozen=True)
class ResetCandidate:
    status: AuthStatus
    reset_time: float


def first_quota_reset(statuses: list[AuthStatus]) -> ResetCandidate | None:
    ok_statuses = [status for status in statuses if status.ok]
    if not ok_statuses or any(has_usable_quota(status) for status in ok_statuses):
        return None

    reset_candidates = [
        ResetCandidate(status=status, reset_time=reset_time)
        for status in ok_statuses
        if (reset_time := reset_time_for_exhausted_auth(status)) is not None
    ]
    if not reset_candidates:
        return None

    return min(
        reset_candidates,
        key=lambda candidate: (
            candidate.reset_time,
            candidate.status.source.label,
            candidate.status.source.display_path or "",
        ),
    )


def first_quota_reset_time(statuses: list[AuthStatus]) -> float | None:
    first_reset = first_quota_reset(statuses)
    return first_reset.reset_time if first_reset is not None else None


def average_usage_json(statuses: list[AuthStatus]) -> dict[str, object]:
    payload = {
        "primaryUsedPercent": average_used_percent(statuses, "primary"),
        "secondaryUsedPercent": average_used_percent(statuses, "secondary"),
    }

    first_reset = first_quota_reset(statuses)
    if first_reset is not None:
        payload["firstTimeToReset"] = first_reset.reset_time
        payload["firstTimeToResetAlias"] = first_reset.status.source.label

    return payload


def print_average_usage(statuses: list[AuthStatus], style: Style) -> None:
    primary = average_used_percent(statuses, "primary")
    secondary = average_used_percent(statuses, "secondary")
    first_reset = first_quota_reset(statuses)
    print(style.bold(style.cyan("Average usage")))
    print(f"Primary: {format_average_percent(style, primary)} used")
    print(f"Secondary: {format_average_percent(style, secondary)} used")
    if first_reset is not None:
        alias = format_status_alias(first_reset.status, style)
        print(
            "First Time to Reset: "
            f"{alias} {format_relative_colored(style, first_reset.reset_time)} "
            f"({format_timestamp(first_reset.reset_time)})"
        )


def print_human_statuses(
    statuses: list[AuthStatus],
    *,
    args: argparse.Namespace,
    show_auth_header: bool,
) -> None:
    style = build_style(args)

    for index, status in enumerate(active_last_statuses(statuses)):
        if index:
            print()
        print_human_status(status, style, show_auth_header=show_auth_header)

    if show_auth_header and statuses:
        print()
        print_average_usage(statuses, style)


def status_json(status: AuthStatus) -> dict:
    payload: dict[str, object] = {}

    if status.ok:
        payload.update(status.rate_result)
        if status.account_result.get("account") is not None:
            payload["account"] = status.account_result.get("account")
        if status.identity:
            payload["identity"] = status.identity
    else:
        payload["error"] = status.error or "unknown error"
        if status.stderr.strip():
            payload["stderr"] = status.stderr.strip()

    payload["ok"] = status.ok
    payload["alias"] = status.source.label
    if status.source.display_path:
        payload["authFile"] = status.source.display_path
    if status.recovered_from is not None:
        payload["recoveredFrom"] = {
            "alias": status.recovered_from.label,
            "authFile": status.recovered_from.display_path,
        }

    return payload


def print_json_statuses(statuses: list[AuthStatus], *, all_mode: bool) -> None:
    if all_mode:
        output: object = {
            "authFiles": [status_json(status) for status in statuses],
            "averageUsage": average_usage_json(statuses),
        }
    else:
        output = status_json(statuses[0]) if statuses else {}

    print(json.dumps(output, indent=2, ensure_ascii=False))


def active_auth_json_path() -> Path:
    return Path(os.path.expanduser("~")) / ".codex" / "auth.json"


def auth_bytes_equal(left: Path, right: Path) -> bool:
    left_bytes = auth_file_bytes(left)
    right_bytes = auth_file_bytes(right)
    return left_bytes is not None and right_bytes is not None and left_bytes == right_bytes


def status_weekly_used(status: AuthStatus) -> float:
    pct = numeric_used_percent(status.rate_limits.get("secondary"))
    return pct if pct is not None else float("inf")


def status_weekly_used_value(status: AuthStatus) -> float | None:
    return numeric_used_percent(status.rate_limits.get("secondary"))


def status_primary_used(status: AuthStatus) -> float:
    pct = numeric_used_percent(status.rate_limits.get("primary"))
    return pct if pct is not None else float("inf")


def status_primary_used_value(status: AuthStatus) -> float | None:
    return numeric_used_percent(status.rate_limits.get("primary"))


def eligible_swap_statuses(statuses: list[AuthStatus]) -> list[AuthStatus]:
    return [
        status
        for status in statuses
        if status.ok and status.source.path is not None and has_usable_quota(status)
    ]


def select_swap_status(statuses: list[AuthStatus]) -> AuthStatus | None:
    eligible = eligible_swap_statuses(statuses)
    if not eligible:
        return None

    return min(
        eligible,
        key=lambda status: (
            status_weekly_used(status),
            status_primary_used(status),
            status.source.label,
            status.source.display_path or "",
        ),
    )


def find_status_matching_auth_path(statuses: list[AuthStatus], path: Path) -> AuthStatus | None:
    for status in statuses:
        if status_matches_auth_path(status, path):
            return status

    return None


def replace_active_auth(selected: AuthStatus, active_path: Path) -> bool:
    selected_auth_path = status_effective_auth_path(selected)
    if selected_auth_path is None:
        raise CodexStatusError("codex-status: selected auth has no source path")

    return replace_auth_file(source_path=selected_auth_path, target_path=active_path)


def replace_auth_file(*, source_path: Path, target_path: Path) -> bool:
    target_path.parent.mkdir(parents=True, exist_ok=True)
    if target_path.exists() and auth_bytes_equal(target_path, source_path):
        return False

    tmp_path: Path | None = None
    try:
        with tempfile.NamedTemporaryFile(
            prefix=".auth.",
            suffix=".json",
            dir=str(target_path.parent),
            delete=False,
        ) as tmp:
            tmp_path = Path(tmp.name)

        shutil.copy2(source_path, tmp_path)
        os.replace(tmp_path, target_path)
        tmp_path = None
        return True
    finally:
        if tmp_path is not None:
            try:
                tmp_path.unlink()
            except OSError:
                pass


def run_swap(parsed: ParsedArgs) -> SwapResult:
    sources = all_auth_sources()
    active_path = active_auth_json_path()
    active_alias = workspace_name_from_matching_auth_alias(None)

    if not sources:
        return SwapResult(
            selected=None,
            previously_active=None,
            statuses=[],
            eligible=[],
            active_alias=active_alias,
            active_auth_file=active_path,
            swapped=False,
            dry_run=parsed.args.dry_run,
            reason="no auth snapshots found",
        )

    statuses = gather_statuses(parsed, sources)
    eligible = eligible_swap_statuses(statuses)
    selected = select_swap_status(statuses)
    previously_active = find_status_matching_auth_path(statuses, active_path)

    if selected is None:
        return SwapResult(
            selected=None,
            previously_active=previously_active,
            statuses=statuses,
            eligible=eligible,
            active_alias=active_alias,
            active_auth_file=active_path,
            swapped=False,
            dry_run=parsed.args.dry_run,
            reason="no auth has usable quota remaining",
        )

    swapped = False
    reason = "dry run"
    if not parsed.args.dry_run:
        swapped = replace_active_auth(selected, active_path)
        reason = "swapped" if swapped else "selected auth is already active"

    return SwapResult(
        selected=selected,
        previously_active=previously_active,
        statuses=statuses,
        eligible=eligible,
        active_alias=active_alias,
        active_auth_file=active_path,
        swapped=swapped,
        dry_run=parsed.args.dry_run,
        reason=reason,
    )


def swap_result_json(result: SwapResult) -> dict:
    selected = result.selected
    payload: dict[str, object] = {
        "ok": selected is not None,
        "swapped": result.swapped,
        "dryRun": result.dry_run,
        "reason": result.reason,
        "activeAuthFile": home_relative(result.active_auth_file),
        "eligibleCount": len(result.eligible),
        "checkedCount": len(result.statuses),
        "authFiles": [status_json(status) for status in result.statuses],
    }

    if result.active_alias:
        payload["previousActiveAlias"] = result.active_alias

    if selected is not None:
        payload["selected"] = {
            "alias": selected.source.label,
            "authFile": selected.source.display_path,
            "effectiveAuthFile": home_relative(status_effective_auth_path(selected))
            if status_effective_auth_path(selected) is not None
            else None,
            "identity": selected.identity,
            "rateLimits": selected.rate_limits,
            "weeklyUsedPercent": status_weekly_used_value(selected),
            "primaryUsedPercent": status_primary_used_value(selected),
        }

    failures = [status_json(status) for status in result.statuses if not status.ok]
    if failures:
        payload["failures"] = failures

    return payload


def print_json_swap_result(result: SwapResult) -> None:
    print(json.dumps(swap_result_json(result), indent=2, ensure_ascii=False))


def same_auth_source(left: AuthStatus, right: AuthStatus | None) -> bool:
    if right is None:
        return False
    if left.source.path is not None and right.source.path is not None:
        return left.source.path == right.source.path
    return left.source.label == right.source.label


def format_status_alias(
    status: AuthStatus | None,
    style: Style,
    fallback: str | None = None,
) -> str:
    label = status.source.label if status is not None else fallback
    if not label:
        return style.dim("n/a")

    return style.magenta(label)


def swap_heading_tags(status: AuthStatus, result: SwapResult) -> list[str]:
    tags: list[str] = []
    if status_is_active(status):
        tags.append("Active")
    if same_auth_source(status, result.previously_active):
        tags.append("Previously Active")
    return tags


def print_human_swap_result(result: SwapResult, *, args: argparse.Namespace) -> None:
    style = build_style(args)
    previously_active = format_status_alias(
        result.previously_active,
        style,
        result.active_alias,
    )

    for index, status in enumerate(active_last_statuses(result.statuses)):
        if index:
            print()

        extra_lines = (
            [f"Previously active: {previously_active}"]
            if same_auth_source(status, result.selected)
            else None
        )
        print_human_status(
            status,
            style,
            show_auth_header=True,
            heading_tags=swap_heading_tags(status, result),
            extra_lines=extra_lines,
        )

    if result.statuses:
        print()
        print_average_usage(result.statuses, style)

    if result.selected is None:
        if result.statuses:
            print()
        print(style.bold(style.cyan("Swap Failed")))
        print(f"Reason: {result.reason}")


def run() -> int:
    parsed = parse_args()

    if parsed.args.command == "swap":
        result = run_swap(parsed)
        if parsed.args.json:
            print_json_swap_result(result)
        else:
            print_human_swap_result(result, args=parsed.args)

        return 0 if result.selected is not None else 1

    if parsed.args.all:
        sources = all_auth_sources()
        if not sources:
            pattern = os.path.join(os.path.expanduser("~"), ".codex", AUTH_FILE_RE.pattern)
            print(f"codex-status: no auth files found matching {pattern}", file=sys.stderr)
            return 1
    else:
        sources = [active_auth_source()]

    statuses = gather_statuses(parsed, sources)

    if parsed.args.json:
        print_json_statuses(statuses, all_mode=parsed.args.all)
    else:
        print_human_statuses(
            statuses,
            args=parsed.args,
            show_auth_header=parsed.args.all,
        )

    return 1 if any(not status.ok for status in statuses) else 0


def main() -> int:
    return run()


if __name__ == "__main__":
    raise SystemExit(main())
