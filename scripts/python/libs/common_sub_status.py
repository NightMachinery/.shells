"""Shared helpers for subscription-status scripts (codex_status.py, claude_code_usage.py).

Consumers live directly in ``~/scripts/python/`` (which Python puts on
``sys.path`` when running them by basename), so they import this module as
``from libs.common_sub_status import ...``.
"""

from __future__ import annotations

import argparse
import os
import re
import select
import subprocess
import sys
import termios
import time
import tty
from dataclasses import dataclass
from datetime import datetime

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
