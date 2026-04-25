#!/usr/bin/env python3
"""
codex_loop.py

Run a Codex CLI prompt repeatedly, waiting for Codex usage limits to reset
when needed.

Usage:
  codex_loop.py [-n 10] [--model MODEL] [--effort EFFORT] [CODEX_FLAGS...] [PROMPT...]
  codex_loop.py -n 10 --model gpt-5.3-codex --effort high "fix the tests"
  codex_loop.py -n 10 --yolo -c model_reasoning_summary=detailed "fix the tests"
  echo "fix the tests" | codex_loop.py -n 10 -

Notes:
  Unknown leading flags are passed through to `codex exec`.

  Examples:
    codex_loop.py -n 10 --yolo -c model_reasoning_summary=detailed "fix the tests"

    codex_loop.py -n 10 --yolo \\
      -c model_reasoning_summary=detailed \\
      -c model_reasoning_effort=high \\
      "fix the tests"

  If your prompt starts with a dash, use "--":
    codex_loop.py -n 10 --yolo -c model_reasoning_summary=detailed -- "- fix the tests"

  For unknown Codex flags that take a separate value and are not listed in
  CODEX_OPTIONS_TAKING_VALUE, use --flag=value or add the flag name to
  CODEX_OPTIONS_TAKING_VALUE.
"""

from __future__ import annotations

import argparse
import json
import shlex
import subprocess
import sys
import time
from dataclasses import dataclass
from typing import Any


DEFAULT_STATUS_CMD = ["codex_status.py", "--json"]


WRAPPER_OPTIONS_TAKING_VALUE = {
    "-n",
    "--count",
    "--model",
    "--effort",
    "--status-cmd",
}


CODEX_OPTIONS_TAKING_VALUE = {
    "-c",
    "--config",
    "-m",
    "--model",
    "--sandbox",
    "--approval-policy",
    "--ask-for-approval",
    "--cd",
    "--profile",
    "--output-last-message",
    "--color",
}


@dataclass
class WaitDecision:
    should_wait: bool
    wait_until: int | None
    reason: str


def now() -> int:
    return int(time.time())


def fmt_seconds(seconds: int) -> str:
    seconds = max(0, seconds)
    hours, rem = divmod(seconds, 3600)
    minutes, secs = divmod(rem, 60)

    if hours:
        return f"{hours}h {minutes}m {secs}s"
    if minutes:
        return f"{minutes}m {secs}s"
    return f"{secs}s"


def run_json(cmd: list[str]) -> dict[str, Any]:
    proc = subprocess.run(
        cmd,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )

    if proc.returncode != 0:
        raise RuntimeError(
            f"Command failed: {shlex.join(cmd)}\n"
            f"exit={proc.returncode}\n"
            f"stderr={proc.stderr.strip()}"
        )

    try:
        return json.loads(proc.stdout)
    except json.JSONDecodeError as exc:
        raise RuntimeError(
            f"Command did not return valid JSON: {shlex.join(cmd)}\n"
            f"stdout={proc.stdout[:1000]}"
        ) from exc


def get_codex_limit(status: dict[str, Any]) -> dict[str, Any] | None:
    """
    Prefer rateLimitsByLimitId.codex, then fallback to top-level rateLimits.
    """
    by_id = status.get("rateLimitsByLimitId")
    if isinstance(by_id, dict):
        codex = by_id.get("codex")
        if isinstance(codex, dict):
            return codex

    top = status.get("rateLimits")
    if isinstance(top, dict):
        return top

    return None


def reset_time_for_window(window: Any) -> int | None:
    if not isinstance(window, dict):
        return None

    resets_at = window.get("resetsAt")
    if isinstance(resets_at, (int, float)):
        return int(resets_at)

    return None


def used_percent(window: Any) -> float | None:
    if not isinstance(window, dict):
        return None

    pct = window.get("usedPercent")
    if isinstance(pct, (int, float)):
        return float(pct)

    return None


def decide_wait(status: dict[str, Any]) -> WaitDecision:
    limit = get_codex_limit(status)
    if not limit:
        return WaitDecision(False, None, "No codex rate-limit object found.")

    reached_type = limit.get("rateLimitReachedType")
    exhausted_windows: list[tuple[str, int]] = []

    for name in ("primary", "secondary"):
        window = limit.get(name)
        pct = used_percent(window)
        resets_at = reset_time_for_window(window)

        if pct is not None and pct >= 100 and resets_at is not None:
            exhausted_windows.append((name, resets_at))

    # If the status command explicitly reports a reached limit, trust it even
    # if usedPercent is missing or rounded below 100.
    if reached_type:
        resets: list[tuple[str, int]] = []

        for name in ("primary", "secondary"):
            resets_at = reset_time_for_window(limit.get(name))
            if resets_at is not None:
                resets.append((name, resets_at))

        if resets:
            # Conservative choice: wait until the soonest reset that is still
            # in the future. If both are past, fall back to a short wait.
            future = [(name, ts) for name, ts in resets if ts > now()]
            if future:
                name, ts = min(future, key=lambda item: item[1])
                return WaitDecision(
                    True,
                    ts,
                    f"Codex rate limit reached ({reached_type}); waiting for {name} window reset.",
                )

        return WaitDecision(
            True,
            now() + 300,
            f"Codex rate limit reached ({reached_type}); no reset timestamp found, waiting 5 minutes.",
        )

    if exhausted_windows:
        future = [(name, ts) for name, ts in exhausted_windows if ts > now()]
        if future:
            name, ts = min(future, key=lambda item: item[1])
            return WaitDecision(
                True,
                ts,
                f"Codex {name} usage window is exhausted.",
            )

    # Important: credits.hasCredits=false does not necessarily mean the account
    # cannot run Codex. In the sample status, primary/secondary are not exhausted
    # and rateLimitReachedType is null, so we should continue.
    return WaitDecision(False, None, "Codex usage available.")


def wait_until_available(status_cmd: list[str], poll_after_sleep: bool = True) -> None:
    while True:
        status = run_json(status_cmd)
        decision = decide_wait(status)

        if not decision.should_wait:
            return

        target = decision.wait_until or (now() + 300)
        sleep_for = max(1, target - now())

        print(
            f"[codex-loop] {decision.reason} Sleeping {fmt_seconds(sleep_for)}.",
            file=sys.stderr,
            flush=True,
        )

        time.sleep(sleep_for)

        if not poll_after_sleep:
            return


def build_codex_cmd(
    prompt: str,
    model: str | None,
    effort: str | None,
    codex_args: list[str],
) -> list[str]:
    cmd = ["codex", "exec"]

    if model:
        cmd.extend(["--model", model])

    if effort:
        # Codex CLI supports configuration overrides. Reasoning effort is stored
        # as model_reasoning_effort in Codex config.
        cmd.extend(["--config", f"model_reasoning_effort={effort}"])

    # Direct Codex flags go after wrapper-generated config so explicit passthrough
    # `-c ...` values can override wrapper defaults if Codex uses last-write-wins.
    cmd.extend(codex_args)

    cmd.append(prompt)
    return cmd


def run_codex_once(
    prompt: str,
    model: str | None,
    effort: str | None,
    codex_args: list[str],
) -> int:
    cmd = build_codex_cmd(prompt, model, effort, codex_args)
    print(f"[codex-loop] Running: {shlex.join(cmd)}", file=sys.stderr, flush=True)

    proc = subprocess.run(cmd)
    return proc.returncode


def read_prompt(args_prompt: list[str]) -> str:
    if not args_prompt or args_prompt == ["-"]:
        prompt = sys.stdin.read()
    else:
        prompt = " ".join(args_prompt)

    prompt = prompt.strip()
    if not prompt:
        raise SystemExit("No prompt provided. Pass a prompt argument or pipe one on stdin.")

    return prompt


def split_leading_options(argv: list[str]) -> tuple[list[str], list[str]]:
    """
    Split argv into:

      1. leading option tokens:
         - wrapper options handled by this script
         - unknown leading Codex options forwarded to `codex exec`

      2. prompt tokens

    Without "--", the prompt starts at the first non-option token that is not
    consumed as the value of a known value-taking option.

    With "--", everything before it is parsed/forwarded as flags and everything
    after it is the prompt.
    """
    if "--" in argv:
        sep = argv.index("--")
        return argv[:sep], argv[sep + 1 :]

    leading: list[str] = []
    i = 0

    while i < len(argv):
        token = argv[i]

        if token == "-":
            return leading, argv[i:]

        if not token.startswith("-"):
            return leading, argv[i:]

        leading.append(token)

        option = token.split("=", 1)[0]
        takes_value = (
            option in WRAPPER_OPTIONS_TAKING_VALUE
            or option in CODEX_OPTIONS_TAKING_VALUE
        )

        if takes_value and "=" not in token:
            if i + 1 >= len(argv):
                raise SystemExit(f"{token} requires a value")

            leading.append(argv[i + 1])
            i += 2
        else:
            i += 1

    return leading, []


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    if argv is None:
        argv = sys.argv[1:]

    leading_args, prompt_args = split_leading_options(argv)

    parser = argparse.ArgumentParser(
        description="Run Codex CLI repeatedly, waiting for usage limits to reset.",
        allow_abbrev=False,
        epilog=(
            "Unknown leading flags are passed through to `codex exec`. "
            "Example: codex_loop.py -n 10 --yolo -c model_reasoning_summary=detailed "
            "\"fix the tests\". Use `--` before the prompt if the prompt starts with "
            "a dash."
        ),
    )

    parser.add_argument(
        "-n",
        "--count",
        type=int,
        default=1,
        help="Number of Codex runs to execute. Default: 1.",
    )

    parser.add_argument(
        "--model",
        default=None,
        help="Model to pass to Codex CLI, for example gpt-5.3-codex.",
    )

    parser.add_argument(
        "--effort",
        default=None,
        choices=["minimal", "low", "medium", "high", "xhigh"],
        help="Reasoning effort to pass via Codex config.",
    )

    parser.add_argument(
        "--status-cmd",
        default=" ".join(DEFAULT_STATUS_CMD),
        help='Command used to check Codex status. Default: "codex_status.py --json".',
    )

    parser.add_argument(
        "--stop-on-failure",
        action="store_true",
        help="Stop the loop if codex exec exits non-zero.",
    )

    args, codex_args = parser.parse_known_args(leading_args)

    args.codex_args = codex_args
    args.prompt = prompt_args

    return args


def main() -> int:
    args = parse_args()

    if args.count < 1:
        raise SystemExit("-n/--count must be >= 1")

    prompt = read_prompt(args.prompt)
    status_cmd = shlex.split(args.status_cmd)

    failures = 0

    for i in range(1, args.count + 1):
        print(f"[codex-loop] Iteration {i}/{args.count}", file=sys.stderr, flush=True)

        wait_until_available(status_cmd)

        rc = run_codex_once(prompt, args.model, args.effort, args.codex_args)

        if rc != 0:
            failures += 1
            print(
                f"[codex-loop] codex exec failed with exit code {rc}.",
                file=sys.stderr,
                flush=True,
            )

            # A failed run may have failed because a limit was hit mid-request.
            # Check status and wait if needed before deciding whether to continue.
            try:
                wait_until_available(status_cmd)
            except Exception as exc:
                print(f"[codex-loop] status check failed: {exc}", file=sys.stderr)

            if args.stop_on_failure:
                return rc

    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
