#!/usr/bin/env python3
"""Transport agy hooks to Brish without ringing on missing/stale functions."""
import os
from pathlib import Path
import shlex
import subprocess
import sys
import json

from agy_accounts import record_owner


TARGETS = {
    "tmux": "agy-session-tmux-autoname",
    "register": "agent-session-register",
    "continue": "agent-auto-continue-hook",
}


def program(kind, arguments, environment):
    target = TARGETS[kind]
    account = environment.get("AGY_ACCOUNT_ID", "m")
    if account == "acc1":  # Sessions opened before named accounts were installed.
        account = "m"
    label = environment.get("AGY_ACCOUNT_LABEL", account)
    if label == "acc1":
        label = "m"
    root = environment.get("ANTIGRAVITY_GEMINI_DIR", str(Path.home() / ".gemini"))
    command = ["agy-account-hook", account, root, label, target]
    if kind in ("register", "continue"):
        command.append("agy")
    command.extend(arguments)
    # Check function existence before invoking it: catching exit 127 afterwards
    # is too late to avoid the garden's command-not-found handler.
    return (
        f"if (( ${{+functions[agy-account-hook]}} && ${{+functions[{target}]}} )); "
        f"then {shlex.join(command)} >/dev/null 2>&1 || true; fi; true"
    )


def main():
    try:
        code = program(sys.argv[1], sys.argv[2:], os.environ)
        payload = sys.stdin.buffer.read()
        try:
            conversation = json.loads(payload).get("conversationId", "")
            account = os.environ.get("AGY_ACCOUNT_ID", "m")
            if account == "acc1":
                account = "m"
            owners = Path(os.environ.get("AGY_ACCOUNTS_DIR", str(Path.home() / ".agy-accounts"))) / "history-owners"
            if conversation:
                record_owner(owners, conversation, account)
        except (ValueError, AttributeError, TypeError, OSError):
            pass
        executable = os.environ.get("BRISHZGO_BIN", str(Path.home() / "go/bin/brishzgo"))
        environment = dict(os.environ, brishz_async="y", brishz_in="MAGIC_READ_STDIN")
        subprocess.run([executable, "--", "eval", code], env=environment,
                       input=payload,
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=4,
                       check=False)
    except (KeyError, IndexError, OSError, subprocess.TimeoutExpired):
        pass
    print("{}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
