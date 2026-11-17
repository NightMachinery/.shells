"""Optional shared policy for non-interactive CLI and web automatic downloads."""
import json
import os
from pathlib import Path
import re


def policy_path():
    return Path(os.environ.get("PODCAST_AUTO_POLICY", "~/.config/podcast-web/automation.json")).expanduser()


def managed_by_library(channel_input):
    match = re.search(r"(?:^|/)(UC[A-Za-z0-9_-]{22})(?:$|/)", channel_input)
    if not match:
        return False
    path = policy_path()
    if not path.is_file():
        return False
    # A malformed existing policy must not silently turn automation back on.
    document = json.loads(path.read_text())
    return bool(document.get("managed", {}).get(match[1], False))


def write_policy(channels, path=None):
    path = Path(path) if path is not None else policy_path()
    path.parent.mkdir(parents=True, exist_ok=True)
    body = json.dumps({"managed": {c["id"]: True for c in channels},
                       "enabled": {c["id"]: bool(c["auto_download"]) for c in channels}}, indent=2) + "\n"
    if path.exists() and path.read_text() == body:
        return
    temporary = path.with_suffix(".tmp")
    temporary.write_text(body)
    temporary.replace(path)
