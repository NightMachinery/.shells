#!/usr/bin/env python3
"""Share explicitly allowed agy configuration and local conversation files."""

import argparse
import json
import os
from pathlib import Path
import sys
import tempfile
import re


SHARED = (
    "GEMINI.md", "AGENTS.md", "policies", "skills", "rules", "commands",
    "config/hooks.json",
    # Directory links preserve SQLite WAL/SHM siblings. Summary indexes and
    # cache/default_project_id.txt must remain account-local.
    "antigravity-cli/conversations", "antigravity-cli/brain",
    "antigravity-cli/annotations",
)


def record_owner(directory: Path, conversation: str, account: str) -> None:
    if not re.fullmatch(r"[a-zA-Z0-9_-]+", conversation):
        return
    directory.mkdir(parents=True, exist_ok=True, mode=0o700)
    try:
        fd = os.open(directory / conversation, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    except FileExistsError:
        return
    with os.fdopen(fd, "w") as stream:
        stream.write(account + "\n")


def seed_owners(directory: Path, shared: Path, account="m") -> None:
    # Called before creating any additional shared-history account. Existing
    # local IDs belong to the original account. Never open transcript contents.
    marker = directory / ".legacy-seeded"
    if marker.exists():
        return
    for relative in ("antigravity-cli/brain", "antigravity-cli/conversations"):
        root = shared / relative
        if root.is_dir():
            for item in root.iterdir():
                record_owner(directory, item.name if item.is_dir() else item.stem, account)
    directory.mkdir(parents=True, exist_ok=True, mode=0o700)
    marker.touch(exist_ok=True)


def prepare(destination: Path, shared: Path) -> None:
    destination, shared = destination.resolve(), shared.resolve()
    if destination == shared:
        return  # The original account stays exactly where it is.
    if destination in shared.parents or shared in destination.parents:
        raise ValueError("account and shared roots must not contain one another")
    cli = destination / "antigravity-cli"
    settings = cli / "settings.json"
    if cli.is_symlink() or settings.is_symlink():
        raise ValueError("refusing shared per-account state/settings symlinks")
    links = []
    for relative in SHARED:
        source, target = shared / relative, destination / relative
        if not source.exists():
            continue
        if target.is_symlink() and target.resolve() == source.resolve():
            continue
        if os.path.lexists(target):
            raise ValueError(f"existing configuration needs manual reconciliation: {target}")
        if target.parent.is_symlink():
            raise ValueError(f"refusing shared configuration parent: {target.parent}")
        links.append((source, target))

    # Read only settings, never any token/credentials file. Copy this ONE key;
    # sharing the entire settings file could share auth method or project state.
    common_settings = shared / "antigravity-cli/settings.json"
    common = json.loads(common_settings.read_text()) if common_settings.exists() else {}
    current = json.loads(settings.read_text()) if settings.exists() else {}
    if not isinstance(common, dict) or not isinstance(current, dict):
        raise ValueError("settings must be JSON objects")
    desired = common.get("statusLine")
    foreign = current.get("statusLine")
    if foreign and desired and foreign.get("command") != desired.get("command"):
        raise ValueError("refusing to replace this account's foreign statusLine")

    destination.mkdir(parents=True, exist_ok=True, mode=0o700)
    cli.mkdir(exist_ok=True, mode=0o700)
    for source, target in links:
        target.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
        target.symlink_to(source)
    if desired is not None and current.get("statusLine") != desired:
        current["statusLine"] = desired
        fd, temporary = tempfile.mkstemp(prefix=".settings-", dir=cli)
        try:
            with os.fdopen(fd, "w") as stream:
                json.dump(current, stream, indent=2)
                stream.write("\n")
            os.replace(temporary, settings)
        finally:
            if os.path.exists(temporary):
                os.unlink(temporary)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--gemini-dir", type=Path, required=True)
    parser.add_argument("--shared-dir", type=Path, required=True)
    parser.add_argument("--owners-dir", type=Path)
    args = parser.parse_args()
    try:
        prepare(args.gemini_dir, args.shared_dir)
        if args.owners_dir:
            seed_owners(args.owners_dir, args.shared_dir)
    except (OSError, ValueError, TypeError, AttributeError) as error:
        print(f"agy account setup: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
