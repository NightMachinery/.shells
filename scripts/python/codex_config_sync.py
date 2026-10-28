#!/usr/bin/env python3
"""Merge the tracked Codex settings into ~/.codex/config.toml.

Codex rewrites config.toml itself: trusted project folders, dismissed notices,
hook trust hashes, desktop preferences, plugin and MCP entries holding this
machine's paths. So the file cannot be versioned whole. The stable settings live
in configFiles/codex/config.base.toml instead, and this merges them in: tracked
values win, every other key stays as Codex left it, and nothing is deleted.

Needs tomlkit, which keeps Codex's own layout; run it through uv:
    uv run --no-project --with tomlkit python3 codex_config_sync.py --base FILE [--check]
"""
import argparse
import os
import sys
import tempfile
import tomllib
from pathlib import Path

import tomlkit


def plain(item):
    return item.unwrap() if hasattr(item, 'unwrap') else item


def merge(dst, src, path, changes):
    for key, value in src.items():
        here = f'{path}.{key}' if path else key
        if isinstance(value, dict):
            if not isinstance(plain(dst.get(key)), dict):
                if key in dst:
                    changes.append(here)
                dst[key] = tomlkit.table()
            merge(dst[key], value, here, changes)
        elif key not in dst or plain(dst[key]) != value:
            changes.append(here)
            dst[key] = value


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--base', type=Path, required=True, help='tracked settings (config.base.toml)')
    parser.add_argument('--config', type=Path, default=Path.home() / '.codex/config.toml')
    parser.add_argument('--check', action='store_true', help='report drift and exit 1 if any; change nothing')
    args = parser.parse_args()

    base = tomllib.loads(args.base.read_text())
    text = args.config.read_text() if args.config.exists() else ''
    doc = tomlkit.parse(text)
    changes = []
    merge(doc, base, '', changes)
    if not changes:
        return 0

    if args.check:
        print(f'{args.config}: differs from {args.base} in: {", ".join(changes)}')
        return 1

    out = tomlkit.dumps(doc)
    tomllib.loads(out)  # never write a file Codex could not read
    args.config.parent.mkdir(parents=True, exist_ok=True)
    mode = args.config.stat().st_mode & 0o777 if args.config.exists() else 0o600
    fd, tmp = tempfile.mkstemp(dir=args.config.parent, prefix='.config.toml.')
    try:
        with os.fdopen(fd, 'w') as handle:
            handle.write(out)
        os.chmod(tmp, mode)
        os.replace(tmp, args.config)
    except BaseException:
        os.unlink(tmp)
        raise
    print(f'{args.config}: updated {", ".join(changes)}')
    return 0


if __name__ == '__main__':
    sys.exit(main())
