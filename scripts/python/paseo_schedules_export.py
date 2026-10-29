#!/usr/bin/env python3
"""Export Paseo schedule definitions into a directory under version control.

The daemon keeps one JSON file per schedule in ~/.paseo/schedules/, holding the
definition together with every run's history and report. Only the definition is
exported: <id>.json (name, status, cadence, target, dates) and <id>.prompt.md
(the prompt alone, so edits diff readably). Run history and next/last run times
change on every run and are left out. The output mirrors the daemon: a schedule
deleted there loses its files here, and git history keeps the old version.

    paseo_schedules_export.py --src ~/.paseo/schedules --out DIR
"""
import argparse
import json
import os
import sys
import tempfile
from pathlib import Path

KEEP = ('id', 'name', 'status', 'cadence', 'target', 'createdAt', 'updatedAt', 'pausedAt', 'expiresAt', 'maxRuns')


def write_if_changed(path, text):
    if path.exists() and path.read_text() == text:
        return False
    fd, tmp = tempfile.mkstemp(dir=path.parent, prefix=f'.{path.name}.')
    try:
        with os.fdopen(fd, 'w') as handle:
            handle.write(text)
        os.chmod(tmp, 0o644)
        os.replace(tmp, path)
    except BaseException:
        os.unlink(tmp)
        raise
    return True


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--src', type=Path, required=True, help="the daemon's schedules directory")
    parser.add_argument('--out', type=Path, required=True, help='export directory')
    args = parser.parse_args()

    if not args.src.is_dir():
        print(f'{args.src}: no schedules directory', file=sys.stderr)
        return 1
    args.out.mkdir(parents=True, exist_ok=True)

    written, kept = [], set()
    for src in sorted(args.src.glob('*.json')):
        record = json.loads(src.read_text())
        sid = record.get('id') or src.stem
        definition = {key: record[key] for key in KEEP if key in record}
        files = {
            args.out / f'{sid}.json': json.dumps(definition, indent=2, sort_keys=True, ensure_ascii=False) + '\n',
            args.out / f'{sid}.prompt.md': record.get('prompt', '').rstrip('\n') + '\n',
        }
        for path, text in files.items():
            kept.add(path.name)
            if write_if_changed(path, text):
                written.append(path.name)

    removed = []
    for path in sorted(args.out.iterdir()):
        if path.is_file() and path.name.endswith(('.json', '.prompt.md')) and path.name not in kept:
            path.unlink()
            removed.append(path.name)

    print(f'{args.out}: {len(kept) // 2} schedules, {len(written)} files written, {len(removed)} removed')
    for name in written:
        print(f'  wrote {name}')
    for name in removed:
        print(f'  removed {name}')
    return 0


if __name__ == '__main__':
    sys.exit(main())
