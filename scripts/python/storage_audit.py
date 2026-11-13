#!/usr/bin/env python3
"""Measure directory allocation with bounded, read-only du subprocesses."""

import argparse
import concurrent.futures
import json
import os
from pathlib import Path
import subprocess
import sys


def measure(path, depth, timeout):
    argv = ["/usr/bin/du", "-x", "-k", "-d", str(depth), str(path)]
    timed_out = False
    try:
        run = subprocess.run(argv, capture_output=True, timeout=timeout)
        output, errors, code = run.stdout, run.stderr, run.returncode
    except subprocess.TimeoutExpired as exc:
        output, errors, code = exc.stdout or b"", exc.stderr or b"", None
        timed_out = True
    rows = []
    for line in output.decode(errors="replace").splitlines():
        size, separator, name = line.partition("\t")
        if separator and size.isdecimal():
            rows.append({"path": name, "allocated_bytes": int(size) * 1024})
    rows.sort(key=lambda row: row["allocated_bytes"], reverse=True)
    return {"root": str(path), "complete": code == 0 and not timed_out,
            "timed_out": timed_out, "exit_code": code, "rows": rows,
            "errors": errors.decode(errors="replace").splitlines()}


def positive(value):
    number = int(value)
    if number <= 0:
        raise argparse.ArgumentTypeError("must be positive")
    return number


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("roots", nargs="+", type=Path)
    parser.add_argument("--children", action="store_true",
                        help="scan each immediate subdirectory as a separate job")
    parser.add_argument("--workers", type=positive, default=4)
    parser.add_argument("--timeout", type=positive, default=120,
                        help="seconds per directory, default 120")
    parser.add_argument("--depth", type=int, default=1)
    args = parser.parse_args()
    if args.depth < 0:
        parser.error("--depth must be nonnegative")
    paths = []
    failures = False
    for root in args.roots:
        root = root.expanduser().absolute()
        # Do not resolve symlinks: a link can point to another volume or a live tree.
        if root.is_symlink() or not root.is_dir():
            parser.error(f"root must be a real directory, not a symlink: {root}")
        if args.children:
            try:
                with os.scandir(root) as entries:
                    paths.extend(Path(e.path) for e in entries
                                 if e.is_dir(follow_symlinks=False))
            except OSError as exc:
                parser.error(str(exc))
        else:
            paths.append(root)
    paths = list(dict.fromkeys(paths))
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.workers) as pool:
        jobs = [pool.submit(measure, p, args.depth, args.timeout) for p in paths]
        for job in concurrent.futures.as_completed(jobs):
            result = job.result()
            failures |= not result["complete"]
            print(json.dumps(result, ensure_ascii=True), flush=True)
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
