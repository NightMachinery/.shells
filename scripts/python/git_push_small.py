#!/usr/bin/env python3
"""
Rewrite the last N local commits into smaller path-based commits and push each one
incrementally to avoid remote per-push size limits.

This utility is intended for repositories where a normal `git push` fails because the
remote rejects a large pack file, for example when the push exceeds GitHub's 2 GiB
single-push limit. The script works by rewriting the last `--split-last-commits` local
commits on the current branch into a sequence of smaller commits, each targeting an
estimated staged payload no larger than `--max-size`, then pushing after each newly
created commit.

## Behavior

* Operates on the current branch.
* Examines the last N commits to rewrite.
* Reapplies each selected commit with `git cherry-pick -n` so its changes are present
  without immediately creating a commit.
* Splits the changed paths into multiple commits whose estimated size stays under the
  configured threshold.
* Pushes after each created commit so the remote receives the history in smaller steps.
* Creates a backup branch before rewriting so recovery is straightforward.
* Refuses to rewrite commits that appear to already be pushed unless explicitly allowed.

This is a practical workaround, not a pack-size oracle. The script estimates commit size
from changed paths and file content as seen locally; Git's actual wire pack can differ
because of compression, delta reuse, and existing remote objects.

## Examples

```
python3 git_push_small.py --split-last-commits=1 --max-size=100mb
python3 git_push_small.py --split-last-commits=3 --max-size=500mb
python3 git_push_small.py --split-last-commits=1 --max-size=1.5GiB --dry-run
python3 git_push_small.py --split-last-commits=1 --max-size=100mb --force-with-lease
```

## Arguments

--split-last-commits=N
Rewrite the last N commits on the current branch. Default: 1.

--max-size=SIZE
Maximum estimated size for each rewritten commit. Supports suffixes such as
b, kb, mb, gb, kib, mib, gib. Examples: 100mb, 500MiB, 1.5GiB.

--dry-run
Show the planned actions without rewriting commits or pushing.

--force-with-lease
Allow rewriting and pushing in situations that require a force push. This should be
used carefully, especially on shared branches.

## Safety

* The working tree must be clean before running the script.
* A backup branch is created before history is rewritten.
* If a single changed file exceeds `--max-size`, the script will generally commit that
  file by itself and continue, but this does not bypass host limits on individual Git
  objects. In that case, Git LFS or history cleanup may still be required.
* This script rewrites history. Do not use it casually on branches other people are
  already consuming unless you understand the consequences.

## Exit status

Returns zero on success. Returns non-zero if preflight checks fail, Git commands fail,
or the rewrite/push sequence cannot be completed safely.
"""

from __future__ import annotations

import argparse
import datetime as dt
import os
import re
import shlex
import subprocess
import sys
from dataclasses import dataclass
from typing import List, Sequence


@dataclass
class CommitMeta:
    sha: str
    subject: str
    body: str
    author_name: str
    author_email: str
    author_date: str


@dataclass
class ChangeEntry:
    status: str
    paths: List[str]
    size_bytes: int


class GitPushSmallError(RuntimeError):
    pass


def eprint(*args: object) -> None:
    print(*args, file=sys.stderr)


def run_git(
    args: Sequence[str],
    *,
    capture_output: bool = True,
    check: bool = True,
    env: dict[str, str] | None = None,
) -> subprocess.CompletedProcess[str]:
    cmd = ["git", *args]
    proc = subprocess.run(
        cmd,
        text=True,
        capture_output=capture_output,
        env=env,
    )
    if check and proc.returncode != 0:
        stderr = proc.stderr.strip()
        stdout = proc.stdout.strip()
        details = stderr or stdout or f"exit code {proc.returncode}"
        raise GitPushSmallError(f"Command failed: {shell_join(cmd)}\n{details}")
    return proc


def run_git_bytes(
    args: Sequence[str],
    *,
    check: bool = True,
) -> subprocess.CompletedProcess[bytes]:
    cmd = ["git", *args]
    proc = subprocess.run(cmd, capture_output=True)
    if check and proc.returncode != 0:
        stderr = proc.stderr.decode("utf-8", errors="replace").strip()
        stdout = proc.stdout.decode("utf-8", errors="replace").strip()
        details = stderr or stdout or f"exit code {proc.returncode}"
        raise GitPushSmallError(f"Command failed: {shell_join(cmd)}\n{details}")
    return proc


def shell_join(parts: Sequence[str]) -> str:
    return " ".join(shlex.quote(p) for p in parts)


def parse_size(value: str) -> int:
    text = value.strip().lower().replace(" ", "")
    match = re.fullmatch(r"(\d+(?:\.\d+)?)([kmgt]?i?b?)?", text)
    if not match:
        raise argparse.ArgumentTypeError(f"Invalid size: {value!r}")
    number = float(match.group(1))
    unit = (match.group(2) or "b")
    multipliers = {
        "b": 1,
        "": 1,
        "k": 1000,
        "kb": 1000,
        "m": 1000**2,
        "mb": 1000**2,
        "g": 1000**3,
        "gb": 1000**3,
        "t": 1000**4,
        "tb": 1000**4,
        "ki": 1024,
        "kib": 1024,
        "mi": 1024**2,
        "mib": 1024**2,
        "gi": 1024**3,
        "gib": 1024**3,
        "ti": 1024**4,
        "tib": 1024**4,
    }
    if unit not in multipliers:
        raise argparse.ArgumentTypeError(f"Invalid size unit: {value!r}")
    return int(number * multipliers[unit])


def human_size(num_bytes: int) -> str:
    units = ["B", "KiB", "MiB", "GiB", "TiB"]
    value = float(num_bytes)
    for unit in units:
        if value < 1024.0 or unit == units[-1]:
            if unit == "B":
                return f"{int(value)} {unit}"
            return f"{value:.2f} {unit}"
        value /= 1024.0
    return f"{num_bytes} B"


def ensure_git_repo() -> None:
    run_git(["rev-parse", "--show-toplevel"])


def ensure_clean_worktree() -> None:
    status = run_git(["status", "--porcelain"]).stdout
    if status.strip():
        raise GitPushSmallError(
            "Working tree is not clean. Commit/stash/discard changes before running this script."
        )


def current_branch() -> str:
    branch = run_git(["rev-parse", "--abbrev-ref", "HEAD"]).stdout.strip()
    if branch == "HEAD":
        raise GitPushSmallError("Detached HEAD is not supported.")
    return branch


def resolve_upstream(branch: str, remote_arg: str | None) -> tuple[str, str]:
    if remote_arg:
        return remote_arg, branch
    proc = run_git(["rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{upstream}"], check=False)
    if proc.returncode == 0:
        upstream = proc.stdout.strip()
        if "/" in upstream:
            remote, remote_branch = upstream.split("/", 1)
            return remote, remote_branch
    return "origin", branch


def count_ahead(remote: str, branch: str) -> int:
    proc = run_git(["rev-list", "--count", f"{remote}/{branch}..HEAD"], check=False)
    if proc.returncode != 0:
        return 0
    return int(proc.stdout.strip() or "0")


def backup_branch_name() -> str:
    timestamp = dt.datetime.now().strftime("%Y%m%d_%H%M%S")
    return f"backup/git_push_small_{timestamp}"


def commit_list(last_n: int) -> List[str]:
    proc = run_git(["rev-list", "--reverse", f"HEAD~{last_n}..HEAD"])
    commits = [line.strip() for line in proc.stdout.splitlines() if line.strip()]
    if len(commits) != last_n:
        raise GitPushSmallError(
            f"Expected {last_n} commits, but found {len(commits)}. "
            "Make sure the branch has at least that many commits."
        )
    return commits


def commit_meta(sha: str) -> CommitMeta:
    fmt = "%H%x00%s%x00%b%x00%an%x00%ae%x00%aI"
    out = run_git(["show", "-s", f"--format={fmt}", sha]).stdout
    parts = out.split("\x00")
    if len(parts) < 6:
        raise GitPushSmallError(f"Failed to read metadata for commit {sha}")
    return CommitMeta(
        sha=parts[0].strip(),
        subject=parts[1].strip(),
        body=parts[2].rstrip("\n"),
        author_name=parts[3].strip(),
        author_email=parts[4].strip(),
        author_date=parts[5].strip(),
    )


def blob_size_in_commit(commit: str, path: str) -> int:
    proc = run_git(["cat-file", "-s", f"{commit}:{path}"], check=False)
    if proc.returncode != 0:
        return 0
    text = proc.stdout.strip()
    return int(text) if text else 0


def changed_entries(commit: str) -> List[ChangeEntry]:
    proc = run_git_bytes(["diff-tree", "--root", "--no-commit-id", "-r", "-M", "--name-status", "-z", commit])
    items = proc.stdout.split(b"\x00")
    if items and items[-1] == b"":
        items.pop()
    entries: List[ChangeEntry] = []
    idx = 0
    while idx < len(items):
        status_token = items[idx].decode("utf-8", errors="surrogateescape")
        idx += 1
        if not status_token:
            continue
        status_code = status_token[0]
        if status_code in {"R", "C"}:
            if idx + 1 >= len(items):
                raise GitPushSmallError(f"Malformed diff-tree output for commit {commit}")
            old_path = items[idx].decode("utf-8", errors="surrogateescape")
            new_path = items[idx + 1].decode("utf-8", errors="surrogateescape")
            idx += 2
            size_bytes = blob_size_in_commit(commit, new_path)
            entries.append(ChangeEntry(status=status_token, paths=[old_path, new_path], size_bytes=size_bytes))
        else:
            if idx >= len(items):
                raise GitPushSmallError(f"Malformed diff-tree output for commit {commit}")
            path = items[idx].decode("utf-8", errors="surrogateescape")
            idx += 1
            size_bytes = 0 if status_code == "D" else blob_size_in_commit(commit, path)
            entries.append(ChangeEntry(status=status_token, paths=[path], size_bytes=size_bytes))
    return entries


def chunk_entries(entries: Sequence[ChangeEntry], max_size: int) -> List[List[ChangeEntry]]:
    if not entries:
        return []
    groups: List[List[ChangeEntry]] = []
    current: List[ChangeEntry] = []
    current_size = 0
    for entry in entries:
        entry_size = max(entry.size_bytes, 1 if entry.status.startswith("D") else 0)
        if current and current_size + entry_size > max_size:
            groups.append(current)
            current = []
            current_size = 0
        current.append(entry)
        current_size += entry_size
        if entry_size > max_size:
            groups.append(current)
            current = []
            current_size = 0
    if current:
        groups.append(current)
    return groups


def flatten_paths(entries: Sequence[ChangeEntry]) -> List[str]:
    seen: set[str] = set()
    result: List[str] = []
    for entry in entries:
        for path in entry.paths:
            if path not in seen:
                seen.add(path)
                result.append(path)
    return result


def stage_group(entries: Sequence[ChangeEntry]) -> None:
    paths = flatten_paths(entries)
    if not paths:
        return
    run_git(["add", "-A", "--", *paths])


def make_commit_message(meta: CommitMeta, index: int, total: int) -> str:
    if total == 1:
        return meta.subject
    return f"{meta.subject} [split {index}/{total} from {meta.sha[:7]}]"


def commit_group(meta: CommitMeta, index: int, total: int) -> None:
    subject = make_commit_message(meta, index, total)
    body_parts: List[str] = []
    if meta.body.strip():
        body_parts.append(meta.body.strip())
    if total > 1:
        body_parts.append(
            f"Split automatically from original commit {meta.sha}. Part {index} of {total}."
        )
    env = os.environ.copy()
    env["GIT_AUTHOR_DATE"] = meta.author_date
    env["GIT_COMMITTER_DATE"] = meta.author_date
    args = [
        "commit",
        "--no-gpg-sign",
        "--author",
        f"{meta.author_name} <{meta.author_email}>",
        "-m",
        subject,
    ]
    for part in body_parts:
        args.extend(["-m", part])
    run_git(args, env=env)


def push_head(remote: str, remote_branch: str, force_with_lease: bool) -> None:
    args = ["push"]
    if force_with_lease:
        args.append("--force-with-lease")
    args.extend([remote, f"HEAD:refs/heads/{remote_branch}"])
    run_git(args)


def verify_clean_after_commit(original_commit: str) -> None:
    status = run_git(["status", "--porcelain"]).stdout
    if status.strip():
        raise GitPushSmallError(
            f"After replaying {original_commit}, the working tree is not clean.\n"
            "This usually means one of the split commits did not stage all required paths."
        )


def original_parent(commit: str) -> str:
    return run_git(["rev-parse", f"{commit}^"]).stdout.strip()


def replay_commit(meta: CommitMeta, max_size: int, remote: str, remote_branch: str, force_with_lease: bool) -> None:
    entries = changed_entries(meta.sha)
    if not entries:
        eprint(f"[warn] Commit {meta.sha[:7]} is empty; recreating it as an empty commit.")
        env = os.environ.copy()
        env["GIT_AUTHOR_DATE"] = meta.author_date
        env["GIT_COMMITTER_DATE"] = meta.author_date
        run_git([
            "commit",
            "--allow-empty",
            "--no-gpg-sign",
            "--author",
            f"{meta.author_name} <{meta.author_email}>",
            "-m",
            meta.subject,
        ], env=env)
        push_head(remote, remote_branch, force_with_lease)
        return

    eprint(f"Applying original commit {meta.sha[:7]}: {meta.subject}")
    cherry = run_git(["cherry-pick", "-n", meta.sha], check=False)
    if cherry.returncode != 0:
        raise GitPushSmallError(
            f"Cherry-pick failed for {meta.sha}. Resolve conflicts manually or lower the split count.\n"
            f"{cherry.stderr.strip() or cherry.stdout.strip()}"
        )

    run_git(["reset"])
    groups = chunk_entries(entries, max_size)
    eprint(f"  -> {len(groups)} split commit(s) planned")
    for idx, group in enumerate(groups, start=1):
        group_size = sum(entry.size_bytes for entry in group)
        if any(entry.size_bytes > max_size for entry in group):
            eprint(
                f"  [warn] A single path in part {idx}/{len(groups)} exceeds the requested max size; "
                "it will be committed alone."
            )
        eprint(f"  -> committing part {idx}/{len(groups)} (~{human_size(group_size)})")
        stage_group(group)
        commit_group(meta, idx, len(groups))
        eprint(f"  -> pushing part {idx}/{len(groups)}")
        push_head(remote, remote_branch, force_with_lease)

    verify_clean_after_commit(meta.sha)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description=(
            "Rewrite the last N commits into smaller path-based commits and push after each new commit."
        )
    )
    parser.add_argument("--split-last-commits", type=int, default=1, help="How many commits from HEAD backward to rewrite (default: 1)")
    parser.add_argument("--max-size", type=parse_size, default=parse_size("100mb"), help="Approximate maximum payload per rewritten commit, e.g. 100mb, 1.5GiB (default: 100mb)")
    parser.add_argument("--remote", default=None, help="Remote name to push to. Defaults to the branch upstream, or origin.")
    parser.add_argument("--force-with-lease", action="store_true", help="Use --force-with-lease when pushing. Needed only if you rewrite commits that are already on the remote.")
    parser.add_argument("--dry-run", action="store_true", help="Show the planned split without rewriting or pushing.")
    return parser


def main() -> int:
    parser = build_parser()
    args = parser.parse_args()

    if args.split_last_commits < 1:
        parser.error("--split-last-commits must be >= 1")
    if args.max_size < 1:
        parser.error("--max-size must be > 0")

    ensure_git_repo()
    ensure_clean_worktree()

    branch = current_branch()
    remote, remote_branch = resolve_upstream(branch, args.remote)
    ahead = count_ahead(remote, remote_branch)
    commits = commit_list(args.split_last_commits)

    if ahead and args.split_last_commits > ahead and not args.force_with_lease:
        raise GitPushSmallError(
            f"The last {args.split_last_commits} commit(s) go further back than the branch is ahead of {remote}/{remote_branch} ({ahead} ahead).\n"
            "Rewriting already-pushed commits would require --force-with-lease."
        )

    if ahead and args.split_last_commits < ahead:
        eprint(
            f"[warn] Branch is {ahead} commit(s) ahead of {remote}/{remote_branch}, but only the last {args.split_last_commits} commit(s) will be split.\n"
            "       The earlier unpushed commits will still be included in the first push."
        )

    plan_lines: List[str] = []
    for sha in commits:
        meta = commit_meta(sha)
        entries = changed_entries(sha)
        groups = chunk_entries(entries, args.max_size) or [[]]
        entry_total = sum(e.size_bytes for e in entries)
        plan_lines.append(
            f"- {sha[:7]} {meta.subject!r}: {len(entries)} path group(s), ~{human_size(entry_total)} total, {len(groups)} resulting commit(s)"
        )

    print(f"Branch:           {branch}")
    print(f"Remote target:    {remote}/{remote_branch}")
    print(f"Split commits:    {args.split_last_commits}")
    print(f"Max chunk size:   {human_size(args.max_size)}")
    print("Plan:")
    for line in plan_lines:
        print(line)

    if args.dry_run:
        return 0

    original_head = run_git(["rev-parse", "HEAD"]).stdout.strip()
    backup = backup_branch_name()
    base = original_parent(commits[0])

    eprint(f"Creating backup branch {backup} at {original_head[:7]}")
    run_git(["branch", backup, original_head])
    eprint(f"Resetting {branch} to {base[:7]}")
    run_git(["reset", "--hard", base])

    try:
        for sha in commits:
            replay_commit(commit_meta(sha), args.max_size, remote, remote_branch, args.force_with_lease)
    except Exception:
        eprint(f"\nA backup of the original history is preserved at: {backup}")
        eprint("You can restore it with:")
        eprint(f"  git reset --hard {backup}")
        raise

    print()
    print("Done.")
    print(f"Backup branch:    {backup}")
    print(f"Original HEAD:    {original_head}")
    print(f"Current HEAD:     {run_git(['rev-parse', 'HEAD']).stdout.strip()}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except GitPushSmallError as exc:
        eprint(f"error: {exc}")
        raise SystemExit(1)
