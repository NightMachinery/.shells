#!/usr/bin/env python3
"""
Rewrite local commits into smaller path-based commits and push each one incrementally
to avoid remote per-push size limits.

Default mode is `split`, which rewrites the last N local commits on the current branch
into smaller path-based commits and pushes after each new commit.

The `resume` subcommand resumes an interrupted split from the current branch. It infers
the original commit SHA and split numbering from HEAD, which must have a subject like:

    SUBJECT [split 31/163 from b0c8276]

Because `--max-size` is not encoded in the commit subject, `resume` still requires it and
verifies that re-chunking the remaining paths produces the expected number of remaining parts.

If `--from-backup-branch` is provided, `resume` can also continue with the original commits
that come after the current split's source commit on that backup branch. This works both when
the current split is incomplete and when HEAD is already the final split part, such as 163/163.

Examples:

    python3 git_push_small.py --split-last-commits=1 --max-size=100mb
    python3 git_push_small.py split --split-last-commits=3 --max-size=500mb
    python3 git_push_small.py split --split-last-commits=1 --max-size=1.5GiB --dry-run
    python3 git_push_small.py split --split-last-commits=1 --max-size=100mb --force-with-lease
    python3 git_push_small.py resume --max-size=20mb
    python3 git_push_small.py resume --max-size=20mb --from-backup-branch=backup/git_push_small_20260411_162223
    python3 git_push_small.py resume --max-size=20mb --dry-run
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


SPLIT_SUBJECT_RE = re.compile(
    r"^(?P<original_subject>.*) \[split (?P<index>\d+)/(?P<total>\d+) from (?P<original_sha>[0-9a-fA-F]{7,40})\]$"
)


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


@dataclass
class SplitHeadInfo:
    original_subject: str
    current_index: int
    total_parts: int
    original_sha: str


class GitPushSmallError(RuntimeError):
    pass


def eprint(*args: object) -> None:
    print(*args, file=sys.stderr)


def shell_join(parts: Sequence[str]) -> str:
    return " ".join(shlex.quote(p) for p in parts)


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


def parse_size(value: str) -> int:
    text = value.strip().lower().replace(" ", "")
    match = re.fullmatch(r"(\d+(?:\.\d+)?)([kmgt]?i?b?)?", text)
    if not match:
        raise argparse.ArgumentTypeError(f"Invalid size: {value!r}")
    number = float(match.group(1))
    unit = match.group(2) or "b"
    multipliers = {
        "": 1,
        "b": 1,
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
            "Working tree is not clean. Commit/stash/discard changes before running this command."
        )


def current_branch() -> str:
    branch = run_git(["rev-parse", "--abbrev-ref", "HEAD"]).stdout.strip()
    if branch == "HEAD":
        raise GitPushSmallError("Detached HEAD is not supported.")
    return branch


def resolve_upstream(branch: str, remote_arg: str | None) -> tuple[str, str]:
    if remote_arg:
        return remote_arg, branch
    proc = run_git(
        ["rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{upstream}"],
        check=False,
    )
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


def backup_branch_name(prefix: str = "git_push_small") -> str:
    timestamp = dt.datetime.now().strftime("%Y%m%d_%H%M%S")
    return f"backup/{prefix}_{timestamp}"


def commit_list(last_n: int) -> List[str]:
    proc = run_git(["rev-list", "--reverse", f"HEAD~{last_n}..HEAD"])
    commits = [line.strip() for line in proc.stdout.splitlines() if line.strip()]
    if len(commits) != last_n:
        raise GitPushSmallError(
            f"Expected {last_n} commits, but found {len(commits)}. "
            "Make sure the branch has at least that many commits."
        )
    return commits


def commit_meta(rev: str) -> CommitMeta:
    fmt = "%H%x00%s%x00%b%x00%an%x00%ae%x00%aI"
    out = run_git(["show", "-s", f"--format={fmt}", rev]).stdout
    parts = out.split("\x00")
    if len(parts) < 6:
        raise GitPushSmallError(f"Failed to read metadata for {rev}")
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
    proc = run_git_bytes(
        ["diff-tree", "--root", "--no-commit-id", "-r", "-M", "--name-status", "-z", commit]
    )
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
            entries.append(
                ChangeEntry(
                    status=status_token,
                    paths=[old_path, new_path],
                    size_bytes=size_bytes,
                )
            )
        else:
            if idx >= len(items):
                raise GitPushSmallError(f"Malformed diff-tree output for commit {commit}")
            path = items[idx].decode("utf-8", errors="surrogateescape")
            idx += 1
            size_bytes = 0 if status_code == "D" else blob_size_in_commit(commit, path)
            entries.append(
                ChangeEntry(status=status_token, paths=[path], size_bytes=size_bytes)
            )
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


def make_commit_message(meta: CommitMeta, part_number: int, total_parts: int) -> str:
    if total_parts == 1:
        return meta.subject
    return f"{meta.subject} [split {part_number}/{total_parts} from {meta.sha[:7]}]"


def commit_group(meta: CommitMeta, part_number: int, total_parts: int) -> None:
    subject = make_commit_message(meta, part_number, total_parts)
    body_parts: List[str] = []
    if meta.body.strip():
        body_parts.append(meta.body.strip())
    if total_parts > 1:
        body_parts.append(
            f"Split automatically from original commit {meta.sha}. "
            f"Part {part_number} of {total_parts}."
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


def replay_commit(
    meta: CommitMeta,
    max_size: int,
    remote: str,
    remote_branch: str,
    force_with_lease: bool,
) -> None:
    entries = changed_entries(meta.sha)
    if not entries:
        eprint(f"[warn] Commit {meta.sha[:7]} is empty; recreating it as an empty commit.")
        env = os.environ.copy()
        env["GIT_AUTHOR_DATE"] = meta.author_date
        env["GIT_COMMITTER_DATE"] = meta.author_date
        run_git(
            [
                "commit",
                "--allow-empty",
                "--no-gpg-sign",
                "--author",
                f"{meta.author_name} <{meta.author_email}>",
                "-m",
                meta.subject,
            ],
            env=env,
        )
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


def parse_split_head_subject(subject: str) -> SplitHeadInfo:
    match = SPLIT_SUBJECT_RE.fullmatch(subject)
    if not match:
        raise GitPushSmallError(
            "HEAD does not look like a split commit created by this script.\n"
            "Expected a subject like:\n"
            "  SUBJECT [split 31/163 from b0c8276]"
        )

    current_index = int(match.group("index"))
    total_parts = int(match.group("total"))
    if current_index < 1 or total_parts < 1 or current_index > total_parts:
        raise GitPushSmallError(
            f"HEAD subject has invalid split numbering: {subject!r}"
        )

    return SplitHeadInfo(
        original_subject=match.group("original_subject"),
        current_index=current_index,
        total_parts=total_parts,
        original_sha=match.group("original_sha"),
    )


def decode_z_paths(data: bytes) -> List[str]:
    items = data.split(b"\x00")
    if items and items[-1] == b"":
        items.pop()
    return [item.decode("utf-8", errors="surrogateescape") for item in items if item]


def git_paths_z(args: Sequence[str]) -> List[str]:
    return decode_z_paths(run_git_bytes(args).stdout)


def staged_paths() -> List[str]:
    return git_paths_z(["diff", "--cached", "--name-only", "-z"])


def unstaged_paths() -> List[str]:
    return git_paths_z(["diff", "--name-only", "-z"])


def untracked_paths() -> List[str]:
    return git_paths_z(["ls-files", "--others", "--exclude-standard", "-z"])


def format_path_sample(paths: Sequence[str], limit: int = 10) -> str:
    shown = list(paths[:limit])
    lines = [f"  {path}" for path in shown]
    if len(paths) > limit:
        lines.append(f"  ... and {len(paths) - limit} more")
    return "\n".join(lines)


def ensure_ref_exists(rev: str) -> None:
    run_git(["rev-parse", "--verify", f"{rev}^{{commit}}"])


def commits_after_on_branch(original_commit: str, branch: str) -> List[str]:
    ensure_ref_exists(branch)
    proc = run_git(["merge-base", "--is-ancestor", original_commit, branch], check=False)
    if proc.returncode != 0:
        raise GitPushSmallError(
            f"Backup branch {branch!r} does not contain original commit {original_commit}."
        )
    proc = run_git(["rev-list", "--reverse", f"{original_commit}..{branch}"])
    return [line.strip() for line in proc.stdout.splitlines() if line.strip()]


def remaining_entries_for_resume(original_commit: str, dirty_paths: Sequence[str]) -> List[ChangeEntry]:
    entries = changed_entries(original_commit)
    dirty_set = set(dirty_paths)
    expected_paths = set(flatten_paths(entries))

    unexpected = sorted(dirty_set - expected_paths)
    if unexpected:
        raise GitPushSmallError(
            "Working tree contains paths that do not belong to the interrupted split commit.\n"
            "Resume only supports the untouched dirty state left behind by the failed run.\n"
            f"{format_path_sample(unexpected)}"
        )

    remaining = [entry for entry in entries if any(path in dirty_set for path in entry.paths)]
    if not remaining:
        raise GitPushSmallError(
            "No remaining paths from the interrupted split commit were found in the working tree."
        )
    return remaining


def create_resume_backups(include_dirty_state: bool) -> tuple[str, str | None]:
    head = run_git(["rev-parse", "HEAD"]).stdout.strip()
    backup_branch = backup_branch_name("git_push_small_resume")
    stash_ref: str | None = None

    eprint(f"Creating resume backup branch {backup_branch} at {head[:7]}")
    run_git(["branch", backup_branch, head])

    if include_dirty_state:
        stash_label = backup_branch.split("/", 1)[1]
        eprint("Capturing current dirty resume state in a stash backup")
        stash_push = run_git(
            ["stash", "push", "--include-untracked", "-m", stash_label],
            check=False,
        )
        stash_text = f"{stash_push.stdout}\n{stash_push.stderr}".strip()

        if stash_push.returncode != 0:
            raise GitPushSmallError(
                "Failed to create a stash backup for the dirty resume state.\n"
                f"{stash_text}"
            )
        if "No local changes to save" in stash_text:
            raise GitPushSmallError("Expected a dirty working tree for resume, but nothing was stashed.")

        stash_ref = "stash@{0}"
        restore = run_git(["stash", "apply", stash_ref], check=False)
        if restore.returncode != 0:
            raise GitPushSmallError(
                f"Created backup branch {backup_branch} and stash {stash_ref}, "
                "but failed to restore the dirty working tree.\n"
                "You can restore it manually with:\n"
                f"  git stash apply {stash_ref}\n\n"
                f"{restore.stderr.strip() or restore.stdout.strip()}"
            )

    return backup_branch, stash_ref


def plan_lines_for_commits(commits: Sequence[str], max_size: int) -> List[str]:
    lines: List[str] = []
    for sha in commits:
        meta = commit_meta(sha)
        entries = changed_entries(sha)
        groups = chunk_entries(entries, max_size) or [[]]
        entry_total = sum(e.size_bytes for e in entries)
        lines.append(
            f"- {sha[:7]} {meta.subject!r}: {len(entries)} path group(s), "
            f"~{human_size(entry_total)} total, {len(groups)} resulting commit(s)"
        )
    return lines


def run_split(args: argparse.Namespace) -> int:
    if args.split_last_commits < 1:
        raise GitPushSmallError("--split-last-commits must be >= 1")
    if args.max_size < 1:
        raise GitPushSmallError("--max-size must be > 0")

    ensure_git_repo()
    ensure_clean_worktree()

    branch = current_branch()
    remote, remote_branch = resolve_upstream(branch, args.remote)
    ahead = count_ahead(remote, remote_branch)
    commits = commit_list(args.split_last_commits)

    if ahead and args.split_last_commits > ahead and not args.force_with_lease:
        raise GitPushSmallError(
            f"The last {args.split_last_commits} commit(s) go further back than the branch is ahead of "
            f"{remote}/{remote_branch} ({ahead} ahead).\n"
            "Rewriting already-pushed commits would require --force-with-lease."
        )

    if ahead and args.split_last_commits < ahead:
        eprint(
            f"[warn] Branch is {ahead} commit(s) ahead of {remote}/{remote_branch}, but only the last "
            f"{args.split_last_commits} commit(s) will be split.\n"
            "       The earlier unpushed commits will still be included in the first push."
        )

    plan_lines = plan_lines_for_commits(commits, args.max_size)

    print(f"Mode:             split")
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
    backup = backup_branch_name("git_push_small")
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


def run_resume(args: argparse.Namespace) -> int:
    if args.max_size < 1:
        raise GitPushSmallError("--max-size must be > 0")

    ensure_git_repo()
    branch = current_branch()
    remote, remote_branch = resolve_upstream(branch, args.remote)

    head_meta = commit_meta("HEAD")
    head_info = parse_split_head_subject(head_meta.subject)
    original_meta = commit_meta(head_info.original_sha)

    if original_meta.subject != head_info.original_subject:
        raise GitPushSmallError(
            "HEAD says it was split from a commit whose subject does not match the current original commit metadata.\n"
            f"HEAD encoded subject: {head_info.original_subject!r}\n"
            f"Original commit subject: {original_meta.subject!r}"
        )

    later_commits: List[str] = []
    if args.from_backup_branch:
        later_commits = commits_after_on_branch(original_meta.sha, args.from_backup_branch)

    current_complete = head_info.current_index == head_info.total_parts
    staged = staged_paths()
    dirty = sorted(set(unstaged_paths()) | set(untracked_paths()))

    resume_groups: List[List[ChangeEntry]] = []
    remaining_entries: List[ChangeEntry] = []

    if current_complete:
        if staged or dirty:
            raise GitPushSmallError(
                "HEAD already looks like the final split part, so resume expects a clean working tree "
                "before continuing with any later commits.\n"
                "Clean or stash your current changes first."
            )
    else:
        if staged:
            raise GitPushSmallError(
                "Resume expects no staged changes. The interrupted run should leave only unstaged/untracked paths.\n"
                f"{format_path_sample(sorted(staged))}"
            )
        if not dirty:
            raise GitPushSmallError(
                "HEAD says the current split is incomplete, but the working tree is clean.\n"
                "Resume needs the dirty remainder left behind by the interrupted run."
            )

        remaining_entries = remaining_entries_for_resume(original_meta.sha, dirty)
        resume_groups = chunk_entries(remaining_entries, args.max_size)
        expected_remaining = head_info.total_parts - head_info.current_index

        if len(resume_groups) != expected_remaining:
            raise GitPushSmallError(
                f"Resume plan mismatch: HEAD says part {head_info.current_index}/{head_info.total_parts}, "
                f"so {expected_remaining} part(s) should remain, but chunking the remaining paths with "
                f"--max-size={human_size(args.max_size)} produced {len(resume_groups)} part(s).\n"
                "Make sure you passed the same --max-size as the interrupted run and that the working tree "
                "still reflects the untouched interrupted replay."
            )

    later_plan_lines = plan_lines_for_commits(later_commits, args.max_size) if later_commits else []

    print(f"Mode:             resume")
    print(f"Branch:           {branch}")
    print(f"Remote target:    {remote}/{remote_branch}")
    print(f"Current HEAD:     {head_meta.sha[:7]} {head_meta.subject!r}")
    print(f"Original commit:  {original_meta.sha[:7]} {original_meta.subject!r}")
    print(f"Max chunk size:   {human_size(args.max_size)}")

    if current_complete:
        print(f"Current split:    complete at {head_info.current_index}/{head_info.total_parts}")
    else:
        remaining_total = sum(entry.size_bytes for entry in remaining_entries)
        print(f"Current split:    incomplete at {head_info.current_index}/{head_info.total_parts}")
        print(f"Next split part:  {head_info.current_index + 1}/{head_info.total_parts}")
        print(f"Remaining paths:  {len(remaining_entries)} path group(s)")
        print(f"Remaining size:   ~{human_size(remaining_total)}")
        print(f"Remaining parts:  {len(resume_groups)}")

    if args.from_backup_branch:
        print(f"Backup branch:    {args.from_backup_branch}")
        print(f"Later commits:    {len(later_commits)}")
        if later_plan_lines:
            print("Continuation plan:")
            for line in later_plan_lines:
                print(line)
    elif current_complete:
        print("Nothing to do:    current split is already complete and no backup branch was provided")

    if args.dry_run:
        return 0

    if current_complete and not later_commits:
        return 0

    backup_branch, stash_ref = create_resume_backups(include_dirty_state=not current_complete)

    try:
        if not current_complete:
            for offset, group in enumerate(resume_groups, start=1):
                part_number = head_info.current_index + offset
                group_size = sum(entry.size_bytes for entry in group)
                if any(entry.size_bytes > args.max_size for entry in group):
                    eprint(
                        f"  [warn] A single path in part {part_number}/{head_info.total_parts} exceeds "
                        "the requested max size; it will be committed alone."
                    )
                eprint(
                    f"  -> committing resumed part {part_number}/{head_info.total_parts} "
                    f"(~{human_size(group_size)})"
                )
                stage_group(group)
                commit_group(original_meta, part_number, head_info.total_parts)
                eprint(f"  -> pushing part {part_number}/{head_info.total_parts}")
                push_head(remote, remote_branch, False)

            verify_clean_after_commit(original_meta.sha)

        for sha in later_commits:
            replay_commit(commit_meta(sha), args.max_size, remote, remote_branch, False)

    except Exception:
        eprint(f"\nResume backup branch: {backup_branch}")
        if stash_ref:
            eprint(f"Resume backup stash:  {stash_ref}")
            eprint("You can restore the dirty resume state with:")
            eprint(f"  git stash apply {stash_ref}")
        eprint("And reset the branch with:")
        eprint(f"  git reset --hard {backup_branch}")
        raise

    print()
    print("Done.")
    print(f"Resume backup:    {backup_branch}")
    if stash_ref:
        print(f"Resume stash:     {stash_ref}")
    print(f"Current HEAD:     {run_git(['rev-parse', 'HEAD']).stdout.strip()}")
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description=(
            "Rewrite large local commits into smaller path-based commits and push incrementally. "
            "Without a subcommand, the script defaults to `split` for backward compatibility."
        )
    )
    subparsers = parser.add_subparsers(dest="command")

    split_parser = subparsers.add_parser(
        "split",
        help="Rewrite the last N commits into smaller path-based commits and push after each one.",
    )
    split_parser.add_argument(
        "--split-last-commits",
        type=int,
        default=1,
        help="How many commits from HEAD backward to rewrite (default: 1)",
    )
    split_parser.add_argument(
        "--max-size",
        type=parse_size,
        default=parse_size("100mb"),
        help="Approximate maximum payload per rewritten commit, e.g. 100mb, 1.5GiB (default: 100mb)",
    )
    split_parser.add_argument(
        "--remote",
        default=None,
        help="Remote name to push to. Defaults to the branch upstream, or origin.",
    )
    split_parser.add_argument(
        "--force-with-lease",
        action="store_true",
        help="Use --force-with-lease when pushing. Needed only if you rewrite commits that are already on the remote.",
    )
    split_parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show the planned split without rewriting commits or pushing.",
    )

    resume_parser = subparsers.add_parser(
        "resume",
        help="Resume an interrupted split and optionally continue later original commits from a backup branch.",
    )
    resume_parser.add_argument(
        "--max-size",
        type=parse_size,
        required=True,
        help="Approximate maximum payload per resumed commit. Must match the interrupted run.",
    )
    resume_parser.add_argument(
        "--remote",
        default=None,
        help="Remote name to push to. Defaults to the branch upstream, or origin.",
    )
    resume_parser.add_argument(
        "--from-backup-branch",
        default=None,
        help="Optional backup branch containing the original unsplit commits. If provided, resume will also continue the later original commits found after the current split's source commit on that branch.",
    )
    resume_parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show the inferred resume plan without committing or pushing.",
    )

    return parser


def normalize_argv(argv: Sequence[str]) -> List[str]:
    args = list(argv)
    if not args:
        return ["split"]
    first = args[0]
    if first in {"split", "resume", "-h", "--help"}:
        return args
    return ["split", *args]


def main(argv: Sequence[str] | None = None) -> int:
    parser = build_parser()
    normalized = normalize_argv(sys.argv[1:] if argv is None else argv)
    args = parser.parse_args(normalized)

    if args.command == "split":
        return run_split(args)
    if args.command == "resume":
        return run_resume(args)

    raise GitPushSmallError(f"Unknown command: {args.command!r}")


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except GitPushSmallError as exc:
        eprint(f"error: {exc}")
        raise SystemExit(1)
