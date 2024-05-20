#!/usr/bin/env python3
##
#: You can mark the unused files, then check if the project compiles and the result is okay. You can then use `trs **/*.remove_me` to remove the marked files.
##
import os
import re
import argparse
from pynight.common_files import list_children
from pynight.common_icecream import ic


def find_used_files(directory, pattern):
    used_files = set()
    for file_path in list_children(
        directory, abs_include_patterns=[r"\.tex$"], recursive=True
    ):
        with open(file_path, "r") as f:
            content = f.read()
            matches = re.finditer(pattern, content)
            used_files.update(m.group("path") for m in matches)
    return used_files


def add_suffix_to_unused_files(
    directory,
    used_files,
    suffix,
    always_used_patterns,
    dry_run=False,
):
    for file_path in list_children(
        directory,
        abs_include_patterns=[r"\.(png|jpe?g|pdf)$"],
        recursive=True,
    ):
        if not any(file_path.endswith(f) for f in used_files) and not any(
            re.search(pattern, file_path) for pattern in always_used_patterns
        ):
            new_file_path = file_path + suffix
            assert not os.path.exists(new_file_path)

            if not dry_run:
                os.rename(file_path, new_file_path)
            print(f"Added suffix to unused file:\n\t{file_path}")


def remove_suffix_from_files(directory, suffix, dry_run=False):
    for file_path in list_children(
        directory,
        abs_include_patterns=[rf"{re.escape(suffix)}$"],
        recursive=True,
    ):
        new_file_path = file_path[: -len(suffix)]
        assert not os.path.exists(new_file_path)

        if not dry_run:
            os.rename(file_path, new_file_path)
        print(f"Removed suffix from file:\n\t{file_path}")


def main():
    parser = argparse.ArgumentParser(description="Manage unused files in a project.")
    parser.add_argument(
        "-d",
        "--project-directory",
        default=".",
        help="Project directory (default: current directory)",
    )
    parser.add_argument(
        "-f",
        "--files-directory",
        default="./files",
        help="Files directory (default: ./files)",
    )
    parser.add_argument(
        "-p",
        "--pattern",
        # default=r"\{(?P<path>files/.*\.(pdf|png|jpe?g))\}",
        default=r"\b(?P<path>files/.*\.(pdf|png|jpe?g))\b",
        help="Regular expression pattern to match file paths",
    )
    parser.add_argument(
        "-s",
        "--suffix",
        default=".remove_me",
        help="Suffix to add to unused files (default: .remove_me)",
    )
    parser.add_argument(
        "-a",
        "--always-used-patterns",
        nargs="+",
        default=[r"(?:^|/)files/(?:DecompXL|spider)/"],
        help="Patterns for files that are always considered used",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Perform a dry run without making any actual changes",
    )

    subparsers = parser.add_subparsers(dest="command", required=True)

    mark_unused_parser = subparsers.add_parser(
        "mark_unused", help="Mark unused files with a suffix"
    )

    rollback_parser = subparsers.add_parser(
        "rollback", help="Remove the suffix from files"
    )

    args = parser.parse_args()

    if args.command == "mark_unused":
        used_files = find_used_files(args.project_directory, args.pattern)
        add_suffix_to_unused_files(
            args.files_directory,
            used_files,
            args.suffix,
            args.always_used_patterns,
            args.dry_run,
        )
    elif args.command == "rollback":
        remove_suffix_from_files(args.files_directory, args.suffix, args.dry_run)


if __name__ == "__main__":
    main()
