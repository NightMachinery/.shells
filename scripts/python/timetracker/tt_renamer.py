#!/usr/bin/env python3
##
import sys
import sqlite3
import re
import argparse
from pynight.common_tui import (
    prompt_user,
    ask,
)


def substitute(pattern, replacement, original):
    return re.sub(pattern, replacement, original)


def rename_in_db(db_path, from_pattern, to, interactive_p=False):
    """Rename patterns in the SQLite database."""

    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    cursor.execute("SELECT rowid, name FROM activity")
    rows = cursor.fetchall()

    updates = dict()

    #: Update each row after substituting
    for row in rows:
        rowid, name = row
        updated_name = substitute(from_pattern, to, name)
        if updated_name != name:
            update = (name, updated_name)
            msg = f"{name} -> {updated_name}"

            if update not in updates:
                if interactive_p:
                    updates[update] = ask(
                        f"{msg}",
                        default=True,
                    )
                else:
                    updates[update] = True

                print(msg, file=sys.stderr)

            if not updates[update]:
                continue

            cursor.execute(
                "UPDATE activity SET name = ? WHERE rowid = ?", (updated_name, rowid)
            )

    #: Commit changes
    conn.commit()

    conn.close()


def main():
    parser = argparse.ArgumentParser(
        description="Rename patterns in a SQLite database.",
    )
    parser.add_argument(
        "-p",
        "--path",
        required=True,
        help="Path to the SQLite database file.",
    )
    parser.add_argument(
        "-f",
        "--from",
        dest="from_pattern",
        required=True,
        help="Pattern to replace.",
    )
    parser.add_argument(
        "-t",
        "--to",
        required=True,
        help="String to replace with.",
    )
    parser.add_argument(
        "-i",
        "--interactive",
        type=bool,
        action=argparse.BooleanOptionalAction,
        default=False,
        help="Ask the user to confirm each rename.",
    )

    args = parser.parse_args()
    rename_in_db(
        args.path,
        args.from_pattern,
        args.to,
        interactive_p=args.interactive,
    )


if __name__ == "__main__":
    main()
