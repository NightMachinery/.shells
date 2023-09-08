#!/usr/bin/env python3
##
import sqlite3
import re
import argparse


def substitute(pattern, replacement, original):
    return re.sub(pattern, replacement, original)


def rename_in_db(db_path, from_pattern, to):
    """Rename patterns in the SQLite database."""

    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    cursor.execute("SELECT rowid, name FROM activity")
    rows = cursor.fetchall()

    #: Update each row after substituting
    for row in rows:
        rowid, name = row
        updated_name = substitute(from_pattern, to, name)
        if updated_name != name:
            cursor.execute(
                "UPDATE activity SET name = ? WHERE rowid = ?", (updated_name, rowid)
            )

    #: Commit changes
    conn.commit()

    conn.close()


def main():
    parser = argparse.ArgumentParser(
        description="Rename patterns in a SQLite database."
    )
    parser.add_argument(
        "--path", required=True, help="Path to the SQLite database file."
    )
    parser.add_argument(
        "--from", dest="from_pattern", required=True, help="Pattern to replace."
    )
    parser.add_argument("--to", required=True, help="String to replace with.")

    args = parser.parse_args()
    rename_in_db(args.path, args.from_pattern, args.to)


if __name__ == "__main__":
    main()
