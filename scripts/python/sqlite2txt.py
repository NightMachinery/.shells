#!/usr/bin/env python3
##
import sqlite3
import sys
import os
import binascii
import csv
import json
import argparse
from typing import Any, Dict, TextIO
from icecream import ic
from pynight.common_json import JSONEncoderWithFallback


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Export SQLite data to CSV or JSON.")
    parser.add_argument("db", metavar="DB", type=str, help="The SQLite database file.")
    parser.add_argument(
        "-o",
        "--output",
        metavar="O",
        type=str,
        default="-",
        help="The output file. Default is stdout.",
    )
    parser.add_argument(
        "-f",
        "--format",
        metavar="F",
        type=str,
        default="csv",
        choices=["csv", "json"],
        help="The output format. Choices are csv and json. Default is csv.",
    )
    parser.add_argument(
        "--json-indent",
        type=str,
        default="compact",
        help="Specify the JSON output indentation. Default is 'compact', which means no indentation.",
    )
    parser.add_argument(
        "-t",
        "--table",
        metavar="T",
        type=str,
        default="MAGIC_ALL_TABLES",
        help="The specific table to export. Default is MAGIC_ALL_TABLES which exports all tables.",
    )
    parser.add_argument(
        "--magnet-link-field",
        metavar="M",
        type=str,
        default=None,
        help="The field to convert to a magnet link. Default is None.",
    )
    return parser.parse_args()


def hash_to_magnet(hash: bytes) -> str:
    hex_hash = binascii.hexlify(hash).decode()
    return f"magnet:?xt=urn:btih:{hex_hash}"


def process_row(
    headers: Dict[str, Any], row: sqlite3.Row, magnet_field: str = None
) -> Dict[str, Any]:
    return {
        header: hash_to_magnet(value) if header == magnet_field else value
        for header, value in zip(headers, row)
    }


def export_data(
    cursor: sqlite3.Cursor,
    output: TextIO,
    format: str,
    json_indent: str,
    table: str,
    magnet_field: str = None,
) -> None:
    """Export data from SQLite cursor to CSV or JSON.

    Args:
        cursor (sqlite3.Cursor): The SQLite cursor.
        output (TextIO): The output destination.
        format (str): The output format.
        json_indent (str): The JSON indentation setting.
        table (str): The name of the table to be exported.
        magnet_field (str): The field to convert to a magnet link.
    """

    indent = None if json_indent == "compact" else int(json_indent)
    encoder = JSONEncoderWithFallback(indent=indent, fallback_function=str)

    cursor.execute(f"SELECT * FROM {table}")
    headers = [description[0] for description in cursor.description]
    if format == "csv":
        writer = csv.DictWriter(output, fieldnames=headers)
        writer.writeheader()
        for row in cursor:
            try:
                writer.writerow(process_row(headers, row, magnet_field))
            except Exception as e:
                print(f"Error encoding row: {row}", file=sys.stderr)
                print(str(e), file=sys.stderr)
    elif format == "json":
        output.write("[\n")
        first_row = True
        for row in cursor:
            try:
                processed_row = process_row(headers, row, magnet_field)
                json_output = encoder.encode(processed_row)
            except Exception as e:
                print(f"Error encoding row: {row}", file=sys.stderr)
                print(str(e), file=sys.stderr)
                continue
            if not first_row:
                output.write(",\n")
            output.write(json_output)
            first_row = False
        output.write("]\n")


def main():
    args = parse_args()
    with sqlite3.connect(args.db) as conn:
        conn.row_factory = sqlite3.Row
        cursor = conn.cursor()

        tables = []
        if args.table == "MAGIC_ALL_TABLES":
            cursor.execute("SELECT name FROM sqlite_master WHERE type='table'")
            tables = [tablerow[0] for tablerow in cursor.fetchall()]
        else:
            tables = [args.table]

        filename, file_extension = os.path.splitext(args.output)

        for table in tables:
            if args.output == "-":
                output = sys.stdout
            else:
                if len(tables) > 1:
                    if file_extension:
                        output_filename = f"{filename}_{table}{file_extension}"
                    else:
                        output_filename = f"{filename}_{table}"
                else:
                    if file_extension:
                        output_filename = f"{filename}{file_extension}"
                    else:
                        output_filename = f"{filename}"

                output = open(output_filename, "w", newline="")

            try:
                export_data(
                    cursor,
                    output,
                    args.format,
                    args.json_indent,
                    table=table,
                    magnet_field=args.magnet_link_field,
                )
            finally:
                if output is not sys.stdout:
                    output.close()


if __name__ == "__main__":
    main()
