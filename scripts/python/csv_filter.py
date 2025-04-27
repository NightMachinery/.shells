#!/usr/bin/env python3
# * @todo
# ** Stable-sort the columns in the order specified by the include patterns.
# *** Some columns might match more than once, just consider their first match.
##
import os
import sys
import csv
import re
import argparse
from operator import itemgetter
import pynight.common_icecream
from types import SimpleNamespace
from pynight.common_icecream import ic
from pynight.common_iterable import flatten1_iterable
from pynight.common_sort import version_sort_key
from pynight.common_debugging2 import ipdb_enable
from pynight.common_str import try_float


def average(lst):
    return sum(lst) / len(lst)


def sort_rows(rows, header, sort_specs):
    for sort_spec in sort_specs:
        index = header.index(sort_spec.column)
        rows.sort(
            key=(
                lambda k: version_sort_key(
                    itemgetter(index)(k),
                    float_p=True,
                )
            ),
            reverse=(sort_spec.order == "d"),
        )
    return rows


def filter_csv(
    input_stream,
    include_patterns=None,
    exclude_patterns=None,
    exclude_row_fn=None,
    include_row_patterns=None,
    exclude_row_patterns=None,
    sort_specs=None,
    preprocess_fns=None,
    input_name=None,
):
    reader = csv.reader(input_stream)
    header = next(reader)

    for pat in include_row_patterns:
        if pat.column_name is None:
            pat.column_name = header[0]

    for pat in exclude_row_patterns:
        if pat.column_name is None:
            pat.column_name = header[0]

    if include_patterns:
        columns_to_write = [
            col
            for col in header
            if any(re.search(pat, col) for pat in include_patterns)
        ]
    else:
        columns_to_write = header

    if exclude_patterns:
        columns_to_write = [
            col
            for col in columns_to_write
            if not any(re.search(pat, col) for pat in exclude_patterns)
        ]

    all_rows = [row for row in reader]

    filtered_rows = []
    for row in all_rows:
        row_dict = {col: row[header.index(col)] for col in columns_to_write}

        if include_row_patterns:
            include_row = all(
                re.search(pat.pattern, row[header.index(pat.column_name)])
                for pat in include_row_patterns
            )
            if not include_row:
                continue

        if exclude_row_patterns:
            exclude_row = any(
                re.search(pat.pattern, row[header.index(pat.column_name)])
                for pat in exclude_row_patterns
            )
            if exclude_row:
                continue

        if exclude_row_fn:
            should_exclude = eval(
                exclude_row_fn,
                {
                    **globals(),
                    "row": row_dict,
                },
            )
            # ic(row_dict, should_exclude)
            if should_exclude:
                continue

        filtered_rows.append([row[header.index(col)] for col in columns_to_write])

    if preprocess_fns:
        for preprocess_fn in preprocess_fns:
            # Check if the current preprocessing function should be applied to this file
            if re.search(preprocess_fn.file_include_pattern, input_name):
                # ic(preprocess_fn.file_include_pattern, input_name)

                # Iterate over each column in the header
                for index, column_name in enumerate(columns_to_write):
                    # Check if the column name matches the regex pattern in preprocess_fn
                    if re.search(preprocess_fn.column, column_name):
                        column_values = [try_float(row[index]) for row in filtered_rows]
                        for row in filtered_rows:
                            current_value = try_float(row[index])
                            new_value = eval(
                                preprocess_fn.code,
                                {
                                    **globals(),  # Include all global variables and functions
                                    "current_value": current_value,
                                    "c": current_value,
                                    "all_values": column_values,
                                    "a": column_values,
                                },
                            )

                            # ic(new_value, current_value, max(column_values))
                            if new_value is not None:
                                row[index] = new_value

    if sort_specs:
        filtered_rows = sort_rows(filtered_rows, header, sort_specs)

    return columns_to_write, filtered_rows


def join_csvs(joined_headers, all_filtered_rows, join_column, join_fn):
    join_dict = {}
    join_col_idxs = [headers.index(join_column) for headers in joined_headers]

    for headers, rows, join_col_idx in zip(
        joined_headers, all_filtered_rows, join_col_idxs
    ):
        for row in rows:
            key = row[join_col_idx]
            # ic(key)
            if key not in join_dict:
                join_dict[key] = []
            join_dict[key].append(row)

    joined_rows = []
    for key, value_groups in join_dict.items():
        joined_row = [None] * len(joined_headers[0])
        joined_row[join_col_idxs[0]] = key
        for group_idx in range(len(joined_headers[0])):
            if group_idx == join_col_idxs[0]:
                continue
            values = []
            for group in value_groups:
                value = try_float(group[group_idx])
                values.append(value)
            joined_value = eval(
                join_fn,
                {
                    **globals(),  # Include all global variables and functions,
                    "v": values,
                },
            )
            joined_row[group_idx] = joined_value

        joined_rows.append(joined_row)

    return joined_rows


def main():
    parser = argparse.ArgumentParser(
        description="Filter columns and rows of a CSV file."
    )
    parser.add_argument(
        "-f",
        "--input",
        type=argparse.FileType("r"),
        nargs="+",
        action="append",
        default=[],
        help="Path to the input CSV files. Default is stdin.",
    )
    parser.add_argument(
        "-o",
        "--output",
        type=argparse.FileType("w"),
        default=sys.stdout,
        help="Path to save the filtered CSV file. Default is stdout.",
    )
    parser.add_argument(
        "-i",
        "--include-patterns",
        nargs="*",
        action="append",
        help="List of patterns to include columns. None means all columns are included.",
    )
    parser.add_argument(
        "-e",
        "--exclude-patterns",
        nargs="*",
        action="append",
        help="List of patterns to exclude columns. None means no columns are excluded.",
    )
    parser.add_argument(
        "-ir",
        "--include-row",
        nargs="*",
        action="append",
        help="Pattern to include rows based on a specific column. Format: '<column_name>:<pattern>'.",
    )
    parser.add_argument(
        "-er",
        "--exclude-row",
        nargs="*",
        action="append",
        help="Pattern to exclude rows based on a specific column. Format: '<column_name>:<pattern>'.",
    )
    parser.add_argument(
        "--exclude-row-fn",
        type=str,
        help="Python code to run for excluding rows. It will be run in a lambda that has access to a dictionary `row` containing the current row's data.",
    )
    #: Add =--exclude-fn="<code>"= where =<code>= is run a lambda that has access to a dictionary =row= which has the data of the current row in a dict format.

    parser.add_argument(
        "-s",
        "--sort",
        type=str,
        nargs="*",
        action="append",
        help="Sort by a specific column. Format: '<column>:D|A|d|a'. D/d for descending (default), A/a for ascending. Multiple sorts are stable.",
    )

    parser.add_argument(
        "--join-column",
        type=str,
        help="Column to join on when multiple input files are provided.",
    )
    parser.add_argument(
        "--join-fn",
        type=str,
        help="Python code to run for joining values from different files. Example: 'sum(v)'",
    )
    #: @prompt Add the ability to specify multiple input files and `join_column`, `join_fn`. `join_fn` is Python code that will be run in a lambda that has the values from the different files in an array named `v`. An example to sum the values is `--join-fn 'sum(v)'`.

    parser.add_argument(
        "--preprocess-fn",
        type=str,
        nargs="*",
        action="append",
        help="Preprocessing function for a specific column. Format: '<column>:<code>'.",
    )
    #: @prompt Add `--preprocess-fn '<column>:<code>'`, where code is run in a lambda that gets two arguments `c` (the current value of that column in the current row) and `a` (a list of all values of this column across all rows in the current file), and the value is replaced by what `<code>` returns. E.g., `--preprocess-fn 'accuracy:c/max(a)'.
    #: Make `--preprocess-fn` accept `<file_include_pattern>:<column>:<code>` where `file_include_pattern` is a regex pattern that is matched against the full path of the files given to the script, with `stdin` being `"-"`. If `file_include_pattern` is empty or missing, assume `r".*"`.

    args = parser.parse_args()

    # Flatten the argument lists using flatten_list function
    flatten_list = flatten1_iterable
    if args.include_patterns:
        args.include_patterns = flatten_list(args.include_patterns)
    if args.exclude_patterns:
        args.exclude_patterns = flatten_list(args.exclude_patterns)
    if args.include_row:
        args.include_row = flatten_list(args.include_row)
    if args.exclude_row:
        args.exclude_row = flatten_list(args.exclude_row)
    if args.input:
        args.input = flatten_list(args.input)

    sort_specs = []
    if args.sort:
        args.sort = flatten1_iterable(args.sort)
        for item in args.sort:
            column, _, order = item.partition(":")
            order = order.lower() if order else "d"
            if order not in ["d", "a"]:
                raise ValueError(f"Invalid sort order: {order}. Must be 'D' or 'A'.")

            sort_specs.append(SimpleNamespace(column=column, order=order))

    preprocess_fns = []
    if args.preprocess_fn:
        args.preprocess_fn = flatten_list(args.preprocess_fn)
        for item in args.preprocess_fn:
            parts = item.split(":")
            if len(parts) >= 3:
                file_include_pattern = parts[0]
                column = parts[1]
                code = ":".join(parts[2:])
            elif len(parts) == 2:
                file_include_pattern, column, code = ".*", parts[0], parts[1]
            else:
                raise ValueError(
                    f"preprocess_fn does not conform to `[<file_include_pattern>:]<column>:<code>`: {item}"
                )

            preprocess_fns.append(
                SimpleNamespace(
                    file_include_pattern=file_include_pattern,
                    column=column,
                    code=code,
                ),
            )

    include_row_patterns = []
    if args.include_row:
        for item in args.include_row:
            column_name, _, pattern = item.partition(":")
            if not column_name:
                column_name = None  # Mark as None; to be replaced with the first column name later
            include_row_patterns.append(
                SimpleNamespace(column_name=column_name, pattern=pattern)
            )

    # ic(args.include_patterns, include_row_patterns)

    exclude_row_patterns = []
    if args.exclude_row:
        for item in args.exclude_row:
            column_name, _, pattern = item.partition(":")
            if not column_name:
                column_name = None  # Mark as None; to be replaced with the first column name later
            exclude_row_patterns.append(
                SimpleNamespace(column_name=column_name, pattern=pattern)
            )

    all_filtered_rows = []
    joined_headers = []
    for input_stream in args.input or [sys.stdin]:
        if input_stream is not sys.stdin:
            #: The =argparse.FileType= object will have a =name= attribute that contains the path string as it was passed to the script, which might be relative.

            # ic(input_stream.name)
            input_name = os.path.abspath(input_stream.name)
        else:
            input_name = "-"

        # ic(input_stream, input_name)
        headers, filtered_rows = filter_csv(
            input_stream=input_stream,
            input_name=input_name,
            include_patterns=args.include_patterns,
            exclude_patterns=args.exclude_patterns,
            include_row_patterns=include_row_patterns,
            exclude_row_patterns=exclude_row_patterns,
            exclude_row_fn=args.exclude_row_fn,
            sort_specs=None,  #: No sorting here
            preprocess_fns=preprocess_fns,
        )
        joined_headers.append(headers)
        all_filtered_rows.append(filtered_rows)

    writer = csv.writer(args.output)

    if args.join_column and args.join_fn:
        joined_rows = join_csvs(
            joined_headers, all_filtered_rows, args.join_column, args.join_fn
        )

        # Sort after joining if sort_specs are provided
        if sort_specs:
            joined_rows = sort_rows(joined_rows, joined_headers[0], sort_specs)

        writer.writerow(joined_headers[0])
        writer.writerows(joined_rows)
    else:
        # Sort before writing if sort_specs are provided
        if sort_specs:
            # ic(sort_specs)
            for rows in all_filtered_rows:
                # ic(rows[0])
                sort_rows(rows, joined_headers[0], sort_specs)
                # ic(rows[0])

        writer.writerow(joined_headers[0])
        for rows in all_filtered_rows:
            writer.writerows(rows)


if __name__ == "__main__":
    pynight.common_icecream.ic_output = sys.stderr
    ipdb_enable()
    main()
