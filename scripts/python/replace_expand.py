#!/usr/bin/env python3

from __future__ import annotations

import argparse
import re
import sys
from collections.abc import Iterable, Sequence
from dataclasses import dataclass
from pathlib import Path
from typing import TextIO


PCRE_NEWLINE_PATTERN = r"\r\n|[\n\v\f\r\x85\u2028\u2029]"


@dataclass(frozen=True)
class Config:
    input_path: str
    input_separator: str
    output_separator: str
    magic_string: str
    replacement_paths: list[str]
    skip_whitespace_only: bool


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description=(
            "Expand input records by replacing MAGIC_STRING with each "
            "replacement record from the given files."
        )
    )

    parser.add_argument(
        "-i",
        "--input",
        default="-",
        help="input file to read, or '-' for stdin; default: %(default)s",
    )
    parser.add_argument(
        "--input-separator",
        default=r"\R",
        help=r"regular expression used to split input records; default: %(default)s",
    )
    parser.add_argument(
        "--output-separator",
        default=r"\n",
        help=r"separator written after each output record; default: %(default)s",
    )
    parser.add_argument(
        "--skip-whitespace-only",
        action=argparse.BooleanOptionalAction,
        default=True,
        help="skip whitespace-only replacement records; default: %(default)s",
    )
    parser.add_argument("magic_string")
    parser.add_argument("replacement_files", nargs="+")

    return parser


def parse_args(argv: Sequence[str] | None = None) -> Config:
    args = build_parser().parse_args(argv)

    return Config(
        input_path=args.input,
        input_separator=args.input_separator,
        output_separator=args.output_separator,
        magic_string=args.magic_string,
        replacement_paths=args.replacement_files,
        skip_whitespace_only=args.skip_whitespace_only,
    )


def decode_separator(separator: str) -> str:
    escape_values = {
        "0": "\0",
        "n": "\n",
        "r": "\r",
        "t": "\t",
        "f": "\f",
        "v": "\v",
        "\\": "\\",
    }

    result: list[str] = []
    index = 0

    while index < len(separator):
        char = separator[index]

        if char != "\\" or index + 1 >= len(separator):
            result.append(char)
            index += 1
            continue

        escaped = separator[index + 1]

        if escaped in escape_values:
            result.append(escape_values[escaped])
        else:
            result.append(f"\\{escaped}")

        index += 2

    return "".join(result)


def compile_input_separator(separator: str) -> re.Pattern[str]:
    pattern = decode_separator(separator)
    pattern = pattern.replace(r"\R", f"(?:{PCRE_NEWLINE_PATTERN})")
    return re.compile(pattern)


def open_input(path: str, *, stdin: TextIO) -> TextIO:
    if path == "-":
        return stdin

    return Path(path).open("r", encoding="utf-8")


def split_records(text: str, *, separator_pattern: re.Pattern[str]) -> list[str]:
    if text == "":
        return []

    records = separator_pattern.split(text)

    if records and records[-1] == "":
        records.pop()

    return records


def read_records(
    file: TextIO,
    *,
    separator_pattern: re.Pattern[str],
) -> list[str]:
    return split_records(file.read(), separator_pattern=separator_pattern)


def iter_replacement_records(
    paths: Iterable[str],
    *,
    separator_pattern: re.Pattern[str],
    skip_whitespace_only: bool,
) -> Iterable[str]:
    for path in paths:
        with Path(path).open("r", encoding="utf-8") as replacement_file:
            for record in read_records(
                replacement_file,
                separator_pattern=separator_pattern,
            ):
                if skip_whitespace_only and record.strip() == "":
                    continue

                yield record


def expand_records(
    input_records: Iterable[str],
    replacement_records: Iterable[str],
    *,
    magic_string: str,
) -> Iterable[str]:
    replacements = list(replacement_records)

    for record in input_records:
        for replacement in replacements:
            yield record.replace(magic_string, replacement)


def write_records(
    records: Iterable[str],
    *,
    output_separator: str,
    stdout: TextIO,
) -> None:
    for record in records:
        stdout.write(record)
        stdout.write(output_separator)


def run(
    config: Config,
    *,
    stdin: TextIO = sys.stdin,
    stdout: TextIO = sys.stdout,
) -> None:
    separator_pattern = compile_input_separator(config.input_separator)
    output_separator = decode_separator(config.output_separator)
    input_file = open_input(config.input_path, stdin=stdin)

    try:
        input_records = read_records(
            input_file,
            separator_pattern=separator_pattern,
        )
        replacement_records = iter_replacement_records(
            config.replacement_paths,
            separator_pattern=separator_pattern,
            skip_whitespace_only=config.skip_whitespace_only,
        )
        expanded_records = expand_records(
            input_records,
            replacement_records,
            magic_string=config.magic_string,
        )

        write_records(
            expanded_records,
            output_separator=output_separator,
            stdout=stdout,
        )
    finally:
        if input_file is not stdin:
            input_file.close()


def main(argv: Sequence[str] | None = None) -> int:
    config = parse_args(argv)
    run(config)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
