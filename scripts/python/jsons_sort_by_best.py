#!/usr/bin/env python3
##
import sys
import json
import argparse
import re
from types import SimpleNamespace
from operator import attrgetter
from pynight.common_icecream import ic


def process_json_file(
    file_path,
    top_n,
    key,
    include_patterns,
    exclude_patterns,
    ignore_case,
    ascending,
):
    try:
        with open(file_path, "r") as f:
            data = json.load(f)

        # Filter items based on include and exclude patterns
        filtered_items = {}

        # ic(include_patterns, exclude_patterns)

        for method, scores in data.items():
            if all(
                re.search(pattern, method, re.IGNORECASE if ignore_case else 0)
                for pattern in include_patterns
            ) and not any(
                re.search(pattern, method, re.IGNORECASE if ignore_case else 0)
                for pattern in exclude_patterns
            ):
                filtered_items[method] = scores

        # Sort the filtered items based on the specified key
        sorted_items = sorted(
            filtered_items.items(),
            key=lambda x: x[1][key],
            reverse=not ascending,
        )

        # Get the top N items
        top_items = sorted_items[:top_n]

        # Create a SimpleNamespace object with the results
        result = SimpleNamespace(
            file_path=file_path,
            top_items=top_items,
            max_score=top_items[0][1][key] if top_items else None,
        )
        return result
    except Exception as e:
        print(f"Error processing {file_path}: {str(e)}", file=sys.stderr)
        return None


def print_result(result, key, number=None):
    if number is not None:
        print(f"{number}. {result.file_path}")
    else:
        print(result.file_path)
    for method, scores in result.top_items:
        print(f"\t{method}.{key}: {scores[key]}")
    print()  # Add a blank line between files


def main():
    parser = argparse.ArgumentParser(
        description="Process JSON files and find top scorers."
    )
    parser.add_argument(
        "--top",
        type=int,
        # default=1,
        default=5,
        help="Number of top items to display per file",
    )
    parser.add_argument("--key", type=str, default="BAUPRC", help="Key to sort by")
    parser.add_argument(
        "--ascending", action="store_true", help="Sort files in ascending order"
    )
    parser.add_argument(
        "-i",
        "--include",
        action="append",
        default=[],
        help="Include keys matching regex (can be repeated)",
    )
    parser.add_argument(
        "-x",
        "--exclude",
        action="append",
        default=[],
        help="Exclude keys matching regex (can be repeated)",
    )
    parser.add_argument(
        "--ignore-case", action="store_true", help="Ignore case in regex matching"
    )
    parser.add_argument(
        "--case-sensitive",
        action="store_true",
        help="Use case-sensitive regex matching",
    )
    parser.add_argument(
        "-n",
        "--numbered",
        type=int,
        nargs="?",
        default=1,
        help="Number the output, starting from the given number",
    )
    parser.add_argument(
        "--no-numbered", action="store_true", help="Disable numbering the output"
    )
    args = parser.parse_args()

    # Determine case sensitivity
    ignore_case = args.ignore_case and not args.case_sensitive

    # Read file paths from stdin
    file_paths = [line.strip() for line in sys.stdin]

    # Process all files and collect results
    results = [
        process_json_file(
            file_path,
            args.top,
            args.key,
            args.include,
            args.exclude,
            ignore_case,
            ascending=args.ascending,
        )
        for file_path in file_paths
    ]

    # Filter out any failed processing attempts
    results = [r for r in results if r is not None]

    # Sort results based on the max score
    results.sort(key=attrgetter("max_score"), reverse=not args.ascending)

    # Determine numbering
    start_number = None
    if not args.no_numbered and args.numbered is not None:
        start_number = args.numbered

    # Print sorted results
    for i, result in enumerate(results, start=start_number or 0):
        number = i if start_number is not None else None
        print_result(result, args.key, number)


if __name__ == "__main__":
    main()
