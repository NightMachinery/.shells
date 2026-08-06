#!/usr/bin/env python3

import argparse
import re
from pynight.common_regex import float_pattern


def convert_string_to_number(string, rial):
    multipliers = {"k": 1000, "m": 1000000, "b": 1000000000}

    total = 0
    # Use regular expression to find all matching components
    pattern = re.compile(rf"(?P<number>{float_pattern})(?P<multiplier>[kmb]?)", re.IGNORECASE)
    matches = pattern.finditer(string)

    # Iterate over the list of numbers
    for match in matches:
        number = float(match.group("number"))
        multiplier = match.group("multiplier").lower()

        total += number * multipliers.get(multiplier, 1)

    if rial:
        total *= 10

    if int(total) == total:
        total = int(total)

    return total


# Create parser object
parser = argparse.ArgumentParser()

# Define command-line arguments
parser.add_argument(
    "number_string", type=str, help="Human number string to convert (always in toman)."
)
parser.add_argument(
    "--rial", action="store_true", help="Output the amount in rial instead of toman."
)

# Parse command-line arguments
args = parser.parse_args()

# Use the command-line arguments
print(convert_string_to_number(args.number_string, args.rial))
