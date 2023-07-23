#!/usr/bin/env python3

import argparse
import re

def convert_string_to_number(string, rial):
    multipliers = {'k': 1000, 'm': 1000000, 'b': 1000000000}

    total = 0
    # Use regular expression to find all matching components
    pattern = re.compile(r'(?P<number>\d+)(?P<multiplier>[kmb]?)', re.IGNORECASE)
    matches = pattern.finditer(string)

    # Iterate over the list of numbers
    for match in matches:
        number = int(match.group('number'))
        multiplier = match.group('multiplier').lower()

        if multiplier in multipliers:
            result = number * multipliers[multiplier]
            if rial:
                total += result * 10
            else:
                total += result
        else:
            total += number
    return total


# Create parser object
parser = argparse.ArgumentParser()

# Define command-line arguments
parser.add_argument('number_string', type=str, help="Number strings to convert.")
parser.add_argument('--rial', action='store_true', help="Use if you want to convert toman to rial.")

# Parse command-line arguments
args = parser.parse_args()

# Use the command-line arguments
print(convert_string_to_number(args.number_string, args.rial))
