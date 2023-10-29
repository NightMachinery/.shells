#!/usr/bin/env python3
##
# `pip install pyjson5`
##
import sys
import json
import pyjson5 as json5


def convert_json5_to_json(input_data):
    try:
        # Parse the JSON5 input
        json_data = json5.loads(input_data)

        # Pretty-print the JSON data
        pretty_json = json.dumps(json_data, indent=4)

        return pretty_json
    except json5.Json5DecoderException as e:
        print(f"Error: Invalid JSON5 input - {e}")
        return None
    except Exception as e:
        print(f"Error: An unexpected error occurred - {e}")
        return None


if __name__ == "__main__":
    # Read JSON5 input from stdin
    input_data = sys.stdin.read()

    # Convert JSON5 to pretty-printed JSON
    output_json = convert_json5_to_json(input_data)

    if output_json is not None:
        # Print the pretty-printed JSON to stdout
        print(output_json)
