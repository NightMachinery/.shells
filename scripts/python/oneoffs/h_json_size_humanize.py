#!/usr/bin/env python3

import json
import argparse
import sys
import humanize


class SizeJSONEncoder(json.JSONEncoder):
    def encode(self, obj):
        if isinstance(obj, dict) and args.size_field in obj and obj[args.size_field]:
            obj["size_human"] = humanize.naturalsize(obj[args.size_field])
        return super().encode(obj)


def stream_json_data(data, output):
    encoder = SizeJSONEncoder()
    output.write("[\n")

    first_row = True
    for item in data:
        try:
            json_output = encoder.encode(item)
        except Exception as e:
            print(f"Error encoding: {item}", file=sys.stderr)
            print(str(e), file=sys.stderr)
            continue
        if not first_row:
            output.write(",\n")
        output.write(json_output)
        first_row = False

    output.write("]")


parser = argparse.ArgumentParser(
    description="Process JSON file and add human-readable size field"
)
parser.add_argument("file", help="JSON file to process")
parser.add_argument(
    "--size-field", default="size", help="Name of the size field (default: size)"
)
parser.add_argument(
    "-o",
    "--output",
    help="Output file path or - for stdout",
    default="-",
)
args = parser.parse_args()

with open(args.file) as file:
    data = json.load(file)

if args.output == "-":
    output = sys.stdout
else:
    output = open(args.output, "w")

stream_json_data(data, output)

output.close()
