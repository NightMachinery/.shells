#!/usr/bin/env python3
#: Write a Python script that gets the path to a '.pt' file. It should use `torch.load` to load this data, and print its batch items from `--start, -s <start_index>` to `--end, -e <end_index>` in pretty-printed JSON.
##
import argparse
import torch
import json
from pynight.common_dict import BatchedDict


def load_and_print_data(file_path, start_index, end_index):
    data = torch.load(file_path)

    if isinstance(data, dict):
        data = BatchedDict(data)

    if end_index is not None:
        selected_data = data[start_index:end_index]
    else:
        selected_data = data[start_index:]

    if True:
        print(selected_data)
    else:
        # Convert to JSON and pretty-print
        print(json.dumps(selected_data, indent=4))


def main():
    parser = argparse.ArgumentParser(
        description="Load and print batch items from a .pt file"
    )
    parser.add_argument("file_path", type=str, help="Path to the .pt file")
    parser.add_argument(
        "--start", "-s", type=int, required=False, default=0, help="Start index"
    )
    parser.add_argument("--end", "-e", required=False, default=None, help="End index")

    args = parser.parse_args()
    end = args.end
    if end is not None:
        end = int(end)

    load_and_print_data(args.file_path, args.start, end)


if __name__ == "__main__":
    main()
