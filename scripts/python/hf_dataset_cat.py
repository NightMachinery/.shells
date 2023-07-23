#!/usr/bin/env python3
#: Write a Python script that gets the path to a dir containing a Huggingface dataset. It should use `datasets.load_from_disk` to load this dataset, and print its rows from `--start, -s <start_index>` to `--end, -e <end_index>` in pretty-printed JSON.
##
import argparse
from pynight.common_json import JSONEncoderWithFallback
from datasets import load_from_disk

# Initialize the argument parser
parser = argparse.ArgumentParser(description='Print a range of rows from a Huggingface dataset')

# Add arguments
parser.add_argument('dir', type=str, help='Path to the directory containing the dataset')
# parser.add_argument('--dir', '-d', type=str, required=True, help='Path to the directory containing the dataset')

parser.add_argument(
    "--torch-shape-get",
    type=bool,
    action=argparse.BooleanOptionalAction,
    default=False,
    help="Use torch_shape_get",
)

parser.add_argument('--start', '-s', type=int, help='Start index', default=0)
parser.add_argument('--end', '-e', type=int, help='End index', default=1)

# Parse the arguments
args = parser.parse_args()

# Load the dataset from disk
dataset = load_from_disk(args.dir)

# Extract the range of data
data_range = dataset[args.start:args.end]

if args.torch_shape_get:
    from pynight.common_torch import torch_shape_get

    data_range = torch_shape_get(data_range)

# Pretty-print the data as JSON
encoder = JSONEncoderWithFallback(indent=2)
print(encoder.encode(data_range,))
