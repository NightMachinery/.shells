#!/usr/bin/env python3
# From https://github.com/ChenjieXu/pyzxing/blob/master/scanner.py
#
# worse than jsQR and zbar
#
# deps:
#   pip install pyzxing
##
import argparse
from pyzxing import BarCodeReader

parser = argparse.ArgumentParser(
    description="A Python Wrapper of ZXing Barcode Scanner")
parser.add_argument('-f', '--file', default=None)
args = parser.parse_args()


def main(args):
    reader = BarCodeReader()

    results = reader.decode(args.file)
    if results:
        if isinstance(results[0], dict):
            results_string = [result.get('parsed') for result in results]
        else:
            results_string = [
                x.get('parsed') for result in results for x in result
            ]

        for result in results_string:
            print(result)


if __name__ == '__main__':
    main(args)
