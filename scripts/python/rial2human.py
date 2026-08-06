#!/usr/bin/env python3

import argparse


def toman_to_human(toman):
    # Use a dictionary to define the magnitude of the toman
    magnitude = {
        1: "",
        1_000: "thousand",
        1_000_000: "million",
        1_000_000_000: "billion",
    }

    human_readable_toman = ""

    for key in sorted(magnitude.keys(), reverse=True):
        if toman >= key:
            count = int(toman // key)
            toman -= count * key

            if human_readable_toman != "":
                human_readable_toman += ", "

            human_readable_toman += f"{count} {magnitude[key]}".strip()

    return human_readable_toman + " toman"


def rial_to_toman(rial):
    return toman_to_human(rial / 10)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("amount", type=str, help="Amount to humanize (in rial).")
    parser.add_argument(
        "--from-toman",
        action="store_true",
        help="Interpret the amount as toman instead of rial.",
    )
    args = parser.parse_args()

    amount = float(args.amount.replace(",", ""))
    if args.from_toman:
        print(toman_to_human(amount))
    else:
        print(rial_to_toman(amount))
