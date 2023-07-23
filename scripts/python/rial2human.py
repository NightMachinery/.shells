#!/usr/bin/env python3

import sys


def rial_to_toman(rial):
    # Convert rial to toman
    toman = rial / 10

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
            count = int(toman / key)
            toman -= count * key

            if human_readable_toman != "":
                human_readable_toman += ", "

            human_readable_toman += f"{count} {magnitude[key]}".strip()

    return human_readable_toman + " toman"


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: <rial_amount>")
        sys.exit(1)

    rial = int(sys.argv[1].replace(",", ""))
    print(rial_to_toman(rial))
