#!/usr/bin/env python3
##
import sys
import math
import argparse
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator
import io
import numpy as np

MAGIC_DISPLAY = "MAGIC_DISPLAY"


def read_numbers():
    """
    Reads lines from stdin and yields valid numbers.
    """
    for line in sys.stdin:
        line = line.strip()

        try:
            yield float(line)
        except ValueError:
            continue


def plot_histogram(
    numbers,
    *,
    bins="auto",
    output=MAGIC_DISPLAY,
    dpi=300,
):
    """
    Plots a histogram of the numbers.
    If output is specified, saves the plot to the file or stdout.
    Otherwise, displays the plot.
    """
    plt.hist(numbers, bins=bins)
    plt.title("Histogram of Input Numbers")
    plt.xlabel("Value")
    plt.ylabel("Frequency")
    plt.grid(True)
    ax = plt.gca()  # Get current axis

    ax.yaxis.set_major_locator(MaxNLocator(integer=True, nbins=50))
    ax.xaxis.set_major_locator(MaxNLocator(integer=True, nbins=50))
    #: Make yticks more fine-grained
    #: Adjust 'nbins' as needed for granularity

    if output == MAGIC_DISPLAY:
        plt.show()
    elif output == "-":
        buf = io.BytesIO()
        plt.savefig(buf, format="png", dpi=dpi)
        sys.stdout.buffer.write(buf.getvalue())
    else:
        plt.savefig(output, dpi=dpi)


def main():
    parser = argparse.ArgumentParser(description="Plot a histogram of input numbers.")

    parser.add_argument(
        "-b",
        "--bin-width",
        default="auto",
        help="Bin width for the histogram. 'auto' for automatic binning.",
    )

    parser.add_argument(
        "-d",
        "--dpi",
        default=300,
        type=int,
        help="DPI (dots per inch) for the output image.",
    )

    parser.add_argument(
        "output",
        nargs="?",
        default=MAGIC_DISPLAY,
        help="Output file for the histogram plot. If '-', output the png file to stdout. If not specified, plot will be displayed.",
    )

    args = parser.parse_args()

    numbers = list(read_numbers())

    if not numbers:
        print("No valid numbers provided.", file=sys.stderr)
        sys.exit(1)

    try:
        bin_width = float(args.bin_width)
        bins = np.arange(
            math.floor(min(numbers)),
            max(numbers) + bin_width,
            step=bin_width,
        )
        #: `bins` must be an integer, a string, or an array
        #: The [[https://matplotlib.org/stable/api/_as_gen/matplotlib.pyplot.hist.html][bins]] parameter can be a single number, in which case it is interpreted as the number of bins. So, it needs to be a strictly positive integer. If you want to set a specific bin size, you need to give a complete list of the desired bin edges.
        #: [[https://stackoverflow.com/questions/66607383/how-can-i-use-a-float-for-the-bin-size-in-a-histogram-plot][python - How can I use a float for the bin size in a histogram plot? - Stack Overflow]]
    except ValueError:
        bins = args.bin_width

    plot_histogram(
        numbers,
        bins=bins,
        output=args.output,
        dpi=args.dpi,
    )


if __name__ == "__main__":
    main()
