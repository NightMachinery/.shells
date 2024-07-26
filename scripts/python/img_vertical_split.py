#!/usr/bin/env python3
import sys
import argparse
import os
from PIL import Image
from collections import Counter


def is_narrow_bar(
    image,
    y,
    width,
    bar_height=7,
    threshold=0.999,
):
    bar = [
        image.getpixel((x, y + i))
        for i in range(-bar_height, bar_height + 1)
        for x in range(width)
        if 0 <= (y + i) < image.height
    ]
    if not bar:  # If the bar list is empty, return False
        return False

    color_counts = Counter(bar)
    dominant_color, count = color_counts.most_common(1)[0]
    return count / len(bar) >= threshold


def split_image(
    input_path, max_height=400, min_height=200, output_prefix=None, **kwargs
):
    # Read the input image and get its dimensions
    image = Image.open(input_path)
    width, height = image.size
    image_format = image.format.lower() if image.format else "png"

    # Determine the output prefix based on the input file name if not provided
    if output_prefix is None:
        base_name = os.path.splitext(os.path.basename(input_path))[0]
        output_prefix = f"{base_name}_"

    # Split the image into chunks
    chunk_start = 0
    chunk_index = 0

    while chunk_start < height:
        chunk_end = min(chunk_start + max_height, height)
        found_split = False

        for y in range(chunk_end, chunk_start + min_height, -1):
            if is_narrow_bar(image, y, width, **kwargs):
                chunk_end = y
                found_split = True
                break

        if not found_split:
            chunk_end = min(chunk_start + max_height, height)

            if height - chunk_end < min_height:
                chunk_end = min(chunk_start + min_height, height)
        else:
            if height - chunk_end < min_height:
                chunk_end = height

        if chunk_end - chunk_start >= min_height:
            chunk = image.crop((0, chunk_start, width, chunk_end))

            dest = f"{output_prefix}{chunk_index}.{image_format}"
            chunk.save(dest, format=image.format)
            print(dest)

            chunk_index += 1
            chunk_start = chunk_end
        else:
            break

    #: Handle the remaining pixels after the last split line
    if height - chunk_start > 0:
        chunk = image.crop((0, chunk_start, width, height))
        chunk.save(f"{output_prefix}{chunk_index}.{image_format}", format=image.format)
        chunk_index += 1


def main():
    parser = argparse.ArgumentParser(description="Split an image into chunks.")
    parser.add_argument("input_path", type=str, help="Path to the input image.")
    parser.add_argument(
        "--max-height", type=int, default=700, help="Maximum height of each chunk."
    )
    parser.add_argument(
        "--min-height", type=int, default=200, help="Minimum height of each chunk."
    )
    parser.add_argument(
        "--output-prefix",
        type=str,
        default=None,
        help="Prefix for the output image files. If not specified, it defaults to the input file name.",
    )
    parser.add_argument(
        "--bar-height",
        type=int,
        default=7,
        help="Height of the bar to detect for splitting.",
    )
    parser.add_argument(
        "--threshold",
        type=float,
        default=0.999,
        help="Threshold for detecting the bar.",
    )

    args = parser.parse_args()
    split_image(
        args.input_path,
        args.max_height,
        args.min_height,
        args.output_prefix,
        bar_height=args.bar_height,
        threshold=args.threshold,
    )


if __name__ == "__main__":
    main()
