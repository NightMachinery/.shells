#!/usr/bin/env python3
import sys
import argparse
import os
from pathlib import Path
import subprocess
from bs4 import BeautifulSoup
import mimetypes


def determine_mime_type(favicon_path):
    """Determine the MIME type of the favicon file."""
    mime_type, _ = mimetypes.guess_type(favicon_path)
    if mime_type is None:
        # Default to x-icon if can't determine
        favicon_path_lower = favicon_path.lower()
        if favicon_path_lower.endswith(".ico"):
            return "image/x-icon"
        elif favicon_path_lower.endswith(".svg"):
            return "image/svg+xml"
        else:
            return "image/x-icon"
    return mime_type


def create_png_fallback(favicon_path, *, output_dir):
    """Create PNG version of the favicon using ImageMagick if it doesn't exist.

    Args:
        favicon_path (str): Path to the original favicon file.
        output_dir (str): Directory where the output PNG should reside.

    Returns:
        str or None: Relative path to the PNG fallback or None if creation failed.
    """
    input_path = Path(favicon_path)
    output_path = input_path.with_suffix(".png")

    # Check if PNG version already exists
    if output_path.exists():
        print(
            f"Warning: PNG fallback '{output_path}' already exists. Using existing file.",
            file=sys.stderr,
        )
    else:
        try:
            subprocess.run(
                [
                    "magick",
                    str(input_path),
                    "-background",
                    "none",
                    "-resize",
                    "32x32",
                    str(output_path),
                ],
                check=True,
                capture_output=True,
                text=True,
            )
        except subprocess.CalledProcessError as e:
            print(f"Error creating PNG fallback: {e.stderr}", file=sys.stderr)
            return None
        except FileNotFoundError:
            print(
                "Error: ImageMagick ('magick' command) not found. Please install ImageMagick.",
                file=sys.stderr,
            )
            return None

    # Compute relative path
    png_relative_path = os.path.relpath(str(output_path), start=output_dir)
    if os.path.dirname(png_relative_path) != "":
        print(
            f"Warning: PNG fallback '{output_path}' is not in the same directory as the output file.",
            file=sys.stderr,
        )

    return png_relative_path


def favicon_url_get(path, *, url_prefix):
    """Generate URL for favicon based on path and optional URL prefix.

    Args:
        path (str): Relative or absolute path to the favicon.
        url_prefix (str or None): URL prefix to prepend to the favicon path.

    Returns:
        str: The complete URL to the favicon.
    """
    return path if url_prefix is None else f"{url_prefix.rstrip('/')}/{path}"


def create_favicon_links(
    favicon_path, *, url_prefix, favicon_relative_path, fallback_icon, output_dir
):
    """Create the favicon link tags.

    Args:
        favicon_path (str): Path to the primary favicon file.
        url_prefix (str or None): URL prefix for the favicon.
        favicon_relative_path (str): Relative path to the favicon from the output directory.
        fallback_icon (str): Fallback icon specification ('MAGIC_AUTO' or a URL/path).
        output_dir (str): Directory where the output HTML resides.

    Returns:
        list of dict: List of attributes for each favicon link tag.
    """
    mime_type = determine_mime_type(favicon_path)

    # Create the primary favicon link
    links = [
        {
            "rel": "icon",
            "type": mime_type,
            "href": favicon_url_get(favicon_relative_path, url_prefix=url_prefix),
        }
    ]

    # Handle fallback icon
    if fallback_icon == "MAGIC_AUTO":
        if mime_type == "image/svg+xml":
            png_relative_path = create_png_fallback(favicon_path, output_dir=output_dir)
            if png_relative_path:
                links.append(
                    {
                        "rel": "alternate icon",
                        "type": "image/png",
                        "href": favicon_url_get(
                            png_relative_path, url_prefix=url_prefix
                        ),
                    }
                )
    elif fallback_icon:
        # Check if fallback_icon is a URL or a file path
        if os.path.isfile(fallback_icon):
            fallback_mime = determine_mime_type(fallback_icon)
            fallback_relative_path = os.path.relpath(fallback_icon, start=output_dir)
            if os.path.dirname(fallback_relative_path) != "":
                print(
                    f"Warning: Fallback icon '{fallback_icon}' is not in the same directory as the output file.",
                    file=sys.stderr,
                )
            href = favicon_url_get(fallback_relative_path, url_prefix=url_prefix)
        else:
            # Assume it's a URL
            fallback_mime = None  # MIME type might not be needed
            href = fallback_icon

        link_attrs = {"rel": "alternate icon", "href": href}
        if fallback_mime:
            link_attrs["type"] = fallback_mime
        links.append(link_attrs)

    return links


def process_html(
    html_content,
    favicon_path,
    *,
    url_prefix,
    favicon_relative_path,
    fallback_icon,
    output_dir,
):
    """Process the HTML content to add or update favicon links.

    Args:
        html_content (str): The original HTML content.
        favicon_path (str): Path to the primary favicon file.
        url_prefix (str or None): URL prefix for the favicon.
        favicon_relative_path (str): Relative path to the favicon from the output directory.
        fallback_icon (str): Fallback icon specification.
        output_dir (str): Directory where the output HTML resides.

    Returns:
        str: The modified HTML content with updated favicon links.
    """
    soup = BeautifulSoup(html_content, "html5lib")

    # Find head tag or create if doesn't exist
    head = soup.head
    if not head:
        head = soup.new_tag("head")
        if soup.html:
            soup.html.insert(0, head)
        else:
            html = soup.new_tag("html")
            html.append(head)
            soup.insert(0, html)

    # Remove existing favicon links
    for link in head.find_all("link", rel=["icon", "shortcut icon", "alternate icon"]):
        link.decompose()

    # Create and add new favicon links
    favicon_links = create_favicon_links(
        favicon_path,
        url_prefix=url_prefix,
        favicon_relative_path=favicon_relative_path,
        fallback_icon=fallback_icon,
        output_dir=output_dir,
    )

    # Find the last meta tag in head to insert after
    meta_tags = head.find_all("meta", recursive=False)
    if meta_tags:
        last_meta_tag = meta_tags[-1]
        insert_position = last_meta_tag
    else:
        insert_position = None

    # Add the new favicon links
    for link_attrs in favicon_links:
        link_tag = soup.new_tag("link")
        for attr, value in link_attrs.items():
            link_tag[attr] = value

        if insert_position:
            insert_position.insert_after("\n    ")  # Add proper indentation
            insert_position.insert_after(link_tag)
            insert_position = (
                link_tag  # Update insert_position to insert after the new link
            )
        else:
            head.insert(0, "\n    ")  # Add proper indentation
            head.insert(0, link_tag)
            insert_position = link_tag  # Update insert_position

    return str(soup)


def main():
    parser = argparse.ArgumentParser(
        description="Add or update favicon links in an HTML file.",
        epilog=(
            "Note:\n"
            "  - The favicon path is inserted in the output HTML file relative to the output file's directory.\n"
            "  - If --fallback-icon is set to MAGIC_AUTO and the primary favicon is an SVG, a PNG fallback will be generated automatically.\n"
            "  - Ensure ImageMagick is installed if using MAGIC_AUTO for PNG fallback creation."
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "favicon",
        help=(
            "Path to the primary favicon file. This path is relative to the output file's directory "
            "if a URL prefix is not specified."
        ),
    )
    parser.add_argument(
        "--url-prefix",
        help=(
            "URL prefix for the favicon. If specified, the favicon path will be appended to this prefix. "
            "Otherwise, the favicon path is treated as relative to the output file's directory."
        ),
        default=None,
    )
    parser.add_argument(
        "--fallback-icon",
        help=(
            "Specification for a fallback icon. Can be a web address (URL) to use as a fallback "
            'or "MAGIC_AUTO" to automatically generate a PNG fallback if the primary favicon is an SVG. '
            'Default is "MAGIC_AUTO".'
        ),
        default="MAGIC_AUTO",
    )
    parser.add_argument(
        "--input",
        default="-",
        help=('Input HTML file. Use "-" to read from standard input (default: stdin).'),
    )
    parser.add_argument(
        "--output",
        default="-",
        help=(
            'Output HTML file with updated favicon links. Use "-" to write to standard output (default: stdout). '
            "If writing to a file, the favicon paths will be relative to the output file's directory."
        ),
    )

    args = parser.parse_args()

    # Determine the output directory
    if args.output == "-":
        if args.input == "-":
            output_dir = os.getcwd()
        else:
            output_dir = os.path.abspath(os.path.dirname(args.input))
    else:
        output_dir = os.path.abspath(os.path.dirname(args.output))

    favicon_relative_path = os.path.relpath(args.favicon, start=output_dir)
    if os.path.dirname(favicon_relative_path) != "":
        print(
            f"Warning: Favicon '{args.favicon}' is not in the same directory as the output file.",
            file=sys.stderr,
        )

    # Read input
    try:
        if args.input == "-":
            html_content = sys.stdin.read()
        else:
            with open(args.input, "r", encoding="utf-8") as f:
                html_content = f.read()
    except Exception as e:
        print(f"Error reading input file: {e}", file=sys.stderr)
        sys.exit(1)

    # Process HTML
    result = process_html(
        html_content,
        favicon_path=args.favicon,
        url_prefix=args.url_prefix,
        favicon_relative_path=favicon_relative_path,
        fallback_icon=args.fallback_icon,
        output_dir=output_dir,
    )

    # Write output
    try:
        if args.output == "-":
            sys.stdout.write(result)
        else:
            # If input and output files are the same, write to a temporary file first
            if args.input == args.output and args.output != "-":
                temp_output_path = args.output + ".tmp"
                with open(temp_output_path, "w", encoding="utf-8") as f:
                    f.write(result)
                os.replace(temp_output_path, args.output)
            else:
                with open(args.output, "w", encoding="utf-8") as f:
                    f.write(result)
    except Exception as e:
        print(f"Error writing output file: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
