#!/usr/bin/env python3
##
import sys
import os
import argparse
import shutil
import re
from ebooklib import epub
from pynight.common_tui import ask

##
import warnings

# Suppress specific warnings
warnings.filterwarnings(
    "ignore",
    message="In the future version we will turn default option ignore_ncx to True.",
)
warnings.filterwarnings(
    "ignore",
    category=FutureWarning,
    message="This search incorrectly ignores the root element",
)
##


def flatten_toc(toc):
    """Flatten the table of contents into a list of (title, href) tuples."""
    flat_toc = []
    for item in toc:
        if isinstance(item, epub.Link):
            flat_toc.append((item.title, item.href))
        elif isinstance(item, tuple):
            navpoint, subpoints = item
            if isinstance(navpoint, epub.Link):
                flat_toc.append((navpoint.title, navpoint.href))
            flat_toc.extend(flatten_toc(subpoints))
    return flat_toc


def parse_arguments():
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(
        description="Select and export EPUB chapters to Markdown."
    )
    parser.add_argument("epub_file", help="Path to the EPUB file.")
    parser.add_argument(
        "output",
        nargs="?",
        help="Output file or base name for chapters when using --all or --export-each.",
    )
    parser.add_argument(
        "--all",
        action=argparse.BooleanOptionalAction,
        default=False,
        help="Export all chapters to separate files.",
    )
    parser.add_argument(
        "--append-name",
        action=argparse.BooleanOptionalAction,
        default=True,
        help="Append chapter name to the output filename.",
    )
    parser.add_argument(
        "--export-each",
        action=argparse.BooleanOptionalAction,
        default=True,
        help="Export each selected chapter to a separate file instead of combining them.",
    )
    return parser.parse_args()


def print_error(message):
    """Print an error message to stderr."""
    print(message, file=sys.stderr)


def check_dependencies():
    """Ensure required external tools are installed."""
    missing_tools = []

    # Check for iterfzf
    try:
        import iterfzf
    except ImportError:
        print_error(
            "Error: iterfzf is not installed. Please install it using pip:\n\tpip install -U iterfzf"
        )
        sys.exit(1)

    # Check for pypandoc
    try:
        import pypandoc
    except ImportError:
        print_error(
            "Error: pypandoc is not installed. Please install it using pip:\n\tpip install pypandoc"
        )
        sys.exit(1)

    # Check for external tools
    for tool in ["fzf", "pandoc"]:
        if not shutil.which(tool):
            missing_tools.append(tool)

    if missing_tools:
        print_error(
            f"Error: The following required tools are missing: {', '.join(missing_tools)}."
        )
        print_error("Please install them and ensure they're in your PATH.")
        sys.exit(1)


def convert_html_to_markdown(html_content, chapter_title):
    """Convert HTML content to Markdown using pypandoc."""
    try:
        import pypandoc  # Import inside the function after dependency check

        html_text = html_content.decode("utf-8", errors="ignore")
        md_content = pypandoc.convert_text(html_text, "markdown", format="html")
        return md_content
    except Exception as e:
        print_error(f"Pandoc error for chapter '{chapter_title}': {e}")
        return None


def get_markdown_content(book, title, href):
    """Retrieve and convert a chapter's HTML content to Markdown."""
    href_no_fragment = href.split("#")[0]
    item = book.get_item_with_href(href_no_fragment)
    if item is None:
        print_error(f"Error: href '{href}' not found in EPUB items.")
        return None
    html_content = item.get_content()
    md_content = convert_html_to_markdown(html_content, title)
    if md_content is None:
        return None
    return f"# {title}\n\n{md_content}"


def write_markdown_to_file(md_content, filename):
    """Write Markdown content to a specified file."""
    #: Ask the user to overwrite if exists:
    if os.path.exists(filename):
        overwrite = ask(
            f"File '{filename}' already exists. Overwrite?",
            default=False,
        )
        if not overwrite:
            print_error("Aborted.")
            return

    try:
        with open(filename, "w", encoding="utf-8") as f:
            f.write(md_content)
        print(f"Exported to '{filename}'.")
    except IOError as e:
        print_error(f"Error writing to file '{filename}': {e}")
        sys.exit(1)


def construct_chapter_filename(dest_basename, dest_ext, index, title, append_name):
    """Construct the filename for a chapter based on the provided parameters."""
    # Sanitize the chapter title for filenames
    sanitized_title = "".join(
        c for c in title if c.isalnum() or c in (" ", "_", "-")
    ).rstrip()

    # Prepare components of the filename
    index_part = f"_c{index}"
    extension_part = dest_ext or ""

    # Compute base length without the title
    base_length = len(dest_basename) + len(index_part) + len(extension_part)
    # Add 1 if append_name is True to account for the extra '_'
    if append_name:
        base_length += 1  # For the underscore before the title

    # Maximum allowed length for the title part
    max_name_length = 256
    max_title_length = max_name_length - base_length

    # Truncate the title if necessary
    if append_name:
        if len(sanitized_title) > max_title_length:
            sanitized_title = sanitized_title[:max_title_length]
        chapter_filename = f"{dest_basename}{index_part}_{sanitized_title}"
    else:
        chapter_filename = f"{dest_basename}{index_part}"

    # Add extension if provided
    chapter_filename += extension_part

    return chapter_filename


def export_selected_chapters(book, selected, output_file):
    """Export selected chapters to a single Markdown file."""
    md_contents = []
    for index, title, href in selected:
        md_content = get_markdown_content(book, title, href)
        if md_content:
            md_contents.append(md_content)
    final_md = "\n\n".join(md_contents)

    if output_file:
        write_markdown_to_file(final_md, output_file)
    else:
        print(final_md)


def export_selected_chapters_individually(book, selected, output_base, append_name):
    """Export each selected chapter to separate Markdown files."""
    dest_basename, dest_ext = os.path.splitext(output_base)

    # Handle cases where output_base has no extension but contains dots
    if not dest_ext and "." in os.path.basename(output_base):
        dest_basename = os.path.splitext(os.path.basename(output_base))[0]

    for index, title, href in selected:
        md_content = get_markdown_content(book, title, href)
        if md_content:
            chapter_filename = construct_chapter_filename(
                dest_basename, dest_ext, index, title, append_name
            )
            write_markdown_to_file(md_content, chapter_filename)


def export_all_chapters(book, flat_toc_with_index, output_base, append_name):
    """Export all chapters to separate Markdown files by reusing the individual export function."""
    return export_selected_chapters_individually(
        book, flat_toc_with_index, output_base, append_name
    )


def interactive_select_chapters(flat_toc_with_index):
    """Use iterfzf to allow the user to select chapters interactively."""
    try:
        from iterfzf import iterfzf  # Import inside the function after dependency check
    except ImportError:
        print_error(
            "Error: iterfzf is not installed. Please install it using pip:\n\tpip install -U iterfzf"
        )
        sys.exit(1)

    display_lines = [f"{index}. {title}" for index, title, _ in flat_toc_with_index]

    # Invoke iterfzf for interactive selection
    try:
        selected_display = iterfzf(
            display_lines,
            multi=True,
            prompt="Select chapters (Use TAB to select, ENTER to confirm): ",
        )
    except Exception as e:
        print_error(f"Error during chapter selection: {e}")
        sys.exit(1)

    if not selected_display:
        print_error("No chapters selected.")
        sys.exit(1)

    selected = []
    for display in selected_display:
        # Extract the chapter number from the display line
        try:
            number_str = display.split(".")[0]
            index = int(number_str)
            if 1 <= index <= len(flat_toc_with_index):
                selected.append(flat_toc_with_index[index - 1])
            else:
                print_error(f"Invalid chapter number selected: {number_str}")
        except (ValueError, IndexError):
            print_error(f"Invalid selection format: {display}")
            sys.exit(1)
    return selected


def main():
    args = parse_arguments()
    epub_file = args.epub_file
    output = args.output
    if not output:
        #: Change the extension of epub_file to `.txt`:
        output = os.path.splitext(epub_file)[0] + ".txt"

    export_all = args.all
    export_each = args.export_each
    append_name = args.append_name

    # Check dependencies before importing optional modules
    check_dependencies()

    if not os.path.exists(epub_file):
        print_error(f"Error: '{epub_file}' does not exist.")
        sys.exit(1)

    try:
        book = epub.read_epub(epub_file)
    except Exception as e:
        print_error(f"Error reading EPUB file '{epub_file}': {e}")
        sys.exit(1)

    toc = book.toc
    flat_toc = flatten_toc(toc)
    # Enumerate flat_toc to store original indices
    flat_toc_with_index = [
        (index, title, href) for index, (title, href) in enumerate(flat_toc, start=1)
    ]

    if not flat_toc_with_index:
        print_error("Error: No chapters found in the EPUB's table of contents.")
        sys.exit(1)

    if export_all:
        if not output:
            print_error(
                "Error: Output base name is required when using --all.\n"
                "Usage: epub_chapter_select.py epub_file [output] --all"
            )
            sys.exit(1)
        export_all_chapters(book, flat_toc_with_index, output, append_name)
    else:
        selected = interactive_select_chapters(flat_toc_with_index)
        if not selected:
            print_error("No valid chapters selected.")
            sys.exit(1)
        if export_each:
            if not output:
                print_error(
                    "Error: Output base name is required when using --export-each.\n"
                    "Usage: epub_chapter_select.py epub_file [output] --export-each"
                )
                sys.exit(1)
            export_selected_chapters_individually(book, selected, output, append_name)
        else:
            export_selected_chapters(book, selected, output)


if __name__ == "__main__":
    main()
