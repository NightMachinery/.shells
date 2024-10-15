#!/usr/bin/env python3
##
#: @o1, @o1-mini
##
import sys
import os
import argparse
import shutil
from ebooklib import epub
from iterfzf import iterfzf
import pandoc  # Importing the Pandoc Python API


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
        help="Output file or base name for chapters when using --all.",
    )
    parser.add_argument(
        "--all",
        action="store_true",
        help="Export all chapters to separate files.",
    )
    return parser.parse_args()


def check_dependencies():
    """Ensure required external tools are installed."""
    missing_tools = []

    try:
        import pandoc

    except ImportError:
        print(
            "Error: pandoc Python package is not installed. Please install it using pip:\n\tpip install --upgrade pandoc"
        )
        sys.exit(1)

    for tool in [
        "fzf",
        "pandoc",
    ]:
        if not shutil.which(tool):
            missing_tools.append(tool)

    if missing_tools:
        print(
            f"Error: The following required tools are missing: {', '.join(missing_tools)}."
        )
        print("Please install them and ensure they're in your PATH.")
        sys.exit(1)

    # Check iterfzf
    try:
        import iterfzf

    except ImportError:
        print("Error: iterfzf is not installed. Please install it using pip:")
        print("\tpip install -U iterfzf")
        sys.exit(1)


def convert_html_to_markdown(html_content, chapter_title):
    """Convert HTML content to Markdown using the Pandoc Python API."""
    try:
        html_text = html_content.decode("utf-8", errors="ignore")
        doc = pandoc.read(html_text, format="html")
        md_content = pandoc.write(doc, format="markdown")
        return md_content

    except Exception as e:
        print(f"Pandoc error for chapter '{chapter_title}': {e}")
        return None


def get_markdown_content(book, title, href):
    """Retrieve and convert a chapter's HTML content to Markdown."""
    href_no_fragment = href.split("#")[0]
    item = book.get_item_with_href(href_no_fragment)
    if item is None:
        print(f"Error: href '{href}' not found in EPUB items.")
        return None
    html_content = item.get_content()
    md_content = convert_html_to_markdown(html_content, title)
    if md_content is None:
        return None
    return f"# {title}\n\n{md_content}"


def export_selected_chapters(book, selected, output_file):
    """Export selected chapters to a single Markdown file."""
    md_contents = []
    for title, href in selected:
        md_content = get_markdown_content(book, title, href)
        if md_content:
            md_contents.append(md_content)
    final_md = "\n\n".join(md_contents)

    if output_file:
        try:
            with open(output_file, "w", encoding="utf-8") as f:
                f.write(final_md)
            print(f"Exported selected chapters to '{output_file}'.")
        except IOError as e:
            print(f"Error writing to file '{output_file}': {e}")
            sys.exit(1)
    else:
        print(final_md)


def export_all_chapters(book, flat_toc, output_base):
    """Export all chapters to separate Markdown files."""
    dest_basename, dest_ext = os.path.splitext(output_base)

    # Handle cases where output_base has no extension but contains dots
    if not dest_ext and "." in os.path.basename(output_base):
        dest_basename = os.path.splitext(os.path.basename(output_base))[0]

    for i, (title, href) in enumerate(flat_toc, start=1):
        md_content = get_markdown_content(book, title, href)
        if md_content:
            # Construct output filename
            chapter_filename = (
                f"{dest_basename}_c{i}{dest_ext}"
                if dest_ext
                else f"{dest_basename}_c{i}"
            )
            try:
                with open(chapter_filename, "w", encoding="utf-8") as f:
                    f.write(md_content)
                print(f"Exported chapter '{title}' to '{chapter_filename}'.")
            except IOError as e:
                print(f"Error writing to file '{chapter_filename}': {e}")


def interactive_select_chapters(flat_toc):
    """Use iterfzf to allow the user to select chapters interactively."""
    lines = [f"{title}\t{href}" for title, href in flat_toc]

    # Prepare display format for iterfzf
    display_lines = [f"{i+1}. {title}" for i, (title, _) in enumerate(flat_toc)]

    # Invoke iterfzf for interactive selection
    try:
        selected_display = iterfzf(
            display_lines,
            multi=True,
            prompt="Select chapters (Use TAB to select, ENTER to confirm): ",
            # Removed 'default' parameter
        )
    except Exception as e:
        print(f"Error during chapter selection: {e}")
        sys.exit(1)

    if not selected_display:
        print("No chapters selected.")
        sys.exit(1)

    selected = []
    for display in selected_display:
        # Extract the chapter number from the display line
        try:
            number_str = display.split(".")[0]
            index = int(number_str) - 1
            if 0 <= index < len(flat_toc):
                selected.append(flat_toc[index])
            else:
                print(f"Invalid chapter number selected: {number_str}")
        except (ValueError, IndexError):
            print(f"Invalid selection format: {display}")
            sys.exit(1)
    return selected


def main():
    args = parse_arguments()
    epub_file = args.epub_file
    output = args.output
    if not output:
        #: Change the extension of epub_file to `.txt`:
        output = os.path.splitext(epub_file)[0] + '.txt'

    export_all = args.all

    # Check dependencies
    check_dependencies()

    if not os.path.exists(epub_file):
        print(f"Error: '{epub_file}' does not exist.")
        sys.exit(1)

    try:
        book = epub.read_epub(epub_file)
    except Exception as e:
        print(f"Error reading EPUB file '{epub_file}': {e}")
        sys.exit(1)

    toc = book.toc
    flat_toc = flatten_toc(toc)

    if not flat_toc:
        print("Error: No chapters found in the EPUB's table of contents.")
        sys.exit(1)

    if export_all:
        if not output:
            print("Error: Output base name is required when using --all.")
            print("Usage: epub_chapter_select.py epub_file [output] --all")
            sys.exit(1)
        export_all_chapters(book, flat_toc, output)
    else:
        selected = interactive_select_chapters(flat_toc)
        if not selected:
            print("No valid chapters selected.")
            sys.exit(1)
        export_selected_chapters(book, selected, output)


if __name__ == "__main__":
    main()
