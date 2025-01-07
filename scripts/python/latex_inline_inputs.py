#!/usr/bin/env python3
#: @alt `latexpand`: Flatten LaTeX file by expanding \include and \input, ... and  remove comments
##

import re
import os
import sys
import argparse
from pathlib import Path

def read_file_content(filepath, base_dir=None):
    """
    Read content from a file, considering both absolute and relative paths.

    Args:
        filepath (str): Path to the file
        base_dir (Path, optional): Base directory for relative paths

    Returns:
        tuple: (Content of the file or None if file not found, resolved absolute path or None)
    """
    try:
        # If base_dir is provided, try relative path first
        if base_dir:
            full_path = (base_dir / filepath).resolve()
            if full_path.exists():
                return (full_path.read_text(encoding='utf-8'), str(full_path))

        # Try absolute path
        abs_path = Path(filepath).resolve()
        if abs_path.exists():
            return (abs_path.read_text(encoding='utf-8'), str(abs_path))

        # Try adding .tex extension if not present
        if not filepath.endswith('.tex'):
            return read_file_content(filepath + '.tex', base_dir)

    except Exception as e:
        print(f"Warning: Could not read file {filepath}: {e}", file=sys.stderr,)

    return (None, None)

def process_input_commands(content, base_dir, inclusion_chain=None, marker_comments=False):
    """
    Replace \input and \include commands with the content of referenced files.

    Args:
        content (str): LaTeX content to process
        base_dir (Path): Base directory for relative paths
        inclusion_chain (list, optional): List of files already included in the current chain
        marker_comments (bool): Whether to include marker comments around inlined files

    Returns:
        str: Processed content with substituted inputs
    """
    if inclusion_chain is None:
        inclusion_chain = []

    # Pattern for \input{filename} and \include{filename}, allowing optional spaces
    pattern = r'\\(input|include)\s*\{([^}]+)\}'

    def is_in_comment(content, match_start):
        """
        Check if a given position is in a comment.

        Args:
            content (str): The full content string.
            match_start (int): The start index of the match.

        Returns:
            bool: True if the position is in a comment, False otherwise.
        """
        # Find the start of the line
        line_start = content.rfind('\n', 0, match_start) + 1
        # Find the position of '%' between line_start and match_start
        comment_pos = content.find('%', line_start, match_start)
        if comment_pos != -1:
            return True
        else:
            return False

    def replace_match(match):
        if is_in_comment(content, match.start()):
            return match.group(0)  # Do not process matches in comments

        command = match.group(1)  # input or include
        filename = match.group(2).strip()
        if filename.startswith('"'):
            filename = filename[1:-1]
            #: strips quotes: `"..."`

        cv_dir = os.environ.get("nightNotesPrivate", "") + "/subjects/resume, CV"
        filename = re.sub(r'\\CVDir(?:\{\})?', cv_dir, filename)

        # Read the referenced file
        file_content, file_path = read_file_content(filename, base_dir)

        if file_content is None or file_path is None:
            print(f"Warning: Could not find file '{filename}'", file=sys.stderr,)
            return match.group(0)  # Keep original if file not found

        if file_path in inclusion_chain:
            print(f"Warning: Circular inclusion detected for file '{filename}'", file=sys.stderr,)
            return match.group(0)  # Keep original to prevent infinite recursion

        inclusion_chain.append(file_path)

        # Recursively process the included content
        processed_file_content = process_input_commands(
            file_content, Path(file_path).parent, inclusion_chain, marker_comments
        )

        inclusion_chain.pop()  # Remove the current file from the chain

        # For \include, wrap content in necessary commands
        if command == 'include':
            processed_file_content = (
                '\\clearpage\n'
                f'{processed_file_content}\n'
                '\\clearpage\n'
            )

        # Add marker comments if requested
        if marker_comments:
            processed_file_content = (
                f'% Begin inlined file {{{filename}}}\n'
                f'{processed_file_content}\n'
                f'% End inlined file {{{filename}}}\n'
            )

        return processed_file_content

    # Replace all matches in the content
    processed_content = re.sub(pattern, replace_match, content)

    return processed_content

def remove_comments_from_content(content):
    """
    Remove all comments from the content.

    Args:
        content (str): The LaTeX content

    Returns:
        str: Content without comments
    """
    lines = content.splitlines()
    new_lines = []
    for line in lines:
        # Remove everything after '%'
        idx = line.find('%')
        if idx != -1:
            line = line[:idx]
        new_lines.append(line)
    return '\n'.join(new_lines)

def main():
    parser = argparse.ArgumentParser(description='Substantiate LaTeX \\input and \\include commands')
    parser.add_argument('input_file', help='Input LaTeX file')
    parser.add_argument('-o', '--output', help='Output file (default: stdout)')
    parser.add_argument(
        '--rm-comments',
        action=argparse.BooleanOptionalAction,
        default=False,
        help='Remove all comments from the output (default: False)'
    )
    parser.add_argument(
        '--marker-comments',
        action=argparse.BooleanOptionalAction,
        default=False,
        help='Include marker comments around inlined files (default: False)'
    )
    args = parser.parse_args()

    # Read input file
    input_path = Path(args.input_file)
    if not input_path.exists():
        print(f"Error: Input file '{args.input_file}' not found")
        return 1

    content = input_path.read_text(encoding='utf-8')
    base_dir = input_path.parent

    # Process the content
    processed_content = process_input_commands(
        content, base_dir, marker_comments=args.marker_comments
    )

    # Remove comments if requested
    if args.rm_comments:
        processed_content = remove_comments_from_content(processed_content)

    # Write output
    if args.output:
        output_path = Path(args.output)
        output_path.write_text(processed_content, encoding='utf-8')
        print(f"Output written to '{args.output}'")
    else:
        print(processed_content)

    return 0

if __name__ == '__main__':
    exit(main())
