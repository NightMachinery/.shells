#!/usr/bin/env python3
import json
import sys
import argparse
from typing import Dict, List, TextIO

def _separator() -> str:
    """Returns the separator string used between conversation chunks."""
    return "\n" + "-" * 40 + "\n"

def format_aistudio_json(data: Dict, *, include_thoughts: bool = False, assistant_only: bool = False) -> str:
    """
    Formats AI Studio JSON data into readable plain text.

    Args:
        data: The loaded JSON data as a Python dictionary.
        include_thoughts: Whether to include thought chunks in the output.
        assistant_only: Whether to output only assistant messages without role tags.

    Returns:
        A string containing the formatted conversation and citations.
    """
    output_lines = []

    # --- Header/Metadata (Optional - can be added if needed) ---
    # Example: Include model name if desired
    # model_name = data.get('runSettings', {}).get('model')
    # if model_name:
    #     output_lines.append(f"Model: {model_name}")
    #     output_lines.append("-" * 20)

    # --- System Instruction ---
    if not assistant_only:
        system_instruction = data.get('systemInstruction', {}).get('text')
        if system_instruction:
            output_lines.append("SYSTEM INSTRUCTION:")
            output_lines.append(system_instruction.strip())
            output_lines.append("\n" + "=" * 40 + "\n")

    # --- Conversation Chunks ---
    chunks = data.get('chunkedPrompt', {}).get('chunks', [])
    if not chunks:
        output_lines.append("No conversation chunks found.")
    else:
        first_chunk = True
        for chunk in chunks:
            role = chunk.get('role', 'unknown')
            text = chunk.get('text', '').strip()
            is_thought = chunk.get('isThought', False)

            if is_thought and not include_thoughts:
                continue

            if assistant_only and role.lower() != 'model':
                continue

            if not first_chunk:
                output_lines.append(_separator())
            first_chunk = False

            if not assistant_only:
                if is_thought:
                    output_lines.append(f"THOUGHT ({role.upper()}):")
                else:
                    output_lines.append(f"{role.upper()}:")

            output_lines.append(text)


    # --- Citations ---
    citations = data.get('citations', [])
    if citations:
        output_lines.append("\n\n" + "=" * 40)
        output_lines.append("CITATIONS:")
        output_lines.append("-" * 10)
        for i, citation in enumerate(citations):
            text = citation.get('text', 'N/A').strip()
            uri = citation.get('uri', 'N/A')
            output_lines.append(f"[{i+1}] Text Snippet: {text}")
            output_lines.append(f"    URI: {uri}")
            if i < len(citations) - 1:
                 output_lines.append("")

    return '\n'.join(output_lines)

def main():
    parser = argparse.ArgumentParser(
        description='Convert AI Studio JSON export to human-readable plain text.'
    )
    parser.add_argument(
        'input_file',
        nargs='?',
        type=argparse.FileType('r', encoding='utf-8'),
        default=(None if sys.stdin.isatty() else sys.stdin),
        help='JSON file exported from AI Studio (reads from stdin if not provided and stdin is not a terminal)'
    )
    parser.add_argument(
        '-o', '--output',
        type=argparse.FileType('w', encoding='utf-8'),
        default=sys.stdout,
        help='Output file path (writes to stdout by default)'
    )
    parser.add_argument(
        '--thoughts',
        action=argparse.BooleanOptionalAction,
        default=False,
        help='Include thought chunks in the output (default: %(default)s)'
    )
    parser.add_argument(
        '--assistant-only',
        action=argparse.BooleanOptionalAction,
        default=False,
        help='Output only assistant messages without role tags (default: %(default)s)'
    )

    args = parser.parse_args()

    if args.input_file is None:
        parser.print_help()
        sys.exit(1)

    try:
        with args.input_file as infile:
            try:
                chat_data = json.load(infile)
            except json.JSONDecodeError as e:
                print(f"Error: Invalid JSON format in input file. {e}", file=sys.stderr)
                sys.exit(1)

        formatted_text = format_aistudio_json(
            chat_data,
            include_thoughts=args.thoughts,
            assistant_only=args.assistant_only
        )

        with args.output as outfile:
            outfile.write(formatted_text)
            # Add a newline at the end if writing to a file for cleaner diffs etc.
            if outfile is not sys.stdout:
                outfile.write('\n')

    except Exception as e:
        print(f"An unexpected error occurred: {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
