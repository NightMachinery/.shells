#!/usr/bin/env python3
##
#: `passgen_words.py --wordlist="/usr/share/dict/words" 4`
##
import argparse
import os
import random
import secrets
import sys
from pathlib import Path
from typing import List, Optional


def load_wordlist(wordlist_path: Path) -> List[str]:
    """Load words from the specified wordlist file."""
    try:
        with open(wordlist_path) as file:
            return [word.strip() for word in file if word.strip()]
    except FileNotFoundError:
        raise FileNotFoundError(f"Wordlist not found at {wordlist_path}")


def generate_passphrase(words: List[str], length: int) -> str:
    """Generate a passphrase of specified length using random words from the wordlist."""
    if len(words) < length:
        raise ValueError(f"Wordlist contains fewer than {length} words")

    return "-".join(secrets.choice(words) for _ in range(length))


def main() -> None:
    """Parse arguments, generate and print a passphrase."""
    parser = argparse.ArgumentParser(
        description="Generate a random passphrase from a wordlist."
    )
    parser.add_argument(
        "length",
        type=int,
        nargs="?",
        default=10,
        help="Number of words in the passphrase",
    )
    parser.add_argument("--wordlist", type=Path, help="Path to the wordlist file")
    args = parser.parse_args()

    # wordlist_path_default = Path.home() / ".wordlist"
    wordlist_path_default = "/usr/share/dict/words"
    wordlist_path = (
        args.wordlist or os.environ.get("PASSGEN_WORDLIST") or wordlist_path_default
    )

    try:
        words = load_wordlist(wordlist_path)
        passphrase = generate_passphrase(words, args.length)
        print(f"{passphrase}")  # Passphrase goes to stdout
    except (FileNotFoundError, ValueError) as e:
        print(f"Error: {e}", file=sys.stderr)  # Errors go to stderr
        sys.exit(1)  # Exit with non-zero status on error


if __name__ == "__main__":
    main()
