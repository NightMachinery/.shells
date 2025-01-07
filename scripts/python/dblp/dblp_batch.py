#!/usr/bin/env python3
import os
import sys
import subprocess
from pathlib import Path
from typing import Iterator, Tuple


def run_dblp_script(script_name: str, url: str, *, check: bool = True) -> str:
    """Run a DBLP script and return its output."""
    result = subprocess.run(
        [script_name, url],
        capture_output=True,
        text=True,
        check=check,
    )
    return result.stdout.strip()


def process_urls(urls: Iterator[str]) -> Iterator[Tuple[str, str, str]]:
    """Process each URL to get author name and relevant papers."""
    for url in urls:
        url = url.strip()
        if not url:
            continue

        try:
            name = run_dblp_script("dblp_author_name.py", url)
            papers = run_dblp_script("dblp_relevance_2025.py", url)
            yield url, name, papers
        except subprocess.CalledProcessError as e:
            print(f"Error processing {url}: {e}", file=sys.stderr)
            continue


def main() -> None:
    # Create base directory
    base_dir = Path.home() / "tmp" / "professors"
    base_dir.mkdir(parents=True, exist_ok=True)

    # Process each URL from stdin
    for url, name, papers in process_urls(sys.stdin):
        # Create professor directory
        prof_dir = base_dir / name
        prof_dir.mkdir(exist_ok=True)

        # Save papers to file
        output_file = prof_dir / "rel25.txt"
        output_file.write_text(papers)

        # Print the path
        print(output_file)


if __name__ == "__main__":
    main()
