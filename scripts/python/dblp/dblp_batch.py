#!/usr/bin/env python3
##
import sys
import argparse
import subprocess
from pathlib import Path
from typing import Iterator, Tuple
from dblp_utils import PatternGroups, save_group_results, run_dblp_script, print_stderr


def process_urls(
    urls: Iterator[str], *, group_set: str
) -> Iterator[Tuple[str, str, str]]:
    """Process each URL to get author name and relevant papers."""
    for url in urls:
        url = url.strip()
        if not url:
            continue

        try:
            name = run_dblp_script("dblp_author_name.py", url)
            papers = run_dblp_script("dblp_relevance_2025.py", url, group_set=group_set)
            yield url, name, papers
        except subprocess.CalledProcessError as e:
            print_stderr(f"Error processing {url}: {e}")
            continue


def main() -> None:
    parser = argparse.ArgumentParser(description="Batch process DBLP profiles")
    parser.add_argument(
        "--group-set",
        choices=list(PatternGroups.GROUP_SETS.keys()),
        default="rel25",
        help="Group set to use for filtering papers",
    )
    args = parser.parse_args()

    # Create base directory
    base_dir = Path.home() / "tmp" / "professors"
    base_dir.mkdir(parents=True, exist_ok=True)

    # Process each URL from stdin
    for url, name, papers in process_urls(sys.stdin, group_set=args.group_set):
        # Create professor directory
        prof_dir = base_dir / name
        prof_dir.mkdir(exist_ok=True)

        # Parse papers into groups
        papers_by_group: Dict[str, List[str]] = {}
        current_group = []
        current_group_name = None

        for line in papers.split("\n"):
            if line == "-----------":
                if current_group_name and current_group:
                    papers_by_group[current_group_name] = current_group
                current_group = []
                if current_group_name:
                    current_group_name = None
                continue

            if current_group_name is None:
                current_group_name = PatternGroups.get_group_names(args.group_set)[
                    len(papers_by_group)
                ]
            current_group.append(line)

        if current_group_name and current_group:
            papers_by_group[current_group_name] = current_group

        # Save results
        save_group_results(prof_dir, args.group_set, papers_by_group)
        print(f"Processed {name}: {prof_dir}")


if __name__ == "__main__":
    main()
