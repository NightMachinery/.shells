#!/usr/bin/env python3
import sys
import argparse
import subprocess
from pathlib import Path
from typing import Iterator, Tuple, Dict, List
from dblp_utils import (
    PatternGroups,
    save_group_results,
    run_dblp_script,
    print_stderr,
    DBLPClient,
    get_papers_by_group_set_as_dict,
)


def process_urls(
    urls: Iterator[str], *, group_set: str
) -> Iterator[Tuple[str, str, Dict[str, List[str]]]]:
    client = DBLPClient()
    for url in urls:
        url = url.strip()
        if not url:
            continue

        try:
            name = run_dblp_script("dblp_author_name.py", url)
            pid = client.extract_pid(url=url)
            publications = client.fetch_publications(pid)
            titles = [pub.title for pub in publications]
            papers_by_group = get_papers_by_group_set_as_dict(titles, group_set)
            yield url, name, papers_by_group
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
    parser.add_argument(
        "-d",
        "--output-dir",
        type=Path,
        default=Path.home() / "tmp" / "professors",
        help="Output directory (default: ~/tmp/professors)",
    )
    args = parser.parse_args()

    # Create output directory with parents if it doesn't exist
    args.output_dir.mkdir(parents=True, exist_ok=True)

    for url, name, papers_by_group in process_urls(sys.stdin, group_set=args.group_set):
        prof_dir = args.output_dir / name
        prof_dir.mkdir(exist_ok=True)

        save_group_results(prof_dir, args.group_set, papers_by_group)
        print(f"Processed {name}: {prof_dir}")


if __name__ == "__main__":
    main()
