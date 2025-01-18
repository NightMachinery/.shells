#!/usr/bin/env python3
import sys
import argparse
import subprocess
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, as_completed
from typing import Iterator, Tuple, Dict, List
from dblp_utils import (
    PatternGroups,
    save_group_results,
    run_dblp_script,
    print_stderr,
    DBLPClient,
    get_papers_by_group_set_as_dict,
)


def process_single_url(url: str, *, group_set: str, client: DBLPClient) -> Tuple[str, str, Dict[str, List[str]]]:
    """Process a single DBLP URL and return the results."""
    try:
        name = run_dblp_script("dblp_author_name.py", url)
        pid = client.extract_pid(url=url)
        publications = client.fetch_publications(pid)
        titles = [pub.title for pub in publications]
        papers_by_group = get_papers_by_group_set_as_dict(titles, group_set)
        return url, name, papers_by_group
    except subprocess.CalledProcessError as e:
        print_stderr(f"Error processing {url}: {e}")
        raise


def process_urls(
    urls: Iterator[str], *, group_set: str, parallel: bool = True, max_workers: int = 4
) -> Iterator[Tuple[str, str, Dict[str, List[str]]]]:
    """Process URLs either sequentially or in parallel."""
    client = DBLPClient()
    urls = [url.strip() for url in urls if url.strip()]  # Pre-process URLs

    if not parallel:
        # Sequential processing
        for url in urls:
            try:
                result = process_single_url(url, group_set=group_set, client=client)
                yield result
            except Exception as e:
                print_stderr(f"Error processing {url}: {e}")
                continue
    else:
        # Parallel processing
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            # Submit all tasks
            future_to_url = {
                executor.submit(process_single_url, url, group_set=group_set, client=client): url
                for url in urls
            }

            # Process completed tasks as they finish
            for future in as_completed(future_to_url):
                url = future_to_url[future]
                try:
                    result = future.result()
                    yield result
                except Exception as e:
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
    parser.add_argument(
        "--parallel",
        action=argparse.BooleanOptionalAction,
        default=True,
        help="Enable/disable parallel processing (default: enabled)",
    )
    parser.add_argument(
        "--max-workers",
        type=int,
        default=32,
        help="Maximum number of parallel workers (default: 4)",
    )
    args = parser.parse_args()

    # Create output directory with parents if it doesn't exist
    args.output_dir.mkdir(parents=True, exist_ok=True)

    for url, name, papers_by_group in process_urls(
        sys.stdin,
        group_set=args.group_set,
        parallel=args.parallel,
        max_workers=args.max_workers
    ):
        prof_dir = args.output_dir / name
        prof_dir.mkdir(exist_ok=True)

        save_group_results(prof_dir, args.group_set, papers_by_group)
        print(f"Processed {name}: {prof_dir}")


if __name__ == "__main__":
    main()
