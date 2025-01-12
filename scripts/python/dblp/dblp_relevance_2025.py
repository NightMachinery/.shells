#!/usr/bin/env python3
import argparse
from typing import Optional, Dict, List
from dblp_utils import (
    DBLPClient,
    PatternGroups,
    get_papers_by_group_set_as_dict,
    print_stderr,
)


def get_relevant_papers(
    url: str, *, group_set: str = "rel25", client: Optional[DBLPClient] = None
) -> Dict[str, List[str]]:
    client = client or DBLPClient()
    pid = client.extract_pid(url=url)
    publications = client.fetch_publications(pid)
    titles = [pub.title for pub in publications]

    return get_papers_by_group_set_as_dict(titles, group_set)


def main() -> None:
    parser = argparse.ArgumentParser(description="Get relevant papers from DBLP")
    parser.add_argument("url", help="DBLP profile URL")
    parser.add_argument(
        "--group-set",
        choices=list(PatternGroups.GROUP_SETS.keys()),
        default="rel25",
        help="Group set to use for filtering papers",
    )
    args = parser.parse_args()

    papers_by_group = get_relevant_papers(args.url, group_set=args.group_set)

    separator = "-----------"
    for group_name in PatternGroups.get_group_names(args.group_set):
        if group_name in papers_by_group:
            print("\n".join(papers_by_group[group_name]))
            print(separator)


if __name__ == "__main__":
    main()
