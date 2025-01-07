#!/usr/bin/env python3
##
import re
import sys
from typing import List, Set, Sequence, Optional, Iterator
from dblp_titles import DBLPClient, PublicationFormatter


def filter_papers(
    titles: Sequence[str], pattern: str, *, case_sensitive: bool = False
) -> List[str]:
    flags = 0 if case_sensitive else re.IGNORECASE
    regex = re.compile(pattern, flags)
    return [title for title in titles if regex.search(title)]


def remove_duplicates_across_groups(
    groups: Sequence[Sequence[str]], *, case_sensitive: bool = False
) -> List[List[str]]:
    seen = set()
    result = []

    for group in groups:
        current_group = []
        for item in group:
            key = item if case_sensitive else item.lower()
            if key not in seen:
                seen.add(key)
                current_group.append(item)
        if current_group:
            result.append(current_group)

    return result


def get_relevant_papers(
    url: str,
    *,
    patterns: Optional[List[str]] = None,
    separator: str = "-----------",
    client: Optional[DBLPClient] = None
) -> List[str]:
    client = client or DBLPClient()
    patterns = patterns or [
        r"interp|explain|salien|attribution|understand|concept|visualiz",
        r"relevan|ground|\bwhy\b|mechanis|circuit|probe|probing|atten",
        r"robust|adversarial",
        r"vision transformer|reason|agent|llm|language model",
    ]
    pid = client.extract_pid(url=url)
    publications = client.fetch_publications(pid)
    titles = [pub.title for pub in publications]

    paper_groups = []
    for pattern in patterns:
        matches = filter_papers(titles, pattern)
        if matches:
            paper_groups.append(matches)

    deduped_groups = remove_duplicates_across_groups(paper_groups, case_sensitive=False)
    return [x for group in deduped_groups for x in [*group, separator]][:-1]


def main() -> None:
    if len(sys.argv) != 2:
        print("Usage: script.py <dblp_url>", file=sys.stderr)
        sys.exit(1)
    papers = get_relevant_papers(sys.argv[1])
    print("\n".join(papers))


if __name__ == "__main__":
    main()
