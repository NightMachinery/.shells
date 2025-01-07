#!/usr/bin/env python3
import json
import argparse
from dblp_utils import DBLPClient, Publication, print_stderr

class PublicationFormatter:
    @staticmethod
    def format_publications(
        publications: List[Publication], *, output_mode: str = "plain-text"
    ) -> str:
        if not publications:
            return "No publications found."

        if output_mode == "plain-text":
            lines = []
            print_stderr(f"\nFound {len(publications)} publications:\n")

            for i, pub in enumerate(publications, 1):
                lines.extend(
                    [
                        f"{i}. Title: {pub.title}",
                        f"   Year: {pub.year}",
                        f"   Venue: {pub.venue}",
                        f"   Authors: {', '.join(pub.authors)}",
                        "",
                    ]
                )
            return "\n".join(lines)

        if output_mode == "titles-only":
            return "\n".join(pub.title for pub in publications)

        if output_mode == "json":
            return json.dumps([vars(pub) for pub in publications], indent=2)

        raise ValueError(f"Unknown output mode: {output_mode}")

def main() -> None:
    parser = argparse.ArgumentParser(description="Fetch publications from DBLP")
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("-u", "--url", help="DBLP profile URL")
    group.add_argument("-p", "--pid", help="DBLP person ID (e.g., 167/3214)")
    parser.add_argument(
        "-o",
        "--output-mode",
        choices=["plain-text", "titles-only", "json"],
        default="plain-text",
        help="Output format (default: plain-text)",
    )
    args = parser.parse_args()

    client = DBLPClient()
    try:
        pid = client.extract_pid(url=args.url, pid=args.pid)
        print_stderr(f"Using PID: {pid}\n")

        publications = client.fetch_publications(pid)
        output = PublicationFormatter.format_publications(
            publications, output_mode=args.output_mode
        )
        print(output)

    except ValueError as e:
        print_stderr(f"Error: {e}")
        exit(1)

if __name__ == "__main__":
    main()
