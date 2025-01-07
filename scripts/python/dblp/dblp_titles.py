#!/usr/bin/env python3
##
import requests
import xml.etree.ElementTree as ET
from typing import List, Dict, Optional, Protocol
import argparse
import re
import sys
import json
from urllib.parse import urlparse
from dataclasses import dataclass


def print_stderr(message: str) -> None:
    print(message, file=sys.stderr)


@dataclass
class Publication:
    title: str
    year: str
    venue: str
    authors: List[str]

    @classmethod
    def from_xml(cls, pub_element: ET.Element) -> "Publication":
        def get_text(element: Optional[ET.Element], default: str = "Unknown") -> str:
            return element.text if element is not None else default

        return cls(
            title=get_text(pub_element.find(".//title")),
            year=get_text(pub_element.find(".//year")),
            venue=get_text(pub_element.find(".//venue")),
            authors=[author.text for author in pub_element.findall(".//author") or []],
        )


class DBLPClient:
    def __init__(self, *, base_url: str = "https://dblp.org"):
        self.base_url = base_url.rstrip("/")

    def extract_pid(
        self, *, url: Optional[str] = None, pid: Optional[str] = None
    ) -> str:
        if pid:
            return pid

        if not url:
            raise ValueError("Either URL or PID must be provided")

        pid_match = re.search(r"pid/(\d+/\d+)", url)
        if pid_match:
            return pid_match.group(1)

        path = urlparse(url).path
        if not path:
            raise ValueError("Invalid URL: no path found")

        path = re.sub(r"\.(html|php)$", "", path.strip("/"))
        segments = path.split("/")
        numeric_segments = [s for s in segments if s.isdigit()]

        if len(numeric_segments) >= 2:
            return f"{numeric_segments[-2]}/{numeric_segments[-1]}"

        raise ValueError("Could not extract PID from URL")

    def fetch_publications(self, pid: str) -> List[Publication]:
        url = f"{self.base_url}/pid/{pid}.xml"
        try:
            response = requests.get(url)
            response.raise_for_status()
            root = ET.fromstring(response.content)
            return [Publication.from_xml(pub) for pub in root.findall(".//r")]

        except requests.RequestException as e:
            print_stderr(f"Error fetching data: {e}")
            return []
        except ET.ParseError as e:
            print_stderr(f"Error parsing XML: {e}")
            return []


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


def parse_args() -> argparse.Namespace:
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
    return parser.parse_args()


def main() -> None:
    args = parse_args()
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
