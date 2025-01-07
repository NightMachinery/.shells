#!/usr/bin/env python3
##
import sys
import requests
from typing import Optional
from dblp_titles import DBLPClient, print_stderr
import xml.etree.ElementTree as ET

def get_author_name(url: str, *, client: Optional[DBLPClient] = None) -> str:
    client = client or DBLPClient()
    pid = client.extract_pid(url=url)

    url = f"{client.base_url}/pid/{pid}.xml"
    try:
        response = requests.get(url)
        response.raise_for_status()
        root = ET.fromstring(response.content)

        name = root.get("name")
        if name:
            return name

        raise ValueError("Could not find author name")

    except Exception as e:
        print_stderr(f"Error fetching name: {e}")
        sys.exit(1)

def main() -> None:
    if len(sys.argv) != 2:
        print("Usage: dblp_name.py <dblp_url>", file=sys.stderr)
        sys.exit(1)

    name = get_author_name(sys.argv[1])
    print(name)

if __name__ == "__main__":
    main()
