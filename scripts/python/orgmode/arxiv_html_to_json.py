#!/usr/bin/env python3
"""Read arXiv abstract-page HTML and emit the Atom JSON subset our tools use."""

import json
import re
import sys
from datetime import date
from html.parser import HTMLParser


class CitationMetadata(HTMLParser):
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.metadata = {}

    def handle_starttag(self, tag, attrs):
        if tag != "meta":
            return
        attrs = dict(attrs)
        name = attrs.get("name", "").lower()
        if name.startswith("citation_"):
            self.metadata.setdefault(name, []).append(attrs.get("content", "").strip())


def parse_arxiv_html(html, *, arxiv_id):
    parser = CitationMetadata()
    parser.feed(html)
    metadata = parser.metadata

    def required(name):
        values = metadata.get("citation_" + name, [])
        if not values or not all(values):
            raise ValueError(f"abstract page missing citation_{name}")
        return values[0]

    page_id = required("arxiv_id")
    if re.sub(r"v\d+$", "", page_id) != re.sub(r"v\d+$", "", arxiv_id):
        raise ValueError("abstract page arXiv ID does not match the requested paper")

    published = date.fromisoformat(required("date").replace("/", "-")).isoformat()
    required("author")
    entry = {
        "id": f"https://arxiv.org/abs/{arxiv_id}",
        "title": required("title"),
        "summary": required("abstract"),
        "published": published,
        "author": [{"name": name} for name in metadata["citation_author"]],
        "link": [{"@href": required("pdf_url"), "@title": "pdf", "@type": "application/pdf"}],
    }
    return {"feed": {"entry": entry}}


def main():
    if len(sys.argv) != 2:
        sys.exit("usage: arxiv_html_to_json.py <arxiv-id> < abstract.html")
    try:
        result = parse_arxiv_html(sys.stdin.read(), arxiv_id=sys.argv[1])
    except ValueError as error:
        sys.exit(f"arxiv_html_to_json: {error}")
    print(json.dumps(result, ensure_ascii=False))


if __name__ == "__main__":
    main()
