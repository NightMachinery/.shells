#!/usr/bin/env python3
"""Extract public Semantic Scholar metadata for the existing Org formatter."""

import json
import re
import sys
from datetime import datetime
from urllib.parse import urljoin, urlsplit

from bs4 import BeautifulSoup


def parse_paper(html, *, url):
    soup = BeautifulSoup(html, "html.parser")

    def meta(*names):
        return [tag.get("content", "").strip() for tag in soup.select("meta")
                if (tag.get("name") or tag.get("property")) in names
                and tag.get("content", "").strip()]

    def texts(selector):
        return [tag.get_text(" ", strip=True) for tag in soup.select(selector)
                if tag.get_text(strip=True)]

    def nodes(value):
        if isinstance(value, list):
            for item in value:
                yield from nodes(item)
        elif isinstance(value, dict):
            yield value
            yield from nodes(value.get("@graph", []))

    article = {}
    for script in soup.select('script[type="application/ld+json"]'):
        try:
            candidates = nodes(json.loads(script.string or script.get_text()))
            for candidate in candidates:
                kinds = candidate.get("@type", [])
                if isinstance(kinds, str):
                    kinds = [kinds]
                if any(isinstance(kind, str) and kind.rsplit("/", 1)[-1] in ("ScholarlyArticle", "Article")
                       for kind in kinds):
                    article = candidate
                    break
        except (ValueError, TypeError):
            continue
        if article:
            break

    def first(*values):
        return next((value for value in values if isinstance(value, str) and value.strip()), "")

    title = first(*meta("citation_title"), article.get("headline"), article.get("name"),
                  *texts('[data-selenium-selector="paper-detail-title"]'))
    if not title:
        raise ValueError("no paper metadata found; the page may require human verification "
                         "or the paper may be unavailable")

    canonical = soup.select_one('link[rel="canonical"]')
    canonical_url = urljoin(url, canonical.get("href", "")) if canonical else url
    # Never publish an unrelated canonical URL supplied by an error page.
    if urlsplit(canonical_url).hostname not in ("www.semanticscholar.org", "semanticscholar.org"):
        canonical_url = url

    data = {"title": [title.strip()], "url": [canonical_url]}

    def put(key, values):
        if not isinstance(values, list):
            values = [values]
        values = list(dict.fromkeys(str(value).strip() for value in values
                                    if isinstance(value, (str, int)) and str(value).strip()))
        if values:
            data[key] = values

    put("abstract", first(*meta("citation_abstract"), article.get("description"),
                          *texts('[data-selenium-selector="abstract-text"]'),
                          *meta("description", "og:description")))

    authors = meta("citation_author")
    if not authors:
        ld_authors = article.get("author", [])
        if not isinstance(ld_authors, list):
            ld_authors = [ld_authors]
        authors = [author.get("name", "") if isinstance(author, dict) else author
                   for author in ld_authors]
    if not authors:
        authors = texts('[data-selenium-selector="author-list"] a[data-selenium-selector="author-name"]')
    put("authors_names", authors)

    raw_date = first(*meta("citation_publication_date", "citation_date"),
                     article.get("datePublished"), *texts('[data-selenium-selector="paper-year"]'))
    for fmt in ("%Y-%m-%d", "%Y/%m/%d", "%B %d, %Y", "%d %B %Y"):
        try:
            put("publicationDate", datetime.strptime(raw_date[:10] if "T" in raw_date else raw_date,
                                                    fmt).strftime("%Y-%m-%d"))
            break
        except ValueError:
            continue
    year = re.search(r"\b(?:19|20)\d{2}\b", raw_date)
    if year:
        put("year", year.group())

    venue = first(*meta("citation_conference_title", "citation_journal_title"),
                  *texts('[data-heap-id="paper-meta-journal"]'))
    part = article.get("isPartOf", {})
    if not venue and isinstance(part, dict):
        venue = first(part.get("name"))
    put("venue", venue)
    put("bibtex", texts(".bibtex-citation"))
    put("doi", meta("citation_doi") or
        [tag.get("href", "") for tag in soup.select('[data-selenium-selector="paper-doi"] .doi__link')])
    paper_id = re.search(r"/([a-fA-F0-9]{40})(?:[/?#]|$)", canonical_url)
    if paper_id:
        put("paperId", paper_id.group(1))
    corpus = re.search(r"(?:Corpus\s*ID:\s*)?(\d+)", " ".join(texts('[data-selenium-selector="corpus-id"]')))
    if corpus:
        put("corpusID", corpus.group(1))

    for key, selector in (("citationCount", '[data-heap-nav="citing-papers"], [data-selenium-selector="citation-count"]'),
                          ("referenceCount", '[data-heap-nav="references"]')):
        for text in texts(selector):
            count = re.search(r"\b[\d,]+(?:\.\d+)?\s*[kKmM]?\b", text)
            if count:
                # Abbreviated display counts are estimates, so omit them.
                number = count.group().replace(",", "").strip()
                if number.isdecimal():
                    put(key, number)
                break

    links = [tag.get("href", "") for tag in soup.select('[data-selenium-selector="paper-link"]')]
    pdfs = meta("citation_pdf_url")
    for key, values in (("links", links), ("pdf_urls", pdfs)):
        put(key, [urljoin(canonical_url, value) for value in values
                  if value and urlsplit(urljoin(canonical_url, value)).scheme in ("http", "https")])
    put("fieldsOfStudy", texts('[data-selenium-selector="entity-name"]'))
    return data


def parse_markdown(markdown, *, url):
    """Read the main paper's BibTeX and counts, excluding related-paper text."""
    text = re.sub(r"\\([^\w\s])", r"\1", markdown)
    start = re.search(r"@\w+\s*\{", text)
    if not start:
        raise ValueError("no paper BibTeX found in fetched Markdown; the page may "
                         "require human verification or the full paper URL")
    depth = 1
    end = start.end()
    while end < len(text) and depth:
        depth += (text[end] == "{") - (text[end] == "}")
        end += 1
    if depth:
        raise ValueError("incomplete paper BibTeX in fetched Markdown")
    bibtex = text[start.start():end]
    fields = {}
    pos = bibtex.find(",") + 1
    while match := re.search(r"(\w+)\s*=\s*", bibtex[pos:]):
        name = match.group(1).lower()
        pos += match.end()
        if bibtex[pos:pos + 1] == "{":
            begin = pos = pos + 1
            depth = 1
            while pos < len(bibtex) and depth:
                depth += (bibtex[pos] == "{") - (bibtex[pos] == "}")
                pos += 1
            value = bibtex[begin:pos - 1]
        elif bibtex[pos:pos + 1] == '"':
            begin = pos + 1
            pos = bibtex.find('"', begin)
            if pos < 0:
                raise ValueError("incomplete quoted BibTeX field")
            value = bibtex[begin:pos]
            pos += 1
        else:
            begin = pos
            while pos < len(bibtex) and bibtex[pos] not in ",}":
                pos += 1
            value = bibtex[begin:pos]
        fields[name] = value.strip()
    title = fields.get("title", "")
    if not title:
        raise ValueError("paper BibTeX has no title")
    data = {"title": [title], "url": [url], "bibtex": [bibtex]}
    for key, field in (("venue", "booktitle"), ("year", "year"), ("doi", "doi")):
        if fields.get(field):
            data[key] = [fields[field]]
    if "venue" not in data and fields.get("journal"):
        data["venue"] = [fields["journal"]]
    if fields.get("author"):
        data["authors_names"] = re.split(r"\s+and\s+", fields["author"])
    if fields.get("url"):
        data["links"] = [fields["url"]]
        corpus = re.search(r"CorpusID:(\d+)", fields["url"])
        if corpus:
            data["corpusID"] = [corpus.group(1)]
    paper_id = re.search(r"/([a-fA-F0-9]{40})(?:[/?#]|$)", url)
    if paper_id:
        data["paperId"] = [paper_id.group(1)]
    for key, label in (("citationCount", "Citations"), ("referenceCount", "References")):
        count = re.search(r"(?im)^(?:#{1,2}\s+)?([\d,]+)\s+" + label + r"\s*$", text)
        if count:
            data[key] = [count.group(1).replace(",", "")]
    # The leading text is often a TLDR, not the paper's complete abstract.
    leading = re.split(r"(?m)^(?:#{1,2}\s|[\d,]+\s+Citations\s*$)", text[end:])[0].strip()
    if leading:
        data["tldr"] = [leading]
    return data


def parse_fetch_page(page, *, url):
    """Use the Fetch title when Semantic Scholar omits its main paper section."""
    content = page.get("content")
    title = page.get("title")
    if not isinstance(content, str) or not isinstance(title, str):
        raise ValueError("Fetch response must contain string title and content")
    match = re.fullmatch(r"(?:\[PDF\]\s*)?(.+?)\s*\|\s*Semantic Scholar", title.strip(), re.I)
    if not match:
        raise ValueError("Fetch did not return a Semantic Scholar paper title; human verification may be required")
    title = match.group(1).strip()
    text = re.sub(r"\\([^\w\s])", r"\1", content)
    primary = re.split(r"(?m)^###\s", text, maxsplit=1)[0]
    if re.search(r"@\w+\s*\{", primary):
        data = parse_markdown(content, url=url)
        normalize = lambda value: re.sub(r"\W+", "", value).casefold()
        if normalize(data["title"][0]) != normalize(title):
            raise ValueError("paper BibTeX does not match the fetched page title")
        return data
    data = {"title": [title], "url": [url], "metadataStatus": ["partial"]}
    paper_id = re.search(r"/([a-fA-F0-9]{40})(?:[/?#]|$)", url)
    if paper_id:
        data["paperId"] = [paper_id.group(1)]
    for key, label in (("citationCount", "Citations"), ("referenceCount", "References")):
        count = re.search(r"(?im)^(?:#{1,2}\s+)?([\d,]+)\s+" + label + r"\s*$", primary)
        if count:
            data[key] = [count.group(1).replace(",", "")]
    return data


def main():
    args = sys.argv[1:]
    mode = args[0] if args[:1] in (["--markdown"], ["--fetch-json"]) else ""
    if mode:
        args = args[1:]
    if len(args) != 1:
        sys.exit("usage: semantic_scholar_html_to_json.py [--markdown|--fetch-json] <paper-url> < page")
    try:
        if mode == "--fetch-json":
            data = parse_fetch_page(json.load(sys.stdin), url=args[0])
        else:
            parse = parse_markdown if mode == "--markdown" else parse_paper
            data = parse(sys.stdin.read(), url=args[0])
    except ValueError as error:
        sys.exit(f"semantic-scholar scraper: {error}")
    print(json.dumps(data, ensure_ascii=False))


if __name__ == "__main__":
    main()
