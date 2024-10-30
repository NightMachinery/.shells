#!/usr/bin/env python3
##
import sys
import json
import re
from typing import List, Optional, Dict, Tuple


def newline2space(s: str) -> str:
    """
    Replaces newlines and multiple spaces with a single space, removes specific patterns.

    Example:
        >>> newline2space("Title [Test]\\nSecond Line")
        'Title {Test} Second Line'
    """
    s = re.sub(r'^[ \t]+', '', s, count=1)
    s = s.replace('\x02', '')
    s = re.sub(r'[\r\n]+', ' ', s)
    s = re.sub(r'[ \t]+', ' ', s)
    s = re.sub(r'(\(\*plb:\w+)-[ \t]+', r'\1', s)
    return s


def org_title_escape(s: str) -> str:
    """
    Escapes titles for Org-mode links.

    Example:
        >>> org_title_escape("Title [Test]")
        'Title {Test}'
    """
    return re.sub(r"(?<!\\)\[", "{", re.sub(r"(?<!\\)\]", "}", s))


def org_link_escape(s: str) -> str:
    """
    Escapes URLs for Org-mode links.

    Example:
        >>> org_link_escape("http://example.com/path[1]")
        'http://example.com/path\\[1\\]'
    """
    return re.sub(r"(?<!\\)\[", r"\\[", re.sub(r"(?<!\\)\]", r"\\]", s))


def camel_case(s: str) -> str:
    """
    Converts strings to CamelCase.

    Example:
        >>> camel_case("conference on computer vision")
        'ConferenceOnComputerVision'
    """
    return "".join(word.capitalize() for word in re.split(r"[ -]", s))


VENUE_NAME_MAPPING: List[Tuple[re.Pattern, str]] = [
    (re.compile(r"(?i)^arXiv(?:\.org)?$"), "arXiv"),
    (
        re.compile(
            r"(?i)IEEE/CVF International Conference on Computer Vision Workshop"
        ),
        "ICCVW",
    ),
    (
        re.compile(
            r"(?i)Conference on Computer Vision and Pattern Recognition Workshops"
        ),
        "CVPRW",
    ),
    (re.compile(r"(?i)European Conference on Computer Vision Workshops"), "ECCVW"),
    (re.compile(r"(?i)British Machine Vision Conference"), "BMVC"),
    (
        re.compile(r"(?i)Workshop on Action and Anticipation for Visual Learning"),
        "AAVL",
    ),
    (re.compile(r"(?i)Workshop on Visual Object Tracking"), "VOT"),
    (
        re.compile(
            r"(?i)Workshop on the Applications of Computer Vision for Drone Technology"
        ),
        "ACVDT",
    ),
    (
        re.compile(
            r"(?i)Workshop on Large Scale 3D Data: Acquisition, Modelling and Analysis"
        ),
        "LS3DA",
    ),
    (
        re.compile(r"(?i)Workshop on Efficient Deep Learning for Computer Vision"),
        "EDLCV",
    ),
    (re.compile(r"(?i)Workshop on Biometrics"), "BIO"),
    (re.compile(r"(?i)Workshop on Scene Understanding and Autonomous Systems"), "SUAS"),
    (
        re.compile(
            r"(?i)International Workshop on Artificial Intelligence for Cultural Heritage"
        ),
        "AI4CH",
    ),
    (
        re.compile(
            r"(?i)Workshop on Fairness, Accountability, and Transparency in Machine Learning"
        ),
        "FAT/ML",
    ),
    (re.compile(r"(?i)Workshop on Automated Knowledge Base Construction"), "AKBC"),
    (re.compile(r"(?i)Workshop on Machine Learning in Health"), "ML4H"),
    (
        re.compile(
            r"(?i)Workshop on Computer Vision for Augmented and Virtual Reality"
        ),
        "CV4ARVR",
    ),
    (re.compile(r"(?i)Workshop on Systems for ML"), "SysML"),
    (re.compile(r"(?i)Workshop on Robot Learning"), "CoRL"),
    (re.compile(r"(?i)Deep Learning for Real-Time Graphics Workshop"), "DLRTG"),
    (
        re.compile(r"(?i)Workshop on Machine Learning for Creativity and Design"),
        "ML4AD",
    ),
    (
        re.compile(
            r"(?i)BlackboxNLP Workshop on Analyzing and Interpreting Neural Networks for NLP"
        ),
        "BlackboxNLPW",
    ),
    (re.compile(r"(?i)ACL Workshop"), "ACLW"),
    (
        re.compile(
            r"(?i)Annual Meeting of the Association for Computational Linguistics"
        ),
        "ACL",
    ),
    (
        re.compile(
            r"(?i)North American Chapter of the Association for Computational Linguistics"
        ),
        "NAACL",
    ),
    (
        re.compile(
            r"(?i)European Chapter of the Association for Computational Linguistics"
        ),
        "EACL",
    ),
    (
        re.compile(
            r"(?i)Asian Chapter of the Association for Computational Linguistics"
        ),
        "AACL",
    ),
    (
        re.compile(
            r"(?i)Transactions of the Association for Computational Linguistics"
        ),
        "TACL",
    ),
    (re.compile(r"(?i)Conference on Computational Natural Language Learning"), "CoNLL"),
    (re.compile(r"(?i)Computational Linguistics"), "CL"),
    (re.compile(r"(?i)Journal of Natural Language Engineering"), "JNLE"),
    (
        re.compile(r"(?i)(?:Conference on )?Computer Vision and Pattern Recognition"),
        "CVPR",
    ),
    (
        re.compile(r"(?i)(?:Conference on )?Uncertainty in Artificial Intelligence"),
        "UAI",
    ),
    (
        re.compile(
            r"(?i)(?:Conference on )?Empirical Methods in Natural Language Processing"
        ),
        "EMNLP",
    ),
    (re.compile(r"(?i)International Conference on Computer Vision"), "ICCV"),
    (re.compile(r"(?i)European Conference on Computer Vision"), "ECCV"),
    (re.compile(r"(?i)Neural Information Processing Systems"), "NeurIPS"),
    (re.compile(r"(?i)International Conference on Machine Learning"), "ICML"),
    (re.compile(r"(?i)AAAI Conference on Artificial Intelligence"), "AAAI"),
    (
        re.compile(r"(?i)International Joint Conference on Artificial Intelligence"),
        "IJCAI",
    ),
    (
        re.compile(
            r"(?i)International Conference on Artificial Intelligence and Statistics"
        ),
        "AISTATS",
    ),
    (re.compile(r"(?i)International Conference on Learning Representations"), "ICLR"),
    (
        re.compile(r"(?i)IEEE International Conference on Robotics and Automation"),
        "ICRA",
    ),
    (
        re.compile(
            r"(?i)IEEE/RSJ International Conference on Intelligent Robots and Systems"
        ),
        "IROS",
    ),
    (re.compile(r"(?i)Journal of Machine Learning Research"), "JMLR"),
    (
        re.compile(
            r"(?i)ACM SIGKDD International Conference on Knowledge Discovery and Data Mining"
        ),
        "KDD",
    ),
    (re.compile(r"(?i)ACM Conference on Information and Knowledge Management"), "CIKM"),
    (re.compile(r"(?i)ACM Symposium on Theory of Computing"), "STOC"),
    (
        re.compile(r"(?i)ACM SIGCHI Conference on Human Factors in Computing Systems"),
        "CHI",
    ),
    (re.compile(r"(?i)ACM SIGGRAPH"), "SIGGRAPH"),
    (re.compile(r"(?i)ACM Transactions on Graphics"), "TOG"),
    (re.compile(r"(?i)ACM SIGCOMM"), "SIGCOMM"),
    (
        re.compile(r"(?i)ACM SIGMOD International Conference on Management of Data"),
        "SIGMOD",
    ),
    (re.compile(r"(?i)ACM International Conference on Supercomputing"), "ICS"),
    (re.compile(r"(?i)ACM/IEEE Design Automation Conference"), "DAC"),
    (
        re.compile(r"(?i)ACM/IEEE International Symposium on Computer Architecture"),
        "ISCA",
    ),
    (
        re.compile(
            r"(?i)IEEE/CVF Winter Conference on Applications of Computer Vision"
        ),
        "WACV",
    ),
    (
        re.compile(
            r"(?i)IEEE Transactions on Pattern Analysis and Machine Intelligence"
        ),
        "TPAMI",
    ),
    (
        re.compile(r"(?i)IEEE Transactions on Neural Networks and Learning Systems"),
        "TNNLS",
    ),
    (re.compile(r"(?i)IEEE Transactions on Image Processing"), "TIP"),
    (re.compile(r"(?i)IEEE Transactions on Robotics"), "T-RO"),
    (re.compile(r"(?i)IEEE Transactions on Cybernetics"), "TCYB"),
    (re.compile(r"(?i)Pattern Recognition Letters"), "PRL"),
    (re.compile(r"(?i)Journal of Artificial Intelligence Research"), "JAIR"),
    (re.compile(r"(?i)Journal of Computer Vision and Image Understanding"), "CVIU"),
    (re.compile(r"(?i)Knowledge-Based Systems"), "KBS"),
    (re.compile(r"(?i)ACM Transactions on Intelligent Systems and Technology"), "TIST"),
    (re.compile(r"(?i)Artificial Intelligence Journal"), "AIJ"),
    (
        re.compile(
            r"(?i)ACM SIGSOFT International Symposium on Software Testing and Analysis"
        ),
        "ISSTA",
    ),
    (
        re.compile(r"(?i)International Symposium on Software Reliability Engineering"),
        "ISSRE",
    ),
    (re.compile(r"(?i)USENIX Annual Technical Conference"), "USENIX ATC"),
    (
        re.compile(
            r"(?i)USENIX Symposium on Networked Systems Design and Implementation"
        ),
        "NSDI",
    ),
    (re.compile(r"(?i)USENIX Security Symposium"), "USENIX Security"),
    (re.compile(r"(?i)International Symposium on Computer Architecture"), "ISCA"),
    (re.compile(r"(?i)IEEE Symposium on Security and Privacy"), "S&P"),
    (re.compile(r"(?i)ACM Conference on Embedded Networked Sensor Systems"), "SenSys"),
    (re.compile(r"(?i)ACM International Conference on Multimedia"), "ACM MM"),
    (re.compile(r"(?i)ACM International Systems and Storage Conference"), "SYSTOR"),
    (re.compile(r"(?i)IEEE Transactions on Mobile Computing"), "TMC"),
    (re.compile(r"(?i)IEEE Transactions on Knowledge and Data Engineering"), "TKDE"),
    (re.compile(r"(?i)ACM Transactions on Information Systems"), "TOIS"),
    (re.compile(r"(?i)International Conference on Very Large Data Bases"), "VLDB"),
    (
        re.compile(
            r"(?i)ACM SIGSOFT Symposium on the Foundations of Software Engineering"
        ),
        "FSE",
    ),
    (re.compile(r"(?i)International Conference on Functional Programming"), "ICFP"),
    (
        re.compile(
            r"(?i)ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages"
        ),
        "POPL",
    ),
    (
        re.compile(
            r"(?i)ACM SIGPLAN Conference on Programming Language Design and Implementation"
        ),
        "PLDI",
    ),
]


def venue_name_get(s: str) -> str:
    """
    Gets the venue abbreviation.

    Example:
        >>> venue_name_get("International Conference on Machine Learning")
        'ICML'
    """
    for pattern, abbreviation in VENUE_NAME_MAPPING:
        if pattern.search(s):
            return abbreviation
    return camel_case(s)


def determine_source(data: Dict) -> str:
    """
    Determines the data source based on the keys present.

    Example:
        >>> determine_source({'feed': {}})
        'arxiv'
    """
    if "feed" in data:
        return "arxiv"
    if "abstract" in data:
        return "semantic-scholar"
    raise ValueError("Unknown data source")


def extract_entry(data: Dict, *, source: str) -> Dict:
    """
    Extracts the entry from the data based on the source.

    Example:
        >>> extract_entry({'feed': {'entry': {}}}, source='arxiv')
        {}
    """
    if source == "arxiv":
        return data["feed"]["entry"]
    elif source == "semantic-scholar":
        return data
    else:
        raise ValueError(f"Unknown source: {source}")


def get_title(entry: Dict, *, source: str) -> str:
    """
    Gets the title from the entry.

    Example:
        >>> get_title({'title': 'Test Title'}, source='arxiv')
        'Test Title'
    """
    raw_title = entry.get("title", "").strip()
    return newline2space(raw_title)


def get_abstract(entry: Dict, *, source: str) -> str:
    """
    Gets the abstract from the entry.

    Example:
        >>> get_abstract({'summary': 'Test Abstract'}, source='arxiv')
        'Test Abstract'
    """
    key = "summary" if source == "arxiv" else "abstract"
    raw_abstract = entry.get(key, "").strip()
    return newline2space(raw_abstract)


def get_authors(entry: Dict, *, source: str) -> List[str]:
    """
    Gets the list of authors from the entry.

    Example:
        >>> get_authors({'author': [{'name': 'Author One'}]}, source='arxiv')
        ['Author One']
    """
    if source == "arxiv":
        authors = entry.get("author", [])
        if isinstance(authors, dict):
            authors = [authors]
        return [author.get("name", "") for author in authors]
    elif source == "semantic-scholar":
        return [author.get("name", "") for author in entry.get("authors", [])]
    else:
        return []


def get_date(entry: Dict, *, source: str) -> Optional[str]:
    """
    Gets the publication date from the entry.

    Example:
        >>> get_date({'published': '2021-01-01'}, source='arxiv')
        '2021-01-01'
    """
    key = "published" if source == "arxiv" else "publicationDate"
    return entry.get(key)


def get_url(entry: Dict, *, source: str) -> Optional[str]:
    """
    Gets the URL from the entry.

    Example:
        >>> get_url({'id': 'http://arxiv.org/abs/1234.5678'}, source='arxiv')
        'http://arxiv.org/abs/1234.5678'
    """
    if source == "arxiv":
        links = entry.get("link", [])
        if isinstance(links, dict):
            links = [links]
        pdf_link = next(
            (link.get("@href") for link in links if link.get("title") == "pdf"), None
        )
        return pdf_link or entry.get("id")
    elif source == "semantic-scholar":
        return entry.get("url") or (
            entry.get("links", [{}])[0].get("url") if entry.get("links") else None
        )
    else:
        return None


def get_venue(entry: Dict, *, source: str) -> Optional[str]:
    """
    Gets the venue from the entry.

    Example:
        >>> get_venue({}, source='arxiv')
        'arXiv'
    """
    if source == "arxiv":
        return "arXiv"
    elif source == "semantic-scholar":
        return entry.get("venue") or entry.get("journal_name")
    else:
        return None


def get_citation_count(entry: Dict, *, source: str) -> Optional[int]:
    """
    Gets the citation count from the entry.

    Example:
        >>> get_citation_count({'citationCount': 42}, source='semantic-scholar')
        42
    """
    if source == "semantic-scholar":
        return entry.get("citationCount")
    else:
        return None


MONTHS: Dict[str, str] = {
    "01": "January",
    "02": "February",
    "03": "March",
    "04": "April",
    "05": "May",
    "06": "June",
    "07": "July",
    "08": "August",
    "09": "September",
    "10": "October",
    "11": "November",
    "12": "December",
}


def extract_year(date_str: str) -> Optional[str]:
    """
    Extracts the year from a date string.

    Example:
        >>> extract_year('2021-01-01')
        '2021'
    """
    match = re.search(r"(\d{4})", date_str)
    return match.group(1) if match else None


def extract_month(date_str: str) -> Optional[str]:
    """
    Extracts the month from a date string.

    Example:
        >>> extract_month('2021-01-01')
        'January'
    """
    match = re.search(r"\d{4}-(\d{2})", date_str)
    if match:
        return MONTHS.get(match.group(1))
    match = re.search(r"([a-zA-Z]+)", date_str)
    if match:
        return match.group(1)
    return None


def get_date_tag(date_str: Optional[str]) -> Optional[str]:
    """
    Builds the date tag from a date string.

    Example:
        >>> get_date_tag('2021-01-01')
        '@2021/January'
    """
    if date_str:
        year = extract_year(date_str)
        if year:
            month = extract_month(date_str)
            return f"@{year}/{month}" if month else f"@{year}"
    return None


def build_properties(entry: Dict, *, source: str) -> Dict[str, str]:
    """
    Builds a dictionary of properties from the entry.

    Example:
        >>> build_properties({'title': 'Test'}, source='arxiv')
        {'title': 'Test', 'url': '', 'arxiv': 'yes'}
    """
    properties: Dict[str, str] = {
        "title": get_title(entry, source=source),
        "url": get_url(entry, source=source) or "",
    }
    authors = get_authors(entry, source=source)
    if authors:
        properties["authors"] = ", ".join(authors)
    venue = get_venue(entry, source=source)
    if venue:
        properties["venue"] = venue
    date = get_date(entry, source=source)
    if date:
        properties["date"] = date
    citation_count = get_citation_count(entry, source=source)
    if citation_count is not None:
        properties["citations"] = str(citation_count)
    if source == "arxiv":
        properties["arxiv"] = "yes"
    return properties


def print_org_output(entry: Dict, *, source: str) -> None:
    """
    Prints the Org-mode formatted output.

    Example:
        >>> print_org_output({'title': 'Test'}, source='arxiv')
        (prints formatted output)
    """
    properties = build_properties(entry, source=source)
    date_tag = get_date_tag(properties.get("date"))
    title = properties.get("title", "")
    url = properties.get("url", "")
    abstract = get_abstract(entry, source=source)
    venue = properties.get("venue", "")
    venue_abbr = venue_name_get(venue) if venue else ""
    citation_count = properties.get("citations")

    tags = [
        tag
        for tag in [
            date_tag,
            f"@citations/{citation_count}" if citation_count else None,
            f"@{venue_abbr}" if venue_abbr else None,
        ]
        if tag
    ]
    tags_str = " ".join(tags)

    print(f"{tags_str} [[{org_link_escape(url)}][{org_title_escape(title)}]]")
    print(":PROPERTIES:")
    for key, value in properties.items():
        print(f":{key}: {value}")
    print(":END:")
    print("* @abstract")
    print(":PROPERTIES:")
    print(":visibility: folded")
    print(":END:")
    print("#+BEGIN_QUOTE")
    print(abstract)
    print("#+END_QUOTE")


def main() -> None:
    """
    Main execution function.
    """
    data = json.load(sys.stdin)
    source = determine_source(data)
    entry = extract_entry(data, source=source)
    print_org_output(entry, source=source)


if __name__ == "__main__":
    main()
