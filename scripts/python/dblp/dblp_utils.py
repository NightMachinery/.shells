#!/usr/bin/env python3
import sys
import re
import subprocess
import requests
import xml.etree.ElementTree as ET
from dataclasses import dataclass
from typing import Dict, List, Mapping, Optional, Sequence, Iterator, Tuple
from pathlib import Path
from urllib.parse import urlparse

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


class PatternGroups:
    PATTERNS: Dict[str, str] = {
        'interp_1': r"interp|explain|salien|attribution|understand|concept|visualiz",
        'interp_2': r"relevan|ground|\bwhy\b|mechanis|circuit|probe|probing|atten",
        'adversarial': r"robust|adversarial",
        'vit': r"vision transformer|vit",
        'llm': r"reason|agent|llm|language model",
    }

    GROUP_SETS: Dict[str, List[str]] = {
        'rel25': ['interp_1', 'interp_2', 'adversarial', 'vit', 'llm']
    }

    @classmethod
    def get_patterns(cls, group_set: Optional[str] = None) -> List[str]:
        if group_set is None:
            return list(cls.PATTERNS.values())
        
        if group_set not in cls.GROUP_SETS:
            raise ValueError(f"Unknown group set: {group_set}")
            
        return [cls.PATTERNS[group] for group in cls.GROUP_SETS[group_set]]

    @classmethod
    def get_group_names(cls, group_set: Optional[str] = None) -> List[str]:
        if group_set is None:
            return list(cls.PATTERNS.keys())
            
        if group_set not in cls.GROUP_SETS:
            raise ValueError(f"Unknown group set: {group_set}")
            
        return cls.GROUP_SETS[group_set]


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

def get_papers_by_group(titles: Sequence[str], pattern: str, *, case_sensitive: bool = False) -> List[str]:
    flags = 0 if case_sensitive else re.IGNORECASE
    regex = re.compile(pattern, flags)
    
    cleaned_titles = [clean_title(title) for title in titles]
    return [title for title, cleaned in zip(titles, cleaned_titles) 
            if regex.search(cleaned)]

def get_papers_by_group_set(titles: Sequence[str], group_set: str) -> Dict[str, List[str]]:
    raw_results = {}
    for group_name in PatternGroups.get_group_names(group_set):
        pattern = PatternGroups.PATTERNS[group_name]
        matches = get_papers_by_group(titles, pattern)
        if matches:
            raw_results[group_name] = matches
    
    groups = [raw_results[name] for name in PatternGroups.get_group_names(group_set) 
             if name in raw_results]
    deduped_groups = remove_duplicates_across_groups(groups, case_sensitive=False)
    
    results = {}
    for i, group_name in enumerate(raw_results.keys()):
        if i < len(deduped_groups) and deduped_groups[i]:
            results[group_name] = deduped_groups[i]
            
    return results

def save_group_results(base_path: Path, name: str, papers_by_group: Dict[str, List[str]], *, separator: str = "-----------") -> None:
    # Filter out empty groups first
    papers_by_group = filter_empty_groups(papers_by_group)
    
    # Only save files for non-empty groups
    for group_name, papers in papers_by_group.items():
        output_file = base_path / f"{group_name}.txt"
        output_file.write_text("\n".join(papers))

    # Create combined results file
    combined_papers = []
    group_names = PatternGroups.get_group_names(name)  # Use the name as group_set
    for group_name in group_names:
        if group_name in papers_by_group:
            combined_papers.extend(papers_by_group[group_name])
            combined_papers.append(separator)
    
    if combined_papers:  # Only create the combined file if there are actual results
        if combined_papers[-1] == separator:
            combined_papers.pop()
        
        output_file = base_path / f"{name}.txt"
        output_file.write_text("\n".join(combined_papers))

def clean_title(title: str) -> str:
    title = re.sub(r'<[^>]+>', '', title)
    title = ' '.join(title.split())
    title = title.rstrip('.')
    return title

def normalize_text(text: str, *, case_sensitive: bool = False) -> str:
    if not case_sensitive:
        text = text.lower()
    return clean_title(text)

def filter_empty_groups(papers_by_group: Dict[str, List[str]]) -> Dict[str, List[str]]:
    return {k: v for k, v in papers_by_group.items() if v}

def run_dblp_script(script_name: str, url: str, *, check: bool = True, **kwargs) -> str:
    cmd = [script_name, url]
    for key, value in kwargs.items():
        cmd.extend([f"--{key.replace('_', '-')}", str(value)])
        
    result = subprocess.run(
        cmd,
        capture_output=True,
        text=True,
        check=check,
    )
    return result.stdout.strip()
