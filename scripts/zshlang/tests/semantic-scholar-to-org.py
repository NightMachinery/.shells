#!/usr/bin/env python3
"""Offline tests for scraped Semantic Scholar metadata and keyless Org output."""

import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import unittest

ROOT = Path(__file__).resolve().parents[2]
PARSER = ROOT / "python/orgmode/semantic_scholar_html_to_json.py"
spec = importlib.util.spec_from_file_location("ss_html", PARSER)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
URL = "https://www.semanticscholar.org/paper/" + "a" * 40
HTML = f'''<html><head><link rel="canonical" href="{URL}">
<meta name="citation_title" content="Synthetic [paper] &amp; test">
<meta name="citation_author" content="Example, Jörg">
<meta property="citation_author" content="Author Two">
<meta name="citation_publication_date" content="2025/05/01">
<meta name="citation_journal_title" content="Test Journal">
<meta name="citation_pdf_url" content="https://example.org/paper.pdf">
<meta name="citation_doi" content="10.0000/example">
<meta name="description" content="A truncated description...">
</head><body><div data-selenium-selector="abstract-text">Full <b>abstract</b> &amp; $x$.</div>
<a data-heap-nav="citing-papers">1,234 Citations</a>
<a data-heap-nav="references">0 References</a>
<span data-selenium-selector="corpus-id">Corpus ID: 12345</span>
<a data-selenium-selector="paper-link" href="https://example.org/project">Project</a>
<span data-selenium-selector="entity-name">Computer Science</span>
<pre class="bibtex-citation">@article{{example, title={{Synthetic paper}}}}</pre>
</body></html>'''
MARKDOWN = r'''\# Synthetic paper
\@inproceedings\{Example2025\,
 title\=\{Synthetic paper\}\,
 author\=\{Example Author and Other Author\}\,
 booktitle\=\{Test Venue\}\,
 year\=\{2025\}\,
 url\=\{https\:\/\/api\.semanticscholar\.org\/CorpusID\:12345\}
\}
A short TLDR\.

1\,234 Citations
\#\# Figures and Tables
\#\# 1\,234 Citations
\#\#\# A related paper
Unrelated text.
\#\# 41 References
'''


class ParserTests(unittest.TestCase):
    def test_deepseek_markdown_has_summary_and_main_paper_metadata(self):
        data = module.parse_markdown(MARKDOWN, url=URL)
        self.assertEqual(data["title"], ["Synthetic paper"])
        self.assertEqual(data["authors_names"], ["Example Author", "Other Author"])
        self.assertEqual(data["citationCount"], ["1234"])
        self.assertEqual(data["referenceCount"], ["41"])
        self.assertEqual(data["corpusID"], ["12345"])
        self.assertEqual(data["tldr"], ["A short TLDR."])
        self.assertNotIn("abstract", data)

    def test_markdown_challenge_and_truncated_bibtex_fail(self):
        for text in ("JavaScript is disabled", "@article{example, title={oops}"):
            with self.subTest(text=text), self.assertRaises(ValueError):
                module.parse_markdown(text, url=URL)

    def test_citation_and_visible_metadata(self):
        data = module.parse_paper(HTML, url=URL)
        self.assertEqual(data["title"], ["Synthetic [paper] & test"])
        self.assertEqual(data["abstract"], ["Full abstract & $x$."])
        self.assertEqual(data["publicationDate"], ["2025-05-01"])
        self.assertEqual(data["citationCount"], ["1234"])
        self.assertEqual(data["referenceCount"], ["0"])
        self.assertEqual(data["authors_names"], ["Example, Jörg", "Author Two"])
        self.assertEqual(data["pdf_urls"], ["https://example.org/paper.pdf"])
        self.assertEqual(data["corpusID"], ["12345"])
        self.assertEqual(data["paperId"], ["a" * 40])
        self.assertEqual(data["fieldsOfStudy"], ["Computer Science"])

    def test_jsonld_graph(self):
        article = {"@graph": [{"@type": "Organization", "name": "Ignore"},
                   {"@type": "ScholarlyArticle", "headline": "LD paper",
                    "description": "LD abstract", "author": {"name": "Example Author"},
                    "datePublished": "2025-05-01T12:00:00Z", "isPartOf": {"name": "LD Venue"}}]}
        html = '<script type="application/ld+json">' + json.dumps(article) + '</script>'
        data = module.parse_paper(html, url=URL)
        self.assertEqual(data["title"], ["LD paper"])
        self.assertEqual(data["abstract"], ["LD abstract"])
        self.assertEqual(data["authors_names"], ["Example Author"])
        self.assertEqual(data["venue"], ["LD Venue"])
        self.assertEqual(data["publicationDate"], ["2025-05-01"])

    def test_visible_title_and_year_only(self):
        html = '<h1 data-selenium-selector="paper-detail-title">Visible paper</h1>'
        html += '<span data-selenium-selector="paper-year">Published 2024</span>'
        data = module.parse_paper(html, url=URL)
        self.assertEqual(data["year"], ["2024"])
        self.assertNotIn("publicationDate", data)
        self.assertNotIn("citationCount", data)

    def test_citation_abstract_beats_truncated_description(self):
        data = module.parse_paper(HTML + '<meta name="citation_abstract" content="Entire abstract">', url=URL)
        self.assertEqual(data["abstract"], ["Entire abstract"])

    def test_abbreviated_count_is_not_invented(self):
        data = module.parse_paper(HTML.replace("1,234 Citations", "1.2K Citations"), url=URL)
        self.assertNotIn("citationCount", data)

    def test_malformed_jsonld_does_not_hide_meta_tags(self):
        data = module.parse_paper('<script type="application/ld+json">broken</script>' + HTML, url=URL)
        self.assertEqual(data["title"], ["Synthetic [paper] & test"])

    def test_challenge_and_empty_page_rejected_without_stdout(self):
        for html in ("", "<title>Human Verification</title><h1>Confirm you are human</h1>"):
            result = subprocess.run([sys.executable, str(PARSER), URL], input=html,
                                    text=True, capture_output=True)
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual(result.stdout, "")
            self.assertIn("human verification", result.stderr)


class PipelineTests(unittest.TestCase):
    def run_shell(self, *, key="", api="unused", html=HTML, command="semantic-scholar-to-json",
                  api_p="", url=URL, deepseek=""):
        env = dict(os.environ, TEST_SS_KEY=key, semantic_scholar_api_p=api_p,
                   TEST_SS_API=api, TEST_SS_HTML=html, TEST_SS_URL=url, NIGHTDIR=str(ROOT),
                   TEST_SS_DEEPSEEK_KEY=deepseek, TEST_SS_MARKDOWN=MARKDOWN)
        script = r'''
SemanticScholar_API_key="${TEST_SS_KEY}"
deepseek_api_key="${TEST_SS_DEEPSEEK_KEY}"
function full-html2 {
    [[ "$1" == https://www.semanticscholar.org/paper/* ]] || return 97
    print -r -- "${TEST_SS_HTML}"
}
function semantic-scholar-to-json-api {
    case "${TEST_SS_API}" in
        unused) print -ru2 -- 'Unexpected API call'; return 98 ;;
        fail) return 22 ;;
        invalid) print -r -- '{"error":"rate limited"}' ;;
        ok) print -r -- '{"title":"API title","citationCount":0,"abstract":null}' ;;
    esac
}
function pbcopy { print -ru2 -- 'Unexpected clipboard write'; return 99 }
function deepseek-fetch { print -r -- "${TEST_SS_MARKDOWN}" }
"$1" "${TEST_SS_URL}"
'''
        return subprocess.run(["zsh", "-c", script, "_", command], env=env,
                              text=True, capture_output=True, timeout=45)

    def test_no_key_skips_api(self):
        result = self.run_shell()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(result.stdout)["abstract"], ["Full abstract & $x$."])
        self.assertNotIn("Unexpected API call", result.stderr)

    def test_deepseek_fallback_uses_real_org_formatter_and_labels_tldr(self):
        result = self.run_shell(html="<title>Human Verification</title>",
                                deepseek="synthetic-test-key", command="semantic-scholar-to-org")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("@citations/1234 @2025 @TestVenue", result.stdout)
        self.assertIn("* @summary", result.stdout)
        self.assertIn("A short TLDR.", result.stdout)
        self.assertNotIn("* @abstract", result.stdout)

    def test_explicit_scraping_with_key_skips_api(self):
        result = self.run_shell(key="synthetic-test-key", api_p="n")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotIn("Unexpected API call", result.stderr)

    def test_api_failures_preserve_scraped_data(self):
        for api in ("fail", "invalid"):
            with self.subTest(api=api):
                result = self.run_shell(key="synthetic-test-key", api=api)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(json.loads(result.stdout)["citationCount"], ["1234"])
                self.assertIn("falling back to scraped data", result.stderr)

    def test_api_enrichment_preserves_scraped_abstract(self):
        result = self.run_shell(key="synthetic-test-key", api="ok")
        self.assertEqual(result.returncode, 0, result.stderr)
        data = json.loads(result.stdout)
        self.assertEqual(data["title"], ["API title"])
        self.assertEqual(data["citationCount"], ["0"])
        self.assertEqual(data["abstract"], ["Full abstract & $x$."])

    def test_external_id_uses_public_site(self):
        result = self.run_shell(url="https://api.semanticscholar.org/arXiv:2505.00661")
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_keyless_org_output_uses_real_formatter(self):
        result = self.run_shell(command="semantic-scholar-to-org")
        self.assertEqual(result.returncode, 0, result.stderr)
        for text in ("@citations/1234", "@2025/May", "Full abstract & $x$.", "Example, Jörg",
                     ":pdf_urls: https://example.org/paper.pdf", "* @abstract"):
            self.assertIn(text, result.stdout)

    def test_missing_optional_fields_do_not_create_nil_tags(self):
        html = '<meta name="citation_title" content="Minimal paper">'
        result = self.run_shell(command="semantic-scholar-to-org", html=html)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("Minimal paper", result.stdout)
        self.assertNotIn("@citations/NIL", result.stdout)
        self.assertNotIn(" NIL ", result.stdout)

    def test_challenge_fails_without_org(self):
        result = self.run_shell(command="semantic-scholar-to-org", html="<title>Human Verification</title>")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")
        self.assertIn("human verification", result.stderr)


if __name__ == "__main__":
    unittest.main()
