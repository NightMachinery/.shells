#!/usr/bin/env python3
"""Offline regressions for the arXiv API fallback and its Org output."""

import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import unittest


ROOT = Path(__file__).resolve().parents[2]
PARSER = ROOT / "python/orgmode/arxiv_html_to_json.py"
spec = importlib.util.spec_from_file_location("arxiv_html_to_json", PARSER)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)

HTML = """<html><head>
<meta name="citation_arxiv_id" content="2505.00661">
<meta name="citation_title" content="A synthetic [paper] &amp; test">
<meta name="citation_author" content="Example, Jörg">
<meta name="citation_author" content="Test, A.">
<meta name="citation_date" content="2025/05/01">
<meta name="citation_online_date" content="2025/11/10">
<meta name="citation_pdf_url" content="https://arxiv.org/pdf/2505.00661">
<meta name="citation_abstract" content="A model&#39;s $x$ &amp; its generalization.">
</head></html>"""

XML = """<feed xmlns="http://www.w3.org/2005/Atom"><entry>
<id>https://arxiv.org/abs/2505.00661v3</id><title>API paper</title>
<summary>API abstract.</summary><published>2025-05-01T17:02:27Z</published>
<author><name>Example Author</name></author>
</entry></feed>"""


class MetadataTests(unittest.TestCase):
    def parse(self, html=HTML, arxiv_id="2505.00661"):
        return module.parse_arxiv_html(html, arxiv_id=arxiv_id)["feed"]["entry"]

    def test_metadata_entities_unicode_and_original_publication_date(self):
        entry = self.parse()
        self.assertEqual(entry["title"], "A synthetic [paper] & test")
        self.assertEqual(entry["author"][0]["name"], "Example, Jörg")
        self.assertEqual(len(entry["author"]), 2)
        self.assertEqual(entry["summary"], "A model's $x$ & its generalization.")
        self.assertEqual(entry["published"], "2025-05-01")

    def test_requested_version_is_preserved(self):
        self.assertEqual(self.parse(arxiv_id="2505.00661v1")["id"],
                         "https://arxiv.org/abs/2505.00661v1")

    def test_missing_fields_rejected(self):
        for field in ("arxiv_id", "title", "author", "date", "pdf_url", "abstract"):
            with self.subTest(field=field), self.assertRaises(ValueError):
                self.parse(HTML.replace(f'"citation_{field}"', '"other"'))

    def test_wrong_paper_rejected(self):
        with self.assertRaisesRegex(ValueError, "does not match"):
            self.parse(arxiv_id="1706.03762")

    def test_invalid_date_rejected(self):
        with self.assertRaises(ValueError):
            self.parse(HTML.replace("2025/05/01", "2025/13/01"))

    def test_error_page_cli_fails_without_stdout(self):
        result = subprocess.run([sys.executable, str(PARSER), "2505.00661"],
                                input="<html>Rate exceeded.</html>",
                                text=True, capture_output=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")
        self.assertIn("missing citation_arxiv_id", result.stderr)


class ShellTests(unittest.TestCase):
    def run_converter(self, *, api="429", html=HTML, html_failure="", command="arxiv-to-org"):
        env = dict(os.environ, NIGHTDIR=str(ROOT), TEST_ARXIV_API=api,
                   TEST_ARXIV_HTML=html, TEST_ARXIV_XML=XML,
                   TEST_ARXIV_HTML_FAILURE=html_failure)
        script = r'''
function h-arxiv-api-query {
    case "${TEST_ARXIV_API}" in
        429) return 22 ;;
        empty) return 0 ;;
        ok) print -r -- "${TEST_ARXIV_XML}" ;;
    esac
}
function gurl {
    [[ "$1" == 'https://arxiv.org/abs/2505.00661' ]] || return 91
    [[ -z "${TEST_ARXIV_HTML_FAILURE}" ]] || return 22
    print -r -- "${TEST_ARXIV_HTML}"
}
"$1" 'https://arxiv.org/abs/2505.00661'
'''
        return subprocess.run(["zsh", "-c", script, "_", command], env=env,
                              text=True, capture_output=True, timeout=30)

    def test_429_falls_back_to_complete_org(self):
        result = self.run_converter()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(result.stdout.startswith("@2025/May @arXiv [["))
        self.assertIn("A synthetic {paper} & test", result.stdout)
        self.assertIn(":authors: Example, Jörg, Test, A.", result.stdout)
        self.assertIn("https://api.semanticscholar.org/arXiv:2505.00661", result.stdout)
        self.assertIn("A model's $x$ & its generalization.", result.stdout)
        self.assertIn("* @abstract", result.stdout)
        self.assertIn("using abstract-page metadata", result.stderr)

    def test_empty_api_falls_back_to_json(self):
        result = self.run_converter(api="empty", command="arxiv-json")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(result.stdout)["feed"]["entry"]["published"], "2025-05-01")

    def test_api_success_does_not_fetch_html(self):
        result = self.run_converter(api="ok", html_failure="yes")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("API paper", result.stdout)
        self.assertNotIn("using abstract-page metadata", result.stderr)

    def test_both_endpoints_fail_without_org(self):
        result = self.run_converter(html_failure="yes")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")

    def test_unrelated_html_rejected_without_org(self):
        result = self.run_converter(html="<html>Service unavailable.</html>")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")
        self.assertIn("missing citation_arxiv_id", result.stderr)


if __name__ == "__main__":
    unittest.main()
