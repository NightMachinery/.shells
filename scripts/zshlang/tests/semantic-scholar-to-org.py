#!/usr/bin/env python3
"""Offline tests for scraped Semantic Scholar metadata and keyless Org output."""

import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
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
PAGE_DATA = {"paper": {
    "id": "a" * 40, "title": {"text": "Synthetic paper"},
    "paperAbstract": {"text": "The complete main paper abstract."},
    "paperAbstractTruncated": "The complete main...",
    "authors": [[{"name": "Example Author"}, {"text": "Example Author"}],
                [{"name": "Other Author"}, {"text": "Other Author"}]],
    "year": {"text": "2025"}, "pubDate": "2025-05-01", "corpusId": 12345,
    "citationStats": {"numCitations": 0, "numReferences": 48, "numKeyCitations": 0},
    "fieldsOfStudy": ["Computer Science"],
    "links": [{"url": "https://example.org/paper.pdf", "linkType": "arxiv"}],
    "primaryPaperLink": {"url": "https://example.org/paper.pdf", "linkType": "arxiv"},
}}


class ParserTests(unittest.TestCase):
    def test_page_data_uses_full_abstract_and_nested_authors(self):
        data = module.parse_page_data(PAGE_DATA, url=URL)
        self.assertEqual(data["authors_names"], ["Example Author", "Other Author"])
        self.assertEqual(data["abstract"], ["The complete main paper abstract."])
        self.assertEqual(data["publicationDate"], ["2025-05-01"])
        self.assertEqual(data["year"], ["2025"])
        self.assertEqual(data["citationCount"], ["0"])
        self.assertEqual(data["referenceCount"], ["48"])
        self.assertEqual(data["links"], ["https://example.org/paper.pdf"])
        self.assertEqual(data["pdf_urls"], data["links"])

    def test_page_data_must_match_requested_paper(self):
        with self.assertRaisesRegex(ValueError, "does not match"):
            module.parse_page_data(PAGE_DATA, url=URL.replace("a" * 40, "b" * 40))

    def test_preview_or_tldr_cannot_replace_full_abstract(self):
        for field in ("authors", "paperAbstract"):
            payload = json.loads(json.dumps(PAGE_DATA))
            payload["paper"].pop(field)
            with self.subTest(field=field), self.assertRaisesRegex(ValueError, "missing"):
                module.parse_page_data(payload, url=URL)
        result = subprocess.run([sys.executable, str(PARSER), "--fetch-json", URL],
                                input=json.dumps({"title": "Synthetic paper | Semantic Scholar", "content": MARKDOWN}),
                                text=True, capture_output=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")
        self.assertIn("missing abstract", result.stderr)

    def test_title_and_related_paper_text_are_not_metadata(self):
        page = {"title": "Synthetic paper | Semantic Scholar",
                "content": "## 48 References\n### Related paper\nUnrelated abstract"}
        with self.assertRaisesRegex(ValueError, "main paper metadata"):
            module.parse_fetch_page(page, url=URL)

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
                    "abstract": "LD abstract", "author": {"name": "Example Author"},
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

    def test_social_description_is_not_a_full_abstract(self):
        html = '<meta name="citation_title" content="Synthetic paper">'
        html += '<meta name="citation_author" content="Example Author">'
        html += '<meta name="description" content="A truncated abstract...">'
        data = module.parse_paper(html, url=URL)
        self.assertNotIn("abstract", data)
        with self.assertRaisesRegex(ValueError, "missing abstract"):
            module.validate_metadata(data)

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
                  api_p="", url=URL, deepseek="", scraper="auto", fetch_fail=False, partial=False,
                  page_data_fail=False, markdown=MARKDOWN):
        with tempfile.TemporaryDirectory() as directory:
            log = Path(directory) / "calls"
            env = dict(os.environ, TEST_SS_KEY=key, TEST_SS_API_P=api_p, TEST_SS_SCRAPER=scraper,
                       TEST_SS_API=api, TEST_SS_HTML=html, TEST_SS_URL=url, NIGHTDIR=str(ROOT),
                       TEST_SS_DEEPSEEK_KEY=deepseek, TEST_SS_MARKDOWN=markdown,
                       TEST_SS_PAGE_DATA=json.dumps(PAGE_DATA),
                       TEST_SS_PAGE_DATA_FAIL="yes" if page_data_fail else "",
                       TEST_SS_CANONICAL=URL, TEST_SS_LOG=str(log),
                       TEST_SS_FETCH_FAIL="yes" if fetch_fail else "", TEST_SS_PARTIAL="yes" if partial else "")
            script = r'''
SemanticScholar_API_key="${TEST_SS_KEY}"
semantic_scholar_api_p="${TEST_SS_API_P}"
semantic_scholar_scraper="${TEST_SS_SCRAPER}"
deepseek_api_key="${TEST_SS_DEEPSEEK_KEY}"
function h-semantic-scholar-page-url {
    print -r -- "resolve:$1" >> "${TEST_SS_LOG}"
    print -r -- "${TEST_SS_CANONICAL}"
}
function full-html2 {
    print -r -- html >> "${TEST_SS_LOG}"
    [[ "$1" == "${TEST_SS_CANONICAL}" ]] || return 97
    print -r -- "${TEST_SS_HTML}"
}
function semantic-scholar-to-json-api {
    print -r -- api >> "${TEST_SS_LOG}"
    case "${TEST_SS_API}" in
        unused) print -ru2 -- 'Unexpected API call'; return 98 ;;
        fail) return 22 ;;
        invalid) print -r -- '{"error":"rate limited"}' ;;
        partial) print -r -- '{"title":"Partial API title"}' ;;
        ok) command jq --null-input --arg url "${TEST_SS_CANONICAL}" '{title:"API title",url:$url,citationCount:0,abstract:"API abstract",authors:[{name:"Example Author"}]}' ;;
    esac
}
function ecgray { print -r -- gray >> "${TEST_SS_LOG}"; print -ru2 -- "$@" }
function pbcopy { print -ru2 -- 'Unexpected clipboard write'; return 99 }
function h-semantic-scholar-scrape-page-data {
    print -r -- page-data >> "${TEST_SS_LOG}"
    [[ "$1" == "${TEST_SS_CANONICAL}" ]] || return 95
    [[ -z "${TEST_SS_PAGE_DATA_FAIL}" ]] || return 22
    print -r -- "${TEST_SS_PAGE_DATA}" | command python3 "${NIGHTDIR}/python/orgmode/semantic_scholar_html_to_json.py" --page-data "$1"
}
function deepseek-fetch-json {
    print -r -- "deepseek:$1" >> "${TEST_SS_LOG}"
    [[ "$1" == "${TEST_SS_CANONICAL}" ]] || return 96
    [[ -z "${TEST_SS_FETCH_FAIL}" ]] || return 22
    if [[ -n "${TEST_SS_PARTIAL}" ]] ; then
        command jq --null-input --arg url "$1" '{url:$url,title:"Partial paper | Semantic Scholar",content:"## 48 References\n\n### Unrelated paper\nUnrelated abstract"}'
    else
        command jq --null-input --arg url "$1" --arg content "${TEST_SS_MARKDOWN}" '{url:$url,title:"Synthetic paper | Semantic Scholar",content:$content}'
    fi
}
"$1" "${TEST_SS_URL}"
'''
            result = subprocess.run(["zsh", "-c", script, "_", command], env=env,
                                    text=True, capture_output=True, timeout=45)
            result.calls = log.read_text().splitlines() if log.exists() else []
            return result

    def test_no_keys_uses_public_page_data_without_api(self):
        result = self.run_shell()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(result.stdout)["abstract"], ["The complete main paper abstract."])
        self.assertEqual(result.calls, ["resolve:" + URL, "page-data"])

    def test_api_success_does_not_scrape(self):
        result = self.run_shell(key="test-key", api="ok")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(result.stdout)["citationCount"], ["0"])
        self.assertEqual(result.calls, ["api"])

    def test_api_failures_go_directly_to_deepseek_with_gray_notice(self):
        for api in ("fail", "invalid", "partial"):
            with self.subTest(api=api):
                result = self.run_shell(key="test-key", api=api, deepseek="test-key")
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.calls, ["api", "gray", "resolve:" + URL, "deepseek:" + URL, "gray", "page-data"])
                self.assertIn("API failed", result.stderr)
                self.assertEqual(json.loads(result.stdout)["authors_names"], ["Example Author", "Other Author"])

    def test_arxiv_input_resolves_before_fetching_semantic_scholar(self):
        source = "https://arxiv.org/abs/2610.22300"
        result = self.run_shell(url=source, deepseek="test-key", command="semantic-scholar-to-org")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.calls, ["resolve:" + source, "deepseek:" + URL, "gray", "page-data"])
        self.assertIn("[[" + URL + "][", result.stdout)
        self.assertNotIn("/paper/arXiv:", result.stdout)
        self.assertIn("* @abstract", result.stdout)
        self.assertIn("The complete main paper abstract.", result.stdout)
        self.assertIn("Example Author", result.stdout)

    def test_explicit_html_scraper_ignores_deepseek_key(self):
        result = self.run_shell(deepseek="test-key", scraper="full-html2")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.calls, ["resolve:" + URL, "html"])

    def test_explicit_deepseek_recovers_page_data_without_html(self):
        result = self.run_shell(deepseek="test-key", scraper="deepseek", fetch_fail=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("page-data", result.calls)
        self.assertNotIn("html", result.calls)

    def test_complete_deepseek_metadata_does_not_need_recovery(self):
        markdown = MARKDOWN.replace(r" year\=\{2025\}\,",
                                    r" abstract\=\{Full abstract from BibTeX\}\," + "\n" + r" year\=\{2025\}\,")
        result = self.run_shell(deepseek="test-key", markdown=markdown)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.calls, ["resolve:" + URL, "deepseek:" + URL])
        self.assertEqual(json.loads(result.stdout)["abstract"], ["Full abstract from BibTeX"])

    def test_auto_tries_html_after_deepseek_and_page_data_failures(self):
        result = self.run_shell(deepseek="test-key", fetch_fail=True, page_data_fail=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.calls, ["resolve:" + URL, "deepseek:" + URL, "gray", "page-data", "gray", "html"])

    def test_explicit_page_data_ignores_deepseek_key(self):
        result = self.run_shell(deepseek="test-key", scraper="page-data")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.calls, ["resolve:" + URL, "page-data"])

    def test_invalid_scraper_fails_before_network(self):
        result = self.run_shell(key="test-key", scraper="typo")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.calls, [])

    def test_explicit_api_disable(self):
        result = self.run_shell(key="test-key", api_p="n", deepseek="test-key")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotIn("api", result.calls)

    def test_partial_fetch_never_uses_related_paper_description(self):
        result = self.run_shell(deepseek="test-key", partial=True, command="semantic-scholar-to-org")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("Synthetic paper", result.stdout)
        self.assertIn(":referenceCount: 48", result.stdout)
        self.assertIn("Example Author", result.stdout)
        self.assertIn("The complete main paper abstract.", result.stdout)
        self.assertNotIn("Unrelated abstract", result.stdout)
        self.assertIn("main paper metadata", result.stderr)

    def test_partial_fetch_and_incomplete_html_fail_without_org(self):
        result = self.run_shell(deepseek="test-key", partial=True, page_data_fail=True,
                                html='<meta name="citation_title" content="Only title">',
                                command="semantic-scholar-to-org")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")
        self.assertIn("missing authors, abstract", result.stderr)

    def test_keyless_org_output_uses_real_formatter(self):
        result = self.run_shell(command="semantic-scholar-to-org", scraper="full-html2")
        self.assertEqual(result.returncode, 0, result.stderr)
        for text in ("@citations/1234", "@2025/May", "Full abstract & $x$.", "Example, Jörg",
                     ":pdf_urls: https://example.org/paper.pdf", "* @abstract"):
            self.assertIn(text, result.stdout)

    def test_challenge_fails_without_org(self):
        result = self.run_shell(command="semantic-scholar-to-org", html="<title>Human Verification</title>", page_data_fail=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")
        self.assertIn("human verification", result.stderr)


class ResolverTests(unittest.TestCase):
    def resolve(self, source, destination=URL):
        with tempfile.TemporaryDirectory() as directory:
            curl = Path(directory) / "curl"
            curl.write_text('''#!/usr/bin/env python3
import os,sys
assert sys.argv[-1] == "https://api.semanticscholar.org/arXiv:2610.22300"
assert "--location" in sys.argv
assert "--head" in sys.argv
print(os.environ["TEST_DESTINATION"],end="")
''')
            curl.chmod(0o755)
            env = dict(os.environ, TEST_RESOLVER_BIN=directory, TEST_DESTINATION=destination)
            return subprocess.run(["zsh", "-c", 'path=("${TEST_RESOLVER_BIN}" "${path[@]}"); h-semantic-scholar-page-url "$1"',
                                   "_", source], env=env, text=True, capture_output=True, timeout=20)

    def test_arxiv_uses_the_real_external_id_redirect(self):
        result = self.resolve("https://arxiv.org/abs/2610.22300")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.strip(), URL)

    def test_existing_full_paper_url_needs_no_redirect(self):
        result = self.resolve(URL)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.strip(), URL)

    def test_unresolved_or_unrelated_destination_is_rejected(self):
        for destination in ("https://api.semanticscholar.org/arXiv:2610.22300", "https://example.org/paper/" + "a" * 40):
            result = self.resolve("https://arxiv.org/abs/2610.22300", destination)
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual(result.stdout, "")


if __name__ == "__main__":
    unittest.main()
