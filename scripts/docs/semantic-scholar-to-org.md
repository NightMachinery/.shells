# Semantic Scholar research notes without a Semantic Scholar key

`semantic-scholar-to-org` (alias `ssorg`) turns public paper metadata into an
Org heading, properties, and a folded abstract or summary:

```zsh
semantic-scholar-to-org 'https://www.semanticscholar.org/paper/<title-slug>/<paper-id>'
```

Arguments, piped URLs, and the interactive clipboard input continue to work.
The Emacs command `night/semantic-scholar-to-org` uses this same shell pipeline.

## Retrieval

The scraper first loads the public page through `full-html2`, retaining its
existing default headless-browser mode (`curlfull_s5`). It extracts citation
meta tags, article JSON-LD, and visible paper fields. These become the list-based
JSON fields expected by the Common Lisp Org formatter. Missing counts are
omitted instead of producing `@citations/NIL`; publication dates retain their
month tags.

If HTML loading or metadata validation fails and `deepseek_api_key` is set,
the scraper calls `deepseek-fetch`. This separate DeepSeek key does not require
a Semantic Scholar API key. It parses the main paper's BibTeX, available counts,
and leading summary from the returned Markdown. Summaries are labeled
`@summary`, since they are not necessarily the paper's complete abstract.
Related-paper descriptions are excluded from that summary.

Prefer the full public paper URL with its title slug and paper ID. The live
DeepSeek test retrieved that URL, while its bare-ID equivalent returned a
JavaScript verification page. Verification pages and incomplete metadata fail
with a clear error rather than generating a blank note. If neither HTML nor
DeepSeek can retrieve the page, conversion cannot proceed.

## Optional API enrichment

Without `SemanticScholar_API_key`, the Graph API is skipped. With a key, the
existing API can enrich scraped metadata. Failed requests or error payloads
fall back to scraped data. Null API values cannot erase a scraped abstract.
Force scraping even when a Semantic Scholar key is configured:

```zsh
semantic_scholar_api_p=n semantic-scholar-to-org '<public-paper-url>'
```

Scraped pages can have fewer fields than the Graph API. The DeepSeek Markdown
may include only a year and TLDR, without a full abstract or PDF URL. Those
fields are not invented.

## Browser dependencies and verification

The global Puppeteer stack and the related plugins listed in `setup/node.g`
were updated to their latest releases. A latest-version isolated test still
received Semantic Scholar's human-verification page, so dependency updates
alone do not guarantee access. DeepSeek remains a useful fallback.

After editing shell definitions, run `brishz-restart` to refresh the persistent
shells used by Emacs. No Elisp change is needed.

```zsh
python3 "$NIGHTDIR/zshlang/tests/semantic-scholar-to-org.py"
python3 "$NIGHTDIR/zshlang/tests/deepseek-fetch.py"
```

The tests use synthetic metadata and cover HTML, JSON-LD, DeepSeek Markdown,
missing optional fields, API fallback, and final output from the real Lisp
formatter. Live conversion also succeeded without a Semantic Scholar key,
using DeepSeek after the headless browser returned a verification page.

Verified by @OpenAI on 2026-10-20.
