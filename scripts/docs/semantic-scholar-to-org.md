# Semantic Scholar paper notes

`semantic-scholar-to-org` (alias `ssorg`) produces an Org paper heading and
properties, authors, and a folded full abstract. Pass a public
Semantic Scholar URL or an arXiv URL; arguments, piped URLs, and interactive
clipboard input are supported. Emacs's `night/semantic-scholar-to-org` uses the
same shell pipeline.

```zsh
semantic-scholar-to-org 'https://arxiv.org/abs/<arxiv-id>'
semantic-scholar-to-org 'https://www.semanticscholar.org/paper/<title-slug>/<paper-id>'
```

## URL identification

An arXiv URL identifies the paper; it is not fetched as a substitute source.
The function normalizes it to Semantic Scholar's external-ID endpoint, follows
Semantic Scholar's HEAD redirect, and validates that the destination is a
public `/paper/<40-character-id>` URL (with an optional title slug).
Existing public paper URLs need no resolution request. Unresolved IDs and
redirects to unrelated hosts fail clearly.

URL identification uses ordinary HTTP redirects. It uses no LLM, title guessing,
search service, or invented `/paper/arXiv:...` URL. The generated Org link points
to the resolved Semantic Scholar page.

## API and scraper selection

When `SemanticScholar_API_key` is configured, the Graph API is tried first.
Complete API metadata is formatted directly without scraping. API transport
failures, error payloads, and missing required fields are reported with `ecgray`, then passed to the
selected scraper. Without a Semantic Scholar key, the API is skipped.
`semantic_scholar_api_p=y` or `n` explicitly enables or disables that attempt.

`semantic_scholar_scraper` accepts these values:

- `auto` (default): use DeepSeek Fetch first when `deepseek_api_key` is set;
  recover incomplete results from public page data, then try `full-html2` if
  necessary. Without a DeepSeek key, start with page data.
- `deepseek`: use DeepSeek Fetch, then public page data if it fails or omits
  required metadata. No browser is started.
- `page-data`: use public page data directly. Neither API key is required.
- `full-html2`: use the existing HTML loader, then public page data if needed;
  `fhMode` still controls the HTML backend, defaulting to `curlfull_s5`.

```zsh
semantic_scholar_scraper=deepseek semantic-scholar-to-org '<paper-url>'
semantic_scholar_scraper=page-data semantic-scholar-to-org '<paper-url>'
semantic_scholar_scraper=full-html2 semantic-scholar-to-org '<paper-url>'
semantic_scholar_api_p=n semantic-scholar-to-org '<paper-url>'
```

An invalid enum value fails before network access. These are shell variables;
prefix assignments can select behavior for one call.

## Available metadata

Every successful conversion requires a title, authors, and a full abstract.
Incomplete results trigger the next backend; exhausting the selected backends
returns an error with no Org output. Dates, citation counts, venue, and links
are retained when supplied by the source, including a citation count of zero.

Public page data comes from Semantic Scholar's own website endpoint,
`https://www.semanticscholar.org/api/1/paper/<paper-id>`. Its paper ID must match
the resolved URL. The parser reads the nested authors, full `paperAbstract`,
publication date, counts, and PDF links. `paperAbstractTruncated` is a separate
preview string and is never used as the full abstract. This website endpoint
is separate from the documented Graph API and may change with the website.

HTML extraction reads citation tags, article JSON-LD, and visible paper fields.
DeepSeek extraction reads structured Fetch JSON, including the page title,
then parses the main paper's BibTeX, counts, and leading summary from Markdown.
A TLDR or social description does not satisfy the full-abstract requirement.

Some Semantic Scholar pages omit their main paper section from fetched Markdown.
Those partial pages are rejected and recovered through page data rather than
emitted as nearly empty notes. Descriptions of referenced papers are never
used as the current paper's abstract or summary.
Verification pages and mismatched bibliography titles are rejected.

The global Puppeteer stack and related plugins listed in `setup/node.g` were
updated to their latest releases. Browser verification can still occur with
current dependencies.
Run `brishz-restart` after shell changes to refresh the persistent shells used
by Emacs; no Elisp reload is needed for this implementation.

```zsh
python3 "$NIGHTDIR/zshlang/tests/semantic-scholar-to-org.py"
python3 "$NIGHTDIR/zshlang/tests/deepseek-fetch.py"
```

The offline tests use synthetic metadata and check redirect resolution,
API-first routing, gray failure notices, each scraper selection, incomplete
metadata rejection, full-abstract recovery, and output from the real Org
formatter. Live checks verify both authors, the complete abstract, publication
date, and PDF link for the reported arXiv input, resolved to Semantic Scholar
without LLM or search assistance.

Verified by @OpenAI on 2026-10-20.
