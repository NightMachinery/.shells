# Semantic Scholar paper notes

`semantic-scholar-to-org` (alias `ssorg`) produces an Org paper heading and
properties, plus a folded abstract or summary when available. Pass a public
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
Successful API metadata is formatted directly without scraping. API transport
failures and error payloads are reported with `ecgray`, then passed to the
selected scraper. Without a Semantic Scholar key, the API is skipped.
`semantic_scholar_api_p=y` or `n` explicitly enables or disables that attempt.

`semantic_scholar_scraper` accepts these values:

- `auto` (default): use DeepSeek Fetch first when `deepseek_api_key` is set;
  try `full-html2` only if DeepSeek fails. Without a DeepSeek key, use `full-html2`.
- `deepseek`: use DeepSeek Fetch only, returning its failure without starting
  a browser.
- `full-html2`: use the existing HTML loader only; `fhMode` still controls its
  backend, with `curlfull_s5` as this scraper's default.

```zsh
semantic_scholar_scraper=deepseek semantic-scholar-to-org '<paper-url>'
semantic_scholar_scraper=full-html2 semantic-scholar-to-org '<paper-url>'
semantic_scholar_api_p=n semantic-scholar-to-org '<paper-url>'
```

An invalid enum value fails before network access. These are shell variables;
prefix assignments can select behavior for one call.

## Available metadata

HTML extraction reads citation tags, article JSON-LD, and visible paper fields.
DeepSeek extraction reads structured Fetch JSON, including the page title,
then parses the main paper's BibTeX, counts, and leading summary from Markdown.
A TLDR is labeled `@summary`, rather than presented as a complete abstract.

Some Semantic Scholar pages omit their main paper section from fetched Markdown.
A recognizable Semantic Scholar page title and available top-level counts are
retained with a gray partial-metadata notice. Missing authors, dates, abstracts,
and citation counts are omitted, as are empty abstract sections. Descriptions
of referenced papers are never used as the current paper's abstract or summary.
Verification pages and mismatched bibliography titles are rejected.

The global Puppeteer stack and related plugins listed in `setup/node.g` were
updated to their latest releases. Browser verification can still occur with
current dependencies, so the default prioritizes DeepSeek when available.
Run `brishz-restart` after shell changes to refresh the persistent shells used
by Emacs; no Elisp reload is needed for this implementation.

```zsh
python3 "$NIGHTDIR/zshlang/tests/semantic-scholar-to-org.py"
python3 "$NIGHTDIR/zshlang/tests/deepseek-fetch.py"
```

The offline tests use synthetic metadata and check redirect resolution,
API-first routing, gray failure notices, each scraper selection, partial pages,
and output from the real Org formatter. Live checks also cover an arXiv input
resolved to its real Semantic Scholar page without LLM or search assistance.

Verified by @OpenAI on 2026-10-20.
