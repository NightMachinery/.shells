# arXiv metadata fallback

`arxiv-to-org` gets metadata through `arxiv-json`, then formats it with
`python/orgmode/paper_to_org.py`. The Emacs command `night/arxiv-to-org` calls
this same shell pipeline through BrishGarden.

The Atom API can return HTTP 429 (`Rate exceeded.`) while the public abstract
page still returns HTTP 200. This happened for paper `2505.00661` on 2026-10-20.
Previously the API's curl exit 22 stopped conversion before any Org was produced.

`arxiv-json` still uses the Atom API first. When the request fails or returns
an empty body, it fetches `https://arxiv.org/abs/<id>` once with `gurl` and parses
its `citation_*` meta tags using Python's standard library. A diagnostic goes
to stderr, leaving stdout available for JSON or Org.

The fallback emits the `feed.entry` subset used by the existing title and Org
consumers: ID, title, summary, original publication date, authors, and PDF link.
It validates the paper ID and required fields before emitting anything, so an
HTTP-200 error/challenge page cannot become a blank paper entry. Version suffixes
are retained in the entry ID. Author names follow the citation metadata's
`surname, given name` format. HTML entities and Unicode are decoded normally.

The API remains preferred because its response has more metadata. The fallback
does not synthesize optional Atom fields such as categories or update timestamps.
It requires the citation metadata to be present; if both endpoints fail, or the
HTML lacks required fields, conversion returns a nonzero status. Malformed but
nonempty successful API responses still follow the existing XML conversion path.

After changing the shell definitions, run `brishz-restart` so existing Emacs
and other BrishGarden callers see the fix. No Elisp change is needed.

## Verification

Run the offline parser and shell integration regressions:

```zsh
python3 "$NIGHTDIR/zshlang/tests/arxiv-to-org.py"
```

The tests cover a rate-limited API, an empty API body, normal API success,
failures at both endpoints, rejected HTML, metadata validation, Unicode,
version suffixes, and final Org formatting. Fixtures use synthetic metadata.

For an end-to-end network check:

```zsh
arxiv-to-org 'https://arxiv.org/abs/2505.00661'
brishz arxiv-to-org 'https://arxiv.org/abs/2505.00661'
```
