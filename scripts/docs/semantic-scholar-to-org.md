# Semantic Scholar paper notes

`semantic-scholar-to-org` (alias `ssorg`) fetches a paper's authors, full abstract,
publication details, counts, and links, then produces an Org entry. Pass an
arXiv or Semantic Scholar paper URL; arguments, piped URLs, and interactive
clipboard input work as before. Emacs uses the same command through BrishGarden.

```zsh
semantic-scholar-to-org '<paper-url>'
semantic-scholar-to-org --no-bibtex '<paper-url>'
semantic-scholar-to-json '<paper-url>'
org-paper-metadata --format json '<paper-url>'
```

## Native Go implementation

Fetching, normalization, and rendering live in `golang/org-paper-metadata/paper`.
The shell wrappers build the `org-paper-metadata` binary on first use and when
its sources change. Go 1.26 or newer is required to build; the installed binary
needs neither Python nor Lisp for page data, Graph API, or DeepSeek retrieval.
The optional HTML backend uses the existing `full-html2` shell command, with
HTML parsing performed in Go.

The Lisp formatter and Python parser paths remain as deprecated CLI shims. They
forward to Go, rather than maintaining separate parsing or formatting code.
Direct JSON-to-Org callers can use:

```zsh
org-paper-metadata --from-json '<paper-url>' < metadata.json
```

## Default order and overrides

`semantic_scholar_scraper=auto` tries, in order:

1. Semantic Scholar's public page-data endpoint, without a key.
2. Graph API when `SemanticScholar_API_key` is configured, or when
   `semantic_scholar_api_p=y` explicitly enables it.
3. DeepSeek Fetch when `deepseek_api_key` is configured.

HTML retrieval is disabled in automatic mode. Use `full-html2` explicitly to
opt in to the browser/shell backend.

Use `semantic_scholar_api_p=n` to omit the Graph API fallback. Explicit scraper
values select `page-data` only, `api` only, `deepseek` followed by page-data
recovery, or `full-html2` followed by page-data recovery. The Go command also
accepts `--scraper` and a per-paper `--timeout`.

```zsh
semantic_scholar_scraper=page-data semantic-scholar-to-org '<paper-url>'
semantic_scholar_scraper=api semantic-scholar-to-org '<paper-url>'
org-paper-metadata --scraper page-data '<paper-url>'
```

Invalid enum values fail before network access. Shell wrappers report backend
failures through `ecgray`. Credentials travel through environment variables and
HTTP headers, never command arguments or repository files.

## URL identification

arXiv URLs are identifiers, rather than substitute metadata sources. The helper
follows Semantic Scholar's external-ID HEAD redirect and validates its final
paper ID. Existing public paper URLs need no redirect. Identification uses no
LLM, title guessing, or search, and never invents `/paper/arXiv:...` URLs.

Public page data comes from
`https://www.semanticscholar.org/api/1/paper/<paper-id>`. Its paper ID must match
the requested paper. Explicit `CANONICAL` responses are followed to the new
paper ID, with validation, cycle detection, and a bounded chain. Arbitrary
mismatched payloads are rejected. This undocumented website endpoint can change,
which is why the other backends remain available.

## Latest arXiv links and privacy

Recognized arXiv drawer URLs use `https://arxiv.org/abs/<id>`,
`https://arxiv.org/pdf/<id>`, or `https://arxiv.org/html/<id>` without a `vN`
suffix. The helper removes pinned version suffixes, normalizes official arXiv
hosts and PDF suffixes, and deduplicates generated link fields. Query strings
and fragments such as `#page=4` are preserved. Versionless identifiers refer to
the latest version, as documented by [arXiv](https://info.arxiv.org/help/arxiv_identifier.html).

The updater also normalizes existing paper drawers locally when their metadata
is still recent, without a network request or a new fetch timestamp. Heading
URLs, personal prose and saved BibTeX remain unchanged by this normalization.
Optional links missing from a refresh are retained in versionless form.

The updater reads and edits notes locally. Retrieval sends only paper
identifiers/public URLs, never note text, drawer contents or local file paths.
Automatic retrieval can send the public paper URL to configured DeepSeek Fetch.
Use `--scraper page-data` to restrict retrieval to Semantic Scholar's keyless
endpoints (page data and anonymous BibTeX export). Source/API credentials go only
to their respective services.

## Org output

Every successful conversion requires a title, authors, and a full abstract.
Incomplete data triggers the next backend. Exhausting the selected backends
returns an error without emitting a partial note. Related-paper text, TLDRs,
and social previews cannot stand in for the full abstract.

The heading retains citation, publication-date, and venue at-tags. Open access
is a drawer property, never an at-tag. The drawer retains IDs, authors, subjects,
counts, publication information, and links, and adds these when available:

- `isOpenAccess`: explicitly reported status; an absent badge is not read as false.
- `keyReferenceCount`: the source's key-reference count.
- `publicationUpdateDate`: supplied update date when different from publication date.
- `metadataFetchedAt`: UTC time of successful retrieval, used for weekly refreshes.

A supplied TLDR appears in a plain quote block before the full abstract,
inside the same folded `@abstract` heading. Comment-marked summary and abstract blocks
identify generated content for later refreshes. Zero counts are preserved;
missing values are omitted.

## Venue tags

Known venues use canonical abbreviations, including `@ECAI`, `@ACCV`, `@AIES`,
`@IJCAI/ECAI`, `@IDA`, `@IWANN`, `@ICSSE`, `@GlobalSIP`, `@IJCV`, and `@TMLR`,
alongside the existing ACL/ICLR/ICML/NeurIPS mappings. Full venue names remain
in the drawer. Unknown venues retain the full-name tag rather than an invented
acronym. Acronym inputs are recognized without regard to case.

On refresh, a former generated tag such as
`@EuropeanConferenceOnArtificialIntelligence` is replaced by `@ECAI` when the
stored venue identifies it. Personal tags survive even between generated tags;
their order relative to regenerated tags may change. Recent entries retain the
normal seven-day skip; `--force` applies the new mappings immediately.

Verified conference names: [ECAI](https://eurai.org/ecai),
[ACCV](https://accv2026.org/), [AIES](https://aaai.org/conference/aies/).

## BibTeX

When a source citation is available, Org output includes a folded `@bibtex`
sibling immediately after `@abstract`:

```org
* @bibtex
:PROPERTIES:
:visibility: folded
:orgPaperMetadataGenerated: bibtex
:END:
#+BEGIN_SRC bibtex
@inproceedings{source_key, …}
#+END_SRC
```

`--bibtex` enables it (the default); `--no-bibtex` omits it. Both flags are
available on `semantic-scholar-to-org`, `org-paper-metadata`, and the updater.
Put flags before URLs or the directory argument. The last BibTeX switch wins.
`--no-bibtex` also skips the extra citation export request. Stdin parsing
(`--from-json` or `--parse`) stays offline and uses only supplied BibTeX.

The helper never synthesizes BibTeX. When the metadata does not already contain
one, it requests Semantic Scholar's own export with an anonymous
`POST https://api.semanticscholar.org/graph/v1/paper/batch?fields=citationStyles`,
sending only `{"ids":["<paper-id>"]}`. No API key is needed or sent, even if one
is configured. The response must identify the requested paper. Export has a
15-second cap within the paper deadline. A missing or failed export emits a
warning and leaves new output without a BibTeX section; metadata can still
refresh. Existing retrieved or manually saved citations survive export failure.
The source block escapes Org control lines; citation fields and keys are kept.

The ownership property marks sections maintained by the tool, not locally
fabricated records. Old marked `@misc{ss<40-hex-id>,...}` or
`@misc{paper<20-hex>,...}` records from the retired serializer are replaced with
retrieved citations during refresh, or removed when no retrieved citation is
available. Cleanup also runs on recent entries and failed metadata fetches,
without an extra request. Unmarked saved citations are never removed. Personal
notes, IDs, custom properties, and child headings in a marked section survive
cleanup, retaining their heading.

Semantic Scholar exports may lack proceedings details. Other keyless options
are [DOI content negotiation](https://citation.doi.org/docs.html)
(`GET https://doi.org/<doi>` with `Accept: application/x-bibtex`) and
[DBLP publication exports](https://blog.dblp.org/2020/08/18/new-dblp-url-scheme-and-api-changes/)
(`https://dblp.org/rec/<publication-key>.bib`). DOI exports reflect registered
publisher metadata; DBLP is useful for computer-science proceedings. These are
alternatives, not automatic requests made by this helper. It does not search
for publication matches or invent missing conference fields.

## Refreshing existing notes

Use [org-paper-metadata-updater](org-paper-metadata-updater.md) to refresh paper
headings under a directory root. It shares this Go package, skips entries fetched
within seven days, and preserves personal notes. Preview with `--diff`; writing
requires `--write`.

## Verification

```zsh
cd "${NIGHTDIR}/golang/org-paper-metadata"
go test -race ./...
python3 "${NIGHTDIR}/zshlang/tests/semantic-scholar-to-org.py"
```

Native tests cover normalization, required fields, redirect identification,
backend order, credential handling, formatting, and Org structure. Shell tests
check delegation, configuration forwarding, gray failures, and exit status.
Live verification covers the reported paper's authors, complete abstract,
TLDR, publication date, counts, and PDF link.

Run `brishz-restart` after shell changes. No Elisp reload is needed.

Verified by @OpenAI on 2026-10-21.
