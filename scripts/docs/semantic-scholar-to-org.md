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
`--scraper page-data` selects keyless Semantic Scholar metadata. Citation
providers are independent: the default also sends public DBLP/DOI identifiers
to those services (and DOI registration-agency redirects). DBLP recovery can
send its public export URL to configured DeepSeek Fetch. Use both
`--scraper page-data --bibtex-source semantic-scholar --dblp-metadata=false`
to restrict all retrieval
to Semantic Scholar. Source/API credentials go only to their respective services.

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

## DBLP publication metadata

DBLP enrichment is enabled by default, independently of citation output.
`--dblp-metadata=false` disables it; the environment default is
`semantic_scholar_dblp_metadata` (enabled). Stdin parsing remains offline.
`--no-bibtex` suppresses citation retrieval and output, while publication
metadata enrichment may still fetch exact external identifiers and SPARQL JSON.

The helper obtains the resolved paper’s DBLP/DOI identifiers from Semantic
Scholar. If its DBLP key is absent or points to CoRR and a DOI is known, an
indexed, exact DOI lookup tries to locate the published DBLP record. DOI case
variants are rechecked; multiple matches, mismatched IDs and conflicting
published DOIs are rejected. No title search or LLM matching is involved.

The matched record and its proceedings supply title, complete ordered authors,
publication year, venue, publication type, proceedings title, publisher, pages,
volume, issue, series/series volume, ISBN and ISSN when available. The canonical
DBLP record URL is recorded as `dblp`. CoRR records never downgrade published
metadata. Incomplete author lists retain the Semantic Scholar authors.

The Semantic Scholar heading URL, full abstract, TLDR, citation counts, subjects,
open-access status and publication dates are retained. DBLP’s publication year
can differ from the first-publication date, for example for an older preprint.
Missing DBLP fields do not erase existing data. Enrichment is bounded to 20
seconds within the paper timeout; failure warns and keeps Semantic Scholar data.
Org updates continue to preserve personal notes and unrelated properties.
This maps source metadata into Org; it does not construct BibTeX.

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

The helper never synthesizes BibTeX. The default `--bibtex-source auto` selects
DBLP, then DOI, then Semantic Scholar. If the DBLP key refers to a CoRR preprint
and a DOI is available, DOI is tried first to prefer the published record.
Each successful source record is kept without rewriting its fields or key.

Identifiers come from the resolved Semantic Scholar paper's `externalIds.DBLP`
and `externalIds.DOI`, or recognized DBLP/DOI URLs in fetched metadata. The
keyless batch request asks for `externalIds,citationStyles`; its paper ID must
match before either identifiers or its fallback citation are trusted. No title
search, fuzzy matching, or LLM identification is used. Missing identifiers skip
a provider. Incorrect upstream associations can still give incorrect citations.

DBLP exports use `https://dblp.org/rec/<publication-key>.bib`. Direct requests
that fail or return bot-check HTML can recover through configured DeepSeek
Fetch, using the same export URL. DeepSeek may also be denied.
`--bibtex-browser` explicitly enables `full-html2` recovery from the official
Trier mirror’s record page with `?view=bibtex`, which passed a live challenge
and returned source BibTeX. Its environment default is
`semantic_scholar_bibtex_browser` (disabled). `fhMode` selects the existing
browser mode, defaulting to `curlfull_s5`; `curlfull_s10` allows more rendering
time. DeepSeek and browser recovery run only after all selected programmatic
providers fail, with the browser last. They are independent of
the metadata scraper, and browser use remains off by default. Recovery accepts only a
complete source BibTeX entry whose `DBLP:` key matches the requested publication. It does not
generate or repair citations. DOI exports use `https://doi.org/<doi>` with
`Accept: application/x-bibtex` and follow registration-agency redirects.
Semantic Scholar's anonymous batch export is cached as the final fallback;
no Semantic Scholar API key is sent by citation requests.

`--bibtex-source dblp`, `doi`, or `semantic-scholar` selects only that citation
provider (Semantic Scholar may still supply identifiers). The environment
default is `semantic_scholar_bibtex_source`. Both Go commands accept the flag,
and the Org shell wrapper forwards it. `--no-bibtex` skips citation export;
DBLP publication-metadata enrichment remains independently configurable.
`bibtexSource` records the selected provider in JSON, without adding an Org
property. Stdin parsing remains offline.

If the DOI is missing but a DBLP key is known, the official SPARQL API resolves
that exact key to its DOI before publisher export is attempted. Ambiguous or
invalid DOI results are rejected. SPARQL supplies identifiers only, never a
locally serialized citation.

Citation enrichment has a 30-second cap within the paper deadline. Individual
SS, SPARQL, DOI and DeepSeek attempts have eight seconds; direct DBLP export
has four. With `--bibtex-browser`, the total citation cap is 50 seconds,
including up to 24 for browser recovery. The whole paper’s timeout still takes
precedence. Unavailable providers fall through.
If none succeeds, a warning is emitted and new output has no BibTeX section;
metadata can still refresh and existing saved source citations survive.
The source block escapes Org control lines.

The ownership property marks sections maintained by the tool, not locally
fabricated records. Old marked `@misc{ss<40-hex-id>,...}` or
`@misc{paper<20-hex>,...}` records from the retired serializer are replaced with
retrieved citations during refresh, or removed when no retrieved citation is
available. Cleanup also runs on recent entries and failed metadata fetches,
without an extra request. Unmarked saved citations are never removed. Personal
notes, IDs, custom properties, and child headings in a marked section survive
cleanup, retaining their heading.

Official interfaces: [DBLP publication exports](https://blog.dblp.org/2020/08/18/new-dblp-url-scheme-and-api-changes/),
[DBLP search API](https://dblp.org/faq/How%2Bto%2Buse%2Bthe%2Bdblp%2Bsearch%2BAPI.html),
[DBLP SPARQL](https://blog.dblp.org/2024/09/09/introducing-our-public-sparql-query-service/),
[official DBLP mirrors](https://dblp.org/faq/Which%2Bmirror%2Bsites%2Bexist%2Bfor%2Bdblp.html),
and [DOI content negotiation](https://citation.doi.org/docs.html).
The helper uses citation exports and exact external-ID crosswalks; it does not
query title search or serialize SPARQL metadata into new BibTeX.

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
