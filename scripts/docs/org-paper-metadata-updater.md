# Refresh Org paper metadata

`org-paper-metadata-updater` walks a directory tree and refreshes existing Org
paper headings whose metadata was last fetched at least seven days ago. It
shares the native Go fetching, normalization, and rendering code used by
`semantic-scholar-to-org`. No Python or Lisp helper runs on the default path.

```zsh
# Preview which existing headings need a refresh. This does not write files.
org-paper-metadata-updater '<directory-root>'

# Inspect the proposed content changes.
org-paper-metadata-updater --diff '<directory-root>'

# Apply them. A subsequent run skips the successfully refreshed entries.
org-paper-metadata-updater --write '<directory-root>'
```

The shell wrapper builds both Go binaries when needed. For a manual install:

```zsh
cd "${NIGHTDIR}/golang/org-paper-metadata"
go install . ./cmd/org-paper-metadata-updater
```

## Selection and freshness

- Only `.org` files and existing headings with a recognized arXiv or Semantic
  Scholar link are candidates. Loose links in prose or lists are ignored,
  without being reported or converted into headings.
- Headings containing distinct paper identifiers are left alone because one
  metadata drawer cannot describe several papers unambiguously.
- Source/example/quote blocks, drawers, COMMENT subtrees, and archived subtrees
  do not contribute candidate headings.
- `metadataFetchedAt`, an RFC3339 UTC property on each paper heading, controls
  freshness of remote metadata. Local arXiv drawer-URL normalization and removal
  of old fabricated BibTeX still run on recent entries without fetching metadata
  or changing the timestamp.
  File modification times are irrelevant. Missing or invalid
  timestamps trigger retrieval; an entry exactly seven days old is eligible.
- Duplicate paper URLs share one fetch during a run. Requests are sequential,
  with a one-second minimum interval between uncached fetches by default.
- Descendant symlinks, hidden subdirectories, `node_modules`, and files carrying an Emacs
  `.#filename` lock are skipped. Existing editor locks are reported separately. An explicitly supplied symlink
  root is resolved before walking.

Useful overrides, placed before the directory argument:

```zsh
org-paper-metadata-updater --write --force '<directory-root>'
org-paper-metadata-updater --write --max-age 336h '<directory-root>'
org-paper-metadata-updater --scraper page-data --timeout 2m '<directory-root>'
```

`--force` bypasses freshness checks. `--max-age` defaults to `168h`; `--timeout`
defaults to one minute per paper, including all fallbacks. `--interval`
controls spacing. Source selection and credentials follow the
[paper converter](semantic-scholar-to-org.md). Automatic mode never launches
`full-html2`; select it explicitly to enable HTML retrieval. Each network fetch
is logged before it starts, followed by its elapsed time on success. Ctrl-C
stops the run without starting another fallback or writing the current file.
Diffs use contextual hunks so unchanged notes remain visibly unchanged.

## What changes

The tool refreshes the paper link's title, generated citation/date/venue
at-tags, and supplied metadata properties. It preserves the original heading
level and URL, TODO state, priority, personal tags, other heading text, custom
properties such as `ID`, `url`, or personal `title`/`abstract` annotations, prose,
and unrelated subheadings. Missing optional
source fields do not erase previously recorded values. The retired
`metadataSource` property is removed when an entry is refreshed. Open access remains a
drawer property, without adding an at-tag.

Generated TLDR and abstract quote blocks use plain Org comment markers so
they can be updated without creating duplicate named-block anchors. Those
blocks are refreshed inside the same folded `@abstract` child. A legacy child
containing only a single generated quote block, optionally followed by its
legacy BibTeX example block, is upgraded to the new format. Saved BibTeX is
preserved byte for byte. Empty legacy abstract sections are filled, including
those containing only a saved BibTeX citation. A separate handwritten
`@summary` sibling is preserved.
If there is no abstract child, one is added. Mixed or handwritten abstract
sections without generated markers are preserved. Other text around marked
blocks is left in place.

## DBLP publication fields

DBLP SPARQL JSON enriches publication fields by default, using exact identifiers.
It can replace a preprint DBLP association with a unique published DOI match,
and supplies canonical title/authors, venue/year, proceedings, publisher, pages,
volume/issue, series, ISBN/ISSN and a DBLP URL when available. Full abstracts,
TLDRs, counts and dates remain from Semantic Scholar. Missing DBLP values do
not erase saved fields, and failed enrichment leaves SS metadata available.
Use `--dblp-metadata=false` to disable it. Recent entries retain their normal
skip; `--force` applies enrichment immediately. This remains active with
`--no-bibtex` because it does not create citations.

## BibTeX sections

Retrieved BibTeX is enabled by default, or explicitly with `--bibtex`. A managed
`@bibtex` child is folded and placed immediately after the `@abstract` subtree,
at the same heading level. Its drawer carries
`:orgPaperMetadataGenerated: bibtex`. Only source-supplied citations are used,
preferentially DBLP, DOI, then Semantic Scholar’s anonymous export, as described
in the
[converter documentation](semantic-scholar-to-org.md#bibtex).

```zsh
org-paper-metadata-updater --diff --bibtex-source auto --bibtex '<directory-root>'
org-paper-metadata-updater --write --force --bibtex '<directory-root>'
org-paper-metadata-updater --write --no-bibtex '<directory-root>'
```

`--bibtex-source` is independent of `--scraper`. To keep all requests limited
to Semantic Scholar use
`--scraper page-data --bibtex-source semantic-scholar --dblp-metadata=false`.
Automatic citations may contact DBLP, DOI registration agencies, and configured
DeepSeek Fetch for DBLP export recovery. `--bibtex-browser` additionally enables
bounded `full-html2` recovery from DBLP’s official Trier mirror as a last
resort after direct exports, exact SPARQL DOI lookup, DOI/SS citations and
optional DeepSeek have failed; it is off by default. Only public identifiers/URLs are sent.

A confirmed DBLP website bot challenge disables only its direct BibTeX export
for the remainder of that run, with one notice instead of repeated attempts and
fallback warnings. SPARQL publication metadata and other citation providers
continue normally. A new run retries DBLP; an individual missing record does
not disable the service. Genuine overall citation failures still warn.

Normal freshness rules apply to additions and refreshes. Use `--force` to add
BibTeX to entries whose metadata is still recent. `--no-bibtex` also removes
marked sections from recent entries without fetching metadata. If a stale
paper's retrieval fails, the requested removal still applies, and its metadata
and fetch timestamp remain unchanged. Writing still requires `--write`.

Recognizable legacy citation blocks at the end of an abstract are relocated
unchanged into a folded `@bibtex` sibling. They remain unmarked because their
keys or contents may have been edited by hand. Existing unmarked `@bibtex`
sections are preserved and do not receive a duplicate generated citation.
`--no-bibtex` never removes an unmarked citation. Ambiguous multiple citation
blocks are left intact.

Managed citations are refreshed on later updates. No citations are synthesized.
Old marked `@misc` records with the retired serializer’s `ss<40-hex-id>` or
`paper<20-hex>` key pattern are replaced with retrieved citations on refresh.
Without a retrieved citation they are removed, including during recent-entry
skips or failed fetches. Unmarked citations and other citation keys are preserved.
An export failure warns without failing an otherwise successful metadata refresh.
If a managed heading
has acquired personal prose, IDs, custom properties, or child headings,
`--no-bibtex` removes the citation block and ownership marker while keeping
that personal content and its heading. A section containing only generated
content is removed completely.

Every successful fetch requires authors and a full abstract. A failed paper's
metadata is left unchanged and does not receive a new timestamp. Local arXiv
URL normalization, cleanup of old fabricated citations, or explicitly requested
removal of managed BibTeX can still change that entry. Successful
papers can still be refreshed; the command exits nonzero when any retrieval or file edit
fails, and prints a summary of updates, recent entries, failures, and locks.

Writes use a synced temporary file in the destination directory followed by
an atomic rename. The tool preserves permissions and line endings, checks the
file contents again before renaming, and refuses read-only files, hard links,
editor locks, or files changed during retrieval. Dry run is the default;
`--write` must be supplied explicitly.

## Tests

Run `go test -race ./...` in `golang/org-paper-metadata`. Offline fixtures cover
freshness boundaries, duplicate fetching, partial failure, dry runs, metadata
and body edits, personal-content preservation, CRLF, nested paper headings,
editor locks, concurrent changes, cancellation, contextual diffs, canonical-ID
redirects, disabled automatic HTML retrieval, and atomic writes. Live smoke tests
use temporary Org files. A read-only audit across a research tree also checked
custom properties, handwritten content, saved citations, source blocks, heading
hierarchy, and every generated field against its fetched source.

Verified by @OpenAI on 2026-10-21.
