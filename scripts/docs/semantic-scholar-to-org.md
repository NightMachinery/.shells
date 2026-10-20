# Semantic Scholar paper notes

`semantic-scholar-to-org` (alias `ssorg`) fetches a paper's authors, full abstract,
publication details, counts, and links, then produces an Org entry. Pass an
arXiv or Semantic Scholar paper URL; arguments, piped URLs, and interactive
clipboard input work as before. Emacs uses the same command through BrishGarden.

```zsh
semantic-scholar-to-org '<paper-url>'
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
4. `full-html2` as the final fallback.

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
the requested paper. This undocumented website endpoint can change, which is
why the other backends remain available.

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
- `metadataSource` and `metadataFetchedAt`: successful retrieval backend and UTC time.

A supplied TLDR appears in a labeled quote block before the full abstract,
inside the same folded `@abstract` heading. Comment-marked summary and abstract blocks
identify generated content for later refreshes. Zero counts are preserved;
missing values are omitted.

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

Verified by @OpenAI on 2026-10-20.
