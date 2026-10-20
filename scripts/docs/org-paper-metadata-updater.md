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
  freshness. File modification times are irrelevant. Missing or invalid
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
defaults to three minutes per paper, including all fallbacks. `--interval`
controls spacing. Source selection and credentials follow the
[paper converter](semantic-scholar-to-org.md).

## What changes

The tool refreshes the paper link's title, generated citation/date/venue
at-tags, and supplied metadata properties. It preserves the original heading
level and URL, TODO state, priority, personal tags, other heading text, custom
properties such as `ID`, prose, and unrelated subheadings. Missing optional
source fields do not erase previously recorded values. The retired
`metadataSource` property is removed when an entry is refreshed. Open access remains a
drawer property, without adding an at-tag.

Generated TLDR and abstract quote blocks use plain Org comment markers so
they can be updated without creating duplicate named-block anchors. Those
blocks are refreshed inside the same folded `@abstract` child. A legacy child
containing only a single generated quote block is upgraded to the new format.
If there is no abstract child, one is added. Mixed or handwritten abstract
sections without generated markers are preserved. Other text around marked
blocks is left in place.

Every successful fetch requires authors and a full abstract. A failed paper is
left unchanged and does not receive a new timestamp. Successful papers can
still be refreshed; the command exits nonzero when any retrieval or file edit
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
editor locks, concurrent changes, and atomic writes. A live smoke test uses
temporary Org files rather than the notes tree.

Verified by @OpenAI on 2026-10-20.
