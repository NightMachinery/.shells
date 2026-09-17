# `arxiv-dl`: exit 22 from `export.arxiv.org`

## Symptom

```
❯ arxiv-dl https://export.arxiv.org/abs/2512.15134
   TTOU 22
  1   h-arxiv-dl : line 16
         .../scraping/arxiv.zsh : line 212
```

`TTOU` is a red herring. `_crash_map_exit_code` (`zshlang/auto-load/others/crash.zsh`)
maps a few low exit codes to signal names on the assumption that they came from
a signal, and 22 happens to be `SIGTTOU`. The real value is curl's exit 22,
"HTTP page not retrieved", which `curlm`'s `--fail` produces for any status
at or above 400.

## Cause

`export.arxiv.org` is broken for `/pdf/` requests, and only for those.
Measured 2026-09-17:

- a cache miss (`/pdf/2512.15134v2`, `/pdf/2512.00001`) answers **HTTP 406**
  with an empty body and `cache-control: private, no-store`;
- a cache hit answers **200 with a truncated body**, cut at a power-of-two
  boundary — 1 MiB for `2512.15134` and `2506.01234`, 10 MiB for `2404.19756`,
  against `content-length` values of 2.8 MB, 4.5 MB and 12.8 MB. curl reports
  that as exit 92 (HTTP/2 `PROTOCOL_ERROR`) or 18;
- the same ids download in full from `arxiv.org/pdf/<id>`;
- `abs/`, `e-print/` and `api/query` on `export` are unaffected.

The truncation is intermittent — the same id came back cut at 1 MiB on one
request and 2 MiB on the next — so a single successful download proves nothing.

## Fix

Two layers, so the generic part is not arXiv-specific.

`curl-2dest <url> ... <dest>`
(`zshlang/auto-load/others/scraping/scraping.zsh`) is the downloader: it takes a
list of URLs, tries them in order until one succeeds, and drops duplicates so
callers can pass mirrors unconditionally. Each attempt writes to `${dest}.part`,
moved into place only on success, so a failed run leaves an existing file alone.
`aa-2dest` is now an alias for it.

`h-arxiv-pdf-dl` (`zshlang/auto-load/others/scraping/arxiv.zsh`) supplies the
arXiv knowledge and nothing else: it expands each URL through
`arxiv-url-alternatives`, which appends the `arxiv.org` twin of anything on
`export.arxiv.org` (via `arxiv-unexportify`) and leaves every other URL alone.
`export` therefore stays first and the mirror is only touched when it fails.
Both `arxiv-dl` and `ss-dl` call it; because the expansion is a no-op off
`export.arxiv.org`, neither needs to test whether a URL is an arXiv one.

Two details inside `curl-2dest` are load-bearing:

- **`curlm_continue_p=n`.** `curlm` defaults to `--continue-at -`. When the
  destination is already complete, curl sends a `Range` header past the end of
  the file, the server answers **416**, and `--fail` turns that into *the same
  exit 22*. A truncated `export` response must not be resumed as valid either.
  `ss-dl` had this bug: re-running it over a finished download failed.
- **`retry-limited`, not `retry`.** `retry` is `retry-limited 0`
  (`zshlang/auto-load/others/error-handling.zsh`), i.e. *unlimited* retries, so
  a 406 loops forever and the remaining URLs are never reached. The budget is 1
  for every URL but the last and `curl_2dest_retries` (default 3) for the last:
  a failed attempt can pull megabytes before dying, so the retries are spent on
  the URL expected to work.

## Why not the old aa-2dest

It was `fhMode=curl full-html "$@"`, which buffers the whole response in a shell
variable. That gives no retry, no fallback and no progress meter, and it is
lossy for binaries: command substitution strips *all* trailing newlines and `ec`
(`print -r --`) adds back exactly one. A PDF ending in `%%EOF\n` round-trips by
luck; a file ending in no newline, or in several, does not.

It survives as `aa-2dest-v1` for the HTML case it was really written for, where
`full-html2`'s link absolutification is the point. Nothing calls it.
