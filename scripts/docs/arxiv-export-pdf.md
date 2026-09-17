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

`h-arxiv-pdf-dl` (`zshlang/auto-load/others/scraping/arxiv.zsh`) takes a list of
URLs plus a destination, appends the `arxiv.org` mirror of each via
`arxiv-unexportify`, and tries them in order. `export` stays first, so the
politeness convention survives and the mirror is only touched when export fails.

Each attempt downloads to `${dest}.part`, which is moved onto `$dest` only on
success, so a failed run leaves an existing file alone.

Two details are load-bearing:

- **`curlm_continue_p=n`.** `curlm` defaults to `--continue-at -`. When the
  destination is already complete, curl sends a `Range` header past the end of
  the file, the server answers **416**, and `--fail` turns that into *the same
  exit 22*. A truncated `export` response must not be resumed as valid either.
  `ss-dl` (`zshlang/auto-load/others/scraping/semantic_scholar.zsh`) had this
  bug: re-running it over a finished download failed.
- **`retry-limited`, not `retry`.** `retry` is `retry-limited 0`
  (`zshlang/auto-load/others/error-handling.zsh`), i.e. *unlimited* retries, so
  a 406 loops forever and a fallback URL is never reached. The retry budget is
  1 for every URL but the last and 3 for the last: a failed attempt can pull
  megabytes before dying, so the retries are spent on the URL expected to work.

`aa-2dest` is no longer on this path. It routed the PDF through
`full-html`/`full-html2`, which buffers the whole response in a shell variable
and offers no retry. (That buffering was *not* corrupting PDFs — zsh command
substitution preserves NUL bytes — but it gives no progress meter and no
fallback.)

## Not fixed

`ss-dl` still rewrites arXiv PDF links onto `export` (`arxiv-exportify`, and a
hardcoded URL in the `api.semanticscholar.org/arXiv:` branch) without the
fallback. Routing those through `h-arxiv-pdf-dl` would also touch non-arXiv
publisher URLs, so it was left for a follow-up.
