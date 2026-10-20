# Fetch public pages as Markdown or JSONL

`deepseek-fetch <url> ...` uses the existing `deepseek_api_key` shell variable
and requires `curl` and `jq`. Authentication travels through a pipe, keeping the
key out of command arguments and persistent files.

```zsh
deepseek-fetch 'https://example.org/article'
deepseek-fetch 'https://example.org/one' 'https://example.org/two'
deepseek-fetch-json 'https://example.org/one' 'https://example.org/two'
```

Markdown output labels every page, including a single input:

```markdown
# Page title

URL: <https://example.org/one>

...page content...

---

# Another page title

URL: <https://example.org/two>

...page content...
```

DeepSeek supplies both `title` and `content`. An empty title becomes `Source N`.
The separator is exactly two newlines, `---`, and two newlines before the next
heading. Titles are flattened to one line; page content otherwise retains its
Markdown, with trailing newlines normalized for joining.

`deepseek-fetch-json` emits JSONL: one compact JSON object per URL, in input
order, with `url`, `title`, and `content`. It preserves the returned title and
content, including embedded and trailing newlines. JSONL avoids ambiguous
splitting when a fetched page itself contains horizontal rules or headings.

Both commands buffer output until every sequential request succeeds. Failures
return nonzero and write diagnostics to stderr. `deepseek_fetch_timeout` sets
the per-request timeout in seconds (default 90). Successful requests use
DeepSeek's billed Fetch service. HTTP success can still contain a verification
or incomplete page; consumers must validate the data they need.

The endpoint is `POST https://api.deepseek.com/v1/tools/fetch`; see the
[official Fetch reference](https://api-docs.deepseek.com/api/fetch/).

Run the offline transport, authentication, JSONL, and formatting checks:

```zsh
python3 "$NIGHTDIR/zshlang/tests/deepseek-fetch.py"
```

Verified by @OpenAI on 2026-10-20.
