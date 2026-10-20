# Fetch public pages as Markdown

`deepseek-fetch <url> ...` calls DeepSeek's standalone Fetch API using the
existing `deepseek_api_key` shell variable. It requires `curl` and `jq`.
Keep the key in private shell configuration. The function sends authentication
through a pipe, keeping it out of command arguments and persistent files.

```zsh
deepseek-fetch 'https://example.org/article'
deepseek-fetch 'https://example.org/one' 'https://example.org/two'
```

A single URL returns its Markdown content. Multiple URLs label the first page
with `URL: <url>` and join subsequent pages with exactly:

```text


---

URL: <next-url>

```

Requests run sequentially. The function buffers the combined output and emits
it only after all requests succeed. Failures return a nonzero status with a
diagnostic on stderr. `deepseek_fetch_timeout` sets the per-request timeout in
seconds (default 90). Successful requests use DeepSeek's billed Fetch service.

Fetch returns extracted Markdown, not raw HTML. A successful HTTP response can
still contain a verification or error page, so callers that need structured
data must validate the content. On Semantic Scholar, a full paper URL containing
both its title slug and ID succeeded where the bare paper-ID URL returned a
verification message.

The endpoint is `POST https://api.deepseek.com/v1/tools/fetch`; see the
[official Fetch reference](https://api-docs.deepseek.com/api/fetch/).

Run the offline transport, authentication, error, and exact-separator checks:

```zsh
python3 "$NIGHTDIR/zshlang/tests/deepseek-fetch.py"
```

Verified by @OpenAI on 2026-10-20.
