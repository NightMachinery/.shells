# Google Cloud Translation

Run `2en-gcp 'text'` to translate to English. With no arguments, the command
reads from stdin or the clipboard. `2fa-gcp` and `2de-gcp` select Persian and
German, and `gcp-translate` takes a target language code as its first argument.

Set `gcp_translate_project` and `gcp_translate_key_file` in
`~/.night-gcp/config.zsh`. Translation and the Chirp speech commands share
the access token at `${XDG_CACHE_HOME:-$HOME/.cache}/gcp-translate/token`.
Zsh reads the cache file's modification time with `zstat` on macOS and Linux.
The token is reused for 3,000 seconds by default. Set
`gcp_translate_token_ttl` in seconds to change that window. A successful
`gcloud` refresh replaces the cache atomically, with mode 600.
