# Omni history helper

Keeps a Chromium history index in memory so Hammerspoon can request a small set
of literal AND matches without copying and parsing the entire history per key.

Normally managed by `hammerspoon/brave/history.lua`: HS builds a source-hash
executable, launches it, and stops it on reload. History-enabled Omni variants
use it automatically. See [Omni configuration and behavior](../../hammerspoon/docs/brave-shortcuts.md).

To build or check it directly, from this directory:

```sh
go build .
go test -race ./...
go vet ./...
go test -run '^$' -bench BenchmarkIndexedSearch -benchmem
```

Requires Go and a C toolchain with system SQLite. No external Go modules are
needed. `-root` selects a Chromium user-data directory; Brave is the default.
`-parent PID` bounds the service's lifetime to its owner. Startup emits one JSON
frame with a local endpoint and ephemeral token; keep it private. POST `/search`
with that bearer token and JSON `{"query":"terms","aliases":{},"exclude":[]}`.
Responses include at most 200 matches, total indexed count, and revision. There
is no browser-facing CORS endpoint or persistent index file.
