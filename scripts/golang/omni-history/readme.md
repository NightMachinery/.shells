# Omni history helper

Keeps a Chromium history index in memory so Hammerspoon can request a small set
of literal AND matches without copying and parsing the entire history per key.

Run `omni-history-boot` to build the local Go tool and supervise it in the
`omni-history` tmux session. The Darwin launcher calls this helper automatically.
Hammerspoon connects to the shared index, which survives client reloads. See [Omni configuration and behavior](../../hammerspoon/docs/brave-shortcuts.md).

To build or check it directly, from this directory:

```sh
go build .
go test -race ./...
go vet ./...
go test -run '^$' -bench BenchmarkIndexedSearch -benchmem
```

Requires Go and a C toolchain with system SQLite. No external Go modules are
needed. `-root` selects a Chromium user-data directory; Brave is the default.
The launcher supplies `-state-file "$HOME/.local/state/omni-history/server.json"`.
A singleton lock prevents duplicate owners; private directory/file modes are
0700/0600. Atomic discovery includes a random loopback endpoint and password.
Set `OMNI_HISTORY_PASSWORD` in the service environment to choose a password;
otherwise a cryptographically random password is generated per run. An explicitly
empty password is refused. No password is printed in this mode.

POST `/search` with Basic authentication, username `omni`, and JSON
`{"query":"terms","aliases":{},"exclude":[]}`. Native clients read the private
discovery file, never a checked-in password. Without `-state-file`, a small
credential frame is written to stdout for private direct callers. `-parent PID`
can additionally bound that direct invocation's lifetime.
Responses include at most 200 matches, total indexed count, and revision. There
is no browser-facing CORS endpoint or persistent index file.
