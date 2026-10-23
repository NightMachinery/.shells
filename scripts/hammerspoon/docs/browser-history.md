# Browser history sources

The shared Go helper accepts a fixed browser ID (`brave`, `chrome`, `edge`,
`safari`, `arc`, `dia`, `firefox`) and lazily starts an independent bounded index
for that source. A request cannot supply a filesystem path. An omitted ID keeps
the original Brave behavior. Responses identify their browser; the HS client
rejects mismatched sources and older helpers for non-Brave requests.

Brave/Chrome/Edge/Arc/Dia use their native Chromium user-data directories and
`Local State`'s last-used profile. Safari uses its main `History.db`. Firefox
uses `profiles.ini`'s installation-default profile, then a default/first relative
profile under its own directory. Externally located Firefox profiles are not
read automatically. These choices do not infer the profile of the front window;
profile switching and Safari's separate profiles/tab groups can differ from the
indexed main/default store.

Source databases and journals are copied read-only into a private temporary
snapshot, checked, and queried with the appropriate Chromium/Safari/Firefox
schema. Up to 5,000 recent HTTP(S) URLs are indexed, with at most 200 matches per
request. Matching stays in memory. Metadata is checked every two seconds;
refreshes are throttled to at least five seconds and periodically rechecked.
Unused sources do not scan, copy, parse or start refresh workers.

The launcher remains the single supervisor (`omni-history-boot`). HS never owns
or rebuilds the process and reloads do not kill it. The API still requires a
password on numeric loopback, rejects browser Origin requests, uses private
atomic discovery and returns sanitized errors. No history/query values are
logged. Source readability depends on the OS permissions of the supervised
process; failures are reported without returning another browser's history.

Rebuild/restart the supervised helper after updating its binary. Existing
clients re-read discovery after password/endpoint rotation. `go test -race ./...`
and `go vet ./...` check source schemas, locked snapshots, authentication,
matching, profile boundaries, isolation and lazy startup.
