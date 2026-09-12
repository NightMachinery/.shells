# Portable clipboard receiver — deferred implementation plan

Status: design only. The current implementation uses OSC 52 for mobile Emacs
frames. No receiver migration or phone installation is part of that change.
This document intentionally contains no personal hostnames, addresses, accounts,
credentials, device identifiers, or absolute user paths.

## Intended capability

Build a small Go command that accepts text from remote shells and writes it to
the receiving computer's clipboard. Support macOS, Linux desktop sessions, and
Android Termux. The receiver should work independently of an attached terminal,
so background jobs and larger copies do not depend on OSC 52 limits.

Keep the existing `clipboard-remote-listen`, `pbcopy-remote`, and
`tcp_to_process` workflow available. Adoption is explicit per receiver and
sender; installing a binary must not replace a current clipboard route.

## Proposed implementation

- Add a standalone `golang/clipboard-listener` module, standard library only,
  with a conservative supported Go version, tests, and `readme.org`. Build via
  `go install .` from its directory, or the repository's `go-install-local`
  helper. Build natively in Termux; do not assume a Linux executable is Android
  compatible. Never commit compiled binaries.
- Provide `serve`, `send`, and `doctor` commands. `send` reads stdin exactly;
  `doctor` checks configuration, backend dependencies, and service availability
  without reading or overwriting clipboard contents.
- Use HTTP `POST /v1/clipboard` with a UTF-8 text body and an explicit success
  response only after the clipboard backend finishes successfully. Unlike raw
  TCP with EOF framing, HTTP provides request boundaries and useful errors and
  can also be called with curl. No clipboard-read endpoint or arbitrary command
  execution endpoint; legacy `MAGIC_BELL_*` input is ordinary text.
- Default to loopback on a separate port, proposed 6071. Direct private-network
  exposure requires an explicit bind address and generated bearer token, stored
  in an owner-only configuration file. Bind to the exact VPN interface address;
  never silently fall back to a wildcard if that address is unavailable.
- Use SSH forwarding or an encrypted private network such as Tailscale for
  transport protection. Plain HTTP with a token alone is unsuitable for the
  public internet. Tailscale ACLs and an application token protect different
  boundaries; a token adds protection against other otherwise permitted peers.
- Authenticate before reading large bodies; reject unsupported methods,
  invalid UTF-8, oversized or incomplete requests. Start with a configurable
  1 MiB limit, bounded concurrency, and request/backend timeouts. Serialize
  clipboard writes and report backend errors. Never log clipboard text or
  credentials. Do not queue offline copies for later unexpected replay.
- Backend commands receive text through stdin via `os/exec`, without shell
  interpolation: `pbcopy` on macOS, `wl-copy` for Wayland, `xclip` for X11, and
  `termux-clipboard-set` for Termux. Permit explicit backend selection and make
  missing session context/dependencies actionable. Verify each backend's text
  fidelity and supported size rather than assuming identical OS semantics.

## Termux installation and optional autostart

Termux requires the `termux-api` package and the matching-source Termux:API
Android app. Install source and binary over an already configured SSH host;
keep the target's address and credentials in private local configuration.

Add `autostart enable`, `autostart disable`, and `autostart status` for Termux.
Manage a dedicated runit service through `termux-services`, and a dedicated
Termux:Boot script to start the supervisor after reboot. Operations must be
idempotent and touch only owned files. Disabling this receiver must not stop
other Termux services or remove somebody else's boot script.

Termux:Boot needs a compatible Android app installation and one manual launcher
tap before boot delivery works. Report these prerequisites rather than claiming
that writing a shell script enables Android boot support. Do not uninstall or
replace Termux to change app signing sources.

Default to no wake lock. An explicit opt-in wake lock improves availability but
can increase idle battery use; Android/vendor battery restrictions can still
interrupt the process. Wake locks are shared Termux state, so disabling this
service must not release another workload's lock. Reboot, screen-off, network
reconnect, and unavailable-VPN-at-start tests are necessary before claiming
reliable unattended availability. A supervisor may retry an exact-address bind
with a delay until the VPN is available.

## Integration and migration tradeoffs

For Emacs, choose the receiver through a frame parameter carrying a private
profile name. Resolve its endpoint and token from private configuration on the
daemon host. Environment variables alone are insufficient when an existing
daemon serves multiple frames. Preserve other frames' existing routing and
report send failures without silently copying to a different device.

OSC 52 is the easiest interactive route: no installation on the destination,
no listener, no credentials, and transport follows SSH. It depends on terminal
support and multiplexer settings, has terminal-specific size limits, and lacks
a write acknowledgement. It targets the terminal client, not an arbitrary device.

The listener is preferable for explicit destinations, background copies, useful
error reporting, and larger payloads. Costs include installation, backend
dependencies, authentication configuration, network availability, and Android
background/battery management. A future desktop migration would also need to
separate existing bell messages from clipboard transport and update senders;
the new authenticated API is deliberately incompatible with raw `socat` input.

## Verification and delivery

- Unit/integration tests use fake clipboard executables and local HTTP servers:
  Unicode and trailing newlines, empty input, limits, bad authentication,
  deadlines, interrupted requests, concurrent writes, and backend failures.
- Test autostart generation and repeated enable/disable in temporary directories;
  preserve unrelated services and boot scripts. `status` must distinguish
  configured startup from a running listener and missing Android prerequisites.
- On an explicitly selected destination, verify a non-sensitive copy, backend
  output, service restart, SSH disconnect, and phone reboot/sleep behavior with
  the user. Preserve clipboard contents where feasible without persisting secrets.
- Keep endpoint/token values out of docs, tests, commits, and command logs.
  Publish generic examples only. Update the tool README, `docs/`, and root README.
- Atomic commit groups: core CLI/protocol/backends and tests; Termux autostart
  and tests; optional per-frame sender integration. Include corresponding docs
  in each group. Deploy only after selecting and authorizing a destination.

## References

- [Termux:API](https://github.com/termux/termux-api)
- [Termux clipboard command](https://github.com/termux/termux-api-package/blob/master/scripts/termux-clipboard-set.in)
- [Termux:Boot setup](https://github.com/termux/termux-boot#how-to-use)
- [Termux services](https://github.com/termux/termux-services)
- [Tailscale address stability](https://tailscale.com/docs/concepts/ip-and-dns-addresses)
- [tmux and OSC 52](https://github.com/tmux/tmux/wiki/Clipboard)
