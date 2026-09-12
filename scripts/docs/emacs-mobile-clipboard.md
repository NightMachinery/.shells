# Mobile Emacs clipboard

`emc-mobile` marks a terminal frame with `night/mobile=t`. The accompanying
Emacs configuration uses its native OSC 52 backend for copies and kills from
that frame. The destination is the terminal client's clipboard, including
Termux over SSH; there is no listener to install or address to configure.

The decision is per frame at copy time. Desktop frames retain their existing
clipboard behavior, even when they display the same buffer. The older TCP
clipboard listener and shell sender are unchanged.

Emacs already includes OSC 52 support. Clipetty was also installed and loaded,
but its mode was disabled. The native backend is used here to avoid Clipetty's
buffer-scoped routing and its additional call to the host clipboard function.

The default OSC 52 limit is 6000 UTF-8 bytes, compatible with Termux 0.118.1's
8192-character OSC buffer after base64 encoding. `emc-mobile` keeps the existing
behavior: oversized text stays in the Emacs kill ring and produces a message.
Newer Termux versions may support larger messages; increase the Emacs option
`night/mobile-clipboard-max-bytes` only when the whole terminal path supports
it. OSC 52 does not acknowledge clipboard writes. Paste via the terminal's
Paste action; no clipboard-read feature is enabled.

Clipboard reads are a separate terminal capability. Emacs has a native OSC 52
query backend (`getSelection` / `xterm--get-selection`), but Termux's current
[OSC 52 handler](https://github.com/termux/termux-app/blob/master/terminal-emulator/src/main/java/com/termux/terminal/TerminalEmulator.java)
only decodes incoming data and copies it; it does not answer `OSC 52 ; c ; ? ST`
queries. Enabling queries in `emc-mobile` therefore cannot retrieve the Termux
clipboard and can introduce timeouts. No automatic query is enabled, and no
probe is sent to the phone. A third-party Emacs package cannot supply a missing
terminal response.

For interactive use, Termux's Paste action sends the clipboard through the
existing terminal connection; Emacs already supports bracketed paste. This
avoids the outgoing OSC 52 copy limit and needs no reverse SSH connection, but
it requires a user paste action. On terminals that implement clipboard queries,
OSC 52 reads can work without reverse SSH, subject to terminal permissions,
response sizes, and multiplexer support; they are not inherently unlimited.

For scripted reads, use [agfi:tealy-paste], or
`h-paste-from-remote-termux phone-alias`. These print the phone's current
clipboard over SSH; see [remote Termux clipboard reads](remote-termux-clipboard.md).
SSH avoids the OSC transport limit but still depends on Termux:API and Android
clipboard access. It is independent of the Emacs copy reachability cache.

`emc-tealy` is the large-copy variant. Copies up to the OSC limit still use OSC
52. Larger copies are piped over authenticated SSH to the host alias `tealy` and
into `termux-clipboard-set`; the text is not placed in command-line arguments.
This keeps small copies instant and supports much larger selections without a
new listener. It requires the SSH alias, authentication, and Termux:API command
to work from the machine running Emacs.

Before the first oversized copy, Emacs performs a non-interactive SSH readiness
check. A successful check or transfer refreshes a 15-minute cache entry in that
Emacs daemon. A failed check or transfer replaces it immediately with a
30-second failure entry. Launching or reconnecting with `emc-tealy` does not
probe the phone; selections within the OSC limit never require SSH. Fifteen
minutes is a useful balance here: routine large copies avoid a network probe,
while stale reachability does not linger for an entire work session. The actual
copy can still fail if the phone goes offline after a successful check. A
shorter success TTL reacts sooner but adds SSH
round trips; a longer TTL reduces probes but trusts stale state longer. The short
failure TTL avoids repeatedly waiting on an offline phone without making a
newly reachable phone seem unavailable for long.

SSH work is asynchronous and serialized per host. If several copies arrive
while a transfer is running, only the newest waiting copy is retained. A later
small OSC copy also waits for that transfer, preventing the older SSH result
from overwriting newer clipboard text. A failed copy remains in the kill ring
and is not replayed automatically.

Run `emc-tealy-cache-clear` to invalidate the `tealy` entry in the current Emacs
daemon immediately. It follows `emc-mobile`'s daemon selection, including
`emc_mobile_own_daemon_p=y`. The equivalent interactive Emacs command is
`M-x night/mobile-clipboard-cache-clear`; with no host argument it clears every
cached alias in that daemon.

Direct `emc-mobile` needs no tmux configuration. For `emc-mobile-tmux`, tmux's
default `set-clipboard external` blocks application-originated OSC 52. Opt in
with `tmux set-option -s set-clipboard on` and ensure the attached terminal has
the `Ms` capability. This is a server-wide permission for all pane applications,
not a setting restricted to the mobile session; no global setting is changed
automatically. A detached session has no receiving client clipboard.

`emc-tealy-tmux` provides the same SSH fallback in a separate `emacs-tealy`
tmux session. Keeping it separate prevents an already-running plain mobile frame
from silently retaining its original clipboard destination. File arguments are
used only when each tmux session is first created.

The deferred Go receiver design and migration tradeoffs are in
[the listener plan](../.docs/plans/clipboard-listener.md). That plan contains no
deployment-specific addresses or credentials and has not been implemented.

Sources: [Emacs native backend](https://github.com/emacs-mirror/emacs/blob/emacs-29/lisp/term/xterm.el),
[Clipetty](https://github.com/spudlyo/clipetty),
[Termux 0.118.1](https://github.com/termux/termux-app/blob/v0.118.1/terminal-emulator/src/main/java/com/termux/terminal/TerminalEmulator.java),
[tmux clipboard configuration](https://github.com/tmux/tmux/wiki/Clipboard).
