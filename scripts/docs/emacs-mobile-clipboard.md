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

The default limit is 6000 UTF-8 bytes, compatible with Termux 0.118.1's
8192-character OSC buffer after base64 encoding. Oversized text stays in the
Emacs kill ring and produces a message. Newer Termux versions may support larger
messages; increase the Emacs option `night/mobile-clipboard-max-bytes` only when
the whole terminal path supports it. OSC 52 does not acknowledge clipboard
writes. Paste via the terminal's Paste action; no clipboard-read feature is enabled.

Direct `emc-mobile` needs no tmux configuration. For `emc-mobile-tmux`, tmux's
default `set-clipboard external` blocks application-originated OSC 52. Opt in
with `tmux set-option -s set-clipboard on` and ensure the attached terminal has
the `Ms` capability. This is a server-wide permission for all pane applications,
not a setting restricted to the mobile session; no global setting is changed
automatically. A detached session has no receiving client clipboard.

The deferred Go receiver design and migration tradeoffs are in
[the listener plan](../.docs/plans/clipboard-listener.md). That plan contains no
deployment-specific addresses or credentials and has not been implemented.

Sources: [Emacs native backend](https://github.com/emacs-mirror/emacs/blob/emacs-29/lisp/term/xterm.el),
[Clipetty](https://github.com/spudlyo/clipetty),
[Termux 0.118.1](https://github.com/termux/termux-app/blob/v0.118.1/terminal-emulator/src/main/java/com/termux/terminal/TerminalEmulator.java),
[tmux clipboard configuration](https://github.com/tmux/tmux/wiki/Clipboard).
