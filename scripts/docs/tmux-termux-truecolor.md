# tmux truecolor for Termux clients

Colored zsh output rendered as literal text inside tmux when attached from
Termux over SSH, while the same command was correct in the plain SSH shell:

```
2:0:255:0m2:0:0:255mHELLO 2:255:100:0mBRAVE2:0:255:0m 2:100:0:255mNEW2:0:0:255m WORLD!
```

## Why

tmux repaints a pane using the *attached client's* terminfo, not the pane's. It
parses the SGR a program writes into a grid cell and re-emits that cell through
`setrgbf` / `setrgbb`. So the spelling a terminal receives is chosen by the TERM
that client reported, and a client that misreports TERM is handed sequences its
emulator cannot parse.

Termux connects with `TERM=xterm-kitty`, which is a lie: it does not answer
tmux's XTVERSION query, so tmux records an empty terminal type for it. Compare
the clients of one server:

```
term=xterm-kitty  type=kitty(0.48.2)  feats=...,RGB,title      <- real kitty
term=xterm-kitty  type=              feats=...,margins,title  <- Termux
```

kitty's terminfo spells 24-bit color with colons:

```
setrgbf=\E[38:2:%p1%d:%p2%d:%p3%dm
setrgbb=\E[48:2:%p1%d:%p2%d:%p3%dm
```

Termux rejects that form, consumes `ESC[38:`, and prints the remainder as text.
It parses the de-facto semicolon form `ESC[38;2;R;G;Bm` correctly, which is why
[agfi:helloworld] was fine outside tmux: [agfi:colorfg] writes semicolons
directly, and only tmux rewrites them. The same Termux limitation is recorded in
`./emacs-truecolor.md`.

The empty terminal type has a second cost. tmux gave the Termux clients the
`margins` feature (DECSLRM, inherited from kitty's terminfo) that the real kitty
clients did not get; if Termux does not implement left and right margins, tmux's
optimized redraws corrupt that pane.

## The fix in `~/.tmux.conf`

```
set -sa terminal-overrides ',xterm-kitty:setrgbf=\E[38;2;%p1%d;%p2%d;%p3%dm:setrgbb=\E[48;2;%p1%d;%p2%d;%p3%dm'
set -as terminal-features ',xterm-256color:RGB'
```

Both options are matched by a TERM glob and tmux builds a client's terminal
once, at attach time. Two consequences worth knowing:

- No other client is affected. The kitty windows on the laptop keep kitty's
  terminfo, the `RGB` feature and colon-form truecolor. Real kitty accepts the
  semicolon form too, so forcing it costs them nothing.
- A change reaches a client only after it detaches and reattaches. Sourcing the
  file into a running server is not enough.

## The fix on the phone

Set `TERM=xterm-256color` in Termux instead of `xterm-kitty`, and keep
`COLORTERM=truecolor`. Truecolor comes back through the `terminal-features`
line above, whose built-in capability strings use semicolons, and the bogus
`margins` capability disappears with the rest of kitty's terminfo.

Whatever Termux genuinely supports can then be handed back deliberately rather
than inherited by accident, for example:

```
set -as terminal-features ',xterm-256color:RGB:usstyle'
```

Verify afterwards:

```
tmux list-clients -F 'term=#{client_termname} type=#{client_termtype} feats=#{client_termfeatures}'
```

`term` should read `xterm-256color`, `RGB` should be present and `margins` gone.

## Detecting kitty from inside tmux

[agfi:isKitty] used to match `#{client_termname}`, so it returned true for the
Termux client and every kitty-only feature gated on it misfired on Android. It
now prefers `#{client_termtype}`, the terminal's own XTVERSION reply, which TERM
cannot spoof; a client that stays silent is treated as not-kitty.
[agfi:tmux-client-termtype-supported-p] guards the check, because the format
only exists in tmux 3.3 and later, and memoizes its answer since
[agfi:true-color-p] calls this on every colored write.

If a real kitty ever fails to answer, the degradation is safe: [agfi:isKitty]
returns false and [agfi:true-color-p] still succeeds through its
`COLORTERM=truecolor` branch.

## Testing

[agfi:helloworld] for a quick check, and [agfi:terminal-ansi-test] for the full
matrix. On Termux the `38;2;` line must be correct; the `38:2::` and `38:2:`
lines are expected to stay broken, since that is the limitation being worked
around.

References:

- [tmux terminal-features](https://man.openbsd.org/tmux#terminal-features)
- [Termux RGB handler](https://github.com/termux/termux-app/blob/master/terminal-emulator/src/main/java/com/termux/terminal/TerminalEmulator.java)
- [termstandard/colors](https://github.com/termstandard/colors)
