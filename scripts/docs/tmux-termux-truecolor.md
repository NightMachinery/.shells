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

## Where the false xterm-kitty came from

`setup/minimal_proxy/.shared.sh` used to assign TERM unconditionally:

```sh
if infocmp xterm-kitty > /dev/null 2>&1; then
  export TERM="xterm-kitty"
else
  export TERM="xterm-256color"
fi
```

That clobbers the inherited TERM on the strength of the terminfo *database*
holding the entry, which is true on every host where kitty-terminfo was ever
installed and says nothing about the terminal on the other end. It has no
check for ssh or tmux either, so it also overrode the TERM tmux sets for its
own panes. `setup/minimal_proxy/gen.org` rsyncs the file to remote hosts, which
is how the phone came to announce `xterm-kitty` to every machine it reached.

It now downgrades only a TERM this host cannot resolve, and never invents one:

```sh
if [ -z "${TERM}" ] ; then
  export TERM="xterm-256color"
elif command -v infocmp > /dev/null 2>&1 && ! infocmp "${TERM}" > /dev/null 2>&1 ; then
  export TERM="xterm-256color"
fi
```

The same guard is repeated verbatim in `~/.bashrc`, which had its own hardcoded
`export TERM=xterm-256color` since commit 638ad035. On a host whose login shell
is bash -- eva, for one -- bash sources `~/.bashrc` for ssh sessions too, so
every connection reported xterm-256color no matter which terminal was attached,
and a kitty client lost its own terminfo even though eva has the entry
installed. The guard is duplicated rather than shared because `.shared.sh` must
stay standalone and cannot call into zshlang.

A genuine kitty client keeps `xterm-kitty`, tmux panes keep `tmux-256color`,
and an unresolvable value still falls back rather than producing `terminals
database is inaccessible`.

Redeploy with the rsync in `setup/minimal_proxy/gen.org` for the phone to pick
this up; until then the `terminal-overrides` line above is what keeps its
colors readable.

## Truecolor inward, to the panes

`terminal-overrides` and `terminal-features` govern what tmux sends *outward*,
to the attached client. They say nothing to the programs running inside a pane,
which learn what the terminal can do from the TERM tmux sets for them,
`default-terminal`. With `tmux-256color` that advertises 256 colors, which is
why overwriting TERM with the outer terminal's name was ever tempting.

`tmux-direct` is the ncurses direct-color variant: `colors#0x1000000`, and
`setaf`/`setab` take a 24-bit RGB value instead of a palette index. It gives
panes truecolor honestly, through terminfo, with no lie about which terminal is
attached.

Measured on a throwaway server, pane writing through `tput`, client attached as
`xterm-kitty`:

- `tmux-256color`: pane sees `colors=256`; `tput setaf 65280` is out of range
  and produces nothing.
- `tmux-direct`: pane sees `colors=16777216`; `tput setaf 65280` reaches the
  client as `ESC[38;2;0;255;0m`, semicolons included, because the override
  above still governs the outward spelling.

The cost is that `setaf`'s parameter changes meaning:

- `tput setaf 2` is `ESC[32m` either way, since indices under 8 take the
  ANSI branch.
- a program writing `ESC[38;5;46m` itself is passed through untouched.
- `tput setaf 46` becomes `ESC[38;2;0;0;46m`, near-black, rather than palette
  index 46.

So the only programs affected are those calling terminfo `setaf` with an index
of 8..255 without first checking `colors`. Tools that emit raw SGR, which is
most of them, are unaffected.

`default-terminal` applies to panes created after the change; existing ones
keep their TERM until recreated. The entry must exist on the machine running
the tmux *server*, not on the client: it is present on macOS ncurses, on eva
and on beta, and absent on the phone, which does not need it.

## Telling a remote host the terminal does truecolor

Making TERM honest cost something: `xterm-256color` is indistinguishable from a
terminal that really is limited to 256 colours, so [agfi:true-color-p] failed
every one of its branches over ssh and [agfi:colorfg] emitted nothing at all.
The colours did not merely degrade, they vanished. Kitty had never hit this,
because `xterm-kitty` answers the `isKitty` branch.

The signal that carries this properly is `COLORTERM`, and the repository
already had the machinery to move it: sshd forwards only what `AcceptEnv`
permits, commonly `LANG LC_*`, so a client mirrors `LC_<NAME>` and the server
restores `<NAME>`. Three things were missing.

`COLORTERM` was not in `env_smuggled_lc_vars`. Termux sets no `COLORTERM` of
its own, though its emulator does 24-bit colour; `.shared.sh` now asserts it
where `TERMUX_VERSION` proves we are actually inside Termux, and never on a
VPS, where the value must come from the client. And
[agfi:env-load-smuggled-lc-vars] ran only from `~/.night-bootstrap.env`, which
the bootstrap generates -- so on a host that never ran the bootstrap, this
laptop included, the restore half never happened at all and the LC_ copies
arrived and were ignored. It is now called from `zshlang/basic/ssh.zsh` under
`isSSH`, before the outward mirroring, so a chained hop keeps working.

Verified end to end, phone to laptop:

```
TERM=xterm-256color COLORTERM=truecolor
true-color-p=0
isKitty=1
helloworld: ESC[48;2;0;0;255mESC[38;2;0;255;0mHELLO ...
```

The client also needs `SendEnv`, which OpenSSH leaves empty by default. The
laptop's `~/.ssh/config` has had those lines for a while; the phone had no ssh
config at all and now carries a matching `Host *` block.

## Choosing the Emacs terminfo entry

[agfi:emc-gateway] selected `xterm-emacs` with `if isKitty || isiTerm`. That is
a proxy for the real question -- does this terminal do 24-bit colour -- and it
silently answered no for Termux once TERM stopped claiming kitty, handing Emacs
a plain `xterm-256color` and losing `setf24`/`setb24`. It now asks
[agfi:true-color-p] directly, which covers kitty and iTerm as before and Termux
as well.
