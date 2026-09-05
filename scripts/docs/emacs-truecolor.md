# Emacs terminal truecolor

`setup/terminfo-24bit.src` defines `xterm-emacs` using semicolon-separated
RGB escapes supported by Kitty and Termux. The `setf24` and `setb24`
capabilities retain full 24-bit color despite inheriting `xterm-256color`.
The previous colon-separated form included an empty colorspace parameter
that Termux does not handle correctly; older versions reject colons entirely.

Install on the machine running Emacs (the remote host for SSH sessions),
from the scripts directory:

```sh
command tic -x -o ~/.terminfo setup/terminfo-24bit.src
```

On macOS, use `/usr/bin/tic` if an Anaconda installation shadows `tic`;
the Anaconda binary crashed during verification, while the system binary
compiled this entry successfully.

Open a new terminal Emacs frame after recompiling:

```sh
TERM=xterm-emacs emc-gateway
```

Keep Termux's normal terminal identity for the SSH connection; select
`xterm-emacs` when launching Emacs. No Android-side terminfo installation
is needed when Emacs runs remotely.

In that terminal frame, `M-: (display-color-cells)` should return `16777216`.
An already-open frame may retain the old capabilities; reopen it before
checking. Android rendering still needs verification on the actual device.

For mobile use, `emc-mobile` delegates to `emc-gateway` with
`TERM=xterm-emacs` and `--frame-parameters '((night/mobile . t))'`.
It forwards file arguments and preserves server selection. With the Doom
`night-mobile` module loaded, marked terminal frames hide line numbers except
on the final logical line (trailing blank lines may also retain numbers).
Desktop windows retain their buffer settings. Existing terminal frames can
opt in or out with `M-x night/mobile-frame-toggle`.

References:

- [Emacs truecolor capabilities](https://www.gnu.org/software/emacs/manual/html_node/efaq/Colors-on-a-TTY.html)
- [Termux RGB handler](https://github.com/termux/termux-app/blob/master/terminal-emulator/src/main/java/com/termux/terminal/TerminalEmulator.java)
