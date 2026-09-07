# sioyek reload and window focus

`sioyek-reload` refreshes the PDF that sioyek already has open, without
bringing sioyek to the front. It lives in `zshlang/auto-load/others/sioyek.zsh`
and is called at the end of every successful compile by `pdflatex-m`
(`zshlang/auto-load/others/latex.zsh`) and `xelatex-m`
(`zshlang/auto-load/others/xelatex.zsh`), so the viewer tracks the document as
it is rebuilt. Under the hood it sends `sioyek --execute-command
reload_no_flicker` to the running instance.

## The problem

Without the `--nofocus` flag, every reload activated sioyek. Done once by hand
that is fine. When an agent recompiles a beamer deck dozens of times in a row,
each compile yanked focus away from whatever you were doing, and the machine
became unusable for the duration.

## Why it happened

The second `sioyek` process does not open anything itself. It forwards its argv
to the running instance over a local socket (`pdf_viewer/main.cpp` in the fork
at `~/code/misc/sioyek`). Unless the forwarded argument list contains the
literal string `--nofocus`, the running instance calls its `focus_on_widget()`
helper, which on everything but Windows calls Qt's `QWidget::raise()`. On macOS
Qt's Cocoa backend implements `raise()` with
`[NSApp activateIgnoringOtherApps:YES]`, so what reads in the source as a
gentle nudge up the stacking order is a full application activation. The
reload commands themselves do nothing focus-related; all the stealing comes
from that one call site.

`--nofocus` is an upstream sioyek flag. It has been there since 2022, is in the
stock Homebrew binary as well as our fork, and is what sioyek's own bundled
`scripts/sioyek.py` passes by default. There is no `prefs.config` equivalent;
the CLI flag is the only knob.

## The knobs

`sioyek-reload` passes `--nofocus` by default. Set `sioyek_reload_focus_p=y` to
let the reload raise the window.

`pdflatex-m` and `xelatex-m` forward their own `pdflatex_focus_p` and
`xelatex_focus_p` (both default `n`) into that variable, so an interactive
compile can bring the viewer up without changing the default that agents get.

The flag only controls sioyek's own raise; it does not call
[agfi:sioyek-focus]. [agfi:open-sioyek] (`open-sioyek-v2` in
`zshlang/auto-load/others/wrappers.zsh`) is unaffected, because it calls
[agfi:sioyek-focus] explicitly after the reload, which is the right thing when
you are deliberately opening a document.

## Gotcha

`reload_no_flicker` exists only in our sioyek fork (branch `night_dev3` at
`~/code/misc/sioyek`, installed by [agfi:build-sioyek]). Upstream only has
`reload`, which repaints with a visible flicker. Against a stock sioyek, the
command name is what fails, not the flag.

## Checking the behaviour

```zsh
hs -q -c 'return hs.application.frontmostApplication():bundleID()'
zsh -ic 'sioyek-reload' ; sleep 1
hs -q -c 'return hs.application.frontmostApplication():bundleID()'
```

The bundle ID must be the same before and after. With
`sioyek_reload_focus_p=y` prepended to the reload, it should become
`info.sioyek.sioyek` instead.
