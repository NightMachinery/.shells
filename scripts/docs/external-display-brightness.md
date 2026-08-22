# external display brightness

Makes `brightness-get` / `-set` / `-inc` / `-dec` work on external monitors, not
just the built-in panel, and lets a specific display be picked when several are
attached.

The logic is `zshlang/auto-load/others/system.zsh`.

## Why there are two backends

nriley `brightness` (`~/code/misc/brightness`, installed to
`/usr/local/bin/brightness`) talks to IOKit, which only built-in panels expose.
Point it at an external monitor and it gives up:

    $ brightness -l
    brightness: unable to get brightness of display 0x2
    display 0: main, active, awake, online, external, ID 0x2

External monitors are reached over DDC/CI instead, through `m1ddc` — a separate
protocol spoken over the video link itself, and Apple Silicon only. Neither tool
can do the other's job, so each display is driven by whichever one can reach it:

    internal   nriley `brightness`, IOKit.   Built-in panels.
    ddc        `m1ddc`, DDC/CI.              External panels, Apple Silicon.
    none       nothing can drive it.

`brightness-displays` reports which is which. The two tools number displays
differently — `brightness -l` numbers every display from 0 and `m1ddc display
list` numbers only the DDC-capable ones from 1 — so the listings are joined on
the CGDirectDisplayID, which both print (`ID 0x2` and `Display ID: 2`).

## Commands

    brightness-displays             # every display and its backend
    brightness-get     [sel]        # 0..1, one line per selected display
    brightness-set 0.5 [sel]
    brightness-inc 0.1 [sel]
    brightness-dec 0.1 [sel]

`brightness-off` and `brightness-on` (`zshlang/auto-load/others/power.zsh`) sit
on top of these and needed no changes, as do the hyper+F1/F2 and
hyper+shift+F1/F2 bindings in `hammerspoon/core/window-media-bindings.lua`.

`brightness-displays` prints TSV — index, backend, backend-local id, `main`,
built-in/external, name, CGDirectDisplayID:

    0	internal	0	-	built-in	Built-in	1
    1	ddc		1	main	external	ACME X270Q	2

That last field is what `hs.screen:id()` returns, which is how blanking finds
the right screen to gamma out.

Contrast rides along on the same 0..1 scale, for external panels only:

    contrast-get-ddc [n]
    contrast-set-ddc 0.5 [n]

## Selectors

Everything defaults to `main`, matching what the hardware brightness keys do.
Nothing touches a second monitor unless asked to.

    main        (default)  the display macOS considers main
    all                    every display
    internal, built-in     built-in panel(s)
    external, ddc          external panel(s)
    <integer>              index, as listed by brightness-displays
    <anything else>        regex, matched against the display name

So:

    brightness-set 0.3 all          # dim everything
    brightness-set 0.3 external     # leave the laptop panel alone
    brightness-set 0.3 'ACME.*'     # by name
    brightness-get all              # one reading per line

A selector matching nothing is an error, as is a selector that resolves to a
display with no backend — an external panel on a machine without `m1ddc`, say.

## Knobs

Environment variables, overridable per call.

    brightness_display       main   default selector, when none is passed
    brightness_ddc_max       100    denominator for the 0..1 <-> luminance conversion
    brightness_ddc_retries   3      re-reads allowed for a corrupt DDC reading

`m1ddc` can report a panel's own ceiling, but that is an extra DDC round trip on
every call and virtually every monitor answers 100. `brightness-ddc-max` asks,
if you want to check yours and pin the variable.

## Blanking: "brightness 0" means two different things

    display-black-on     [sel]
    display-black-off              # no selector; restores exactly what it blanked
    display-black-toggle [sel]
    display-black-p                # is anything blanked?

The reason this is not just `brightness-set 0`:

    built-in   IOKit brightness 0 really does cut the backlight. The panel goes
               black and nothing else is needed.
    external   DDC luminance 0 is the *dimmest backlight setting*, not off. The
               panel stays visibly lit — a grey glow, not a dark screen.

So external panels get the image blacked in software with a zero gamma table
(`hs.screen:setGamma`), with DDC luminance and contrast floored underneath it so
what leaks through the backlight is as dark as the hardware allows. Built-in
panels are left alone gamma-wise; the backlight being off is enough, and not
touching their gamma keeps the working display's colour intact.

Neither is a power-off. The backlight on an external panel is still running, so
this saves nothing and still glows faintly in a dark room. `display-off` /
`displaysleep` (`pmset displaysleepnow`) remain the only real power saving, at
the cost of sleeping every display and waking on any keypress.

`display-black-on` records each display's prior brightness and contrast in redis
(`display_black_saved`), and `display-black-off` puts those exact values back —
so unlike the old fixed 0.435, you land where you started.

### hyper+shift+F1 / F2

`brightness-off` and `brightness-on` in `zshlang/auto-load/others/power.zsh` now
call `display-black-on` / `display-black-off`, so the existing bindings in
`hammerspoon/core/window-media-bindings.lua` needed no change and keep their
`caffeinate-on` behaviour — blank the screen, leave the machine running.

### If a screen is ever left black

`display-black-off` restores gamma unconditionally, before it looks at any saved
state, so running it blind is the fix. Failing that, from another machine or
blind-typed:

    hs -c 'hs.screen.restoreGamma()'

Unplugging and replugging the monitor also resets the gamma table.

## DDC reads are unreliable; writes are not

Roughly one read in thirteen came back corrupt on the setup this was written
against — a 27" 4K panel behind a USB-C hub. Pinned at 50, `m1ddc get luminance`
returned `-7` three times in forty tries, and **exited 0 every time**, so the
exit code is no help. The value being out of range is the only signal there is.

`brightness-get-ddc` therefore range-checks each reading and re-reads up to
`brightness_ddc_retries` times. Forty consecutive reads were clean afterwards.

Writes never misbehaved, and neither did `chg` — which is why
`brightness-inc-ddc` uses m1ddc's own `chg luminance` rather than a
get-then-set. It is one round trip instead of two, it does its own read
internally, and forty consecutive +1/-1 pairs landed back on exactly 50.

Do not assume a different monitor, cable or hub behaves the same; DDC/CI over
cheap hubs and HDMI adapters is where this class of tool usually fails. Some
monitors also ship with DDC/CI switched off in their OSD menu.

## Held-down brightness keys drift slightly

The Hammerspoon bindings fire `awaysh-fast brightness-dec` on every key repeat,
which backgrounds each one, so a held key issues overlapping read-modify-write
cycles and a few steps get lost. Ten *serial* calls are exact; ten racing ones
land a step or two short. This predates the DDC backend — get-then-set raced the
same way — and it does not matter much for a brightness key, which only has to
ramp in the right direction.

## Install

`m1ddc` is in `setup/brewables_mac`. To add it to a machine that lacks it:

    ensure-dep-m1ddc                # zshlang/basic/deps.zsh
    brew install m1ddc              # the same thing, by hand

Deliberately *not* auto-installed from `brightness-displays`: that function runs
on every brightness key repeat and on `brightness-auto-loop`'s 3-second cycle,
which is no place for a `brew install`. Without it, external panels simply
report backend `none`.

## Remember

After editing any of this, run `brishz-restart`. BrishGarden holds persistent
shells, so the Hammerspoon bindings keep running the old code until it does.
