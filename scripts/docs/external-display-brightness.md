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
    display-black-off    [sel]     # bare: restores everything that was blanked
    display-black-toggle [sel]
    display-black-p                # is anything blanked?

Each of those, plus `brightness-off` / `brightness-on`, has `-main`, `-all`,
`-internal` and `-external` suffixed forms:

    display-black-on-all
    display-black-toggle-external
    brightness-off-all

Every one of those also has a `-loop` version, suffixed last
(`display-black-on-all-loop`); see "Keeping it blank" below.

Only this family gets them. `brightness-get-internal` and `brightness-get-ddc`
already exist as *backend* helpers taking a display index, and `brightness-set`
takes its value first, so `brightness-set-all 0.5` would put the selector where
the value goes.

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

It is idempotent: run on a display that is already blanked, it re-applies the
zeros and the gamma table but keeps the levels it remembered the first time. It
has to be, because a blanked display reads back as 0 — remembering *that* would
make `display-black-off` "restore" the screen to black. As a side effect, the
repeat call skips the DDC read entirely, which is the slow and unreliable half
of the operation. Rows for displays outside the selector are kept rather than
overwritten, so blanking the internal panel after the external one does not
forget how to restore the external one.

Restoring a subset works: `display-black-off internal` un-blanks the laptop and
leaves the monitor black. That needs a little care, because `hs.screen.restoreGamma()`
is global — so anything still meant to be blanked has its gamma re-applied
afterwards. Whether a display was gamma'd is the last field of the saved state.

### Keeping it blank

A one-shot blackout does not stay. macOS restores gamma and brightness on wake,
on a display reconfiguration, and whenever a DDC write is lost, so the screen
quietly comes back. Every function above therefore has a `-loop` version, with
the suffix last:

    lo_s=30 display-black-on-all-loop     # blank now, re-assert every 30s
    display-black-off-all-loop            # stop the loop, restore the levels
    display-black-toggle-all-loop
    display-black-loop-p                  # is the loop running?

`lo_s` is the interval in seconds and defaults to 5. It is read when the loop is
started, so changing it means restarting: `display-black-on-loop` kills any
existing loop first, and there is only ever one.

The loop is a background subshell whose argv is marked `DBLACK_LOOP_MARKER`
(`awaysh-bnamed`, so it runs in the brish garden and outlives the terminal that
started it), and stopping it is `kill-marker` plus a final `display-black-off`.
See the mark-me pattern in `PE/Zsh.org`. To check on it or kill it by hand:

    pgrep -fl DBLACK_LOOP_MARKER
    kill-marker DBLACK_LOOP_MARKER

The loop body is just `display-black-on`, so a display plugged in while the loop
is running gets blanked on the next tick, and raising the brightness by hand is
undone within `lo_s` seconds.

### Waking always ends it

Sleep does not stop the loop, it freezes it. Without help you would wake to a
black panel, re-blacked every `lo_s` seconds, at a login screen where the
brightness keys can no longer win — and the way to get there is ordinary:
blank the screen, then close the lid.

Closing the lid sleeps the machine no matter what we assert. That is the
clamshell path, and `caffeinate` only creates idle-sleep assertions; see
`caffeinate.md`. With only the built-in panel attached, blanking it before
shutting the lid therefore gains nothing and costs the wake.

So `h-blackout-release` ends the blackout, and is called from two deliberately
independent places:

- `h-hook-wake`, fired by `hammerspoon/core/power-watcher.lua` on
  `systemDidWake` and `screensDidWake`.
- `h-hook-unlock`, fired by the Swift `lock-watcher` — no Hammerspoon involved,
  so a config that failed to load still leaves a usable screen.

It returns immediately when nothing is blanked, which matters because it runs on
every wake and every unlock and `display-black-off` always calls out to
Hammerspoon. When nothing is blanked it still releases the `blackout` caffeinate
key, since that key can outlive its blackout when the garden restarts.

The reverse order was always safe: with the lid already shut, `brightness -l`
does not report the built-in panel at all, so `-all` cannot blank it.

### hyper+shift+F1 / F2

`brightness-off` and `brightness-on` in `zshlang/auto-load/others/power.zsh` call
`display-black-on` / `display-black-off` and pass a selector straight through, so
they keep their `caffeinate-on` behaviour — blank the screen, leave the machine
running.

The bindings in `hammerspoon/core/window-media-bindings.lua` use the `-all`
forms. Blanking only the main display leaves the other screen lit, which defeats
the point with the lid open.

They also use the `-loop` forms — `brightness-off-all-loop` and
`brightness-on-all-loop` — so F1 starts the keep-blank loop and F2 stops it and
restores. `brightness-off-loop` keeps the `caffeinate-on` of its one-shot
sibling, which is the reason the keys go through `power.zsh` at all rather than
calling `display-black-on-all-loop` directly.

That assertion is held under the key `blackout` and released by F2, so it cannot
switch off a `caffeinate-on` something else is relying on, and it no longer
outlives the blackout the way it used to. See `caffeinate.md`.

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
