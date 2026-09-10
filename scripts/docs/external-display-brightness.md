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

These names are generated rather than written out, so grepping the source for
`brightness-off-all-loop` finds only its callers and never a definition. Two
nested `h_aliasfn` loops build them from the base name and the selector — one
in `system.zsh` for the `display-black-*` family, one in `power.zsh` for
`brightness-off` / `brightness-on` — which means the assembled string appears
nowhere in the repository. Ask the shell instead of grep:

    whence -w brightness-off-all-loop     # -> shell function
    agfi brightness-off-loop              # the real body it forwards to

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
started it), and stopping it is `kill-marker`, a short wait, then a final
`display-black-off`. The wait is not ceremony: `kill-marker` goes through
`kill-withchildren`, so the loop's own children do die with it, but a
grandchild spawned while it was enumerating can outlive the kill and an
iteration killed midway may still be holding a slow DDC write. Either one
lands *after* the restore, re-blanking the screen or writing a blanked reading
over the remembered levels, so the teardown waits for `display-black-loop-p` to
go quiet rather than trusting the kill. A kill that worked leaves on the first
check.
See the mark-me pattern in `PE/Zsh.org`. To check on it or kill it by hand:

    pgrep -fl DBLACK_LOOP_MARKER
    kill-marker DBLACK_LOOP_MARKER

The loop body is just `display-black-on`, so a display plugged in while the loop
is running gets blanked on the next tick, and raising the brightness by hand is
undone within `lo_s` seconds.

### Why a remembered level is never zero

`display-black-on` prefers a level it already remembers over a fresh reading,
which is what lets the loop re-assert a blackout every few seconds without
forgetting where the brightness started: a blanked display reads back as 0, and
remembering *that* would make `display-black-off` "restore" the screen to black.

The same trap reaches past the loop, though. `display-black-off` deletes the
remembered row when it restores, so a blackout started again straight after one
ends finds nothing remembered and falls through to a fresh reading — taken
while the restore it just issued has not landed, because DDC writes are slow.
Pressing F1 again right after F2 hits that window exactly, and each round of
on-off ratchets the level down a little further. That was a real bug, and it is
why `h-display-black-level-usable` refuses to remember a reading of zero in any
of its spellings, recording `-` (unknown) instead. `display-black-off` leaves an
unknown level alone, so the worst case became a brightness that did not change
rather than one that walks towards black.

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

F1 also locks the keyboard. A black screen on its own is not a safe one: the
focused window still has focus, so a brushed key types into it unseen, and the
hardware brightness keys undo the blackout from the inside. So while the screen
is black, `hammerspoon/core/blackout-lock.lua` swallows every key, and by default
every click and scroll, with two exceptions — the hyper key itself and F2 with
shift under hyper, so the way out is exactly the way out it was. The knobs
(`blackoutLockEnabled`, `blackoutLockMouse`, `blackoutLockScreenAfterSeconds`),
the shell interface and the limits are in `hammerspoon/docs/hammerspoon.md`
under "Blackout keyboard lock". The one worth repeating here is Secure Input: a
password field or the login screen hides keystrokes from event taps, so the lock
cannot block typing there.

A blackout that has been up for more than an hour is one nobody is watching, so
F2 then locks the macOS session before it restores the display — whoever ends
it meets the login screen, not the desktop. hyper+shift+cmd+F1 starts a blackout
that does the same regardless of age, for when you want the lock now; the
choice belongs to whoever starts the black, not whoever ends it, because the
hand that presses F2 later may not be yours. Both end through `blackoutRestore`
in Hammerspoon, and both survive a Hammerspoon reload — the start time and the
lock-first mark are kept in redis while the screen is black. A bare
`display-black-off` or `brightness-on-all-loop` from a shell restores the
display without locking the session, because a shell restore is the owner
acting from ssh, not a hand at the keyboard.

The lock can never outlive the black. `display-black-off` calls `blackoutLockOff`
over `hammerspoon -c` immediately after its unconditional gamma restore, and it
is the single point every unblack path reaches — F2, `h-hook-wake`, `h-hook-unlock`
from the Swift lock-watcher, or the function run bare from another machine. The
wake watcher releases it independently as well, and after a week it ends on its
own as a last resort — locking the session first, then restoring, so the worst
case is a login screen and never a live keyboard on an unlocked desktop behind a
black screen. Whichever way the screen comes back, the keyboard comes back with
it.

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

## Concurrent DDC access, and the lock

This section used to say that a held key "lands a step or two short" and that it
"does not matter much for a brightness key, which only has to ramp in the right
direction". Both halves were wrong, and measuring them is what found the bug.

The bindings fired `awaysh-fast brightness-dec` on every key repeat, and
`awaysh1` is `( insubshell-eval "$cmd" &| ) &|` — doubly detached, with no
serialisation of any kind. A repeat arrives roughly every 30ms and one m1ddc
round trip costs 200–380ms here, so a held key ran about ten overlapping
read-modify-write cycles at once — and `chg` is a read-modify-write m1ddc
performs internally. Ten at once, measured:

    parallel, unlocked    0.99 -> 1.00    ten decrements, one step BRIGHTER
    sequential            0.99 -> 0.91    eight of ten landed

Not a step or two short: arbitrary, and able to move the level the opposite way
from the key being pressed. It is also the honest explanation for "the
brightness keys sometimes do nothing".

It was never only the keys. Four writers reach the same bus independently — the
key repeat, `display-black-on-loop`'s re-assert every `lo_s` seconds,
`brightness-auto-loop`'s 3-second cycle, and `display-black-off`'s restore —
and none of them took a lock, so any two that overlapped could corrupt each
other. The blackout loop racing the restore is the same shape as "why a
remembered level is never zero" above, one layer down.

So every per-panel DDC access now goes through `h-m1ddc`, which holds a
per-display `zsystem flock` (`h-ddc-lock-do`). With it, ten racing decrements
land ten of ten.

`zsystem flock` rather than redis because the lock must be released when its
holder dies, which a file descriptor does for free and a redis key with a TTL
cannot: too short an expiry silently restores the two-writer bug, too long
wedges the brightness keys until it lapses. It is built into zsh, so unlike
`flock(1)` — homebrew-only on darwin — it can never be the missing dependency.
Only `m1ddc display list` stays outside the lock, since it takes no display
number.

A lock on its own would turn a one-second key hold into twenty seconds of
queue, at ~600ms per locked `chg`, so the presses are coalesced at the source
as well.

## The brightness band

hyper+F1/F2 put an alert-v2 band on every screen showing where the level
landed. The coalescing above had to be written regardless, and once Hammerspoon
is accumulating the delta it may as well say what it did.

The dispatch keeps exactly one garden call in flight. Presses arriving during a
flight accumulate and leave as a single larger delta, so the total always
matches what was pressed while the number of DDC round trips stays proportional
to time rather than to keystrokes. Ten presses measured as two calls — an
immediate `brightness-inc -0.0100`, then `brightness-inc -0.0900` for the other
nine — landing exactly -0.10.

The call is `brightness-inc <delta> ; brightness-get`, so the reply carries the
new level and the band never needs a read of its own. Between replies the band
shows the level stepped by the same delta the shell is applying, which is what
makes it feel immediate: a reply is ~600ms behind the press.

That cached level is trusted only for `hyper_brightness_trust_seconds` — three
by default, matched to `brightness-auto-loop`'s cycle, the fastest of the other
writers. Past that the band shows an ellipsis instead of a number, because
being briefly uninformative beats being briefly wrong, and the monitor's own
buttons cannot be observed at all. So the first press of a burst may show `…`
for one frame before the true value arrives.

The band is raised with `peek = false`. Holding hyper normally fades every
alert band to a whisper so it stops covering whatever you are about to act on —
but these keys deliberately leave hyper entered so the level can be stepped
repeatedly, which meant the peek faded the one band the keypress exists to
show. See "Bands that must not fade" in `hammerspoon/docs/hammerspoon.md`.

The knobs are globals in the usual `x = x or default` style:
`hyper_brightness_step` (0.01), `hyper_brightness_band_seconds` (1.5),
`hyper_brightness_bar_cells` (20) and `hyper_brightness_trust_seconds` (3).

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
