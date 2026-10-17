# headphones-battery

Warns, via a Hammerspoon alert, when Sony or Bose headphones are running low:
a Sony earbud or its case, or the battery of Bose over-ear headphones.

macOS has no battery reading for these at all. `ioreg -k BatteryPercent`, which
`bluetooth-batteries-darwin` in `zshlang/auto-load/others/monitor/bluetooth.zsh`
uses, only reports Apple HID devices; on this machine it lists the Magic
Keyboard and the Magic Trackpad and nothing else. The makers' own protocols are
the only source, so every reading costs a real Bluetooth session of roughly a
second:

- Sony: `sonyctl` (`~/code/misc/sonyctl`), over Sony's MDR protocol.
- Bose: `bosectl` (`~/code/misc/bosectl`), over Bose's BMAP protocol.

Both install to `~/.local/bin`.

## Where the code is

- `zshlang/auto-load/others/headphones-battery.zsh`: what the brands share.
  `h-headphones-battery-json` fetches the reading and
  `h-headphones-battery-alert-low` decides whether anything is low and raises
  the alert.
- `zshlang/auto-load/others/sony.zsh` and `bose.zsh`: each brand's commands,
  knobs and connect hook. They pass their knobs to the shared alert with
  `@opts`.

This split works because both tools print the same battery JSON:

    {"batteries":[{"part":"left","level_percent":80,"charging":"no"}, ...]}

`part` is `left`, `right`, `case` or `battery`; `charging` is `yes`, `no`,
`complete` or `unknown`. A new brand only needs a tool that prints this shape.

## Commands

    sony-battery                    # current levels, for a human
    sony-battery-alert-low          # alert iff something is low; else silent
    bose-battery
    bose-battery-alert-low

The `-alert-low` commands are the ones to schedule. They say nothing when the
headphones are fine, unreachable, or off, so they are safe to run unattended:

    30 * * * * $HOME/scripts/zshlang/wrappers/brishz/brishz.dash sony-battery-alert-low
    40 * * * * $HOME/scripts/zshlang/wrappers/brishz/brishz.dash bose-battery-alert-low

## Knobs

Every one is an environment variable, overridable per call.

Sony, with a `sony_battery_` prefix:

    sony_battery_case_min           35     alert below this, for the case
    sony_battery_bud_min            15     alert below this, for a bud
    sony_battery_alert_dur          15     seconds the alert stays up
    sony_battery_charging_skip_p    y      ignore parts that are charging
    sony_battery_alert_color        warn   band colour, amber by default
    sony_battery_connect_delay      3      settle time before the connect hook reads

Bose, with a `bose_battery_` prefix:

    bose_battery_min                15     alert below this
    bose_battery_alert_dur          15     seconds the alert stays up
    bose_battery_charging_skip_p    y      ignore the battery while charging
    bose_battery_alert_color        warn   band colour, amber by default
    bose_battery_connect_delay      3      settle time before the connect hook reads

So a one-off with different limits:

    sony_battery_bud_min=30 sony-battery-alert-low

## What the alert looks like

One line with every part on it, the low ones in bold and the rest dimmed:

    🎧 Sony battery  L 80%  R 12%  case NA
                                ^^^^^^ bold, on an amber band

    🎧 Bose battery  12%

It only appears when something is actually low; the parts that are fine are
there for context, not as a reason to interrupt you. In the levels, `+` means
charging and `=` means charged. Headphones with a single battery (Bose, a Sony
WH- model) show just the level, since the line already names them.

The emphasis uses `hs-alert-v2`'s `md` markup mode; see
`hammerspoon/alert/markup.lua`. The jq program emits `**bold**` around the
low parts and `[...]{dim}` around the rest.

Each brand raises its alerts under one id (`sony-battery`, `bose-battery`), so
the timer and the connect hook rewrite the same band instead of stacking two
that disagree about the level.

## `NA` means no reading, and it never counts as low

Two everyday situations leave a Sony part with no level at all. The earbuds
only learn the case's level while docked in it, so out of the case there is
nothing to report. And an earbud sitting in the case is invisible to the one
still in your ear.

The tools report those as `null`, which the alert renders as `NA`. Getting this
right is the whole reason the distinction exists in the tools rather than here:
`status` and every other consumer are fixed by the same change.

**The trap:** in jq, `null < 35` is **true**. A plain
`select(.level_percent < threshold)` would therefore treat every unknown part
as critically low, which is exactly the bug being avoided. The jq program
guards every comparison behind `known`, i.e. `.level_percent != null`. Anything
new that reads this JSON has to do the same.

## Bose does not report charging yet

`bosectl` reports `charging` as `unknown`: which byte of the battery reply
carries it has not been confirmed on a charging set. So
`bose_battery_charging_skip_p` has nothing to act on yet, and a Bose set that is
low will warn even while it charges.

## Two triggers, deliberately

A timer catches a battery draining while you are wearing the headphones. It
cannot catch the other case, reaching for headphones that are already nearly
flat, because by then you have already put them on.

So there is also a connect hook per brand, `sony-battery-on-audio-change` and
`bose-battery-on-audio-change`, consumers of `h-hook-audio-output-change`,
which fires when a device becomes the default output. Each filters on the
device name, so other headsets and speakers cost nothing but a string compare:
Sony's on its model names (`WF-`, `WH-`, `1000XM`), Bose's on the word `bose`,
which its headphones carry in their name out of the box. A Bose set renamed
without it is missed by the hook, though the timer still covers it.

Neither trigger subsumes the other. The hook needs no scheduling; the timer is
up to you.

## The audio-output-change hook

`h-hook-audio-output-change` lives in
`zshlang/auto-load/others/monitor/hooks.zsh` and is called by
`hammerspoon/core/audio-watcher.lua` with the new device's name and transport,
already debounced and filtered to `dOut` events.

The fan-out is on the zsh side for a specific reason: `hs.audiodevice.watcher`
is a module-level singleton with a single callback slot, and `audio-watcher.lua`
has claimed it. A second feature calling `setCallback` would silently replace
that callback and disable the audio guard. Adding a consumer means adding a line
to `h-hook-audio-output-change`, never touching the Lua.

Consumers run under `awaysh-fast`, so one that fails or blocks cannot stop the
others.

`zshlang/hooks/audio-output-change.zsh` is the external entry point, for callers
that can only run a file path. Hammerspoon does not need it and calls the
function directly.

After editing the zsh side, run `brishz-restart`: the persistent garden keeps
executing the old code otherwise. After editing the Lua, `hsr`.

## Notes

- Neither tool will wake the headphones to answer. A command on a disconnected
  device fails immediately with exit status 3 and leaves the link down;
  `--auto-connect` opts in. This is what makes polling viable: otherwise every
  tick would hold the headphones awake and steal the system's audio output. So
  an alert check while they are off or in the case costs about 40 ms and does
  nothing.
- Each control channel serves one session at a time, so a check that lands
  while you are running `sonyctl` or `bosectl` by hand may fail. The tools
  retry three times, then the check gives up silently.
- There is no throttling. At a ten-minute interval a low battery will warn six
  times an hour until charged; pick the interval accordingly, hourly being
  reasonable. If it does nag, the cheapest fix is a `redism setex` guard around
  the alert, in the manner of `audio_guard_snooze`.
