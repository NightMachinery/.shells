# sony-battery

Warns, via a Hammerspoon alert, when the Sony earbud case or a bud is running
low.

macOS has no battery reading for these at all. `ioreg -k BatteryPercent` — what
`bluetooth-batteries-darwin` in `zshlang/auto-load/others/monitor/bluetooth.zsh`
uses — only reports Apple HID devices; on this machine it lists the Magic
Keyboard and the Magic Trackpad and nothing else. Sony's protocol is the only
source, so every reading costs a real Bluetooth session of roughly a second.

The logic is `zshlang/auto-load/others/sony.zsh`, on top of `sonyctl`
(`~/code/misc/sonyctl`, installed to `~/.local/bin/sonyctl`).

## Commands

    sony-battery                    # current levels, for a human
    sony-battery-alert-low          # alert iff something is low; else silent

`sony-battery-alert-low` is the one to schedule. It says nothing when the
headphones are fine, unreachable, or off, so it is safe to run unattended:

    30 * * * * $HOME/scripts/zshlang/wrappers/brishz/brishz.dash sony-battery-alert-low

## Knobs

Every one is an environment variable with a `sony_battery_` prefix, overridable
per call.

    sony_battery_case_min           35     alert below this, for the case
    sony_battery_bud_min            15     alert below this, for a bud
    sony_battery_alert_dur          15     seconds the alert stays up
    sony_battery_charging_skip_p    y      ignore parts that are charging
    sony_battery_alert_color        warn   band colour, amber by default
    sony_battery_connect_delay      3      settle time before the connect hook reads

So a one-off with different limits:

    sony_battery_bud_min=30 sony-battery-alert-low

## What the alert looks like

One line with every part on it, the low ones in bold and the rest dimmed:

    🎧 Sony battery  L 80%  R 12%  case NA
                                ^^^^^^ bold, on an amber band

It only appears when something is actually low; the parts that are fine are
there for context, not as a reason to interrupt you. In the levels, `+` means
charging and `=` means charged.

The emphasis uses `hs-alert-v2`'s `md` markup mode — see
`hammerspoon/core/alert-engine.lua`. The jq program emits `**bold**` around the
low parts and `[...]{dim}` around the rest.

## `NA` means no reading, and it never counts as low

Two everyday situations leave a part with no level at all. The earbuds only
learn the case's level while docked in it, so out of the case there is nothing
to report. And an earbud sitting in the case is invisible to the one still in
your ear.

`sonyctl` reports those as `null`, which this script renders as `NA`. Getting
this right is the whole reason the distinction exists in sonyctl rather than
here: `status` and every other consumer are fixed by the same change.

**The trap:** in jq, `null < 35` is **true**. A plain
`select(.level_percent < threshold)` would therefore treat every unknown part
as critically low — which is exactly the bug being avoided. The jq program
guards every comparison behind `known`, i.e. `.level_percent != null`. Anything
new that reads this JSON has to do the same.

## Two triggers, deliberately

A timer catches a bud draining while you are wearing it. It cannot catch the
other case — reaching for headphones that are already nearly flat — because by
then you have already put them in.

So there is also a connect hook: `sony-battery-on-audio-change`, a consumer of
`h-hook-audio-output-change`, which fires when the earbuds become the default
output device. It filters on the device name, so other headsets and speakers
cost nothing but a string compare.

Neither subsumes the other. The hook needs no scheduling; the timer is up to
you.

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

After editing the zsh side, run `brishz-restart` — the persistent garden keeps
executing the old code otherwise. After editing the Lua, `hsr`.

## Notes

- `sonyctl` will not wake the headphones to answer. Any MDR command on a
  disconnected device fails immediately with exit status 3 and leaves the link
  down; `--auto-connect` opts in. This is what makes polling viable — otherwise
  every tick would hold the earbuds awake and steal the system's audio output.
  So an alert check while they are in the case costs about 40 ms and does
  nothing.
- Only one MDR session can be open at a time, so a check that lands while you
  are running `sonyctl` by hand may fail. It retries three times, then gives up
  silently.
- There is no throttling. At a ten-minute interval a low case will warn six
  times an hour until charged; pick the interval accordingly, hourly being
  reasonable. If it does nag, the cheapest fix is a `redism setex` guard around
  the alert, in the manner of `audio_guard_snooze`.
- A WH- model reports one battery rather than left/right/case; it is compared
  against `sony_battery_bud_min` and shown as `bat`.
