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

    sony_battery_case_min           35    alert below this, for the case
    sony_battery_bud_min            15    alert below this, for a bud
    sony_battery_alert_dur          3     seconds the alert stays up
    sony_battery_charging_skip_p    y     ignore parts that are charging
    sony_battery_case_zero_skip_p   y     treat case 0% as "no reading"
    sony_battery_connect_delay      3     settle time before the connect hook reads

So a one-off with different limits:

    sony_battery_bud_min=30 sony-battery-alert-low

## Why the case reads 0%

Whenever the buds are out of the case, the case reports `level_percent: 0`.
That is the absence of a reading, not a reading of zero — and the `present`
flag is still true, so sonyctl cannot filter it out. Taken at face value it is
below any sane threshold, so the alert would fire on every single run forever.

`sony_battery_case_zero_skip_p` drops it. The cost is that a genuinely flat
case never warns, which is cheap: the buds still work, and a real number
appears the moment they dock. Set it to `n` to see the raw behaviour.

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
- In the alert, `+` means charging and `=` means charged.
