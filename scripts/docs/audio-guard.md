# audio-guard

Mutes the output device when audio would be leaking into the shared office and
nobody is at the desk.

The laptop gets left at the desk at the office. If something is playing through
the built-in speakers, or through the DisplayPort monitor, it fills a shared
room with nobody there to stop it. The guard notices and mutes; the unlock hook
puts it back.

The social question itself is `office-public-audio-p` in
`zshlang/auto-load/others/monitor/location.zsh` — at the office and not on
headphones. `bell-auto` asks exactly the same question, and now shares the
predicate; see `bell-auto.md`.

The logic is `zshlang/auto-load/others/audio-guard.zsh`. The periodic half is a
LaunchAgent, installed from `launchers/audio-guard/`.

## Triggers

Three, each independently switchable. Only `idle` is enabled out of the box.

- `idle` — enabled. The LaunchAgent ticks every 10 minutes; after
  `audio_guard_idle_t` seconds of no HID input (default 3600) it mutes.
- `lock` — disabled. Mutes from `h-hook-lock` the moment the screen locks, which
  catches walking away in seconds rather than an hour.
- `headphones` — disabled. Mutes the moment the default output device changes to
  something leaky. This is the case the idle rule cannot catch at all: AirPods
  running out of battery or dropping their Bluetooth link mid-playback dumps
  everything into the speakers while idle is still zero.

Before enabling `headphones`, add a meeting guard. Headphones dropping mid-call
is precisely when you do not want the output muted, and `meeting-p`
(`luna-bells.zsh`) already exists to detect it.

## Commands

    audio-guard-status              # every input and what the guard believes
    audio-guard-enable lock         # no argument means all triggers
    audio-guard-disable             # bare = the master off switch
    audio-guard-auto headphones     # clear the override, back to the default
    audio-guard-toggle idle
    audio-guard-snooze 2h           # suppress everything, self-expiring
    audio-guard-unsnooze
    audio-guard-restore             # undo a mute the guard placed

Switches live in redis, so they survive restarts and are readable from any
shell, the same way `office-on` / `office-off` / `office-auto` work.

Snooze expires by itself on purpose. An off switch you have to remember to undo
is one you find still off next month.

## Ownership, and why restore is safe

The guard records the *name* of the device it muted. Restore is a no-op unless
that claim is held, so it can never unmute a mute you set yourself.

That is necessary but not sufficient, because the claim goes stale: the guard
mutes, you unmute by hand, you later mute again for your own reason — and the
claim still says the mute is the guard's. `audio-guard-reconcile` closes it. Any
time the claim is held but the device is observed unmuted, the claim is dropped,
because that is proof the mute is no longer ours. It only ever touches the
guard's own bookkeeping, never the mute state, so it is always safe to run.

It runs from two places. The tick reconciles every 10 minutes, which is the
backstop for a Hammerspoon that is down or reloading. `audio-watcher.lua`
reconciles within about a second, by watching the device's own mute property.

Watching the property rather than any one input path is what makes that general.
There is no useful zsh-side hook here: hyper+F10 is `volumeMuteKey` →
`systemKey("MUTE")`, a synthetic key event that never enters zsh, and the menu
bar and System Settings do not either. The CoreAudio property is what all of
them ultimately change. Verified by toggling mute with `osascript`, which
bypasses Hammerspoon entirely, and still receiving `mute(scope=outp)`.

The callback returns immediately unless the device is now *unmuted*: only an
unmute can invalidate a claim, so the guard's own mutes cost a string compare
rather than a garden round-trip. Muting also emits two `vmvc` (virtual main
volume change) events, which are filtered out for the same reason.

Reconciliation is not gated on any trigger, for the same reason restore is not:
a stale claim is a correctness problem regardless of which trigger created it.

Restore unmutes **by device name**, through Hammerspoon's `findOutputByName`,
rather than unmuting "the default device". macOS mute is per-device and
persistent: mute the speakers, switch to AirPods, restore the wrong one, and the
speakers stay silently muted until you next trip over them.

A mute is only claimed if it actually took effect. Not every device supports
software mute — the DisplayPort monitor here reports a nil volume and ignores
the request outright — and claiming a mute that never happened is worse than not
muting, because the next restore would unmute a device the guard never touched.

## Restore happens at unlock, not on the tick

`audio-guard-restore` is called from `h-hook-unlock`, and the tick deliberately
never unmutes. The tick is the path that fires most often, so it is the one
whose surprises are most expensive, and here it buys almost nothing:
`sysadminctl -screenLock status` reports **immediate** and `pmset -g` reports
`displaysleep 10`, so the screen has always locked at minute 10 long before the
60-minute idle threshold. Every mute the idle trigger places is followed by an
unlock.

The gap is `caffeinate-on`, which passes `caffeinate -d` and prevents display
sleep, so there is no lock and no unlock event. Covered by the notification and
a manual `audio-guard-restore`, or turn the backstop on with
`audio_guard_restore_at_tick_p=y`.

Restore is not gated on any trigger: a mute the guard placed must stay
reversible even if you have since turned off the trigger that placed it.

## Configuration

- `audio_guard_idle_t` — idle seconds before the idle trigger fires. Default 3600.
- `audio_guard_back_t` — idle seconds below which you count as back. Default 60.
- `audio_guard_notif_p` — notify on mute and restore. Default `y`.
- `audio_guard_snooze_default` — default snooze length. Default `2h`.
- `audio_guard_restore_at_tick_p` — restore from the tick too. Default `n`.

## The LaunchAgent

    ./launchers/audio-guard/install-audio-guard.zsh            # install, idempotent
    ./launchers/audio-guard/install-audio-guard.zsh --check
    ./launchers/audio-guard/install-audio-guard.zsh --uninstall

Label `com.user.audio-guard`, `StartInterval` 600, logging to
`~/logs/audio-guard.log`. No sudo: this is a per-user LaunchAgent, unlike the
LaunchDaemon in `launchers/pf/`. `StartInterval` does not fire while the machine
is asleep and coalesces to a single firing on wake, which is what we want.

The payload is `zshlang/wrappers/audio-guard-tick.dash`, which shells into
BrishGarden so the full zshlang environment is available, the same way
`rem-today-notify.dash` does.

Ten minutes rather than something tighter because the guard is already an hour
late by construction, so a worst-case extra ten minutes costs nothing.

## Hammerspoon, and not blocking its main thread

Hammerspoon runs Lua on the main thread, which is also its UI and event thread.
Anything blocking there freezes hotkeys, window management and every keystroke
for the duration. `core/redis.lua` documents a previous version of itself that
could freeze the machine for up to 50 minutes this way.

**Do not use `brishzeval2bg` from a watcher.** The name is misleading: the
trailing `&` backgrounds the command inside the garden, but Hammerspoon still
waits for the round-trip, because `brishzeval2` goes through `pipe_simple`
(`lua/pipe.lua`), which does blocking `posix.read` loops and then `posix.wait`.
Measured here with `hs.timer.absoluteTime`:

    brishzeval2bg("true")                                780.6 ms cold, 51.9 ms warm
    hs.task.new("brishz2.dash", nil, {"true"}):start()             1.5 ms

So `core/audio-watcher.lua` uses `hs.task`, which is genuinely asynchronous.
Alongside that:

- It returns immediately unless the event is `dOut`. The watcher fires for
  volume, mute and input events too, and a volume nudge must cost a string
  compare and nothing more.
- It debounces with `hs.timer.doAfter`. One device switch emits a burst of
  events; without coalescing, a single AirPods disconnect becomes several garden
  round-trips.
- It passes the device name and transport as arguments, so the zsh side never
  calls back into Hammerspoon to rediscover what the Lua callback already knew.
  That would be a subprocess spawned by a Hammerspoon callback making an IPC
  call into the very Hammerspoon that spawned it. `headphones-p` and
  `office-public-audio-p` therefore take optional `<name> <transport>`
  arguments, classified by `h-headphones-classify-p`.
- The periodic tick keeps Hammerspoon out of the loop entirely by resolving the
  device with `audio-output-get-system-profiler` instead. Its ~200 ms is free in
  a background job; a main-thread stall never is.
- The lock hook runs `awaysh-fast audio-guard-on-lock`, because `hammerspoon` is
  `gtimeout 30s hs -A -t 5` and a wedged Hammerspoon would otherwise stall
  locking the screen.

The watcher registers unconditionally and the zsh side checks the trigger.
Gating in Lua would mean reading redis from Hammerspoon, and `redisClient` in
`core/redis.lua` connects without auth and may legitimately be nil — trading
1.5 ms for a new failure mode. With the trigger off the residual cost is one
`hs.task` spawn per default-output-device change, a few times a day.

`hs.audiodevice.watcher` is a module-level singleton with one callback slot, not
a constructor. It was unused elsewhere when this was written; a second consumer
must chain onto the existing callback rather than call `setCallback` again,
which would silently replace it.

The mute watcher is a separate, *per-device* watcher, because
`hs.audiodevice.watcher` reports the device list and the default changing, not a
device's own mute property. Being bound to one device, it has to follow the
default around: `attachMuteWatcher` re-attaches whenever the default output
changes. The device is held in a global so it is not garbage collected, which
would silently stop the watcher.

## Gotchas

**`hs.task` does not inherit an interactive PATH.** It gets the bare launchd one,
`/usr/bin:/bin:/usr/sbin:/sbin`. `brishz.dash` shells out to `jq`, which lives in
`/opt/homebrew/bin`, so the task exits 22 with `brishz.dash: 41: jq: not found` —
and with a nil callback, which discards both streams, it fails completely
silently. `audio-watcher.lua` repairs `PATH` via `task:environment()` and
`setEnvironment` rather than replacing the environment wholesale, since brishz
also needs `HOME`. Any other `hs.task` caller that runs a brew-installed binary
needs the same treatment.

**The plist's PATH needs `/opt/homebrew/bin` too**, for the same family of
reason. The installer refuses to proceed if `dash` is not on the plist's PATH.

**There is no reliable "is audio playing" signal here.** Both candidates were
tested with nothing playing and both said yes:
`hs.audiodevice.defaultOutputDevice():inUse()` returned true, and
`pmset -g assertions` showed coreaudiod holding
`BuiltInSpeakerDevice.context.preventuseridlesleep` for 2h42m. So the guard does
not gate on playback. Muting a silent device is harmless and restore makes it
invisible.

**Wi-Fi is not a usable office signal on this machine.**
`networksetup -getairportnetwork en0` reports "not associated" while docked; the
default route is the USB LAN `en10`. `office-p-net`'s DNS and subnet fingerprint
is what works, and it already handles this.

**`office-p` includes an external-display fallback**, so the guard also fires at
home when docked to a monitor. Accepted rather than special-cased: restore makes
it cheap, and `office-off` overrides it.

**BrishGarden caches shells.** Run `brishz-restart` after editing
`audio-guard.zsh`, or the LaunchAgent keeps executing whatever the garden loaded
at startup. Testing in a fresh `zsh -ic` says nothing about what the job runs.

**No backticks in `:` docstrings.** They are double-quoted, so zsh runs command
substitution on them.
