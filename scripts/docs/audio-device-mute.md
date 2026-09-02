# Per-device audio mute

Reads and writes the mute flag of a *specific* audio output device, rather than
of whichever device happens to be the default right now.

`volume-mute-internal-p` is the reason this exists: "are the internal laptop
speakers muted?", answered correctly while you are wearing headphones.

## Why the existing predicate cannot answer it

macOS mute is per-device and persistent. `volume-mute-p`
(`zshlang/auto-load/others/system.zsh`) asks
`osascript -e "output muted of (get volume settings)"`, and that only ever
reports the **current default output device**. A representative moment, with a
monitor, some earbuds and the laptop all present (invented values):

    Some Monitor          uid=00000000-…-000000000000  muted=nil    DisplayPort
    Some Earbuds          uid=00-00-00-00-00-00:output muted=false  Bluetooth   ← default
    MacBook Air Speakers  uid=BuiltInSpeakerDevice     muted=true   Built-in

`volume-mute-p` says "not muted". The internal speakers are muted. Unplug the
earbuds and you land on silent speakers with nothing having reported it.

`audio-guard` already had to reach past the default device for the same reason;
`h-audio-guard-unmute-device` unmutes by name, and its comment spells out why.

## There is no system_profiler backend, and there cannot be

The usual slow-but-Hammerspoon-free fallback in this repo is
`system_profiler SPAudioDataType`, as in `h-audio-default-get`
(`zshlang/auto-load/others/monitor/monitor.zsh`). It is **not usable here**:
`SPAudioDataType` reports name, manufacturer, transport, sample rate and which
device is default, and no mute field at all. Verified against the JSON output;
do not go looking again.

That is why the second backend is CoreAudio rather than another parser.

## The two backends

`h-volume-mute-device-get-hs` — `hammerspoon -c` into
`audioDeviceMutedGet()` in `hammerspoon/core/audio-devices.lua`. About 10 ms
warm. Needs Hammerspoon running.

`h-volume-mute-device-get-swift` — `swift/audio_device_mute.swift`, a scriptisto
CoreAudio program. Slower, since it is a fresh process, but it needs nothing
running. Its *first* invocation after an edit compiles and takes seconds; every
call after that is cached by scriptisto.

`volume-mute-device-p` is the gateway: Hammerspoon first, CoreAudio if
Hammerspoon is unreachable. Only an unreachable backend falls through — "no such
device" and "no mute control" are answers, not failures, and asking the second
backend would only pay for the same no twice.

Each backend echoes exactly one word — `true`, `false`, `nodevice`, `nomute` —
and fails only when the backend itself could not run. That split is what makes
the fall-through rule expressible.

The Lua is a named function in a file rather than an inline expression because
`hammerspoon -c` hangs on payloads of a few hundred characters and takes the ipc
port down with it, the same trap documented on `hs-alert-v2`.

## Naming a device

`<device>` is one of:

- `builtin` — the internal speakers.
- a device UID, e.g. `BuiltInSpeakerDevice`.
- an exact device name, e.g. `MacBook Air Speakers`.

`builtin` resolves by the UID `BuiltInSpeakerDevice` first and by a Built-in
transport scan second, and **never by name**: the name is model dependent
("MacBook Air Speakers", "MacBook Pro Speakers", ...). Both backends implement
this identically, so they cannot disagree about which device `builtin` is.

Only devices with output streams are considered, or `builtin` would also match
the built-in *microphone*, which is a different device with its own mute flag.

## Exit status is three-valued

    0   muted
    1   not muted
    2   could not tell

`2` covers: no backend reachable, no such device, and a device with no mute
control at all — a DisplayPort monitor typically has none, which is also why
`h-audio-guard-mute` re-checks after every write.

This departs from the rest of the `-p` family on purpose. Folding "unknown" into
`1` would let a guard silently conclude the speakers are live when it has no idea
— and silently concluding the wrong thing is exactly the failure this whole
change exists to remove.

The cost is that a bare `if volume-mute-internal-p ; then` reads `2` as "not
muted", the way any shell conditional would. Callers that must distinguish have
to inspect `$?`. `volume-mute-device-toggle` does, and refuses to flip a device
whose state it cannot read rather than guessing.

## Commands

    volume-mute-internal-p              # 0 muted / 1 not / 2 unknown
    volume-mute-internal                # mute the laptop speakers
    volume-unmute-internal
    volume-mute-internal-toggle         # alerts; refuses to guess on unknown

    volume-mute-device-p <device>       # the generic layer
    volume-mute-device <device>         # volume_what_v=false to unmute
    volume-unmute-device <device>
    volume-mute-device-toggle <device>

    volume-mute-device-p-hs <device>    # pin a backend, for debugging
    volume-mute-device-p-swift <device>

    audio_device_mute.swift list        # uid, name, transport, muted

The setters report on the state *after* the write rather than on the request
being accepted. A device that ignores a mute ignores an unmute too, and calling
that success is the failure mode worth catching.

`volume-mute-internal-toggle` alerts under the `volume-mute-` id prefix
(`volume_mute_alert_id_prefix`) with an `internal` suffix, so its band rewrites
itself rather than stacking and does not collide with the `output` and `input`
bands or with `volumeInc`'s own `volume` id.

## Ground truth

To check any of the above without going through this code at all:

    hs -c 'local t={} for _,d in ipairs(hs.audiodevice.allOutputDevices()) do t[#t+1]=d:name().." "..tostring(d:uid()).." "..tostring(d:outputMuted()) end return table.concat(t,"\n")'

## See also

- `audio-guard.md` — the largest consumer of the mute API.
- `h-hammerspoon-eval` (`zshlang/auto-load/others/hammerspoon.zsh`) — strips the
  `-- Loading extension: …` chatter Hammerspoon interleaves into its output the
  first time an extension loads. Unfiltered, a result reads as
  `true-- Loading extension: task` and a success is read as a failure. This was
  open-coded twice before; both call sites now use the helper.
