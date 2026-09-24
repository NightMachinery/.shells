# Switching the default microphone

Commands to pick the system default input device, and a one-symbol indicator of
it at the left of the xbar date plugin (`zshlang/menubar/date.sh`). The code is
in `zshlang/auto-load/others/monitor/monitor.zsh`, next to
[agfi:audio-input-get].

## Why this exists: the lid disconnects the built-in microphone

On Apple Silicon and T2 Macs, closing the lid disconnects the built-in
microphone in hardware (Apple's "hardware microphone disconnect", a privacy
feature). No software can re-enable it. The device stays listed and can stay
the default input, so recordings "succeed" and contain digital silence: measured
with sox `stat` and ffmpeg `astats`, a closed-lid built-in mic gives an RMS of
exactly 0 and -inf dB. [[file:stt-input-device.md]] hit the same trap from the
STT side.

So with the lid shut, another input is needed. An iPhone on the same Apple ID
appears as a Continuity microphone named "<phone name> Microphone".

## Commands

`<spec>` is `builtin`, `iphone`, a device UID, or an exact device name, as
printed by [agfi:audio-input-list].

- [agfi:audio-input-switch] `<spec>` makes it the default, prints the new
  device, warns when you pick `builtin` while the lid is shut, and refreshes the
  menubar.
- [agfi:audio-input-toggle] `<spec> [<other>]` flips between the two; `<other>`
  defaults to `builtin`.
- [agfi:audio-input-p] `<spec>` tests whether that is the current default.
- `iphone-mic-on`, `iphone-mic-off`, `iphone-mic-toggle` and `iphone-mic-p`
  are these, fixed to `iphone`.
- [agfi:clamshell-p] reports whether the lid is closed (`ioreg`'s
  `AppleClamshellState`).

`builtin` and `iphone` are matched by transport, never by name. The built-in
device's name depends on the model, and the iPhone's name is personal, so it
stays out of this public repository. Shortcuts named after a specific phone
belong in `~/.privateShell` as thin wrappers around `iphone-mic-*`.

### How the iPhone is recognised

CoreAudio gives a Continuity microphone a transport type that neither
Hammerspoon nor `system_profiler` has a name for. Hammerspoon reports it as
`UNKNOWN`, and `system_profiler -json` as `coreaudio_device_type_unknown`.
[agfi:h-audio-input-kind-classify] treats an unknown transport, or a name
containing "iphone", as the iPhone. A different device that also has an
unknown transport would be misclassified. None has turned up yet.

## Backends

Like the rest of the audio helpers, each operation has a fast Hammerspoon
backend and a slower one that works without it, plus a gateway that tries them
in that order.

- Listing: [agfi:audio-input-devices-get-hs] (the named Lua function
  `audioInputDevicesGet` in `hammerspoon/core/audio-devices.lua`) and
  [agfi:audio-input-devices-get-system-profiler].
- Switching: [agfi:audio-input-switch-hs] (Lua `audioInputDefaultSetByName`)
  and [agfi:audio-input-switch-sas], which uses `SwitchAudioSource` from
  Homebrew's `switchaudio-osx` ([agfi:ensure-dep-switchaudio] installs it).
  [agfi:audio-input-switch-darwin] is the gateway. Both backends re-read the
  default after writing it, rather than trusting their own return value.
- State: [agfi:audio-input-state-get-hs] (Lua `audioInputStateGet`), falling back
  to [agfi:audio-input-get-system-profiler], which has no mute information.

The Lua side lives in named functions because `hammerspoon -c` hangs on long
payloads (see the header of `audio-devices.lua`). A device name travels inside
a Lua long string, so a name containing `]]` is refused.

## The menubar symbol

[agfi:audio-input-glyph-get] prints one symbol, chosen from the associative
array `audio_input_glyphs` by the device kind: `builtin`, `builtin-clamshell`
(the lid is shut, so this mic records silence), `bluetooth`, `iphone`, `other`,
`none`. A muted device uses `<kind>-muted` if that key exists, and `muted`
otherwise. A muted built-in mic shows as muted even with the lid shut, because
muting is the deliberate state, and it is what a soft mute leaves behind. A mic counts as muted when CoreAudio says
so, or when its input volume is 0, which is how the osascript mute backend in
`system.zsh` mutes. To change a symbol, set the array entry after the library
loads.

xbar does not render ANSI escapes in the menubar title (tested: the escape
codes show up as literal text), so "muted" cannot be drawn as dimming or
strikethrough. It has to be a symbol of its own.

### Keeping it current

The plugin reruns every minute. Three things refresh it sooner, through
[agfi:menubar-refresh], which opens xbar's `refreshPlugin` URL:

- [agfi:audio-input-switch-darwin], after every switch.
- [agfi:input-volume-mute-toggle], the mute hotkey.
- [agfi:h-hook-audio-input-change], called by `hammerspoon/core/audio-watcher.lua`
  whenever the default input changes by any route (System Settings, a raw
  `SwitchAudioSource`). The watcher's event code for this is `"dIn "`, with a
  trailing space. It was first written as `"dIn"` and never fired. A temporary
  callback that logged the raw event strings is what exposed this.

Other mute changes, such as the System Settings slider, still wait for the
next minute.

## Soft mute: muting a mic that has no mute control

The Continuity microphone exposes neither mute nor input volume. A CoreAudio
probe found no mute or volume property on its main element or on any channel
(the built-in mic has both, writable). Hammerspoon returns nil for both, and
osascript's `input volume` reads `missing value` and ignores writes.

So hyper+F5 ([agfi:input-volume-mute-toggle]) soft-mutes such a mic instead:

1. [agfi:audio-input-soft-mute] records the current mic and the built-in mic's
   mute state in Redis (`input_soft_mute_device` and
   `input_soft_mute_builtin_was_muted`).
2. It mutes the built-in mic, then switches to it. Muting first means the
   built-in mic is never the live default, even for a moment, and the mute
   flag holds if the lid is opened later.
3. The next press ([agfi:audio-input-soft-unmute]) switches back first, then
   restores the built-in mic's own mute state.

The claim is dropped, and the built-in mic's mute state restored, as soon as
the default changes by any other route: an explicit [agfi:audio-input-switch]
(including `iphone-mic-off`, which therefore gives you a live built-in mic),
or any switch that [agfi:h-hook-audio-input-change] sees. Soft mute's own
switches pass `audio_input_switch_keep_soft_mute_p` so that they do not cancel
themselves. If the device to go back to has vanished, you stay on the muted
built-in mic and the next press unmutes it normally.

## Limitations

- Soft mute changes the system default only. An app that opened the iPhone mic
  by name keeps hearing it: a recording already in progress (the STT recorders
  pick their device by name when they start), or a meeting app set to a
  specific mic rather than the system default.
- On 2026-09-24 the iPhone microphone was listed and could be selected, but it
  recorded digital silence, both from a terminal and from inside Hammerspoon
  (which already has microphone permission for STT). The phone itself was not
  inspected, so the cause is untested.
- Linux: [agfi:audio-input-switch] is not implemented there.
