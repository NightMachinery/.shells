# STT input device selection

The voice-recording hotkeys (F11/F12, F1/F2, hyper-`.`, hyper-`'`) record from
the **system default input device**, resolved fresh on every keypress. This
note explains why it has to be resolved rather than configured, and what the
remaining caveats are.

## Why not `-i ":0"`

`hammerspoon/core/stt.lua` used to record with:

```
ffmpeg -f avfoundation -i ":0" ...
```

The comment on that line claimed `:0` was the default audio input device. It is
not. `:0` is an AVFoundation device **index**, and AVFoundation enumerates in
its own order, which has nothing to do with the macOS default. On this machine:

```
AVFoundation audio devices:   [0] MacBook Air Microphone
                              [1] WF-1000XM6
system default input:         WF-1000XM6 (Bluetooth)
```

So every recording came from the built-in mic regardless of what macOS was set
to. With the laptop lid closed that mic is not merely quiet — measured with
`ffmpeg -af astats`, it returns **-inf dB, digital silence**. The failure was
invisible: you got an empty or hallucinated transcript with no indication that
the wrong microphone had been used.

Hardcoding `:1` instead would not fix it. Indices renumber as devices connect
and disconnect, so the correct index changes the moment the earbuds are
unpaired.

## Selecting by name

AVFoundation's `-i` accepts a **device name** in place of the index, so the
device is resolved at record time instead:

```
ffmpeg -f avfoundation -i ":WF-1000XM6" ...
```

The name comes from `hs.audiodevice.defaultInputDevice():name()`, which is an
in-process CoreAudio call — no subprocess, no measurable latency added to the
hotkey. The alternative, parsing `ffmpeg -f avfoundation -list_devices true`
for an index, would cost roughly half a second per keypress and could not be
cached, since indices shift.

CoreAudio's device name and AVFoundation's `localizedName` are the same string
in practice, which is what makes this work.

Two entry points do this, each with its own resolver because one runs in-process
and the other from the shell:

- `whisper.getInputDeviceSpec` in `hammerspoon/core/stt.lua`, used by
  `whisper.getRecordCommand`
- [agfi:audio-input-get], used by [agfi:ffmpeg-record]

The zsh side mirrors the existing output family: [agfi:audio-input-get-hs] asks
the running Hammerspoon (fast), [agfi:audio-input-get-system-profiler] parses
`system_profiler SPAudioDataType` (slow, but works with Hammerspoon down), and
[agfi:audio-input-get] is the gateway that tries one then the other. Both
families share the `h-audio-default-get*` helpers, parameterised by
`audio_default_what` (`input` or `output`).

[agfi:sox-record] and the `sox-rec` recorder mode need no such handling: sox's
CoreAudio driver opens the system default input on its own.

## Caveats

A device whose name contains a colon cannot be selected this way. ffmpeg parses
`-i` as `[video]:[audio]`, so such a name is silently mis-parsed rather than
rejected. Both resolvers refuse a name containing a colon and fall back to `:0`
with a visible warning, on the grounds that a loud wrong answer beats a quiet
one.

If two input devices share a name, AVFoundation picks the first. Rename one in
Audio MIDI Setup if this ever matters.

## Which mic am I on?

Recording start briefly shows the resolved device name. This is not only for
debugging. Activating a Bluetooth headset's microphone forces macOS into
HFP/SCO, which collapses that headset's **playback** to narrowband for as long
as the mic is live — so anything you are listening to audibly degrades while
you dictate. Following the system default is still the right behaviour, but the
choice should be visible at the time rather than inferred later from a bad
transcript.

To change which microphone is used, change the macOS default input (System
Settings, or the sound menu with Option held). There is no separate setting to
keep in sync.

## Related

- `docs/audio-guard.md` — the output-side counterpart, which uses
  [agfi:audio-output-get]
- `hammerspoon/docs/hammerspoon.md` — module layout and load order
