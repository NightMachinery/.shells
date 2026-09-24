--- * Per-device audio mute
--
-- macOS mute is per-device and persistent, so `the default output device is not
-- muted' says nothing about the laptop speakers while you are on headphones.
-- These two answer for a NAMED device instead. The zsh side is
-- [agfi:volume-mute-device-p] / [agfi:volume-mute-internal-p]; see
-- docs/audio-device-mute.md.
--
--- ** Why this is a file and not an inline `hammerspoon -c' string
--
-- hammerspoon -c hangs on payloads of a few hundred characters and takes the
-- ipc port down with it until the client is killed -- the same trap documented
-- at [agfi:hs-alert-v2]. The resolution logic below is well past that limit,
-- while `audioDeviceMutedGet([[builtin]])' is about thirty characters on the
-- wire. audio-guard.zsh gets away with an inline string because its one is
-- short; do not grow it.

-- `builtin' first by UID, then by transport. The UID is stable across Apple
-- Silicon Macs and the transport scan is the safety net; the NAME is model
-- dependent ("MacBook Air Speakers", "MacBook Pro Speakers", ...) and is
-- deliberately never matched against here. Kept in step with findDevice() in
-- swift/audio_device_mute.swift, the other backend of the same predicate.
local BUILTIN_SPEAKER_UID = "BuiltInSpeakerDevice"

-- Output devices only, or `builtin' also matches the built-in MICROPHONE, a
-- different device with its own mute flag.
local function findBuiltinOutput()
    local byUID = hs.audiodevice.findDeviceByUID(BUILTIN_SPEAKER_UID)
    if byUID and byUID:isOutputDevice() then return byUID end

    for _, d in ipairs(hs.audiodevice.allOutputDevices()) do
        if d:transportType() == "Built-in" then return d end
    end
end

-- spec: "builtin", a device UID, or an exact device name.
local function findOutput(spec)
    if spec == "builtin" then return findBuiltinOutput() end

    local byUID = hs.audiodevice.findDeviceByUID(spec)
    if byUID and byUID:isOutputDevice() then return byUID end

    return hs.audiodevice.findOutputByName(spec)
end

-- Returns "true", "false", "nodevice", or "nomute", always as a STRING: the
-- caller reads it over ipc, where a Lua nil is indistinguishable from an empty
-- line, and where `hs' exits 0 whether or not we found anything. So the result
-- word is the only thing that carries the outcome.
function audioDeviceMutedGet(spec)
    local d = findOutput(spec)
    if not d then return "nodevice" end

    local muted = d:outputMuted()
    if muted == nil then return "nomute" end

    return tostring(muted)
end

function audioDeviceMutedSet(spec, muted)
    local d = findOutput(spec)
    if not d then return "nodevice" end

    d:setOutputMuted(muted)

    -- The state AFTER the write, not the state we asked for: a DisplayPort
    -- monitor accepts the request and ignores it, and reporting that as success
    -- is the failure worth catching. h-audio-guard-mute re-checks for the same
    -- reason.
    local now = d:outputMuted()
    if now == nil then return "nomute" end

    return tostring(now)
end

--- * Default audio input
--
-- The zsh side is [agfi:audio-input-switch] and [agfi:audio-input-glyph-get];
-- see docs/audio-input-switch.md. Policy (which device counts as the iPhone,
-- what `builtin' means, which glyph to show) lives in zsh. These only report
-- and act, and they are named functions for the same `hammerspoon -c' payload
-- limit described above.

-- One line per input device: name, transport, UID, tab separated. hs reports a
-- Continuity (iPhone) microphone's transport as "UNKNOWN", since it has no
-- name for that CoreAudio transport type.
function audioInputDevicesGet()
    local lines = {}
    for _, d in ipairs(hs.audiodevice.allInputDevices()) do
        lines[#lines + 1] = table.concat(
            {d:name() or "", d:transportType() or "", d:uid() or ""}, "\t")
    end
    return table.concat(lines, "\n")
end

-- Returns "ok", "nodevice", or "failed". The default is re-read after the
-- write rather than trusting setDefaultInputDevice's boolean.
function audioInputDefaultSetByName(name)
    local d = hs.audiodevice.findInputByName(name)
    if not d then return "nodevice" end

    d:setDefaultInputDevice()

    local now = hs.audiodevice.defaultInputDevice()
    if now and now:uid() == d:uid() then return "ok" end
    return "failed"
end

-- name, transport, muted, volume: one per line. muted is "true", "false", or
-- "nomute" (a Continuity microphone has no mute control); volume is a number
-- or "novolume".
function audioInputStateGet()
    local d = hs.audiodevice.defaultInputDevice()
    if not d then return "" end

    local muted = d:inputMuted()
    local volume = d:inputVolume()
    return table.concat({
        d:name() or "",
        d:transportType() or "",
        muted == nil and "nomute" or tostring(muted),
        volume == nil and "novolume" or tostring(math.floor(volume + 0.5)),
    }, "\n")
end
