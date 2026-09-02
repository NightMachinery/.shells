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
