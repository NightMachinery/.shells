--- * Audio Output Watcher
--
-- Mute the speakers the instant the default output device changes to something
-- the whole office can hear -- typically AirPods dying or dropping their
-- Bluetooth link mid-playback, which is the loudest leak there is and the one
-- the idle-based guard cannot catch, because idle is zero when it happens.
--
-- The zsh side is audio-guard-on-audio-change, and it is DISABLED by default;
-- enable with `audio-guard-enable headphones'. See docs/audio-guard.md.
--
--- ** Why this file is so careful about blocking
--
-- Every garden call here goes through brishz_eval_hs (core/helpers.lua), which
-- is hs.task-based and asynchronous. It must not use the synchronous
-- brishz_eval, which blocks the main thread; the measurements and the reasoning
-- live with the helper.

audioWatcherDebounceTimer = nil
audioWatcherMuteDevice = nil

local DEBOUNCE = 1.0

-- A single device switch emits a burst of events; without coalescing, one
-- AirPods disconnect becomes several garden round-trips.
local function notifyAudioChanged()
    local device = hs.audiodevice.defaultOutputDevice()
    if not device then return end

    -- Pass the name and transport as arguments. The naive alternative lets the
    -- zsh side rediscover them via audio-output-get-hs, i.e. a subprocess we
    -- spawned calling IPC back into the very Hammerspoon that spawned it. We
    -- already know the answer here, so we hand it over.
    -- Dispatched through a general hook rather than straight to the audio
    -- guard: this watcher is a singleton with one callback slot (see the note
    -- at setCallback below), so a second feature cannot subscribe here. The zsh
    -- side fans out to consumers instead.
    brishz_eval_hs(("h-hook-audio-output-change %q %q"):format(
                    device:name() or "", device:transportType() or ""), "audio-watcher")

    -- The mute watcher below is bound to one specific device, so it has to
    -- follow the default around.
    attachMuteWatcher()
end

--- ** Ownership reconciliation
--
-- The guard remembers which device it muted, and that claim goes stale the
-- moment you unmute by hand: it mutes, you unmute, you later mute again for your
-- own reason, and a restore would clobber your mute. The tick reconciles every
-- 10 minutes; this closes the window to about a second.
--
-- Doing it here rather than in the zsh volume-unmute is what makes it general.
-- hyper+F10 is volumeMuteKey -> systemKey("MUTE"), a synthetic key event that
-- never enters zsh, and the menu bar and System Settings do not either. This
-- watcher listens to the CoreAudio property instead of to any one input method,
-- so it sees all of them: verified by toggling mute with osascript, which
-- bypasses Hammerspoon entirely, and still receiving mute(scope=outp).
--
-- Not gated on any trigger, for the same reason audio-guard-restore is not: a
-- stale claim is a correctness problem no matter which trigger created it.
local function deviceMuteCallback(uid, event, scope)
    -- Muting also emits vmvc (virtual main volume change), twice; we want only
    -- the mute property, on the output scope.
    if event ~= "mute" or scope ~= "outp" then return end

    local device = audioWatcherMuteDevice
    if not device then return end

    -- Only an unmute can invalidate a claim. Returning here on our own mutes
    -- keeps this from spawning a garden round-trip every time the guard fires.
    if device:outputMuted() then return end

    brishz_eval_hs("audio-guard-reconcile", "audio-watcher")
end

-- Per-device rather than global: hs.audiodevice.watcher reports the device list
-- and the default changing, not a device's own mute property.
function attachMuteWatcher()
    if audioWatcherMuteDevice then
        audioWatcherMuteDevice:watcherStop()
        audioWatcherMuteDevice = nil
    end

    local device = hs.audiodevice.defaultOutputDevice()
    if not device then return end

    -- Kept in a global so it is not garbage collected, which would silently stop
    -- the watcher.
    audioWatcherMuteDevice = device
    device:watcherCallback(deviceMuteCallback)
    device:watcherStart()
end

local function audioDeviceCallback(event)
    -- The watcher fires for volume, mute, input and output events alike. A
    -- volume nudge must cost a string compare and nothing more.
    if event ~= "dOut" then return end

    if audioWatcherDebounceTimer then
        audioWatcherDebounceTimer:stop()
    end
    audioWatcherDebounceTimer = hs.timer.doAfter(DEBOUNCE, notifyAudioChanged)
end

-- hs.audiodevice.watcher is a MODULE-LEVEL SINGLETON with one callback slot,
-- not a constructor. It was unused elsewhere in this config when this file was
-- written, so we may claim it -- but a second consumer must chain onto this
-- callback rather than call setCallback again, which would silently replace it.
hs.audiodevice.watcher.setCallback(audioDeviceCallback)
hs.audiodevice.watcher.start()

attachMuteWatcher()

-- Registered unconditionally, with the trigger check left to the zsh side.
-- Gating here would mean reading Redis from Hammerspoon, and redisClient in
-- core/redis.lua connects without auth and may legitimately be nil -- trading
-- 1.5 ms for a new failure mode. While the trigger is off the residual cost is
-- one hs.task spawn per default-output-device change: a few times a day, not
-- per event, thanks to the filter and the debounce above.
