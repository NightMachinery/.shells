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
-- Hammerspoon runs Lua on the main thread, which is also its UI and event
-- thread. Anything blocking here freezes hotkeys, window management and every
-- keystroke for the duration. Compare core/redis.lua, which documents a
-- previous version that could freeze the machine for up to 50 minutes.
--
-- In particular this file must NOT use brishzeval2bg. Its name is misleading:
-- the trailing `&' backgrounds the command inside the garden, but Hammerspoon
-- still synchronously waits for the round-trip, because brishzeval2 goes
-- through pipe_simple (lua/pipe.lua), which does blocking posix.read loops and
-- then posix.wait. Measured on this machine with hs.timer.absoluteTime:
--
--   brishzeval2bg("true")                            780.6 ms cold, 51.9 ms warm
--   hs.task.new("brishz2.dash", nil, {"true"}):start()          1.5 ms
--
-- hs.task is genuinely asynchronous (NSTask, callback on completion), and is
-- already the idiom elsewhere in this config: core/hyper-mode.lua,
-- core/mouse.lua, and core/choosers.lua all use it, the last one to run
-- brishz2.dash exactly like this.

audioWatcherDebounceTimer = nil

local DEBOUNCE = 1.0

-- hs.task does NOT inherit an interactive PATH. It gets the bare launchd one,
-- /usr/bin:/bin:/usr/sbin:/sbin, and brishz.dash shells out to jq, which lives
-- in /opt/homebrew/bin. Without this the task exits 22 with
-- "brishz.dash: 41: jq: not found" and, because a nil callback discards both
-- streams, fails completely silently. Same class of bug as the PATH in
-- launchers/audio-guard/com.user.audio-guard.plist.
local BREW_PATHS = "/opt/homebrew/bin:/usr/local/bin"

local function taskWithPath(bin, callback, args)
    local task = hs.task.new(bin, callback, args)
    if not task then return nil end

    -- Repair PATH rather than replacing the environment wholesale: brishz needs
    -- HOME, and setEnvironment replaces the table entirely.
    local env = task:environment() or {}
    env.PATH = BREW_PATHS .. ":" .. (env.PATH or "/usr/bin:/bin:/usr/sbin:/sbin")
    task:setEnvironment(env)

    return task
end

-- A single device switch emits a burst of events; without coalescing, one
-- AirPods disconnect becomes several garden round-trips.
local function notifyAudioChanged()
    local device = hs.audiodevice.defaultOutputDevice()
    if not device then return end

    -- Pass the name and transport as arguments. The naive alternative lets the
    -- zsh side rediscover them via audio-output-get-hs, i.e. a subprocess we
    -- spawned calling IPC back into the very Hammerspoon that spawned it. We
    -- already know the answer here, so we hand it over.
    local cmd = ("audio-guard-on-audio-change %q %q"):format(
        device:name() or "", device:transportType() or "")

    -- A nil callback would discard the exit code and both streams, which is how
    -- the missing-jq failure above stayed invisible. Log failures instead.
    local task = taskWithPath("/usr/local/bin/brishz2.dash", function(exitCode, _, stdErr)
        if exitCode ~= 0 then
            print("audio-watcher: brishz2.dash exited " .. tostring(exitCode) ..
                      ": " .. tostring(stdErr))
        end
    end, {cmd})

    if task then task:start() end
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

-- Registered unconditionally, with the trigger check left to the zsh side.
-- Gating here would mean reading Redis from Hammerspoon, and redisClient in
-- core/redis.lua connects without auth and may legitimately be nil -- trading
-- 1.5 ms for a new failure mode. While the trigger is off the residual cost is
-- one hs.task spawn per default-output-device change: a few times a day, not
-- per event, thanks to the filter and the debounce above.
