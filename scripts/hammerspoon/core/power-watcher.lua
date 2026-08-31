--- * Power Watcher
--
-- Wakes are the one moment a screen blackout has to be undone for us. Closing
-- the lid sleeps the machine regardless of what we assert -- clamshell is not
-- idle sleep, and caffeinate only creates idle-sleep assertions -- so blanking
-- the built-in panel and shutting the lid leaves brightness 0 behind, with the
-- keep-blank loop (see display-black-on-loop) merely frozen rather than
-- stopped. It resumes at wake and re-asserts the zero every few seconds, at a
-- login screen where the brightness keys can no longer win.
--
-- The zsh side is h-hook-wake, which fans out to consumers there; see
-- docs/external-display-brightness.md.

-- Global on purpose: boot.lua loads modules with dofile, so a file-local
-- watcher is collected and stops firing. Same reason as audio-watcher.lua.
powerWatcher = nil

local function onPowerEvent(event)
    -- macOS emits several of these per wake, and both of ours can fire for the
    -- same one. h-hook-wake is idempotent and returns immediately when there is
    -- nothing blanked, so the duplicates cost a garden round-trip and nothing
    -- else -- cheaper than tracking state here to suppress them.
    if event == hs.caffeinate.watcher.systemDidWake or event ==
        hs.caffeinate.watcher.screensDidWake then
        -- Asynchronous: never block Hammerspoon's main thread on the garden.
        -- See brishz_eval_hs in core/helpers.lua.
        brishz_eval_hs("h-hook-wake", "power-watcher")
    end
end

-- A reload builds a fresh Lua state, and core/reload.lua path-watches this
-- directory, so saving this file is already enough to load it. The stop is for
-- the other way in -- dofile'ing this file into a live Hammerspoon -- where the
-- previous watcher would otherwise keep firing until it is collected, and every
-- wake would round-trip to the garden twice.
if powerWatcher then powerWatcher:stop() end

powerWatcher = hs.caffeinate.watcher.new(onPowerEvent)
powerWatcher:start()
