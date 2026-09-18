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

        -- A wake ends the blackout, so it ends the keyboard lock that came with
        -- it (core/blackout-lock.lua). Done here as well as from
        -- display-black-off, so the keys come back even if the garden is slow
        -- or down. Guarded: that module loads after this one.
        --
        -- Two calls, because they are two different events. blackoutLockOff
        -- gives the keyboard back; blackoutEnded forgets the blackout, and is
        -- the only thing that may clear the lock-first mark. A wake really is
        -- the blackout ending -- it lands on a login screen anyway -- so both
        -- are right here.
        if blackoutLockOff then blackoutLockOff(true) end
        if blackoutEnded then blackoutEnded() end
    end

    -- Unlocking is the way back from the top blackout rung (hyper+cmd+F1,
    -- see core/blackout-lock.lua): it locks the session outright, and while
    -- the login window is up no chord can reach us at all, because the user
    -- session stops receiving key events. So the unlock has to be what
    -- restores the display, and until now the only thing listening for it was
    -- swift/lock_watcher.swift, which fires h-hook-unlock in the garden.
    --
    -- That watcher was found dead, weeks after it quietly exited, with the
    -- screen left black through every unlock in between. One unsupervised
    -- process should not be the single way out of a black screen, so this
    -- listens too. It is the same belt-and-braces the wake path above already
    -- is, and h-blackout-release is written to be run from several places and
    -- to return immediately when nothing is blanked.
    --
    -- Deliberately h-blackout-release rather than the whole h-hook-unlock:
    -- the rest of that hook (audio guard, battery limit, idle reset) is not
    -- idempotent in the way the blackout release is, and duplicating it here
    -- would double-fire it whenever the swift watcher is alive.
    if event == hs.caffeinate.watcher.screensDidUnlock then
        brishz_eval_hs("h-blackout-release", "power-watcher-unlock")

        -- The Lua half, as above: the garden restores the display, these give
        -- the keyboard back and forget the blackout. Done here as well so the
        -- keys come back even if the garden is slow or down.
        if blackoutLockOff then blackoutLockOff(true) end
        if blackoutEnded then blackoutEnded() end
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
