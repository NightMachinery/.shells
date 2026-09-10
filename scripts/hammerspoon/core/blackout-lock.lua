--- * Blackout keyboard lock
--- While the screen is blacked out (hyper+shift+F1), a stray keypress still
--- types into whatever window has focus, a click still lands on whatever is
--- under the pointer, and the hardware brightness keys quietly undo the black.
--- This module locks all of that out for the life of the blackout, leaving
--- exactly one way back in: the black-off chord, hyper+shift+F2.
---
--- It is an hs.eventtap. Taps run before Carbon hotkeys and before any app, so
--- a callback returning true drops the event for everyone -- every other hyper
--- binding included. The allowlist is two things: F18, the physical hyper key,
--- so the hyper modal can still be entered; and F2 with shift while hyper mode
--- is entered, so the existing black-off binding in window-media-bindings.lua
--- fires. That binding also calls blackoutLockOff, synchronously.
---
--- The tap exists only while a blackout is up. A permanently installed tap
--- would see every keystroke of every app (see the note in core/fim.lua), so
--- it is installed on black-on and torn down on black-off.
---
--- A blackout that has been up for a while is one nobody is watching, so
--- ending it should not hand the desktop to whoever pressed the chord. Past
--- blackoutLockScreenAfterSeconds, blackoutRestore locks the macOS session
--- first and restores the display second, so the person meets the login
--- screen. hyper+shift+cmd+F1 starts a blackout marked lock-first, which does
--- that regardless of age: the one who *starts* the black decides, because
--- whoever presses F2 later may be a stranger. The invariant: the keyboard
--- lock never releases into an unlocked session on its own. Only F2 inside
--- the grace period does that, and that is a person's deliberate act.
---
--- The start time and the lock-first mark are saved in redis (key
--- blackout_lock) when redis is up, so a Hammerspoon reload loses neither:
--- on load, the module reads them back, re-installs the tap with the expiry
--- that remains, and trusts the key only while zsh's display_black_saved says
--- something is still blanked.
---
--- Shell interface:
---   hs -c 'blackoutLockOn()'          -- or blackoutLockOn(30), for a test
---   hs -c 'blackoutLockOff()'
---   hs -c 'return blackoutLockActive()'
---   hs -c 'blackoutRestore()'         -- what hyper+shift+F2 does
---   hs -c 'blackoutRestore(true)'     -- lock the session first, always
---
--- Every failure points the same way, at keys coming back: a Hammerspoon crash
--- or reload drops the tap, macOS disables a tap whose callback stalls, and the
--- expiry below locks the session and restores the display on its own. Secure
--- Input is the one thing that makes the lock *weaker*: a focused password
--- field, or the login screen, hides keystrokes from taps, so nothing can be
--- swallowed there. Engaging while it is on warns rather than pretends.

--- ** Configuration

--- Whether hyper+shift+F1 engages the lock along with the blackout. Set to
--- false to get the old behaviour: black screen, live keyboard. `== nil` rather
--- than `or`, so an explicit false set before this file loads survives.
if blackoutLockEnabled == nil then blackoutLockEnabled = true end

--- Whether clicks and the scroll wheel are swallowed too. Mouse movement is
--- never tapped: it is a firehose, and moving the pointer over a black screen
--- does nothing.
if blackoutLockMouse == nil then blackoutLockMouse = true end

--- After this many seconds of blackout, restoring the display locks the
--- session first. 0 locks first always; false never does. Measured from
--- blackoutBegin, so it works with the keyboard lock disabled too.
if blackoutLockScreenAfterSeconds == nil then blackoutLockScreenAfterSeconds = 60 * 60 end

--- Backstop. After this long, the blackout is treated as forgotten: the session
--- is locked and the display restored, so the failure state is a visible login
--- screen and never a live keyboard on an unlocked desktop behind black. A
--- long time, because the real ways out are the chord, a wake, and
--- display-black-off, and a blackout over a holiday must not end on its own.
blackoutLockMaxSeconds = blackoutLockMaxSeconds or 14 * 24 * 60 * 60  -- 2 weeks

--- ** State
--- Global, so a dofile into a live Hammerspoon can find the previous run's tap
--- and stop it. A dangling eventtap is held by the objc runtime, not by the
--- Lua chunk, and an orphaned one would keep eating keystrokes.
local previousState = blackoutLockState

blackoutLockState = blackoutLockState or {
    tap = nil,
    timer = nil,
    -- Epoch seconds the current blackout began, or nil when none is up.
    since = nil,
    -- Started with hyper+shift+cmd+F1: lock the session before restoring,
    -- whatever the age.
    lockFirst = false,
    restoreTimer = nil,
    recoverTimer = nil,
    -- The chord dispatch tap; see ** Chord dispatch below. Lives here rather
    -- than in a local so a reload can find and stop the previous run's.
    chordTap = nil,
}

--- How long the lock screen gets to come up before the display is restored
--- under it.
local kLockSettleSeconds = 0.7

local kAlertId = "blackout-lock"
local kReleaseAlertId = "blackout-lock-release"
local kSecureInputAlertId = "blackout-lock-secure-input"
local kMinSeconds = 5

--- The hyper toggle, by keycode: it arrives with no flags of its own.
local kF18KeyCode = hs.keycodes.map.f18 or 79
--- The escape chord is hyper+shift+F2 -- the existing black-off binding.
local kEscapeKeyCode = hs.keycodes.map.f2 or 120

local types = hs.eventtap.event.types

local kKeyTypes = { types.keyDown, types.keyUp, types.systemDefined }
local kMouseTypes = {
    types.leftMouseDown, types.leftMouseUp,
    types.rightMouseDown, types.rightMouseUp,
    types.otherMouseDown, types.otherMouseUp,
    types.scrollWheel,
}

--- ** Helpers

local function alert(text, opts)
    if alert_gateway then
        return alert_gateway(text, opts)
    end
    hs.alert(text, opts.seconds or 2)
end

local function dismiss(id)
    if alert_gateway_dismiss then alert_gateway_dismiss(id) end
end

local function hyperEntered()
    return hyper_modality ~= nil and hyper_modality.entered_p == true
end

--- ** Persistence
--- "<epoch seconds> <0|1>": when the black began, and whether it is
--- lock-first. Absent when nothing is black. Written on every begin and
--- cleared on every release; a no-op when redis is down, which then costs only
--- the reload survival.
local kRedisKey = "blackout_lock"
--- zsh's "is anything blanked" flag (redis-defvar in system.zsh), consulted
--- so a saved start is never trusted after the black itself is gone.
local kRedisBlackKey = "display_black_saved"

local function persist(st)
    if not redisSet then return end
    if st.since then
        redisSet(kRedisKey, string.format("%d %d", math.floor(st.since), st.lockFirst and 1 or 0))
    else
        redisDel(kRedisKey)
    end
end

--- true drops the event, false lets it through.
local function handleEvent(event)
    local t = event:getType()

    if t == types.keyDown or t == types.keyUp then
        local keyCode = event:getKeyCode()

        if keyCode == kF18KeyCode then
            return false
        end

        if keyCode == kEscapeKeyCode and hyperEntered() then
            local flags = event:getFlags()
            if flags.shift and not flags.cmd and not flags.alt and not flags.ctrl then
                return false
            end
        end

        return true
    end

    -- systemDefined (brightness, media keys) and every mouse type we tapped.
    return true
end

local function stopTap(st)
    if st.tap then
        st.tap:stop()
        st.tap = nil
    end
    if st.timer then
        st.timer:stop()
        st.timer = nil
    end
    -- Deliberately not st.restoreTimer: a pending lock-then-restore must still
    -- restore the display even if something releases the keyboard lock during
    -- the settle, or the login screen would come up black.
end

local function stopRestoreTimer(st)
    if st.restoreTimer then
        st.restoreTimer:stop()
        st.restoreTimer = nil
    end
end

if previousState and previousState ~= blackoutLockState then
    stopTap(previousState)
    stopRestoreTimer(previousState)
    if previousState.chordTap then previousState.chordTap:stop() end
end

--- ** Interface

function blackoutLockActive()
    return blackoutLockState.tap ~= nil
end

function blackoutLockOn(seconds)
    local st = blackoutLockState
    stopTap(st)

    local duration = tonumber(seconds) or blackoutLockMaxSeconds
    duration = math.max(kMinSeconds, math.min(duration, blackoutLockMaxSeconds))

    local eventTypes = {}
    for _, t in ipairs(kKeyTypes) do table.insert(eventTypes, t) end
    if blackoutLockMouse then
        for _, t in ipairs(kMouseTypes) do table.insert(eventTypes, t) end
    end

    st.tap = hs.eventtap.new(eventTypes, handleEvent)
    st.tap:start()

    -- Expiry: a forgotten blackout. Lock up and show the login screen rather
    -- than quietly giving the keyboard back to an unlocked desktop.
    st.timer = hs.timer.doAfter(duration, function()
        blackoutRestore(true)
    end)

    dismiss(kReleaseAlertId)

    -- Visible for the moment before the screen goes black: black-on is
    -- asynchronous through the garden, and this is synchronous.
    --
    -- The lock-first mark shows up nowhere else: both chords go equally black,
    -- and the mark cannot be revoked once set, so this band is the only chance
    -- to confirm it. Hence its own sentence and its own colour -- blood, darker
    -- than the amber the plain chord keeps and darker than the crimson `crit'
    -- the Secure Input warning below may fire alongside it.
    local lockFirst = st.lockFirst

    alert(lockFirst and "Input locked. Ending the blackout locks the screen."
                     or "Input locked.", {
        id = kAlertId,
        color = lockFirst and "blood" or "warn",
        seconds = lockFirst and 5 or 4,
        screens = "all",
    })

    if hs.eventtap.isSecureInputEnabled() then
        alert("Secure Input is on: the keyboard lock cannot swallow keys right now.", {
            id = kSecureInputAlertId,
            color = "crit",
            seconds = 6,
            screens = "all",
        })
    end

    return true
end

--- silent skips the release flash, for the wake and expiry paths where nobody
--- is watching the screen come back.
function blackoutLockOff(silent)
    local st = blackoutLockState
    local wasActive = blackoutLockActive()

    stopTap(st)
    st.since = nil
    st.lockFirst = false
    persist(st)
    dismiss(kAlertId)
    dismiss(kSecureInputAlertId)

    if wasActive and not silent then
        alert("Input unlocked", {
            id = kReleaseAlertId,
            color = "free",
            seconds = 1.5,
            screens = "all",
        })
    end

    return true
end

--- What hyper+shift+F1 calls, and hyper+shift+cmd+F1 with lockFirst=true.
--- Records when the black began, whether or not the keyboard lock is on,
--- because the lock-before-restore rule needs the age either way. A second F1
--- during a blackout keeps the original time; a cmd+F1 during one upgrades it
--- to lock-first, and nothing downgrades it short of ending the black.
function blackoutBegin(lockFirst)
    local st = blackoutLockState
    if not st.since then
        st.since = hs.timer.secondsSinceEpoch()
    end
    if lockFirst then
        st.lockFirst = true
    end
    persist(st)
    if blackoutLockEnabled then
        blackoutLockOn()
    end
    return true
end

local function shouldLockScreen(force)
    if force or blackoutLockState.lockFirst then return true end
    local after = blackoutLockScreenAfterSeconds
    if not after then return false end
    local since = blackoutLockState.since
    -- Unknown age (nothing black, or a reload lost the time): do not lock a
    -- session out from under someone who pressed F2 with no blackout up.
    if not since then return false end
    return hs.timer.secondsSinceEpoch() - since >= after
end

--- What hyper+shift+F2 calls: releases the keyboard lock and asks the garden
--- to restore the display. Past blackoutLockScreenAfterSeconds, or with
--- forceLock, the session is locked first and the display restored under the
--- login screen. Returns whether it locked.
function blackoutRestore(forceLock)
    local st = blackoutLockState
    local lockFirst = shouldLockScreen(forceLock)

    local function restore()
        st.restoreTimer = nil
        blackoutLockOff(lockFirst)
        brishz_eval_hs('awaysh-fast brightness-on-all-loop', 'blackout-restore')
    end

    if lockFirst then
        hs.caffeinate.lockScreen()
        stopRestoreTimer(st)
        st.restoreTimer = hs.timer.doAfter(kLockSettleSeconds, restore)
    else
        restore()
    end

    return lockFirst
end

--- ** Chord dispatch
--- The hyper F1/F2 chords do not go through `hs.hotkey' like every other
--- hyper binding, because Carbon drops them. Measured over fourteen presses in
--- one afternoon: hyper mode was entered, the hotkey was listed by
--- `hs.hotkey.getHotkeys()', the event carried exactly the right modifiers, and
--- about a fifth of the presses never reached their callback regardless -- while
--- an eventtap watching the same two keys saw every single one. What that
--- investigation ruled out, and how to run the next one, is in "When a hyper
--- chord does nothing" in docs/hammerspoon.md.
---
--- A dropped blackout leaves the screen lit; a dropped escape chord leaves a
--- locked keyboard in front of a lit screen; a dropped brightness step is
--- merely irritating, and was reported independently. All four now take the
--- delivery path this module already trusts for the lock itself.
---
--- A side effect worth keeping: nothing modal binds bare F1/F2 any more, which
--- is what used to shadow STT's globals on every hyper transition and fill the
--- console with "Disabled previous hotkey F1" pairs. Those STT binds have since
--- been retired as well -- core/stt.lua keeps dictation on F11/F12 -- so no
--- hs.hotkey contends for these two keycodes at all now.
---
--- The tap runs only while hyper mode is entered -- which pressing the chord
--- requires anyway -- so it is off almost always, for the privacy and latency
--- reasons in core/fim.lua. hyper-mode.lua starts and stops it from the
--- modality's entered/exited callbacks.

local kChordKeys = {
    [hs.keycodes.map.f1 or 122] = "f1",
    [hs.keycodes.map.f2 or 120] = "f2",
}

--- Which chord this event is, or nil for anything the tap must not touch.
--- `fn' is deliberately not tested: every F-key press here carries it,
--- successes included, so Carbon ignores it too and matching on it would
--- reject every real press.
local function chordFor(keyName, flags)
    if flags.alt or flags.ctrl then return nil end

    if flags.shift then
        if keyName == "f1" then
            return flags.cmd and "black-lock-first" or "black"
        end
        if keyName == "f2" and not flags.cmd then
            return "restore"
        end
        return nil
    end

    --- No shift: the brightness keys, dispatched here for the same reason --
    --- they get dropped too. Swallowed rather than passed on, so that nothing
    --- downstream can claim them: hyper+F1 meant dictation for as long as STT
    --- bound the bare keys globally, and the next binder would collide the
    --- same way.
    if flags.cmd then return nil end
    if keyName == "f1" then return "brightness-dec" end
    if keyName == "f2" then return "brightness-inc" end
    return nil
end

--- The blackout chords are one-shot and leave the mode, exactly as an
--- auto-trigger hs.hotkey binding did. The brightness keys deliberately do
--- not: holding hyper and stepping the level repeatedly is the point, which
--- is why they were bound with auto_trigger_p=false.
local kChordExitsMode = {
    ["black"] = true,
    ["black-lock-first"] = true,
    ["restore"] = true,
}

local function runChord(chord)
    if chord == "restore" then
        if blackoutChordRestore then blackoutChordRestore() end
    elseif kChordExitsMode[chord] then
        if blackoutChordBegin then blackoutChordBegin(chord == "black-lock-first") end
    elseif hyperBrightnessStep then
        hyperBrightnessStep(chord == "brightness-dec" and "dec" or "inc")
    end
end

local function handleChordEvent(event)
    local keyName = kChordKeys[event:getKeyCode()]
    if not keyName then return false end

    local chord = chordFor(keyName, event:getFlags())
    --: Bare F1/F2 are the brightness keys, and they stay on hs.hotkey.
    if not chord then return false end

    --- While the lock is up the only chord that does anything is the one that
    --- ends it -- the same rule handleEvent enforces above, repeated here so
    --- the outcome does not depend on which of the two taps macOS happens to
    --- call first.
    if blackoutLockActive() and chord ~= "restore" then
        return true
    end

    --- Act on the press; the release is swallowed too, so the focused app
    --- never sees half a chord.
    if event:getType() == types.keyDown then
        --- Both off the callback. macOS disables a tap whose callback stalls,
        --- and these actions cross into the garden and may lock the session.
        --- The exit has to be deferred for a second reason: it runs
        --- blackoutChordTapStop, and stopping this very tap from inside its own
        --- callback is not a thing worth finding out the hard way.
        hs.timer.doAfter(0, function()
            --- What the auto-trigger wrapper in modal-mode.lua would have
            --- done, in the same order: leaving the mode entered would make
            --- the next F18 press toggle it off rather than on.
            if kChordExitsMode[chord] and hyper_triggered then hyper_triggered() end
            runChord(chord)
        end)
    end

    return true
end

function blackoutChordTapStart()
    local st = blackoutLockState

    --- Rebuilt rather than reused, so a reload can never leave the previous
    --- chunk's closure taking keys -- the same reason blackoutLockOn rebuilds
    --- its own tap. Cheap at this rate: once per hyper entry.
    if st.chordTap then st.chordTap:stop() end
    st.chordTap = hs.eventtap.new({ types.keyDown, types.keyUp }, handleChordEvent)
    st.chordTap:start()

    return true
end

function blackoutChordTapStop()
    local st = blackoutLockState
    if st.chordTap then st.chordTap:stop() end

    return true
end

--- ** Surviving a reload
--- Reads the saved start back. Trusted only while zsh's display_black_saved
--- key is present: if the black ended while Hammerspoon was not running to
--- hear display-black-off, the saved start is stale and is deleted rather than
--- locking a keyboard in front of a lit screen. Returns what it did.
function blackoutLockRecover()
    local st = blackoutLockState
    if st.since then return "already" end
    if not redisGet then return "no-redis" end

    local raw, ok = redisGet(kRedisKey)
    if not ok then return "no-redis" end
    if not raw or raw == "" then return "nothing" end

    local black = redisGet(kRedisBlackKey)
    local since, first = tostring(raw):match("^(%d+)%s+([01])")
    if not black or black == "" or not since then
        redisDel(kRedisKey)
        return "stale"
    end

    st.since = tonumber(since)
    st.lockFirst = (first == "1")

    if blackoutLockEnabled then
        local remaining = blackoutLockMaxSeconds - (hs.timer.secondsSinceEpoch() - st.since)
        blackoutLockOn(math.max(kMinSeconds, remaining))
    end
    return "recovered"
end

local function scheduleRecover(delay, attempt)
    local st = blackoutLockState
    st.recoverTimer = hs.timer.doAfter(delay, function()
        st.recoverTimer = nil
        local result = blackoutLockRecover()
        -- redis.lua connects on a retry timer of its own when redis is slow to
        -- come up; follow it for a while rather than giving up at boot.
        if result == "no-redis" and attempt < 6 then
            scheduleRecover(5, attempt + 1)
        elseif result ~= "already" and result ~= "nothing" then
            print("blackout-lock: recover: " .. result)
        end
    end)
end

if previousState and previousState.recoverTimer then
    previousState.recoverTimer:stop()
end
scheduleRecover(0.5, 1)
--- @end
