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
--- screen. hyper+shift+cmd+F2 does that regardless of age. The invariant: the
--- keyboard lock never releases into an unlocked session on its own. Only F2
--- inside the grace period does that, and that is a person's deliberate act.
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
--- screen and never a live keyboard on an unlocked desktop behind black. Long,
--- because the real ways out are the chord, a wake, and display-black-off.
blackoutLockMaxSeconds = blackoutLockMaxSeconds or 12 * 60 * 60

--- ** State
--- Global, so a dofile into a live Hammerspoon can find the previous run's tap
--- and stop it. A dangling eventtap is held by the objc runtime, not by the
--- Lua chunk, and an orphaned one would keep eating keystrokes.
local previousState = blackoutLockState

blackoutLockState = blackoutLockState or {
    tap = nil,
    timer = nil,
    -- Epoch seconds the current blackout began, or nil when none is up (or
    -- when a reload lost track: then F2 restores without locking).
    since = nil,
    restoreTimer = nil,
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

--- true drops the event, false lets it through.
local function handleEvent(event)
    local t = event:getType()

    if t == types.keyDown or t == types.keyUp then
        local keyCode = event:getKeyCode()

        if keyCode == kF18KeyCode then
            return false
        end

        -- hyper+shift+F2 restores; hyper+shift+cmd+F2 locks the session first.
        if keyCode == kEscapeKeyCode and hyperEntered() then
            local flags = event:getFlags()
            if flags.shift and not flags.alt and not flags.ctrl then
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
    alert("Keyboard locked. hyper+shift+F2 releases.", {
        id = kAlertId,
        color = "warn",
        seconds = 4,
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
    dismiss(kAlertId)
    dismiss(kSecureInputAlertId)

    if wasActive and not silent then
        alert("Keyboard unlocked", {
            id = kReleaseAlertId,
            color = "free",
            seconds = 1.5,
            screens = "all",
        })
    end

    return true
end

--- What hyper+shift+F1 calls. Records when the black began, whether or not
--- the keyboard lock is on, because the lock-before-restore rule needs the age
--- either way. A second F1 during a blackout keeps the original time.
function blackoutBegin()
    local st = blackoutLockState
    if not st.since then
        st.since = hs.timer.secondsSinceEpoch()
    end
    if blackoutLockEnabled then
        blackoutLockOn()
    end
    return true
end

local function shouldLockScreen(force)
    if force then return true end
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
--- @end
