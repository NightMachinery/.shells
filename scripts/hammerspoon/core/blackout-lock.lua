--- * Blackout keyboard lock
--- While the screen is blacked out (hyper+shift+F1), a stray keypress still
--- types into whatever window has focus, a click still lands on whatever is
--- under the pointer, and the hardware brightness keys quietly undo the black.
--- This module locks all of that out for the life of the blackout, leaving
--- exactly one way back in from the keyboard: the black-off chord,
--- hyper+shift+F2 -- and, once the top rung has locked the session, only
--- unlocking it again.
---
--- It is an hs.eventtap. Taps run before Carbon hotkeys and before any app, so
--- a callback returning true drops the event for everyone -- every other hyper
--- binding included. The allowlist is four things: F18, the physical hyper
--- key, so the hyper modal can still be entered; F2 with shift while hyper
--- mode is entered, which ends the blackout; F1 with shift and cmd while hyper
--- mode is entered, which marks the blackout lock-first; and F1 with cmd alone
--- while hyper mode is entered, which locks the session on the spot. The last
--- two act *without* ending the blackout, and can only make the way out
--- stricter. None of the three chords reaches an app: the lock tap only passes
--- them, and the chord tap below swallows them itself.
---
--- Those four are the allowlist for a *person*. Software is not held to it:
--- this lock is here to stop another person physically using the machine, and
--- it was never meant to stop local tools. Every event carries the state id of
--- the source that made it, and anything that made a source of its own -- this
--- config, the clipboard manager, the speech-to-text tool, an agent driving a
--- test instance -- carries a private id that is neither of the two well-known
--- states. Those pass, ahead of every rule above. The physical HID stream and
--- the generic combined-session state are both dropped as before; the note in
--- handleEvent says why the generic one is denied alongside the hand at the
--- keyboard.
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
--- that regardless of age; pressed during a blackout already up, it marks that
--- one instead. The mark only ever moves toward locking, so whoever *starts*
--- the black can never be talked back out of it, and that is the point:
--- whoever presses F2 later may be a stranger. The invariant: the keyboard
--- lock never releases into an unlocked session on its own. Only F2 inside
--- the grace period does that, and that is a person's deliberate act.
---
--- That makes a ladder of three rungs, and hyper+cmd+F1 is the top one: it
--- stops waiting for the ending and locks the session now, black as well.
--- Fresh, it blacks and locks; on a blackout already up, it marks it
--- lock-first and locks on the spot, with a cue of its own so the rung reached
--- can be told by ear once there is no screen left to tell it on. There is no
--- chord back from it, and there is not meant to be: chords cannot reach the
--- login screen, where Secure Input hides every keystroke from every tap. The
--- way back is unlocking the session -- Touch ID, or a password typed at a
--- screen you cannot see. The Swift lock watcher turns that into h-hook-unlock,
--- which runs h-blackout-release, which reaches display-black-off, which calls
--- blackoutLockOff and blackoutEnded here over ipc. The display comes back on
--- its own.
---
--- A blackout that *starts* may be held back by a note first: see ** The
--- blackout note below, and alert-at-next-blackout in zsh.
---
--- Which is why releasing the input lock and ending the blackout are two
--- functions and not one. blackoutLockOff gives the keyboard back and touches
--- nothing else; blackoutEnded forgets the blackout, and is the only thing
--- that clears the lock-first mark. They are almost always called together,
--- because in practice the lock is armed with the mark and released when the
--- black ends -- but "almost always" was doing real damage while it was
--- assumed: anything that just wanted its keyboard back cancelled the mark on
--- the way past, and F2 then restored onto a live desktop.
---
--- The start time and the lock-first mark are saved in redis (key
--- blackout_lock) when redis is up, so a Hammerspoon reload loses neither:
--- on load, the module reads them back, re-installs the tap with the expiry
--- that remains, and trusts the key only while zsh's display_black_saved says
--- something is still blanked.
---
--- Shell interface:
---   hs -c 'blackoutLockOn()'          -- or blackoutLockOn(30), for a test
---   hs -c 'blackoutLockOff()'         -- keyboard back; the blackout's own
---                                        state, lock-first mark included, is
---                                        left exactly as it was
---   hs -c 'blackoutEnded()'           -- the black is over: forget it, and
---                                        with it the lock-first mark
---   hs -c 'return blackoutLockActive()'
---   hs -c 'return blackoutLockPassed()' -- automated events let through
---   hs -c 'blackoutRestore()'         -- what hyper+shift+F2 does
---   hs -c 'blackoutRestore(true)'     -- lock the session first, always
---   hs -c 'blackoutUpgrade()'         -- what hyper+shift+cmd+F1 does to a
---                                        blackout that is already up
---   hs -c 'blackoutLockNow()'         -- what hyper+cmd+F1 does, fresh or not
---   hs -c 'blackoutChordRun("black")' -- the whole chord path, note gate and
---                                        all; also "black-lock-first",
---                                        "black-lock-now", "restore"
---   hs -c 'blackoutLockScreenEnabled = false'
---                                     -- stand the session lock down for a
---                                        test; a reload re-arms it
---   hs -c 'return blackoutNoteText()' -- the pending note, or nil
---   hs -c 'blackoutNoteShow()'        -- show it and nothing else
---   hs -c 'return blackoutNotePending()'
---   hs -c 'blackoutNoteFire()'        -- stop waiting; black now
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

--- The cue hyper+shift+cmd+F1 plays when it marks a blackout that is already
--- up. A name from /System/Library/Sounds, or false for silence. A sound at
--- all, because by then the screen is black and no band can be seen; a name
--- rather than a boolean, so the knob picks the cue instead of merely gating
--- one buried in the code.
if blackoutUpgradeSound == nil then blackoutUpgradeSound = "Submarine" end

--- The cue hyper+cmd+F1 plays when it locks the session on a blackout that is
--- already up. Distinct from blackoutUpgradeSound on purpose: by then there is
--- no screen left to tell the two rungs apart on, so they are told apart by
--- ear and by nothing else. Glass is short and bright where Submarine is low
--- and slow, which is about as far apart as /System/Library/Sounds gets.
---
--- Keyed to the rung *reached*, not to the step taken: 1 -> 3 and 2 -> 3 both
--- play this one. A fresh rung-three start plays nothing -- the screen is
--- still lit and the band is the confirmation, for the moment it lasts.
if blackoutLockNowSound == nil then blackoutLockNowSound = "Glass" end

--- Whether hs.caffeinate.lockScreen() is really called. Locking the session is
--- the one thing this module does that a test cannot undo -- it lands the
--- tester on the login screen, where Secure Input hides the very chords that
--- would get them back -- so every lock in this file goes through lockSession
--- below, and this turns that into a console line for the length of a test:
---   hs -c 'blackoutLockScreenEnabled = false'
---
--- Deliberately *not* `== nil'-guarded like the knobs above: a reload re-arms
--- it. An override that outlived the test would quietly break the invariant
--- this module exists for, and the cost of re-arming is typing it again.
blackoutLockScreenEnabled = true

--- ** Configuration: the blackout note
--- A note to yourself, read on the way into the dark. `alert-at-next-blackout'
--- in zshlang/auto-load/others/power.zsh appends a Markdown bullet to this
--- file; the next blackout that *starts* puts the file on screen for
--- blackoutNoteSeconds, moves it aside to "<file>.last", and then blacks. The
--- point is the one moment you are guaranteed to be looking at the screen and
--- about to stop: "the render is still going", "unplug the drive".
---
--- The path is written out in both languages rather than asked for over the
--- garden. A blackout must not wait on a brishz round trip, and a path fetched
--- from a shell that may be wedged is a path that fails exactly when the
--- screen is about to go black. The two defaults must agree; the zsh side
--- (alert_at_next_blackout_file) names this file in its comment for the same
--- reason.
blackoutNoteFile = blackoutNoteFile
    or ((os.getenv("HOME") or "") .. "/tmp/alert_at_next_blackout.md")

--- How long the note holds the blackout back. Any blackout chord pressed
--- during it skips the rest of the wait; nothing cancels the blackout.
blackoutNoteSeconds = blackoutNoteSeconds or 5

--- How the note looks. Gold, because a note to yourself is a sticky note, and
--- because it is nowhere near the warn/blood/midnight ladder the lock bands
--- use, which means something else entirely. Large, because it is read from
--- across a desk in the seconds before the screen goes, not from a chair in
--- front of it; the size is the band's own and moves no other alert.
blackoutNoteColor = blackoutNoteColor or "gold"
blackoutNoteTextSize = blackoutNoteTextSize or 40

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
    -- Which rung of the ladder this blackout is at, 1 to 3, or nil when none
    -- is up. What the band in blackoutLockOn reads. Only ever raised while a
    -- blackout is up, and not persisted: the one fact it adds over lockFirst,
    -- "the session is already locked", is macOS's own state and would be
    -- stale the moment it was read back. Recovery sets it from the mark.
    rung = nil,
    restoreTimer = nil,
    recoverTimer = nil,
    -- The chord dispatch tap; see ** Chord dispatch below. Lives here rather
    -- than in a local so a reload can find and stop the previous run's.
    chordTap = nil,
    -- Resolved hs.sound handles and the names they were resolved from, one
    -- entry per cue -- "upgrade" and "lock-now". Here rather than in a local
    -- because a sound collected mid-play stops, and one slot per cue so the
    -- two rungs do not evict each other between presses. A dofile keeps the
    -- previous table, which may predate these fields, so playCue reads them
    -- defensively.
    sounds = nil,
    soundNames = nil,
    -- A note is on screen and the blackout it delays is waiting on this timer;
    -- pendingChord is which chord is waiting. See ** The blackout note.
    noteTimer = nil,
    pendingChord = nil,
}

--- How long the lock screen gets to come up before the display is restored
--- under it.
local kLockSettleSeconds = 0.7

local kAlertId = "blackout-lock"
local kReleaseAlertId = "blackout-lock-release"
local kSecureInputAlertId = "blackout-lock-secure-input"
local kNoteAlertId = "blackout-note"
local kMinSeconds = 5

--- The hyper toggle, by keycode: it arrives with no flags of its own.
local kF18KeyCode = hs.keycodes.map.f18 or 79
--- The escape chord is hyper+shift+F2 -- the existing black-off binding.
local kEscapeKeyCode = hs.keycodes.map.f2 or 120
--- The black chord is hyper+shift+F1; with cmd it also marks a blackout that
--- is already up, and with cmd alone it locks the session now -- the two
--- other things the lock lets by.
local kBlackKeyCode = hs.keycodes.map.f1 or 122

local types = hs.eventtap.event.types
local properties = hs.eventtap.event.properties

--- The two well-known CGEvent source states, the ones the lock still drops.
--- kCGEventSourceStateHIDSystemState: the physical input stream, a hand on
--- this keyboard or this trackpad.
local kSourceHID = 1
--- kCGEventSourceStateCombinedSessionState: the generic source an event gets
--- when whoever posted it created none of its own.
local kSourceCombinedSession = 0

--- There is deliberately no constant for the *private* ids these two are
--- tested against. A private id is minted per event source and is different
--- every time -- two Hammerspoon-made events measured on this machine came
--- back 1364438702 and 841834532. So the test below can only ever be "neither
--- well-known one". Never turn it into an equality check against a value
--- somebody once observed; it would pass on one launch and deny on the next.

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

--- Plays a named cue, caching the handle per key. Cached because a sound
--- object collected mid-play stops; keyed by name as well, so a knob changed
--- from the console takes effect on the next press rather than on the next
--- reload. Returns whether a sound was there to play.
local function playCue(key, name)
    if not name then return false end

    local st = blackoutLockState
    st.sounds = st.sounds or {}
    st.soundNames = st.soundNames or {}

    if st.soundNames[key] ~= name then
        st.soundNames[key] = name
        st.sounds[key] = hs.sound.getByName(name)
        if not st.sounds[key] then
            print("blackout-lock: no such sound: " .. tostring(name))
        end
    end

    if st.sounds[key] then st.sounds[key]:play() end
    return st.sounds[key] ~= nil
end

--- The one place this module locks the session, so that one knob can stand
--- them all down for a test. Returns whether it really locked.
local function lockSession()
    if not blackoutLockScreenEnabled then
        print("blackout-lock: lockScreen suppressed (blackoutLockScreenEnabled = false)")
        return false
    end
    hs.caffeinate.lockScreen()
    return true
end

--- The lock band's words and colour for a rung: present tense as the lock
--- goes on, future tense while a note is holding the blackout back. One band
--- and one id either way, so the present-tense one replaces the promise in
--- place when the note's time is up. Colours are a ladder, warn -> blood ->
--- midnight, each darker than the last; blackoutLockOn says why.
local function lockBand(rung, future)
    if rung >= 3 then
        return future and "Screen will lock. Input will lock. Unlock to restore."
                      or "Locking now. Input locked. Unlock to restore.",
               "midnight"
    elseif rung == 2 then
        return future and "Input will lock. Ending the blackout will lock the screen."
                      or "Input locked. Ending the blackout locks the screen.",
               "blood"
    end
    return future and "Input will lock." or "Input locked.", "warn"
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

--- Events let through by the source-state rule below, since this file loaded.
--- Cheap enough to keep unconditionally, which a log line per event would not
--- be; blackoutLockPassed reads it.
local passedCount = 0

--- true drops the event, false lets it through.
local function handleEvent(event)
    --- Local automation goes through; a person at this machine does not. What
    --- the lock is for is keeping someone else from using the machine while
    --- the screen is black, and software was only ever collateral: an agent
    --- driving a test instance, the clipboard manager and the speech-to-text
    --- tool were all swallowed along with the stranger.
    ---
    --- The discriminator is the source state id. Anything that is neither of
    --- the two well-known states is a *private* source, and a private source
    --- can only be minted by a process running on this machine -- which is
    --- what every local tool that posts events does, this config included.
    ---
    --- Denying the combined-session state as well as the HID one is
    --- deliberate, and it is why this is a two-value test rather than "not
    --- HID". kSourceHID is the hand on the keyboard the lock exists for.
    --- kSourceCombinedSession is the generic state an event carries when
    --- nothing claimed a source of its own, which makes it the likelier
    --- vehicle for something injected, or arriving from off the machine --
    --- precisely the case the lock must still catch. Denying it costs local
    --- automation nothing, because local automation always has a source of
    --- its own.
    ---
    --- Read defensively and fail toward the lock: an id that is not a number
    --- falls through to the rules below rather than passing.
    local sourceState = event:getProperty(properties.eventSourceStateID)
    if type(sourceState) == "number"
        and sourceState ~= kSourceHID
        and sourceState ~= kSourceCombinedSession then
        passedCount = passedCount + 1
        return false
    end

    local t = event:getType()

    if t == types.keyDown or t == types.keyUp then
        local keyCode = event:getKeyCode()

        if keyCode == kF18KeyCode then
            return false
        end

        --- The three chords that may act while the lock is up: F2 with shift
        --- ends the blackout, F1 with shift and cmd marks it lock-first, and
        --- F1 with cmd alone locks the session now. Passed rather than acted
        --- on here -- the chord dispatch tap owns all three and swallows them
        --- itself, so none ever reaches an app. That tap runs exactly while
        --- hyper mode is entered, which is what makes hyperEntered() the
        --- right guard: nothing is let by that has no tap waiting to eat it.
        --- The release is covered by that tap while F18 is still held, and by
        --- the deny below once the mode has exited under it. Kept in step
        --- with chordFor by hand; the note on the gate in handleChordEvent
        --- says why the rule is deliberately written out in both places
        --- rather than shared.
        if hyperEntered() then
            local flags = event:getFlags()
            --- The escape, whatever else is stuck down with it; chordFor
            --- says why it alone is read this loosely.
            if keyCode == kEscapeKeyCode and flags.shift then
                return false
            end
            --- ctrl is the contrast chord and alt is nothing, and both stay
            --- on the deny side of this guard on purpose: a black screen must
            --- not be adjustable from the outside any more than it is
            --- readable. Falling through to the `return true' below is what
            --- drops them, so this reads as an omission unless said out loud.
            if not flags.alt and not flags.ctrl then
                if flags.shift then
                    if keyCode == kBlackKeyCode and flags.cmd then
                        return false
                    end
                elseif flags.cmd and keyCode == kBlackKeyCode then
                    --- Rung three. Like rung two it does not end the
                    --- blackout, and like rung two it can only make the way
                    --- out stricter.
                    return false
                end
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

local function stopNoteTimer(st)
    if st.noteTimer then
        st.noteTimer:stop()
        st.noteTimer = nil
    end
    st.pendingChord = nil
end

--- Only a dofile into a live Hammerspoon gets here with a previous state; the
--- ordinary reload is hs.reload(), a fresh Lua state, and every timer dies
--- with the old one. A note countdown caught by a dofile is stopped with the
--- rest, and the blackout it was holding back goes with it, deliberately: the
--- note file is not moved aside until the blackout actually fires, so the
--- next one shows the same note again and nothing is lost.
if previousState and previousState ~= blackoutLockState then
    stopTap(previousState)
    stopRestoreTimer(previousState)
    stopNoteTimer(previousState)
    if previousState.chordTap then previousState.chordTap:stop() end
end

--- ** Interface

function blackoutLockActive()
    return blackoutLockState.tap ~= nil
end

--- How many events the source-state rule has let through since this file was
--- loaded, so "did my synthetic click land" is answerable without a log line
--- per event: read it, post the event, read it again. Counts only that rule --
--- F18 and the two chords are a hand at the keyboard, not automation. A
--- Hammerspoon reload resets it, since it reloads this chunk.
---
--- Read it in a *separate* `hs -c` from the one that posts. The tap callback
--- runs on the main thread's run loop, which has not turned yet when `post()`
--- returns, so a before/post/after sequence inside one invocation always
--- reports no change -- and `hs.timer.usleep` between them makes it worse, not
--- better, since it blocks the very run loop the callback is waiting on. Two
--- of us have now read that as a regression before spotting it.
function blackoutLockPassed()
    return passedCount
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
    -- The lock-first mark has no other way to show itself: all three chords
    -- go equally black, and the mark cannot be revoked once set, so this band
    -- is its one confirmation at the start. Hence its own sentence and its
    -- own colour, and a ladder of them: warn for the plain chord, blood for
    -- lock-first, midnight for lock-now, each darker than the last and all
    -- darker than the crimson `crit' the Secure Input warning below may fire
    -- alongside. The rung-three band is up for about as long as it takes the
    -- login window to cover it; that is still the moment before the screen
    -- goes, with the person looking at it, which is what makes it worth
    -- drawing. blackoutUpgrade and blackoutLockNow reuse the colours for a
    -- rung reached later, where the screen is already black and a sound has
    -- to carry it instead.
    local rung = st.rung or (st.lockFirst and 2 or 1)
    local text, color = lockBand(rung, false)

    alert(text, {
        id = kAlertId,
        color = color,
        seconds = rung >= 2 and 5 or 4,
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

--- Gives the keyboard back, and does nothing else. In particular it does not
--- touch `since' or `lockFirst': ending the *input lock* is not the same event
--- as the *blackout* ending, and only the second one may revise the decision
--- that ending the blackout locks the screen. It used to clear both here, so
--- anything that merely wanted its keyboard back -- `hs -c blackoutLockOff()'
--- most of all -- silently cancelled a lock-first mark, and F2 afterwards
--- restored straight onto a live desktop. The mark is supposed to move only
--- toward locking; that path moved it the other way, and did it invisibly.
---
--- In practice the two do go together, because the lock is armed with the mark
--- and released when the black ends. That stays the normal case: every caller
--- that really is ending the blackout calls blackoutEnded below as well, and
--- blackoutRestore does both in the right order. What changed is only that
--- this function no longer assumes it.
---
--- silent skips the release flash, for the wake and expiry paths where nobody
--- is watching the screen come back.
function blackoutLockOff(silent)
    local st = blackoutLockState
    local wasActive = blackoutLockActive()

    stopTap(st)
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

--- The blackout itself is over: forget when it began and whether ending it
--- locks the screen, and drop the saved copy. The other half of the split
--- above, and the *only* place the lock-first mark is ever cleared -- so the
--- one way to lose it is the black actually ending, which is what the module
--- has always claimed and now does.
---
--- Every path that ends a blackout calls this: blackoutRestore below, the wake
--- watcher in core/power-watcher.lua, and zsh's display-black-off, which is
--- the point every unblack route reaches. Missing one is not dangerous in the
--- way the old clobber was -- a stale `since' makes shouldLockScreen say yes,
--- so the failure is an extra lock screen rather than a skipped one, which is
--- the direction this module errs in everywhere else.
---
--- Clearing `since' also makes the saved key go away, since persist() deletes
--- it when there is nothing black. The reload path is unchanged and needs no
--- migration: the format is the same two fields, and a key left behind by a
--- caller that forgot to call this is already handled -- blackoutLockRecover
--- deletes it as stale when zsh's display_black_saved is gone.
function blackoutEnded()
    local st = blackoutLockState

    st.since = nil
    st.lockFirst = false
    st.rung = nil
    persist(st)

    return true
end

--- What hyper+shift+F1 calls, hyper+shift+cmd+F1 with lockFirst=true, and
--- hyper+cmd+F1 with lockNow=true as well. Records when the black began,
--- whether or not the keyboard lock is on, because the lock-before-restore
--- rule needs the age either way. A second F1 during a blackout keeps the
--- original time, and nothing here can downgrade the mark or the rung: the
--- parameters only ever raise them. Raising them on a blackout that is
--- already up is blackoutUpgrade's and blackoutLockNow's job, which is what
--- the chords reach then, so that the garden is not sent to re-black an
--- already black screen.
---
--- lockNow does not lock anything here. It only names the rung, so the band
--- blackoutLockOn is about to draw can say which chord asked for it; the lock
--- itself is blackoutLockNow's, placed after this returns.
function blackoutBegin(lockFirst, lockNow)
    local st = blackoutLockState
    if not st.since then
        st.since = hs.timer.secondsSinceEpoch()
    end
    if lockFirst or lockNow then
        st.lockFirst = true
    end
    st.rung = math.max(st.rung or 0, lockNow and 3 or lockFirst and 2 or 1)
    persist(st)
    if blackoutLockEnabled then
        blackoutLockOn()
    end
    return true
end

--- What hyper+shift+cmd+F1 does to a blackout that is already up: sets the
--- mark, and nothing else. Not blackoutChordBegin, which would send the garden
--- off to re-run brightness-off-all-loop and restart the keep-blank loop on a
--- screen that is already black. Not blackoutLockOn either, which would
--- rebuild the tap and restart the expiry from this press rather than from the
--- start of the black -- handing a forgotten blackout another
--- blackoutLockMaxSeconds is the opposite of what pressing cmd asks for. Only
--- the mark moves, and it only ever moves toward locking.
---
--- Returns false when no blackout is up, which is the caller's cue to start
--- one instead.
function blackoutUpgrade()
    local st = blackoutLockState
    if not st.since then return false end

    st.lockFirst = true
    st.rung = math.max(st.rung or 0, 2)
    persist(st)

    --: Shown even when the mark was already set. Re-flashing is the only way a
    --: second press can look like anything other than a dropped one.
    alert("Ending the blackout locks the screen.", {
        id = kAlertId,
        color = "blood",
        seconds = 5,
        screens = "all",
    })

    --- Behind a real blackout that band is invisible -- which is why
    --- blackoutLockOn shows its own *before* the screen goes dark -- so the
    --- sound is the confirmation that actually reaches a person here. The band
    --- goes up regardless: it costs nothing, it is right in the moment before
    --- the garden's black lands, and it is what a screen-lit test can see.
    playCue("upgrade", blackoutUpgradeSound)

    return true
end

--- Rung three, hyper+cmd+F1. Rung two marks a blackout so that *ending* it
--- locks the screen; this one stops waiting for the ending and locks the
--- session now.
---
--- Both cases live here rather than split across this file and
--- window-media-bindings.lua, because they are one act reached from two
--- places: the mark, the lock, and -- only when nothing is black yet -- the
--- garden. The one thing it borrows from that file is blackoutChordBegin,
--- which is exactly what runChordNow below borrows for the other two rungs.
---
--- Ordering, fresh: black first, lock second, both inside one run-loop turn.
--- Everything blackoutChordBegin does is non-blocking -- the garden call goes
--- out over hs.task, the tap is installed, the band is queued -- so the lock
--- is requested a fraction of a millisecond later, and the band gets its turn
--- to draw before the login window covers it. The other way round it would be
--- drawn behind the lock screen and never seen at all. Nothing is lost by
--- blacking first: brightness-off-all-loop runs in the garden, which does not
--- care what the session is doing, and display-black-on-loop re-asserts gamma
--- at the login screen just as happily.
---
--- Ordering, on a blackout already up: the mark is set before the lock, so a
--- lockScreen that somehow never arrives still leaves a blackout that locks on
--- the way out.
---
--- No sound on a fresh start, a sound on an escalation. Sound is what this
--- module reaches for when there is no screen left to say anything on; on a
--- fresh start there is one, still lit, with the person looking at it. A
--- repeat press at this rung replays the cue, for the same reason
--- blackoutUpgrade re-flashes: a second press has to look like something.
---
--- The way back is unlocking the session; the header says how that reaches
--- this module. Returns whether it started a blackout, as opposed to locking
--- one already up.
function blackoutLockNow()
    local st = blackoutLockState

    if not st.since then
        if blackoutChordBegin then
            blackoutChordBegin(true, true)
        else
            --- window-media-bindings.lua failed to load, so there is no garden
            --- call to make. Arm and lock anyway: a lit screen on a locked
            --- session beats a chord that did nothing.
            blackoutBegin(true, true)
        end
        lockSession()
        return true
    end

    st.lockFirst = true
    st.rung = 3
    persist(st)

    alert("Locking the session now.", {
        id = kAlertId,
        color = "midnight",
        seconds = 5,
        screens = "all",
    })

    playCue("lock-now", blackoutLockNowSound)
    lockSession()

    return false
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
        --- Both halves, and this is the path that proves the split is safe:
        --- shouldLockScreen has already read the mark, above, before either
        --- call can clear it. Order within restore() is therefore free, but
        --- the lock comes off first so the keyboard is back at the earliest
        --- moment, exactly as before.
        blackoutLockOff(lockFirst)
        blackoutEnded()
        brishz_eval_hs('awaysh-fast brightness-on-all-loop', 'blackout-restore')
    end

    if lockFirst then
        lockSession()
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
--- merely irritating, and was reported independently. All of them now take the
--- delivery path this module already trusts for the lock itself, the
--- hyper+ctrl+F1/F2 contrast pair included -- it is the same two keycodes on
--- the same tap, so it would be dropped the same way.
---
--- A side effect worth keeping: nothing modal binds bare F1/F2 any more, which
--- is what used to shadow STT's globals on every hyper transition and fill the
--- console with "Disabled previous hotkey F1" pairs. Those STT binds have since
--- been retired as well -- core/stt.lua keeps dictation on its hyper chords --
--- so no hs.hotkey contends for these two keycodes at all now.
---
--- The tap runs only while hyper mode is entered -- which pressing the chord
--- requires anyway -- so it is off almost always, for the privacy and latency
--- reasons in core/fim.lua. hyper-mode.lua starts and stops it from the
--- modality's entered/exited callbacks.

--- The same two constants the lock tap matches on, so the taps cannot drift
--- apart on a keycode.
local kChordKeys = {
    [kBlackKeyCode] = "f1",
    [kEscapeKeyCode] = "f2",
}

--- Which chord this event is, or nil for anything the tap must not touch.
--- `fn' is deliberately not tested: every F-key press here carries it,
--- successes included, so Carbon ignores it too and matching on it would
--- reject every real press.
local function chordFor(keyName, flags)
    --- The way out first, and leniently: F2 with shift is the escape whatever
    --- else is down. Sticky Keys is on, and a modifier left stuck by the last
    --- thing typed must not turn the one chord that ends a blackout into a
    --- dropped press -- in front of a locked keyboard that is the failure
    --- this whole module exists to prevent. Nothing is given away: restore
    --- is the way out already, and a stray cmd cannot make F2 mean anything
    --- else, because F2 has no other meaning here. The F1 rungs stay strict,
    --- since there the modifiers are what tells the rungs apart.
    if keyName == "f2" and flags.shift then return "restore" end

    --- Contrast, on ctrl. Placed after the escape above so the way out of a
    --- blackout keeps winning over everything, and before the blanket reject
    --- below, which is what used to drop these.
    ---
    --- ctrl rather than alt was chosen on a live test: ctrl+F1 and ctrl+F2 do
    --- nothing on this machine today. On paper they should -- entries 12 (full
    --- keyboard access) and 7 (focus the menu bar) in
    --- com.apple.symbolichotkeys.plist are both still `enabled', on keycodes
    --- 122 and 120 with mask 0x840000, and that 0x800000 is not the fn key
    --- (there is none here; hidutil maps fn to F18, the hyper toggle) but the
    --- tag macOS puts on every function-key event, which is exactly what the
    --- note below records. They appear to be dead entries left behind when Full
    --- Keyboard Access moved to its own Accessibility control. If one ever
    --- wakes up, alt binds nothing on F1/F2 and this is where to move it.
    if flags.ctrl and not flags.alt and not flags.shift and not flags.cmd then
        if keyName == "f1" then return "contrast-dec" end
        if keyName == "f2" then return "contrast-inc" end
        return nil
    end

    if flags.alt or flags.ctrl then return nil end

    if flags.shift then
        if keyName == "f1" then
            return flags.cmd and "black-lock-first" or "black"
        end
        return nil
    end

    --- No shift. cmd on F1 is rung three, "lock the session now and black";
    --- cmd on F2 stays nil, because the way *out* never grows a cmd variant.
    --- The mark belongs to whoever starts the black, and whoever presses F2
    --- later may be a stranger.
    ---
    --- Without cmd: the brightness keys, dispatched here for the same reason
    --- -- they get dropped too, and so is the contrast pair above. Swallowed
    --- rather than passed on, so that nothing downstream can claim them:
    --- hyper+F1 meant dictation for as long as STT bound the bare keys
    --- globally, and the next binder would collide the same way.
    if flags.cmd then
        if keyName == "f1" then return "black-lock-now" end
        return nil
    end
    if keyName == "f1" then return "brightness-dec" end
    if keyName == "f2" then return "brightness-inc" end
    return nil
end

--- Which rung a chord asks for. Doubles as the "does this chord start a
--- blackout" test, which is what the note gate below needs -- `restore' exits
--- the mode too but starts nothing. Used to resolve a press that lands during
--- a note countdown: the higher of the two wins, never the later one.
local kChordRung = {
    ["black"] = 1,
    ["black-lock-first"] = 2,
    ["black-lock-now"] = 3,
}

--- The blackout chords are one-shot and leave the mode, exactly as an
--- auto-trigger hs.hotkey binding did -- the two cmd ones included when they
--- only act on a blackout already up rather than starting one. The level keys,
--- brightness and contrast alike, deliberately do not: holding hyper and
--- stepping repeatedly is the point, which is why they were bound with
--- auto_trigger_p=false.
local kChordExitsMode = {
    ["black"] = true,
    ["black-lock-first"] = true,
    ["black-lock-now"] = true,
    ["restore"] = true,
}

--- What a chord actually does, once the note gate in blackoutChordRun below
--- has had its turn.
local function runChordNow(chord)
    if chord == "restore" then
        if blackoutChordRestore then blackoutChordRestore() end
    elseif kChordRung[chord] and blackoutLockState.since then
        --- A blackout is already up, so climb the ladder rather than start a
        --- second one. Keyed on `since' and not blackoutLockActive(), because
        --- that one asks after the *tap*, and with blackoutLockEnabled false
        --- there is no tap to ask while a blackout is very much up. `since' is
        --- the one field that means "a blackout is up" in both configurations.
        ---
        --- Rung one falls through to blackoutChordBegin on purpose, as it
        --- always did: with the keyboard lock on it never gets here (the gate
        --- in handleChordEvent swallows it), and with the lock off, re-running
        --- the garden is the old behaviour and harmless.
        if chord == "black-lock-now" then
            blackoutLockNow()
        elseif chord == "black-lock-first" then
            blackoutUpgrade()
        elseif blackoutChordBegin then
            blackoutChordBegin(false, false)
        end
    elseif chord == "black-lock-now" then
        blackoutLockNow()
    elseif kChordRung[chord] then
        if blackoutChordBegin then blackoutChordBegin(chord == "black-lock-first", false) end
    elseif chord == "contrast-dec" or chord == "contrast-inc" then
        --- Explicit, and before the brightness arm below, which is a catch-all:
        --- anything reaching it is assumed to be a brightness step.
        if hyperContrastStep then
            hyperContrastStep(chord == "contrast-dec" and "dec" or "inc")
        end
    elseif hyperBrightnessStep then
        hyperBrightnessStep(chord == "brightness-dec" and "dec" or "inc")
    end
end

--- ** The blackout note
--- Read at chord time, moved aside when the blackout it delayed actually
--- fires -- not when it is shown. A reload, or a Hammerspoon killed
--- mid-countdown, therefore loses the blackout and keeps the note, which is
--- the right way round: a note nobody has read yet is worth more than a
--- blackout nobody asked for twice. The knobs are under ** Configuration.

local function noteFilePath()
    local file = tostring(blackoutNoteFile or "")
    if file == "" then return nil end
    --- `~' is the shell's, not the filesystem's. Expanded here so the knob may
    --- be written either way and still match the zsh side's default.
    local home = os.getenv("HOME")
    if home and file:sub(1, 1) == "~" then
        file = home .. file:sub(2)
    end
    return file
end

--- The pending note's text, or nil when there is none. Read-only, so a caller
--- may ask as often as it likes. The same io.open/read("a") the alert engine's
--- alertV2FromFile uses, and for the same reason: a path is cheap to carry and
--- a payload is not.
function blackoutNoteText()
    local file = noteFilePath()
    if not file then return nil end

    local handle = io.open(file, "r")
    if not handle then return nil end
    local text = handle:read("a")
    handle:close()

    text = (text or ""):gsub("%s+$", "")
    if text == "" then return nil end
    return text
end

--- Moves the note aside so the next blackout does not show it again.
--- os.rename over the destination, because POSIX rename(2) replaces it: one
--- syscall, and ".last" is always either the whole previous note or the whole
--- one before it, never half of either. If it fails -- a read-only home, a
--- path that has become a directory -- the note is deleted instead, because a
--- note that cannot be moved aside would otherwise reappear at every blackout
--- for ever.
local function noteArchive()
    local file = noteFilePath()
    if not file then return false end

    local ok = os.rename(file, file .. ".last")
    if not ok then
        os.remove(file)
        return false
    end
    return true
end

--- Whether a note is on screen holding a blackout back. For tests, and for
--- the skip in blackoutChordRun.
function blackoutNotePending()
    return blackoutLockState.noteTimer ~= nil
end

--- Puts the note on screen and touches nothing else -- no blackout, no rename
--- -- for looking at the rendering:  hs -c 'blackoutNoteShow()'
--- Returns the text it showed, or nil when there is no note.
---
--- Centre rather than top: the top strip is where the agent banner and the
--- "Input locked" band live, and this one is meant to be *read* in the few
--- seconds before the screen goes, not noticed out of the corner of an eye.
--- Pinned for the same reason -- a wall of command output elsewhere must not
--- push it off. Block-aligned, so a list of bullets keeps one left edge and
--- still sits in the middle of the screen. No countdown: it would be appended
--- to the last bullet and read as part of it, and the band's own lifetime is
--- the countdown anyway.
function blackoutNoteShow(seconds)
    local text = blackoutNoteText()
    if not text then return nil end

    alert(text, {
        id = kNoteAlertId,
        markup = "md",
        color = blackoutNoteColor,
        textSize = blackoutNoteTextSize,
        align = "block",
        seconds = math.max(1, tonumber(seconds) or blackoutNoteSeconds),
        pinned = true,
        position = "center",
        screens = "all",
    })
    return text
end

--- Fires the blackout the note was holding back: the wait ends here, whether
--- it ran out or a second press cut it short. `chord' overrides what was
--- pending; blackoutChordRun passes the *higher* of the two rungs.
function blackoutNoteFire(chord)
    local st = blackoutLockState

    chord = chord or st.pendingChord or "black"
    stopNoteTimer(st)
    dismiss(kNoteAlertId)
    noteArchive()
    runChordNow(chord)

    return chord
end

--- The note gate. True means it has taken the chord: the note is up, and the
--- blackout will fire when the timer does. False means there was no note and
--- the caller should get on with it.
local function noteGate(chord)
    local st = blackoutLockState
    if not blackoutNoteShow() then return false end

    stopNoteTimer(st)
    st.pendingChord = chord

    --- The lock band goes up now as well, in the future tense, so the top of
    --- the screen says what is about to happen while the middle says what to
    --- remember. Same id as the band blackoutLockOn will draw, which then
    --- replaces the promise in place. Only when the lock is enabled, since
    --- that is the only case in which the promise is true; a skip press that
    --- raises the rung is answered by blackoutLockOn's own band, so this one
    --- need not follow it.
    if blackoutLockEnabled then
        local text, color = lockBand(kChordRung[chord] or 1, true)
        alert(text, {
            id = kAlertId,
            color = color,
            seconds = math.max(1, tonumber(blackoutNoteSeconds) or 1) + 1,
            screens = "all",
        })
    end
    --: The same floor blackoutNoteShow applies, so the band and the timer
    --: cannot disagree about how long the note has.
    st.noteTimer = hs.timer.doAfter(math.max(1, tonumber(blackoutNoteSeconds) or 1), function()
        blackoutNoteFire()
    end)
    return true
end

--- The chord dispatcher, and the shell's way into the whole sequence:
---   hs -c 'blackoutChordRun("black")'
--- Global for that reason; handleChordEvent below is the only other caller.
function blackoutChordRun(chord)
    local st = blackoutLockState

    --- A press landing during a note countdown skips the rest of the wait.
    --- Nothing cancels the blackout -- the note is a warning, not a
    --- confirmation prompt -- so the only question is which rung fires, and
    --- the answer is the higher of the two, never the later one. A rung-one
    --- press arriving after a rung-three one would otherwise unlock what the
    --- rung-three press had already decided, and in this module the mark only
    --- ever moves toward locking.
    ---
    --- Anything without a rung -- the way out, a brightness step from a second
    --- hyper entry -- runs as usual and leaves the countdown alone.
    if blackoutNotePending() and kChordRung[chord] then
        local pending = st.pendingChord
        if (kChordRung[pending] or 0) > kChordRung[chord] then
            chord = pending
        end
        return blackoutNoteFire(chord)
    end

    --- A blackout only now starting is what a note is for. An escalation of
    --- one already up is not: the screen is black, nobody can read anything,
    --- and the person pressing it is asking for the lock, not for a delay
    --- before it.
    if kChordRung[chord] and not st.since then
        if noteGate(chord) then return chord end
    end

    runChordNow(chord)
    return chord
end

local function handleChordEvent(event)
    local keyName = kChordKeys[event:getKeyCode()]
    if not keyName then return false end

    local chord = chordFor(keyName, event:getFlags())
    --: Anything with no chord of its own is left alone entirely.
    if not chord then return false end

    --- While the lock is up three chords still do something: the one that
    --- ends the blackout, and the two that tighten it -- hyper+shift+cmd+F1
    --- marks a blackout already up as lock-first, and hyper+cmd+F1 locks the
    --- session outright. Nothing goes the other way, and plain hyper+shift+F1
    --- stays swallowed, since re-blacking a black screen would only restart
    --- the garden's loop.
    ---
    --- The same rule handleEvent enforces above, repeated here so the outcome
    --- does not depend on which of the two taps macOS happens to call first --
    --- and it really does vary: hs.eventtap inserts at the head of the chain,
    --- and this tap is rebuilt on every hyper entry while the lock's is built
    --- once at black-on, so this one is almost always asked first. Almost.
    --- Edit the two together, or the ladder acquires a rung that works only
    --- when nothing is black.
    if blackoutLockActive()
        and chord ~= "restore"
        and chord ~= "black-lock-first"
        and chord ~= "black-lock-now" then
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
            blackoutChordRun(chord)
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
    --- Rung three is not saved (see the state table), so a recovered rung-three
    --- blackout comes back as rung two. Nothing downstream can tell: the
    --- session is either still locked, which the login screen says for
    --- itself, or was unlocked, in which case h-hook-unlock has ended the
    --- blackout already and there is nothing to recover.
    st.rung = st.lockFirst and 2 or 1

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
