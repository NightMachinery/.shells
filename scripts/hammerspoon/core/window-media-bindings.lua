---
function bindWithRepeat(mods, key, pressedfn)
    -- WAIT [[id:6b34d65d-0fe4-4fcc-b388-39d532880a6c][@me {FR} Add the ability to customize the repeat rate for `repeatfn` per hotkey · Issue #3587 · Hammerspoon/hammerspoon]]
    --
    -- The above is especially needed for pageup and pagedown.
    ---
    if mods == "hyper" or mods == hyper or not mods then
        hyper_bind_v2{
            key=key,
            pressedfn=pressedfn,
            repeatfn=pressedfn,
        }
    else
        alert_gateway("bindWithRepeat: with mods", { color = "notice" })
        hs.hotkey.bind(mods, key, pressedfn, nil, pressedfn)
    end
end

function bindWithRepeatV2(params)
    local binder = params.binder or hyper_bind_v2
    params.repeatfn = params.repeatfn or params.pressedfn

    binder(params)
end
----
function pressPageUp()
    eventtap.keyStroke({}, hs.keycodes.map['pageup'])
end
bindWithRepeat(hyper, "up", pressPageUp)

bindWithRepeat(hyper, "down", function()
                   eventtap.keyStroke({}, hs.keycodes.map['pagedown'])
end)

-- =bindToKey= did not repeat these keys.
-- bindToKey{
--     binder = hyper_bind_v2,
--     from = "down",
--     to = "pagedown",
-- }
-- bindToKey{
--     binder = hyper_bind_v2,
--     from = "up",
--     to = "pageup",
-- }
---
-- hyper+F5: toggle the default mic's mute. The common case, a mic with its
-- own mute control and no soft mute in effect, is done here: flip its
-- inputMuted, say so, and refresh the menubar. That works while BrishGarden
-- is down, and skips the round trip. Everything else goes to zsh's
-- input-volume-mute-toggle, because its soft mute (for an iPhone mic, which
-- has no mute control) is a state machine in redis that every shell shares
-- (input_soft_mute_device): a mic with no mute control, or a soft mute in
-- effect, or a stale claim of one for zsh to clean up. A redis that is down
-- reads as no soft mute, as it does in zsh.
-- @duplicateCode/41f14dff47c97b495877bd43aa221281: input-volume-mute-toggle
-- in zshlang/auto-load/others/system.zsh: the plain-toggle branch, the band's
-- text, id and time, and the menubar refresh.
-- You can set hyper+F5 to the dictation command in macOS settings.
local kInputMuteAlertId = "volume-mute-input"

function inputMuteToggle()
    local dev = hs.audiodevice.defaultInputDevice()
    local muted = dev and dev:inputMuted()
    local soft = redisGet and redisGet("input_soft_mute_device")
    if muted == nil or (soft ~= nil and soft ~= "") then
        brishz_eval_hs('awaysh-fast input-volume-mute-toggle', 'hyper+F5')
        return
    end

    dev:setInputMuted(not muted)
    alert_gateway(dev:inputMuted() and "input muted" or "INPUT UNMUTED",
                  { id = kInputMuteAlertId, seconds = 2, flashSeconds = 0.35 })
    gardenTask("/usr/bin/open", { "-g", "xbar://app.xbarapp.com/refreshPlugin?path=date.1m.bash" },
               function() end, 5, nil, "inputMuteToggle")
end

hyper_bind_v1("f5", function() inputMuteToggle() end)

-- Bare hyper+F1/F2 steps brightness; hyper+ctrl+F1/F2 steps contrast. Both are
-- dispatched from the eventtap in core/blackout-lock.lua along with the
-- blackout chords, because Carbon drops these too. Key repeat comes free
-- there: a tap sees the autorepeat keyDowns, which is what bindWithRepeatV2's
-- repeatfn was for.
--
-- The stepping itself is core/level-stepper.lua, which coalesces presses so a
-- key hold cannot outrun the DDC bus; the measurements that forced that are in
-- its header. Brightness and contrast are the same problem on the same bus
-- under the same lock, so they are two instances of it rather than two copies.
--
-- Both act on the active screen's display, the one with the focused window,
-- resolved per press through core/screens.lua. Contrast on the laptop panel,
-- which has none, goes to the external displays instead.
--
-- The knobs it seeds, per instance and live-editable from the console; their
-- defaults are kKnobDefaults in core/level-stepper.lua:
--   hyper_brightness_step              hyper_contrast_step
--   hyper_brightness_band_seconds      hyper_contrast_band_seconds
--   hyper_brightness_bar_cells         hyper_contrast_bar_cells
--   hyper_brightness_trust_seconds     hyper_contrast_trust_seconds
--   hyper_brightness_screens           hyper_contrast_screens
--   hyper_brightness_fallback_screens  hyper_contrast_fallback_screens
local brightnessStepper = levelStepperNew{
    title = "Brightness",
    id = "hyper-brightness",
    family = "brightness",
    knobPrefix = "hyper_brightness_",
    label = "hyperBrightnessStep",
}

local contrastStepper = levelStepperNew{
    title = "Contrast",
    id = "hyper-contrast",
    family = "contrast",
    knobPrefix = "hyper_contrast_",
    label = "hyperContrastStep",
    -- IOKit has no contrast, so the built-in panel is never a target.
    internalOK = false,
}

--- Globals, because core/blackout-lock.lua's chord tap calls them by name and
--- tests them for existence first, so a half-loaded config degrades to a
--- dropped keypress rather than an error per press.
---
--- `dir' is the direction, not a boolean: the shell side is brightness-dec and
--- brightness-inc, so that is what travels.
function hyperBrightnessStep(dir)
    brightnessStepper.step(dir)
end

function hyperContrastStep(dir)
    contrastStepper.step(dir)
end
-- `-all`, so these blank every display rather than just whichever is currently
-- main. Blanking only the main one leaves the other screen lit, which defeats
-- the point when the lid is open.
--
-- The `-loop` versions, because a one-shot blackout does not stay: macOS
-- restores gamma and brightness on wake, on a display reconfiguration, and
-- whenever a DDC write is lost. F1 starts a background loop that re-asserts it
-- every few seconds; F2 stops that loop and restores the levels.
--
-- F1 also locks the keyboard and mouse for the life of the blackout (see
-- core/blackout-lock.lua), unless blackoutLockEnabled is false. The lock lets
-- exactly three chords through: F2, shift+cmd+F1, and cmd+F1. F2 goes through
-- blackoutRestore, which releases the lock synchronously and, once the
-- blackout is older than blackoutLockScreenAfterSeconds, locks the session
-- before restoring, so a long-unwatched screen comes back as a login window.
-- shift+cmd+F1 starts a blackout that locks first regardless of age, and
-- pressed during one already up it marks that one instead, through
-- blackoutUpgrade rather than through this file -- the screen is already
-- black, so there is nothing for the garden to do. The mark only ever moves
-- toward locking: the person starting the black decides, since whoever
-- presses F2 later may be a stranger.
--
-- hyper+cmd+F1 is the rung above that: it stops waiting for the ending and
-- locks the session now, through blackoutLockNow in core/blackout-lock.lua:
-- the blackout is released, the panel slept and the session locked. There is
-- no chord back from it -- chords cannot reach the login screen, where Secure
-- Input hides keystrokes from every tap -- so the way back is unlocking the
-- session, which runs h-hook-unlock -> h-blackout-release in the garden.
--
-- When the garden call that starts a blackout provably never reached the
-- garden, nothing went black, so the keyboard lock that went up with it is
-- taken back and a band says so; otherwise it would be a locked keyboard in
-- front of a lit screen. Only for a blackout this press started: one already
-- up keeps its lock and its mark. Ending one without the garden is
-- blackoutNativeRelease's job.
--
-- A blackout that starts may first be held back by a note for a few seconds;
-- see "The blackout note" in core/blackout-lock.lua and alert-at-next-blackout
-- in zsh. That gate sits in front of this function, not inside it.
--
-- These four chords are not hs.hotkey bindings, which is why only their
-- actions live here. Carbon drops roughly one press in five -- a shrug for a
-- brightness step, unacceptable for a chord that blanks the screen and for the
-- only way back from a locked keyboard -- so core/blackout-lock.lua dispatches
-- them from an eventtap and owns their delivery. The bare F1/F2 brightness
-- keys above stay on hs.hotkey. See "When a hyper chord does nothing" in
-- docs/hammerspoon.md.
--
-- Rungs one and two only. hyper+cmd+F1 does not come through here: it starts
-- no blackout, it sleeps the panel and locks, which is display-off-lock in the
-- garden. See blackoutLockNow.
function blackoutChordBegin(lockFirst)
    local fresh = not (blackoutLockState and blackoutLockState.since)
    brishz_eval_hs('awaysh-fast brightness-off-all-loop', 'blackout', {
        onFail = function(_, notSent)
            if not (fresh and notSent) then return end
            if blackoutLockOff then blackoutLockOff(true) end
            if blackoutEnded then blackoutEnded() end
            alert_gateway("No blackout: BrishGarden is down, so the keyboard is unlocked again; run ivy",
                          { id = "garden-down", color = "crit", seconds = 15, screens = "all" })
        end,
    })
    if blackoutBegin then blackoutBegin(lockFirst) end
end

function blackoutChordRestore()
    if blackoutRestore then
        blackoutRestore()
    else
        brishz_eval_hs('awaysh-fast brightness-on-all-loop')
    end
end

-- Nothing dispatches the chords if that module failed to load, since the tap
-- is its. Fall back to hs.hotkey for the way *out*, which is the one that has
-- to exist even on a half-loaded config -- a flaky F2 beats no F2 at all. The
-- blackout chords themselves, all three rungs, are deliberately not restored
-- here: without blackout-lock there is no keyboard lock to escape from either.
if not blackoutChordTapStart then
    hyper_bind_v2{
        mods={"shift"},
        key="F2",
        pressedfn=blackoutChordRestore,
    }
end
---

-- hyper+F6: toggle Do Not Disturb, through the same two Shortcuts as zsh,
-- run here so it works while BrishGarden is down: 'Get Focus' writes the
-- current focus to a .txt file (the extension picks the output type), then
-- 'Focus Off' or 'Focus Set: Do Not Disturb'. The band takes the zsh side's
-- id, text and timing, so a toggle from either side rewrites the same band.
-- @duplicateCode/db397b926549ecdad428d7482282e83e: focus-do-not-disturb-toggle,
-- focus-get, focus-off and focus-do-not-disturb-on in
-- zshlang/auto-load/others/macOS/focus.zsh.
local kFocusDndAlertId = "focus-dnd"

local function focusDndBand(text, color)
    alert_gateway(text, { id = kFocusDndAlertId, seconds = 5, flashSeconds = 0.35, color = color })
end

function focusDndToggle()
    local label = "focusDndToggle"
    local tmp = os.tmpname()
    os.remove(tmp)
    tmp = tmp .. ".txt"
    gardenTask("/usr/bin/shortcuts", { "run", "Get Focus", "-o", tmp }, function(code, _, err)
        local f = io.open(tmp, "r")
        local focus = f and f:read("a") or ""
        if f then f:close() end
        os.remove(tmp)
        if code ~= 0 then
            print(label .. ": Get Focus exited " .. tostring(code) .. ": " .. err)
            return focusDndBand("Do Not Disturb: could not read the focus (" .. tostring(code) .. ")", "warn")
        end

        local on = focus:gsub("^%s+", ""):gsub("%s+$", "") == "Do Not Disturb"
        gardenTask("/usr/bin/shortcuts", { "run", on and "Focus Off" or "Focus Set: Do Not Disturb" },
                   function(code2, _, err2)
            if code2 ~= 0 then
                print(label .. ": shortcut exited " .. tostring(code2) .. ": " .. err2)
                return focusDndBand("Do Not Disturb: the shortcut failed (" .. tostring(code2) .. ")", "warn")
            end
            focusDndBand(on and "Do Not Disturb: off" or "Do Not Disturb: ON")
        end, 20, nil, label)
    end, 20, nil, label)
end

hyper_bind_v1("F6", function() focusDndToggle() end)

hyper_bind_v1("F7", function()
                  -- brishz_eval_hs('awaysh-fast hear-prev')
                  mediaPreviousKey()
end)

hyper_bind_v1("F8", function()
                  -- brishz_eval_hs('awaysh-fast hear-play-toggle')
                  mediaPlayPauseKey()
end)

hyper_bind_v1("F9", function()
                  -- brishz_eval_hs('awaysh-fast hear-next')
                  mediaNextKey()
end)

hyper_bind_v1("F10", function()
                  -- brishz_eval_hs('awaysh-fast volume-mute-toggle')
                  volumeMuteKey()
end)

function volumeInc(v, device)
    v = v or 5

    d = device or hs.audiodevice.defaultOutputDevice()
    v = d:outputVolume() + v
    if v > 100 then
        v = 100
    end
    if v < 0 then
        v = 0
    end

    d:setOutputVolume(v)

    -- A held volume key fires this repeatedly; the id makes each press rewrite
    -- the same band rather than stacking a new one per repeat.
    alert_gateway("vol: " .. math.floor(v + 0.5), { id = "volume", seconds = 0.4 })
end

bindWithRepeatV2{
    binder=hyper_bind_v2,
    key="F11",
    pressedfn=function()
        ---
        -- brishz_eval_hs('awaysh-fast volume-dec')
        ---
        -- volumeInc(-5)
        volumeDecKey()
        ---
    end,
    auto_trigger_p=false
}
bindWithRepeatV2{
    binder=hyper_bind_v2,
    key="F12",
    pressedfn=function()
        ---
        -- brishz_eval_hs('awaysh-fast volume-inc')
        ---
        -- volumeInc(5)
        volumeIncKey()
        ---
    end,
    auto_trigger_p=false,
}
--- * Mission Control, Window/App Switcher, Expose
hyper_bind_v2{
    -- key="e",
    mods={"ctrl"}, key='[',
    pressedfn=hs.spaces.toggleMissionControl
}
-- Use Escape to exit the mission control.
-- You can move between spaces as you always do, i.e., with hyper+arrows.

--- ** hs.expose
-- [[id:adf82ba0-fccb-4922-bb6b-cdaab1fe0411][@upstreamBug? =hs.expose= doesn't show all apps.]]
if false then
    hs.expose.ui.fitWindowsInBackground = false
    -- hs.expose.ui.fitWindowsInBackground = true

    expose = hs.expose.new(
        nil, {
            fitWindowsInBackground = hs.expose.ui.fitWindowsInBackground, -- probably @redundant
            showThumbnails=true,
            -- showThumbnails=false,
            onlyActiveApplication=false,
            includeOtherSpaces=true,
    })

    -- expose_app = hs.expose.new(nil,{onlyActiveApplication=true})
    -- show windows for the current application

    -- expose_space = hs.expose.new(nil,{includeOtherSpaces=false})
    -- current space only

    -- expose_browsers = hs.expose.new{'Safari','Google Chrome'}
    -- -- specialized expose using a custom windowfilter
    -- for your dozens of browser windows :)

    -- then bind to a hotkey
    hyper_bind_v2{mods={"ctrl"}, key='[', pressedfn=function()expose:toggleShow()end}
end
--- * kitty (hyper+z)
--
-- kitty is only ever reached through this key, and a press must show all
-- the tabs of the one kitty instance (bundle net.kovidgoyal.kitty; nothing
-- else in this config knows any other). Two modes, chosen by
-- `kitty_hotkey_mode' below:
--
--   "panel"  (default) every tab lives in a kitty *panel* OS window that
--            floats over whatever is in front, fullscreen apps included, and
--            hides again on the next press. The main kitty creates it for
--            itself over remote control, asked by kittyPanelShow and
--            kittyPanelHide in core/kitty-panel.lua (async hs.task calls on
--            `kitten @', so nothing here blocks and nothing needs the
--            garden). kitty's own quick-access kitten was rejected because it
--            runs as a second app bundle.
--   "window" a normal OS window, shown maximized on the mouse's screen and
--            hidden on the next press. From a fullscreen app this switches to
--            kitty's desktop and back, as macOS itself would: a normal window
--            cannot be put over a fullscreen space, and Hammerspoon cannot
--            move one there (a forced hs.spaces.moveWindowToSpace into a
--            fullscreen space returns true and does nothing; measured
--            2026-09-07, macOS 14.3.1 / Hammerspoon 1.1.1).
--
-- Both modes share the launch (kitty quits when its last window closes),
-- the return of focus after a hide, and the eviction of a window that was
-- born inside someone else's fullscreen space (macOS parks a new window in
-- whatever space is active; that was "hyper+z keeps opening Telegram").
--
-- The panel was reverted once today over black frames on key presses, until
-- a kitty restart cured the same frames on the normal window: the
-- long-running process was to blame, not the panel. The story, with
-- measurements: ~/notes/public/subjects/tools/CLI/terminal emulators/Kitty/hotkey window.org

kitty_hotkey_mode = kitty_hotkey_mode or "panel"

local kittyBundleID = "net.kovidgoyal.kitty"

--- ** Shared: spaces, eviction, focus return

local function spaceIsUser(spaceID)
    return hs.spaces.spaceType(spaceID) == "user"
end

-- The first user space on `screen`: where a normal window lives when it is
-- not a guest.
local function kittyHomeSpace(screen)
    for _, sid in ipairs(hs.spaces.spacesForScreen(screen) or {}) do
        if spaceIsUser(sid) then return sid end
    end
    return nil
end

local function windowInSpace(win, spaceID)
    return has_value(hs.spaces.windowSpaces(win) or {}, spaceID)
end

local function windowInAnyUserSpace(win)
    for _, sid in ipairs(hs.spaces.windowSpaces(win) or {}) do
        if spaceIsUser(sid) then return true end
    end
    return false
end

-- Evicts `win` from a fullscreen space to the user space of its screen.
-- Leaving a fullscreen space needs the `force' flag (without it: "source
-- space ... is not a user space"). No-op when it already sits in a user
-- space.
local function kittyEvictFromFullscreen(win)
    if windowInAnyUserSpace(win) then return end
    local home = kittyHomeSpace(win:screen())
    if not home then return end
    local ok, err = hs.spaces.moveWindowToSpace(win, home, true)
    if not ok then
        alert_gateway("kitty: could not leave fullscreen space: " .. tostring(err), { color = "warn" })
    end
end

-- The panel is kitty's one non-standard window.
local function kittyPanelWindow(app)
    for _, w in ipairs(app:allWindows()) do
        if not w:isStandard() then return w end
    end
    return nil
end

-- Whether the panel is on screen: one Accessibility query to kitty alone,
-- 1.6 ms. isVisible as well, in case a hidden panel is ever still listed.
local function kittyPanelShown(app)
    local w = app and kittyPanelWindow(app)
    return (w and w:isVisible()) and true or false
end

local function kittyStandardWindow(app)
    for _, w in ipairs(app:allWindows()) do
        if w:isStandard() then return w end
    end
    return nil
end

-- Where hiding puts you back: the newest app in recentApps
-- (core/app-hotkeys.lua) other than kitty, read at press time. The same
-- list the app hotkeys return through, fed by an application watcher on
-- every switch, so it is never stale (the old `kitty_prev_app' was, whenever
-- kitty had been reached by another route, and nil after every reload).
local function kittyReturnCandidates()
    return recentAppsCandidates(function(bid) return bid == kittyBundleID end)
end

-- Focuses the first candidate that is still running and not hidden
-- (appReturnUsable). A dead hs.application raises or answers nil, hence the
-- pcall.
local function kittyFocusAfterHide(candidates)
    for _, back in ipairs(candidates or {}) do
        local ok, done = pcall(function()
            if not appReturnUsable(back) then return false end
            local win = back:focusedWindow()
            if win then win:focus() else back:activate() end
            return true
        end)
        if ok and done then return end
    end
end

-- In panel mode this watcher hides the panel when kitty is left by any route
-- other than hyper+z (an app hotkey, Cmd-Tab, a click): a panel floats above
-- fullscreen windows, so unlike a normal window it cannot be put behind the
-- app you just switched to. kitty's own hide-on-focus-loss would do this too,
-- but it also hides on Maccy and Handy. The check is one Accessibility query
-- to kitty alone; a hidden panel has no visible windows.
--
-- Global, not local: Hammerspoon only keeps a watcher alive while something
-- references it, and a file-level local is gone once the file has loaded.
-- As a local this watcher worked for a few minutes after every reload and
-- then silently stopped, which looked like "hyper+l leaves the panel on top".
kittyFocusWatcher = hs.application.watcher.new(function(_, event, app)
    if event ~= hs.application.watcher.activated or not app then return end
    local bid = app:bundleID()
    if bid == kittyBundleID or recentAppsTransient[bid] then return end

    if kitty_hotkey_mode == "panel" then
        local kitty = getApp(kittyBundleID)
        if kitty and kittyPanelWindow(kitty) then
            kittyPanelHide("kittyFocusWatcher")
        end
    end
end)
kittyFocusWatcher:start()

-- The app in front when kitty was asked for. recentAppsWatcher has it
-- already, unless it came to the front before the last reload.
local function kittyRemember(front)
    if front then recentAppsPush(front) end
end

--- ** Panel mode

-- Show or hide is decided here, by whether kitty is frontmost *and* its panel
-- is on screen; the panel itself is kitty's business. Frontmost alone is not
-- enough: when a dialog (a sudo password prompt, say) takes focus, the
-- watcher hides the panel, and when the dialog closes macOS hands focus back
-- to kitty with nothing on screen. Deciding by frontmost, every press then
-- "hid" the hidden panel, and it came back only after another app had been
-- focused by hand. When kitty had to be launched, the show can take a few
-- seconds while the session's tabs start; nothing waits.
function kittyPanelToggle(app, front, shown)
    if shown == nil then shown = app and app:isFrontmost() and kittyPanelShown(app) end
    if shown then
        -- Read now, not in the timer: the hide may activate something and
        -- the watcher would overwrite the memory before the timer fires.
        local back = kittyReturnCandidates()
        kittyPanelHide("kittyPanelToggle")
        hs.timer.doAfter(0.35, function() kittyFocusAfterHide(back) end)
        return
    end

    kittyRemember(front)
    kittyPanelShow("kittyPanelToggle")
end

--- ** Window mode

function kittyWindowToggle(app, front)
    if not app then
        hs.application.launchOrFocusByBundleID(kittyBundleID)
        return
    end

    local win = kittyStandardWindow(app)
    if not win then
        app:activate()
        return
    end

    if app:isFrontmost() then
        -- Read before hide(): the activation hide() causes updates the memory.
        local back = kittyReturnCandidates()
        kittyEvictFromFullscreen(win)
        app:hide()
        kittyFocusAfterHide(back)
        return
    end

    kittyRemember(front)

    -- Show, on the screen the mouse is on.
    local mouseScreen = hs.mouse.getCurrentScreen()
    if win:screen():id() ~= mouseScreen:id() then
        win:moveToScreen(mouseScreen)
    end

    kittyEvictFromFullscreen(win)

    -- Between user spaces the move works and saves a space switch. A
    -- fullscreen target is left alone: see the header.
    local target = hs.spaces.activeSpaceOnScreen(mouseScreen)
    if target and spaceIsUser(target) and not windowInSpace(win, target) then
        local ok, err = hs.spaces.moveWindowToSpace(win, target)
        if not ok then
            alert_gateway("kitty: could not move to this space: " .. tostring(err), { color = "warn" })
        end
    end

    app:activate()
    win:focus()
    win:maximize()
end

--- ** The key

function kittyHandler()
    -- For the "since press" figure that core/kitty-panel.lua prints.
    kittyPressAt = hs.timer.absoluteTime()

    -- getApp (core/app-hotkeys.lua) is a bundle-ID lookup that never
    -- enumerates every running process.
    local app = getApp(kittyBundleID)
    local front = hs.application.frontmostApplication()
    local frontmost = app and app:isFrontmost()
    local panel = kitty_hotkey_mode == "panel"
    local shown = panel and frontmost and kittyPanelShown(app)

    -- One line per press in the console, so "it did nothing" can be traced:
    -- the mode, what was in front, whether the panel was on screen, and which
    -- way this press went.
    print(string.format("kittyHandler: press (%s); kitty %s%s; frontmost=%s; -> %s",
                        kitty_hotkey_mode,
                        app and (frontmost and "frontmost" or "running") or "not running",
                        (panel and frontmost) and (shown and ", panel shown" or ", panel hidden") or "",
                        front and front:name() or "?",
                        (panel and shown or (not panel and frontmost)) and "hide" or "show"))

    if panel then
        kittyPanelToggle(app, front, shown)
    else
        kittyWindowToggle(app, front)
    end
end

hyper_bind_v1('z', kittyHandler)

---
function escapeTripleQuotes(s)
    local result = {}
    local i = 1
    local len = #s

    while i <= len do
        local c = s:sub(i, i)
        if c == '"' and s:sub(i + 1, i + 2) == '""' and (i == 1 or s:sub(i - 1, i - 1) ~= '\\') then
            table.insert(result, '\\')
            table.insert(result, '"""')
            i = i + 3
        else
            table.insert(result, c)
            i = i + 1
        end
    end

    return table.concat(result)
end
-- function escapeTripleQuotes(s)
--     -- Escape unescaped triple quotes
--     ---
--     -- @GPT4T
--     -- Lua's pattern matching system does not support lookbehind assertions.
--     -- This pattern matches a sequence that is either at the start of the string (^)
--     -- or not following a backslash ([^\\]), followed by triple quotes.
--     -- The parentheses around [^\\] and the triple quotes capture these sequences for use in the replacement.
--     local pattern = "(^|[^\\])\"\"\""

--     -- The replacement adds a backslash before the triple quotes.
--     -- %1 refers to the first captured group (either the start of the string or a character that is not a backslash),
--     -- ensuring we preserve it in the output.

--     -- local result = s:gsub(pattern, "%1\\\"\"\"")
--     local result = s:gsub(pattern, "ICED")

--     return result
-- end

function pasteBlockified()
    -- Get the clipboard content
    local clipboardContent = hs.pasteboard.getContents()
    if clipboardContent then
        -- Remove trailing whitespace
        clipboardContent = string.gsub(clipboardContent, "%s*$", "")

        if active_app_re_p("emacs|kitty", "insensitive") then
            clipboardContent = escapeTripleQuotes(clipboardContent)
        end
    end

    -- Check if the clipboard content contains multiple lines.
    -- [[https://stackoverflow.com/questions/55586867/how-to-put-in-markdown-an-inline-code-block-that-only-contains-a-backtick-char][How to put (in markdown) an inline code block that only contains a backtick character (`) - Stack Overflow]]
    if true or clipboardContent:match("\n") then
        -- Multiple lines
        -- Update: I have made this codepath be taken even for single line inputs, as that behavior is more often wanted.
        markdownContent = "```\n" .. clipboardContent .. "\n```\n"
    else
        -- Single line
        clipboardContent = string.gsub(clipboardContent, "^%s*", "")

        if clipboardContent:match("`") then
            markdownContent = "```" .. clipboardContent .. "```"
        else
            markdownContent = "`" .. clipboardContent .. "`"
        end
    end

    -- Put the markdownContent back to clipboard
    hs.pasteboard.setContents(markdownContent)

    -- Trigger a "paste" event
    doPaste()

    -- Set the original clipboard content back to clipboard after some delay (in seconds)
    hs.timer.doAfter(0.2, function() hs.pasteboard.setContents(clipboardContent) end)
end

hyper_bind_v1(",", pasteBlockified)
