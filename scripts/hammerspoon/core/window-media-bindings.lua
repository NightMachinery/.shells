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
-- You can set hyper+F5 to the dictation command in macOS settings.
hyper_bind_v1("f5", function()
                  brishz_eval_hs('awaysh-fast input-volume-mute-toggle')
                  -- @needed awaysh-fast
end)

-- Bare hyper+F1/F2. Dispatched from the eventtap in core/blackout-lock.lua
-- along with the blackout chords, because Carbon drops these too. Key repeat
-- comes free there: a tap sees the autorepeat keyDowns, which is what
-- bindWithRepeatV2's repeatfn was for.
--
-- One DDC operation costs 200-380ms on this panel and a locked `chg' is a read
-- plus a write, so ~600ms; key repeat is ~30ms. Firing one detached job per
-- press used to overlap them ten to one, and unserialised DDC does not lose
-- steps so much as invent them: ten concurrent decrements measured *one step
-- brighter* than they started. h-ddc-lock-do in system.zsh fixed the
-- corruption, but a lock alone turns a one-second key hold into twenty seconds
-- of queue. So the presses are coalesced here, at the source.
--
-- The rule is one garden call in flight at a time. Presses arriving during a
-- flight accumulate into `pending' and leave as a single larger delta, so the
-- total always matches what was pressed while the number of DDC round trips
-- stays proportional to time rather than to keystrokes. The call asks for the
-- new level in the same breath, which costs nothing extra -- the reply is what
-- the band displays, and it makes the optimistic level self-correcting after
-- every flush rather than drifting.
hyper_brightness_step = hyper_brightness_step or 0.01
hyper_brightness_band_seconds = hyper_brightness_band_seconds or 1.5
hyper_brightness_bar_cells = hyper_brightness_bar_cells or 20

--- How long a level read from the panel is worth believing. The cache is only
--- ever authoritative until something else writes, and three other things do:
--- brightness-auto-loop on a 3s cycle, display-black-on-loop on a 5s one, and
--- the monitor's own buttons, which we cannot see at all. Past this the band
--- shows an ellipsis rather than a stale number -- the reply is ~600ms behind
--- the press, and being briefly uninformative beats being briefly wrong.
--- Matched to the fastest of those writers.
hyper_brightness_trust_seconds = hyper_brightness_trust_seconds or 3

local kBrightnessBandId = "hyper-brightness"

--- Accumulated but unsent, in 0..1. Signed.
local brightnessPending = 0
--- True while a garden call is out. The whole serialisation is this flag.
local brightnessInFlight = false
--- Last known levels, one per selected display, or nil before the first reply.
local brightnessLevels = nil
--- When those came off the panel, for hyper_brightness_trust_seconds.
local brightnessLevelsAt = 0

local function brightnessBar(level)
    local cells = hyper_brightness_bar_cells
    local filled = math.max(0, math.min(cells, math.floor(level * cells + 0.5)))
    return string.rep("\u{25AE}", filled) .. string.rep("\u{25AF}", cells - filled)
end

local function brightnessBandShow()
    local fresh = brightnessLevels and #brightnessLevels > 0
        and (hs.timer.secondsSinceEpoch() - brightnessLevelsAt) <= hyper_brightness_trust_seconds

    local text
    if not fresh then
        --- Either nothing has come back yet, or what did is old enough that
        --- another writer may have moved the panel since.
        text = "Brightness\n" .. ("\u{2026}")
    else
        local rows = {}
        for _, level in ipairs(brightnessLevels) do
            rows[#rows + 1] = string.format("%s  %d%%", brightnessBar(level), math.floor(level * 100 + 0.5))
        end
        text = "Brightness\n" .. table.concat(rows, "\n")
    end

    --- flashSeconds 0 deliberately: a fullscreen wash on every step of a key
    --- hold would be unusable. Same id throughout, so the band updates in place
    --- and its deadline is pushed out rather than a second one stacking up.
    alert_gateway(text, {
        id = kBrightnessBandId,
        seconds = hyper_brightness_band_seconds,
        flashSeconds = 0,
        screens = "all",
    })
end

local function brightnessParse(out)
    if not out or out == "" then return nil end

    local levels = {}
    for line in tostring(out):gmatch("[^\r\n]+") do
        local n = tonumber((line:gsub("%s", "")))
        if n then levels[#levels + 1] = math.max(0, math.min(1, n)) end
    end

    if #levels == 0 then return nil end
    return levels
end

local brightnessFlush

--- Applies the delta to what we believe the levels are, so the band can move
--- on the press rather than on the reply. Corrected by every flush.
--- Deliberately leaves brightnessLevelsAt alone: stepping our own guess is not
--- evidence about the panel, so a stale cache stays stale until a reply lands.
local function brightnessStepOptimistic(delta)
    if not brightnessLevels then return end
    for i, level in ipairs(brightnessLevels) do
        brightnessLevels[i] = math.max(0, math.min(1, level + delta))
    end
end

brightnessFlush = function()
    if brightnessInFlight or brightnessPending == 0 then return end

    local delta = brightnessPending
    brightnessPending = 0
    brightnessInFlight = true

    --- One command, one round trip: step, then report where that landed. The
    --- shell's own lock makes the pair atomic against the blackout loop and
    --- brightness-auto.
    local cmd = string.format("brightness-inc %.4f ; brightness-get", delta)
    brishz_eval_out_hs(cmd, function(out)
        brightnessInFlight = false

        local levels = brightnessParse(out)
        if levels then
            brightnessLevels = levels
            brightnessLevelsAt = hs.timer.secondsSinceEpoch()
            brightnessBandShow()
        end

        --- Whatever was pressed while that was out.
        brightnessFlush()
    end, "hyperBrightnessStep")
end

--- `dir' is the direction, not a boolean: the shell side is brightness-dec and
--- brightness-inc, so that is what travels.
function hyperBrightnessStep(dir)
    local delta = (dir == "dec") and -hyper_brightness_step or hyper_brightness_step

    brightnessPending = brightnessPending + delta
    brightnessStepOptimistic(delta)
    brightnessBandShow()
    brightnessFlush()
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
-- exactly the F2 chord through. F2 goes through blackoutRestore, which
-- releases the lock synchronously and, once the blackout is older than
-- blackoutLockScreenAfterSeconds, locks the session before restoring, so a
-- long-unwatched screen comes back as a login window. shift+cmd+F1 starts a
-- blackout that locks first regardless of age: the person starting the black
-- decides, since whoever presses F2 later may be a stranger.
--
-- These three chords are not hs.hotkey bindings, which is why only their
-- actions live here. Carbon drops roughly one press in five -- a shrug for a
-- brightness step, unacceptable for a chord that blanks the screen and for the
-- only way back from a locked keyboard -- so core/blackout-lock.lua dispatches
-- them from an eventtap and owns their delivery. The bare F1/F2 brightness
-- keys above stay on hs.hotkey. See "When a hyper chord does nothing" in
-- docs/hammerspoon.md.
function blackoutChordBegin(lockFirst)
    brishz_eval_hs('awaysh-fast brightness-off-all-loop')
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
-- blackout chords themselves are deliberately not restored here: without
-- blackout-lock there is no keyboard lock to escape from either.
if not blackoutChordTapStart then
    hyper_bind_v2{
        mods={"shift"},
        key="F2",
        pressedfn=blackoutChordRestore,
    }
end
---

hyper_bind_v1("F6", function()
                  brishz_eval_hs('awaysh-fast focus-do-not-disturb-toggle')
end)

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
--            itself over remote control; the zsh side is
--            [agfi:kitty-panel-ensure] / [agfi:kitty-panel-show] /
--            [agfi:kitty-panel-hide], run in the garden so nothing here
--            blocks. kitty's own quick-access kitten was rejected because it
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

local function kittyStandardWindow(app)
    for _, w in ipairs(app:allWindows()) do
        if w:isStandard() then return w end
    end
    return nil
end

-- Where hiding puts you back. An application watcher keeps the last app
-- other than kitty that was activated, so the memory is refreshed by every
-- switch you make and is never stale (the old `kitty_prev_app' was, whenever
-- kitty had been reached by another route, and nil after every reload).
-- Nothing here enumerates windows: hs.window.orderedWindows asks every
-- process through Accessibility, and the "Handy Web Content" processes take
-- 1.5 s each to answer (see axLatencyReport in core/app-hotkeys.lua).
--
-- Apps that take focus for a moment and give it back must not become the
-- return target: Maccy's popup (hyper+v is a passthrough key), Handy's
-- dictation overlay (cmd+'), Hammerspoon itself for choosers and the Secure
-- Input webview on hyper.
local kittyReturnTo = nil
local kittyTransientBundles = {
    ["org.hammerspoon.Hammerspoon"] = true,
    ["org.p0deje.Maccy"] = true,
    ["com.pais.handy"] = true,
}

local function kittyFocusAfterHide(back)
    if not back then return end
    -- The app may have quit since; a dead hs.application answers nil.
    pcall(function()
        local win = back:focusedWindow()
        if win then win:focus() else back:activate() end
    end)
end

-- In panel mode the watcher also hides the panel when kitty is left by any
-- other route (an app hotkey, Cmd-Tab, a click): a panel floats above
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
    if bid == kittyBundleID or kittyTransientBundles[bid] then return end

    kittyReturnTo = app

    if kitty_hotkey_mode == "panel" then
        local kitty = getApp(kittyBundleID)
        if kitty and kittyPanelWindow(kitty) then
            brishz_eval_hs("kitty-panel-hide", "kittyFocusWatcher")
        end
    end
end)
kittyFocusWatcher:start()

local function kittyRemember(front)
    if front and front:bundleID() ~= kittyBundleID and not kittyTransientBundles[front:bundleID()] then
        kittyReturnTo = front
    end
end

--- ** Panel mode

-- Show or hide is decided here, by whether kitty is frontmost; the panel
-- itself is kitty's business. When kitty had to be launched, the show can
-- take a few seconds while the session's tabs start; nothing waits.
function kittyPanelToggle(app, front)
    if app and app:isFrontmost() then
        -- Read now, not in the timer: the hide may activate something and
        -- the watcher would overwrite the memory before the timer fires.
        local back = kittyReturnTo
        brishz_eval_hs("kitty-panel-hide", "kittyPanelToggle")
        hs.timer.doAfter(0.35, function() kittyFocusAfterHide(back) end)
        return
    end

    kittyRemember(front)
    brishz_eval_hs("kitty-panel-show", "kittyPanelToggle")
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
        local back = kittyReturnTo
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
    -- getApp (core/app-hotkeys.lua) is a bundle-ID lookup that never
    -- enumerates every running process.
    local app = getApp(kittyBundleID)
    local front = hs.application.frontmostApplication()

    -- One line per press in the console, so "it did nothing" can be traced:
    -- the mode, what was in front, and which way this press went.
    print(string.format("kittyHandler: press (%s); kitty %s; frontmost=%s; -> %s",
                        kitty_hotkey_mode,
                        app and (app:isFrontmost() and "frontmost" or "running") or "not running",
                        front and front:name() or "?",
                        (app and app:isFrontmost()) and "hide" or "show"))

    if kitty_hotkey_mode == "panel" then
        kittyPanelToggle(app, front)
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
