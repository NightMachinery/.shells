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

bindWithRepeatV2{
    binder=hyper_bind_v2,
    key="F1",
    pressedfn=function()
        brishz_eval_hs('awaysh-fast brightness-dec')
    end,
    auto_trigger_p=false
}
bindWithRepeatV2{
    binder=hyper_bind_v2,
    key="F2",
    pressedfn=function()
        brishz_eval_hs('awaysh-fast brightness-inc')
    end,
    auto_trigger_p=false
}

-- `-all`, so these blank every display rather than just whichever is currently
-- main. Blanking only the main one leaves the other screen lit, which defeats
-- the point when the lid is open.
--
-- The `-loop` versions, because a one-shot blackout does not stay: macOS
-- restores gamma and brightness on wake, on a display reconfiguration, and
-- whenever a DDC write is lost. F1 starts a background loop that re-asserts it
-- every few seconds; F2 stops that loop and restores the levels.
hyper_bind_v2{
    mods={"shift"},
    key="F1",
    pressedfn=function()
        brishz_eval_hs('awaysh-fast brightness-off-all-loop')
    end,
}
hyper_bind_v2{
    mods={"shift"},
    key="F2",
    pressedfn=function()
        brishz_eval_hs('awaysh-fast brightness-on-all-loop')
    end,
}
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
--- * kitty toggle (hyper+z)
--
-- Not an `appHotkey`, for two reasons. kitty quits when its last window
-- closes, so the hotkey has to launch it. And a window of kitty's can end up
-- inside someone else's *fullscreen* space (macOS parks a window there when
-- it is created while that space is active; kitty's "claude-work-slides"
-- window was found living in Purple Telegram's), after which every activation
-- of kitty, from anywhere, jumps to that app's space. That was the "hyper+z
-- keeps opening Telegram" bug, and its cousin "shows the desktop": once the
-- owner left fullscreen, macOS dumped the window on the lone user space.
--
-- The cure is to evict the window back to a user space whenever it is found
-- in a fullscreen one, both before showing and before hiding, so activation
-- always lands on kitty's own space. This needs the `force' flag of
-- hs.spaces.moveWindowToSpace; without it, leaving a fullscreen space is
-- refused ("source space ... is not a user space"). Measured 2026-09-07 on
-- macOS 14.3.1 / Hammerspoon 1.1.1: forced 1774 (fullscreen) -> 5 (user)
-- returned true and took effect.
--
-- What this cannot do is the reverse: bring kitty *over* a fullscreen app.
-- hs.spaces refuses a plain move into a fullscreen space, and a forced one
-- returns true and does nothing (also measured). The previous handler tried
-- exactly that on every press. From a fullscreen space, hyper+z therefore
-- switches to kitty's user space, which is what macOS itself does; the
-- quick-access kitty on hyper+shift+z is the one that floats over fullscreen.
--
-- Hiding has to put you back where you were. When showing switched spaces
-- (kitty on the desktop, you on a fullscreen Brave), macOS does not switch
-- back on hide; it activates whatever is next in its own order, which was
-- Telegram. So the window that was in front at show time is remembered and
-- focused again on hide. The old handler kept a "previous app" too, but it
-- went stale whenever kitty was reached by any other route (and nil after
-- every reload). Here the memory is a window, and an application watcher
-- forgets it the moment anything other than kitty is activated, so it only
-- ever describes the show that is still in effect.

local kittyBundleID = "net.kovidgoyal.kitty"

local function spaceIsUser(spaceID)
    return hs.spaces.spaceType(spaceID) == "user"
end

-- The first user space on `screen`: where kitty lives when it is not a guest.
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

-- The window to return to on hide; nil unless a hyper+z show is in effect.
local kittyReturnTo = nil

local kittyFocusWatcher = hs.application.watcher.new(function(_, event, app)
    if event == hs.application.watcher.activated
        and app and app:bundleID() ~= kittyBundleID then
        kittyReturnTo = nil
    end
end)
kittyFocusWatcher:start()

local function kittyReturn()
    local back = kittyReturnTo
    kittyReturnTo = nil
    if not back then return end
    -- The window may have closed since; a dead hs.window answers nil.
    local ok = pcall(function()
        if back:application() then back:focus() end
    end)
    if not ok then
        print("kittyHandler: could not return to the previous window")
    end
end

-- Evicts `win` from a fullscreen space to the user space of its screen.
-- No-op when it already sits in a user space.
local function kittyEvictFromFullscreen(win)
    if windowInAnyUserSpace(win) then return end
    local home = kittyHomeSpace(win:screen())
    if not home then return end
    local ok, err = hs.spaces.moveWindowToSpace(win, home, true)
    if not ok then
        alert_gateway("kitty: could not leave fullscreen space: " .. tostring(err), { color = "warn" })
    end
end

function kittyHandler()
    -- getApp (core/app-hotkeys.lua) is a bundle-ID lookup that never
    -- enumerates every running process.
    local app = getApp(kittyBundleID)
    if not app then
        hs.application.launchOrFocusByBundleID(kittyBundleID)
        return
    end

    local win = app:focusedWindow() or app:mainWindow() or app:allWindows()[1]
    if not win then
        app:activate()
        return
    end

    if app:isFrontmost() then
        kittyEvictFromFullscreen(win)
        app:hide()
        -- Must stay synchronous right after hide(): the activation that
        -- hide() causes reaches kittyFocusWatcher on the next run-loop turn
        -- and clears kittyReturnTo. A doAfter here would return nowhere.
        kittyReturn()
        return
    end

    kittyReturnTo = hs.window.frontmostWindow()

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

hyper_bind_v1('z', kittyHandler)

--- * Quick-access kitty (hyper+shift+z)
-- kitty's own dropdown terminal. It is a panel window that floats over
-- fullscreen apps (measured: shown over Telegram's fullscreen space with no
-- space switch), so it needs none of the juggling above. Running the kitten
-- toggles it: the first run *is* the instance and lives as long as it does,
-- later runs tell it to show or hide and exit at once. It is a separate kitty
-- instance (bundle net.kovidgoyal.kitty-quick-access, app name
-- kitty-quick-access), configured fullscreen and opaque in
-- configFiles/kitty/quick-access-terminal.conf; it does not show the regular
-- kitty's windows.
--
-- Not `--detach`: on this machine the detached child died silently (exit 0,
-- no log, no instance). hs.task is already asynchronous, so the first run
-- simply stays a running task; the table keeps it from being collected.
--
-- Hiding leaves the quick-access app active with no window (measured), so
-- keystrokes would go nowhere. When the press is a hide, focus the topmost
-- remaining window on the current space once the panel is gone.
local kittenBin = "/Applications/kitty.app/Contents/MacOS/kitten"
local quickAccessBundleID = "net.kovidgoyal.kitty-quick-access"
local quickAccessTasks = {}

local function quickAccessRefocus()
    for _, win in ipairs(hs.window.orderedWindows()) do
        local app = win:application()
        if app and app:bundleID() ~= quickAccessBundleID then
            win:focus()
            return
        end
    end
end

function quickAccessKittyToggle()
    local hiding = hs.application.frontmostApplication():bundleID() == quickAccessBundleID

    local task
    task = taskWithPath(kittenBin, function(code, _, err)
        quickAccessTasks[task] = nil
        if code ~= 0 then
            print("quick-access kitty: kitten exited " .. tostring(code) .. ": " .. tostring(err))
        end
    end, {"quick-access-terminal"})
    if task then
        quickAccessTasks[task] = true
        task:start()
    end

    if hiding then
        hs.timer.doAfter(0.3, quickAccessRefocus)
    end
end

hyper_bind_v2{ mods={"shift"}, key='z', pressedfn=quickAccessKittyToggle }
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
