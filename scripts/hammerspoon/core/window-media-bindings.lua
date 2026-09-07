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
--- * kitty (hyper+z)
--
-- We only ever reach kitty through this key, and a press must show *all* the
-- tabs over whatever is in front, fullscreen apps included. That rules out a
-- normal OS window: macOS keeps those off fullscreen spaces, and Hammerspoon
-- cannot move one there (a forced hs.spaces.moveWindowToSpace into a
-- fullscreen space returns true and does nothing; measured 2026-09-07, macOS
-- 14.3.1 / Hammerspoon 1.1.1). A previous handler tried exactly that on every
-- press. kitty's window had meanwhile been born inside Telegram's fullscreen
-- space (macOS parks a new window in whatever space is active), so every
-- activation jumped to Telegram, and to the desktop once Telegram left
-- fullscreen.
--
-- A *panel* window floats over everything, and the main kitty can create one
-- for itself and move its tabs into it. That part lives in zsh,
-- [agfi:kitty-panel-ensure] / [agfi:kitty-panel-show] / [agfi:kitty-panel-hide]
-- (zshlang/auto-load/others/terminal emulators/kitty.zsh), because it is a
-- chain of dependent `kitty @' calls with jq in between; the garden runs it
-- without blocking this thread. kitty's own quick-access kitten was rejected:
-- it runs as a second app bundle (net.kovidgoyal.kitty-quick-access) that
-- nothing else in our code knows about.
--
-- This side only decides show or hide, by whether kitty is frontmost, and
-- puts you back where you were. When kitty had to be launched, the show can
-- take a few seconds while the session's tabs start; nothing here waits.
--
-- Hiding has to put you back where you were: a panel that goes away leaves
-- focus on a window-less kitty, and macOS does not hand it on by itself. The
-- old handler kept a "previous app" that went stale whenever kitty was
-- reached by another route. Here an application watcher keeps the last
-- non-kitty app that was activated, so the memory is refreshed by every
-- switch you make and is never older than the show in effect. It is an app
-- rather than a window, and nothing enumerates windows: hs.window.orderedWindows
-- asks every process through Accessibility, and the "Handy Web Content"
-- processes take 1.5 s each to answer (axLatencyReport), so one call stalled
-- Hammerspoon for three seconds.

local kittyBundleID = "net.kovidgoyal.kitty"

-- The app to return to on hide: the last one activated that is not kitty.
local kittyReturnTo = nil

-- Apps that take focus for a moment and give it straight back. The panel
-- must survive them, and they must not become the return target either.
-- hyper+v is a passthrough key (core/hyper-mode.lua) that reaches Maccy;
-- cmd+' is Handy's dictation overlay; Hammerspoon activates itself for
-- choosers and for the Secure Input webview on hyper.
local kittyTransientBundles = {
    ["org.hammerspoon.Hammerspoon"] = true,
    ["org.p0deje.Maccy"] = true,
    ["com.pais.handy"] = true,
}

-- Leaving kitty by any route other than hyper+z (an app hotkey, Cmd-Tab, a
-- click) must hide the panel too. It sits in the overlay layer, above even
-- fullscreen windows, so unlike the old normal window it cannot be put
-- behind the app you just switched to: hyper+l used to "show kitty" because
-- the still-visible panel covered Telegram. kitty's own hide-on-focus-loss
-- would do this, but it also hides on Maccy and Handy, hence this watcher
-- with an allowlist instead. The visibility check is one Accessibility
-- query to kitty alone; a hidden panel has no visible windows.
local kittyFocusWatcher = hs.application.watcher.new(function(_, event, app)
    if event ~= hs.application.watcher.activated or not app then return end
    local bid = app:bundleID()
    if bid == kittyBundleID or kittyTransientBundles[bid] then return end

    kittyReturnTo = app

    local kitty = getApp(kittyBundleID)
    if kitty and #kitty:allWindows() > 0 then
        brishz_eval_hs("kitty-panel-hide", "kittyFocusWatcher")
    end
end)
kittyFocusWatcher:start()

local function kittyFocusAfterHide(back)
    if not back then return end
    -- The app may have quit since; a dead hs.application answers nil.
    pcall(function()
        local win = back:focusedWindow()
        if win then win:focus() else back:activate() end
    end)
end

function kittyHandler()
    -- getApp (core/app-hotkeys.lua) is a bundle-ID lookup that never
    -- enumerates every running process.
    local app = getApp(kittyBundleID)

    -- One line per press in the console, so "it did nothing" can be traced:
    -- what was in front, and which way this press went.
    local front = hs.application.frontmostApplication()
    print(string.format("kittyHandler: press; kitty %s; frontmost=%s; -> %s",
                        app and (app:isFrontmost() and "frontmost" or "running") or "not running",
                        front and front:name() or "?",
                        (app and app:isFrontmost()) and "hide" or "show"))

    if app and app:isFrontmost() then
        -- Read now, not in the timer: the hide may activate something and
        -- the watcher would overwrite the memory before the timer fires.
        local back = kittyReturnTo
        brishz_eval_hs("kitty-panel-hide", "kittyHandler")
        hs.timer.doAfter(0.35, function() kittyFocusAfterHide(back) end)
        return
    end

    if front and front:bundleID() ~= kittyBundleID then
        kittyReturnTo = front
    end
    brishz_eval_hs("kitty-panel-show", "kittyHandler")
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
