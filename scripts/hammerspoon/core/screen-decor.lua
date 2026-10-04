--- * Screen Decor: a fullscreen stand-in for an empty desktop
--- An app that activates with only a panel over another app's fullscreen
--- Space (the kitty panel, hyper+z) hands the active display, the lit menu
--- bar where Maccy and the menu-bar extras open, to a display that shows a
--- desktop Space. A display showing a fullscreen Space is never chosen. So
--- before such a show, ScreenDecor.coverFor puts every other screen that
--- sits on an empty desktop into a fullscreen Space of Screen Decor's own:
--- one window painted with that screen's desktop picture, which looks like
--- the desktop it covers. screen_decor_cover_with can instead bring forward
--- an app already fullscreen on that screen. See docs/multi-monitor.md,
--- "Which display macOS counts as active", for the measurements behind this.
---
--- Screen Decor is the small Cocoa app in screen-decor/ next to this file.
--- It is built on demand into screen_decor_app (screen-decor/build.sh),
--- and rebuilt whenever a source is newer than the build. Its first cover
--- launches it, and it stays running afterwards; a later cover only asks the
--- running app over a distributed notification, and its window keeps its
--- fullscreen Space, so the cost is that Space's slide.
---
--- Generic: nothing here knows about kitty. kittyPanelToggle
--- (core/window-media-bindings.lua) calls coverFor when kitty_panel_decor_p
--- is on.

ScreenDecor = ScreenDecor or {}

local kBundleID = "night.screen-decor"
local kCoverNote = "night.screen-decor.cover"

-- Where the app is built and run from.
if screen_decor_app == nil then screen_decor_app = os.getenv("HOME") .. "/Applications/ScreenDecor.app" end

-- What covers a screen. An enum:
--   "decor"           always Screen Decor, so the screen keeps looking like
--                     the desktop it was left on
--   "fullscreen-app"  bring forward an app that is already fullscreen in
--                     another Space of that screen (the newest one,
--                     screenAppWindowOn in core/app-hotkeys/main.lua), and
--                     use Screen Decor only when there is none
if screen_decor_cover_with == nil then screen_decor_cover_with = "decor" end

local srcDir = nightdir .. "/hammerspoon/screen-decor"
local kSources = { "ScreenDecor.m", "Info.plist", "build.sh" }

-- How long a cover may take before coverFor gives up waiting: the first one
-- launches the app, the others only slide a Space in.
local kLaunchWaitSeconds, kWarmWaitSeconds = 4, 1.5
-- How long to wait for the re-focused window's app to come to the front.
local kRefocusWaitSeconds = 0.6
local kPollSeconds = 0.02

local function mtime(path)
    return hs.fs.attributes(path, "modification")
end

-- Whether the built app is missing or older than any of its sources.
local function stale()
    local built = mtime(screen_decor_app .. "/Contents/MacOS/ScreenDecor")
    if not built then return true end
    for _, f in ipairs(kSources) do
        local m = mtime(srcDir .. "/" .. f)
        if m and m > built then return true end
    end
    return false
end

-- Builds the app in the background when it is stale. A cover asked for
-- meanwhile is skipped rather than kept waiting for the compiler.
function ScreenDecor.ensureBuilt()
    if ScreenDecor.building or not stale() then return end
    local running = hs.application.get(kBundleID)
    -- A running copy keeps the old binary until it quits; quit it so the next
    -- cover launches the new one.
    if running then running:kill() end
    local key
    local task = hs.task.new(srcDir .. "/build.sh", function(rc, out, err)
        hsUnpin(key)
        ScreenDecor.building = nil
        if rc == 0 then
            print("screen-decor: built " .. screen_decor_app)
        else
            print("screen-decor: build failed (" .. tostring(rc) .. "): " .. tostring(err) .. tostring(out))
        end
    end, { screen_decor_app })
    if not (task and task:start()) then
        return print("screen-decor: could not start " .. srcDir .. "/build.sh")
    end
    key = hsPin(task)
    ScreenDecor.building = key
    print("screen-decor: building " .. screen_decor_app)
end

local function showsFullscreen(screen)
    local ok, t = pcall(function() return hs.spaces.spaceType(hs.spaces.activeSpaceOnScreen(screen)) end)
    return ok and t == "fullscreen"
end

local function showsDesktop(screen)
    local ok, t = pcall(function() return hs.spaces.spaceType(hs.spaces.activeSpaceOnScreen(screen)) end)
    return ok and t == "user"
end

-- The screens coverFor would cover for a show on `target': every other screen
-- showing a desktop Space with no normal window on it. Empty when `target'
-- shows a desktop Space itself, since the active display then has somewhere
-- to stay. A desktop with windows is left alone: covering it would hide them.
function ScreenDecor.wanted(target, stack)
    local out = {}
    if not (target and showsFullscreen(target)) then return out end
    stack = stack or Screens.windowStack()
    for _, r in ipairs(Screens.list()) do
        local s = r.screen
        if s:id() ~= target:id() and showsDesktop(s) and #Screens.normalWindowsOn(s, nil, stack) == 0 then
            out[#out + 1] = s
        end
    end
    return out
end

-- Polls pred() every kPollSeconds until it holds or `seconds' pass, then
-- calls cb(held).
local function waitFor(pred, seconds, cb)
    local deadline = hs.timer.secondsSinceEpoch() + seconds
    local function tick()
        if pred() then return cb(true) end
        if hs.timer.secondsSinceEpoch() >= deadline then return cb(false) end
        hsAfter(kPollSeconds, tick)
    end
    tick()
end

-- An app already fullscreen in another Space of `screen', as a
-- screenReturnTarget-shaped table, or nil.
local function fullscreenAppOn(screen)
    if not screenAppWindowOn then return nil end
    local ok, t = pcall(screenAppWindowOn, screen, function(w) return w:isFullScreen() end)
    return ok and t or nil
end

-- Covers `screens' and calls cb(ok) once every one shows a fullscreen Space.
-- Each is covered as screen_decor_cover_with says.
function ScreenDecor.cover(screens, cb)
    if #screens == 0 then return cb(true) end
    local decorScreens = {}
    for _, s in ipairs(screens) do
        local t = screen_decor_cover_with == "fullscreen-app" and fullscreenAppOn(s)
        if t then
            print("screen-decor: covering " .. tostring(s:name()) .. " with " .. tostring(t.app:bundleID()))
            screenReturnFocus(t, "screen-decor")
        else
            decorScreens[#decorScreens + 1] = s
        end
    end
    if screen_decor_cover_with ~= "decor" and screen_decor_cover_with ~= "fullscreen-app" then
        print("screen-decor: unknown screen_decor_cover_with " .. tostring(screen_decor_cover_with) .. "; using the decor")
    end

    local app = hs.application.get(kBundleID)
    local wait = kWarmWaitSeconds
    if #decorScreens == 0 then
        -- Every screen got an app; nothing for the decor to do.
    elseif app then
        for _, s in ipairs(decorScreens) do
            hs.distributednotifications.post(kCoverNote, s:getUUID())
        end
    else
        local argv = { "-a", screen_decor_app, "--args" }
        for _, s in ipairs(decorScreens) do
            argv[#argv + 1] = "--cover"
            argv[#argv + 1] = s:getUUID()
        end
        local key
        local opener = hs.task.new("/usr/bin/open", function() hsUnpin(key) end, argv)
        key = opener and opener:start() and hsPin(opener)
        wait = kLaunchWaitSeconds
    end
    waitFor(function()
        for _, s in ipairs(screens) do
            if not showsFullscreen(s) then return false end
        end
        return true
    end, wait, cb)
end

-- The window to give focus back to on `target' once the decor has taken
-- it: the focused window if it is there, else the front one there.
local function windowToRestore(target, stack)
    local ok, w = pcall(function()
        local f = hs.window.focusedWindow()
        if f and f:screen() and f:screen():id() == target:id() then return f end
        return nil
    end)
    if ok and w then return w end
    return Screens.frontWindowOn(target, stack)
end

--- Prepares a show on `target' (an hs.screen) of a panel that activates its
--- app: covers what ScreenDecor.wanted names (with the decor or a
--- fullscreen app, per screen_decor_cover_with), gives focus back to the window
--- the user was in on `target', and then calls cb(covered), where covered
--- says whether anything was covered. Nothing to cover calls cb(false) at
--- once, and so does a cover skipped while the app builds; a cover that
--- times out still calls cb(true), so the show goes ahead either way. A call
--- made while another cover is still under way is dropped and returns false:
--- the show that one is preparing is already coming.
function ScreenDecor.coverFor(target, cb)
    if ScreenDecor.covering then
        print("screen-decor: a cover is already in progress; dropping this one")
        return false
    end
    local stack = Screens.windowStack()
    local screens = ScreenDecor.wanted(target, stack)
    if #screens == 0 then return cb(false) end
    if ScreenDecor.building or stale() then
        ScreenDecor.ensureBuilt()
        return cb(false)
    end

    local t0 = hs.timer.absoluteTime()
    local back = windowToRestore(target, stack)
    local token = {}
    ScreenDecor.covering = token
    local function ms() return (hs.timer.absoluteTime() - t0) / 1e6 end

    ScreenDecor.cover(screens, function(ok)
        if ScreenDecor.covering ~= token then return end
        local coveredAt = ms()
        local okb, frontBid = pcall(function() return hs.application.frontmostApplication():bundleID() end)
        frontBid = okb and frontBid or "?"
        if not ok then print(string.format("screen-decor: cover not seen after %.0f ms; going on", coveredAt)) end
        if not back then
            ScreenDecor.covering = nil
            print(string.format("screen-decor: covered %d screen(s) in %.0f ms; no window to give focus back to", #screens, coveredAt))
            return cb(true)
        end
        local okf = pcall(function() back:focus() end)
        local pid = okf and back:pid() or nil
        waitFor(function()
            local f = hs.application.frontmostApplication()
            return pid == nil or (f and f:pid() == pid)
        end, kRefocusWaitSeconds, function(refocused)
            ScreenDecor.covering = nil
            print(string.format("screen-decor: covered %d screen(s) in %.0f ms (%s in front), focus back%s at %.0f ms",
                                #screens, coveredAt, tostring(frontBid), refocused and "" or " NOT seen", ms()))
            cb(true)
        end)
    end)
end

ScreenDecor.ensureBuilt()
