-- Look up a running app without walking every process on the system.
--
-- `hs.application.get()` wraps `hs.application.find()`, which enumerates every
-- running application and builds an accessibility object for each one. An AX
-- query against a process that cannot answer -- one that is hung, or SIGSTOPped
-- -- blocks until the AX timeout. Since these hotkeys call this on every press,
-- and Hammerspoon's event taps share the same Lua thread, one stuck app adds
-- latency to app switching *and* to every keystroke on the machine.
--
-- That is not hypothetical: on 2026-08-10 a SIGSTOPped Microsoft AutoUpdate,
-- stopped for over two days, made `hs.application.runningApplications()` take
-- more than 60 seconds, and the whole machine felt sluggish as a result.
--
-- `applicationsForBundleID()` maps to NSRunningApplication's
-- runningApplicationsWithBundleIdentifier:, a direct lookup that never touches
-- another app's accessibility interface. Every appName below is a bundle ID,
-- so the fast path always applies.
--
-- The name fallback is skipped for bundle IDs, and that is the point rather
-- than an optimisation. A bundle lookup returns nothing in two cases: the ID
-- is wrong, or -- far more commonly -- the app simply is not running. In the
-- second case falling through cost a full enumeration on *every* press of a
-- hotkey for a not-currently-running app: measured at 114 ms across 98 apps
-- with nothing hung, and it is exactly the path that took 60+ seconds with a
-- SIGSTOPped app on it.
--
-- Falling through cannot help there anyway. `hs.application.get()` matches on
-- bundle ID or name, so if the bundle lookup found nothing, the only way the
-- name lookup finds something is if an app is literally *named* "io.mpv".
-- A dot is the test because bundle IDs are reverse-DNS and app names are not;
-- a plain name like 'mpv' keeps the old behaviour.
function getApp(appName)
    local apps = hs.application.applicationsForBundleID(appName)
    if apps and #apps > 0 then
        return apps[1]
    end

    if appName:find(".", 1, true) then
        return nil
    end

    return hs.application.get(appName)
end

-- Find which app is making app-switching slow.
--
-- `toggleFocus` calls app:isFrontmost() and app:activate(), which are
-- accessibility round-trips to the *target* app. An app that is slow to answer
-- -- busy, hung, or paged out into the compressor -- makes its own hotkey feel
-- laggy, and any code that enumerates all apps feel laggy for everything.
--
-- Run `axLatencyReport()` from the Hammerspoon console while things feel slow.
-- Anything over ~50 ms is suspect; hundreds of ms is your culprit.
function axLatencyReport(limit)
    limit = limit or 20

    local rows = {}
    for _, app in ipairs(hs.application.runningApplications()) do
        local name = app:name() or app:bundleID() or "?"
        local t = hs.timer.absoluteTime()
        -- Cheap AX query: forces a round-trip without changing any state.
        pcall(function() return app:isFrontmost() end)
        local ms = (hs.timer.absoluteTime() - t) / 1e6
        table.insert(rows, { name = name, ms = ms })
    end

    table.sort(rows, function(a, b) return a.ms > b.ms end)

    local out = { string.format("%-34s %10s", "APP", "AX ms") }
    for i = 1, math.min(limit, #rows) do
        table.insert(out, string.format("%-34s %10.1f", rows[i].name:sub(1, 34), rows[i].ms))
    end
    table.insert(out, string.format("(%d apps; anything over ~50 ms is worth a look)", #rows))

    local report = table.concat(out, "\n")
    print(report)
    return report
end

function focusAppYabai(appName)
    local app = getApp(appName)
    if app then
        local mainWindow = app:mainWindow()
        if mainWindow then
            local windowID = mainWindow:id()
            hs.execute("/opt/homebrew/bin/yabai -m window --focus " .. windowID)
            -- brishz_eval_hs("yabai -m window --focus " .. windowID)
        end
    end
end

function focusApp(appName)
    local launch_p = false

    local app = nil
    app = getApp(appName)

    if app then
        if app:isFrontmost() then
        else
            app:activate()
        end
    else
        if launch_p then
            hs.application.launchOrFocus(appName)
            app = getApp(appName)
        end
    end
end

--- * Recent apps
--
-- The apps activated most recently, newest first, one entry per process:
-- where a hide returns you to. The app hotkeys (the second press hides) and
-- the kitty toggle (core/window-media-bindings.lua) both read it. An
-- application watcher feeds it on every activation, so it follows every
-- switch, whatever made it, and nothing here enumerates windows:
-- hs.window.orderedWindows asks every process through Accessibility, and the
-- "Handy Web Content" processes take 1.5 s each to answer.
--
-- Apps that take focus for a moment and give it back are left out, so they
-- never become a return target: Maccy's popup (hyper+v is a passthrough
-- key), Handy's dictation overlay (cmd+'), and Hammerspoon itself, for
-- choosers and the Secure Input webview on hyper. A password dialog (sudo's
-- askpass, say) does get in, and is gone by the next hide; the entries
-- behind it are why this is a list and not one app.
recentAppsTransient = {
    ["org.hammerspoon.Hammerspoon"] = true,
    ["org.p0deje.Maccy"] = true,
    ["com.pais.handy"] = true,
}

recentApps = {}
local recentAppsMax = 8

function recentAppsPush(app)
    local ok, bid, pid = pcall(function() return app:bundleID(), app:pid() end)
    if not ok or not pid or recentAppsTransient[bid] then return end
    for i = #recentApps, 1, -1 do
        local okp, same = pcall(function() return recentApps[i]:pid() == pid end)
        if not okp or same then table.remove(recentApps, i) end
    end
    table.insert(recentApps, 1, app)
    recentApps[recentAppsMax + 1] = nil
end

--- The recent apps, newest first, less those skip(bundleID, pid) rejects.
--- Read it at press time: the switch the press causes updates the list.
function recentAppsCandidates(skip)
    local list = {}
    for _, a in ipairs(recentApps) do
        local ok, keep = pcall(function() return not (skip and skip(a:bundleID(), a:pid())) end)
        if ok and keep then list[#list + 1] = a end
    end
    return list
end

--- Whether `app' can take focus back: still running, and not hidden (you
--- hid it on purpose). A dead hs.application raises or answers nil, hence
--- the pcall. isHidden is one Accessibility query to that app alone.
function appReturnUsable(app)
    local ok, usable = pcall(function() return app:isRunning() and not app:isHidden() end)
    return ok and usable or false
end

--- ** Recent apps per screen
--
-- A hide returns you to the previous app *on the screen you were on*: with
-- one list for every screen, hiding an app on the laptop could hand focus to
-- whatever was last used on the monitor. recentAppsByScreen[uuid] is the
-- same kind of list as recentApps, one per screen. An activation is filed
-- under the active screen a moment after it (hs.screen.mainScreen(), which
-- asks no app anything), and focus moving to another screen without an
-- activation, between two windows of one app, is filed by the registry's
-- `active' event (core/screens.lua).
recentAppsByScreen = {}

local function screenKey(screen)
    local ok, u = pcall(function() return screen:getUUID() end)
    return ok and u and u:upper() or nil
end

-- The delay before reading the active screen after an activation: the
-- screen follows the new key window, which macOS settles after it reports
-- the activation.
local kRecentScreenDelay = 0.1

function recentAppsPushOn(app, screen)
    local key = screen and screenKey(screen)
    local ok, bid, pid = pcall(function() return app:bundleID(), app:pid() end)
    if not key or not ok or not pid or recentAppsTransient[bid] then return end
    local list = recentAppsByScreen[key] or {}
    recentAppsByScreen[key] = list
    for i = #list, 1, -1 do
        local okp, same = pcall(function() return list[i]:pid() == pid end)
        if not okp or same then table.remove(list, i) end
    end
    table.insert(list, 1, app)
    list[recentAppsMax + 1] = nil
end

-- Files the frontmost app under the active screen after kRecentScreenDelay,
-- reading both then: macOS can report the screen change before the
-- activation that caused it, and filing at once would put the app being
-- left under the screen being entered. `pid', when given, files only if
-- that app is still in front by then.
local function recentAppsFileFront(pid)
    hsAfter(kRecentScreenDelay, function()
        local ok, front = pcall(function() return hs.application.frontmostApplication() end)
        if ok and front and (pid == nil or front:pid() == pid) then
            recentAppsPushOn(front, hs.screen.mainScreen())
        end
    end)
end

-- Global, so it is not collected.
recentAppsWatcher = hs.application.watcher.new(function(_, event, app)
    if event ~= hs.application.watcher.activated or not app then return end
    recentAppsPush(app)
    recentAppsFileFront(app:pid())
end)
recentAppsWatcher:start()
if Screens then Screens.on("active", function() recentAppsFileFront() end) end
do
    local front = hs.application.frontmostApplication()
    if front then recentAppsPush(front) end
    recentAppsFileFront()
end

--- Where focus should go when the app with pid `skipPid' leaves `screen':
---   { app = <hs.application>, window = <hs.window or nil>, kittyPanel = true|nil, why = "..." }
--- or nil. In order:
---   1. the newest app filed under `screen' that still has a normal window
---      there. When the app being left is in native fullscreen, the rest of
---      the screen's windows are in another Space, which CoreGraphics'
---      on-screen list leaves out, so there an app filed under the screen
---      with no window on screen anywhere is taken on its record (if
---      appReturnUsable). Anywhere else such an app is passed over: it is a
---      Finder after a click on the desktop, or an app with every window
---      closed or minimized, and activating it would show nothing;
---   2. the frontmost normal window on `screen' of any other app, for apps
---      used before the last reload or pushed out of the list;
---   3. the newest usable app anywhere, the old rule, when nothing else is
---      on that screen: focus has to go somewhere.
--- `window' is set when the app's front window is on another screen, so
--- bringing the app forward would land there: that window is focused
--- instead, and an app whose window cannot be fetched is passed over. The
--- kitty panel is never on screen while another app is frontmost, so in
--- panel mode kitty is taken on its record alone and comes back through
--- kittyPanelShow, which shows on the working screen.
--- `skipBid' rejects one more bundle id (kitty's own toggle passes kitty).
--- One CoreGraphics read (Screens.windowStack) per call. A window in that
--- list belongs to a running app that is not hidden, so steps 1 and 2 ask
--- no app anything until they pick one.
function screenReturnTarget(screen, skipPid, skipBid)
    local function skip(pid, bid) return pid == skipPid or (skipBid and bid == skipBid) end
    local stack = Screens.windowStack()
    local key = screen and screenKey(screen)

    -- Per pid: its first normal window anywhere, and its first on `screen'.
    local frontOf, onScreenOf = {}, {}
    for _, e in ipairs(stack) do
        if Screens.isNormalEntry(e) then
            if not frontOf[e.pid] then frontOf[e.pid] = e end
            if not onScreenOf[e.pid] then
                local s = Screens.screenOfFrame(e.frame)
                if s and screen and s:id() == screen:id() then onScreenOf[e.pid] = e end
            end
        end
    end

    -- Whether the app being left (frontmost, on `screen') is in native
    -- fullscreen: two Accessibility queries to it, asked at most once and
    -- only when step 1 meets an app with no window on screen.
    -- hs.spaces.spaceType would answer too, but took 21 to 37 ms here.
    local fullscreenHere = nil
    local function leavingFullscreen()
        if fullscreenHere == nil then
            local ok, full = pcall(function()
                local w = hs.application.frontmostApplication():focusedWindow()
                return w ~= nil and w:isFullScreen()
            end)
            fullscreenHere = ok and full == true
        end
        return fullscreenHere
    end

    local function landing(app, pid, why, onRecord)
        local e = onScreenOf[pid]
        if not e then
            if onRecord and not frontOf[pid] and leavingFullscreen() and appReturnUsable(app) then
                return { app = app, why = why .. ", in another Space" }
            end
            return nil
        end
        local t = { app = app, why = why }
        if frontOf[pid] ~= e then
            t.window = Screens.entryWindow(e)
            if not t.window then return nil end
        end
        return t
    end

    local panelMode = kitty_hotkey_mode == "panel" and kittyPanelShow
    for _, a in ipairs((key and recentAppsByScreen[key]) or {}) do
        local ok, pid, bid = pcall(function() return a:pid(), a:bundleID() end)
        if ok and not skip(pid, bid) then
            if panelMode and bid == "net.kovidgoyal.kitty" then
                if appReturnUsable(a) then return { app = a, kittyPanel = true, why = "last used on this screen" } end
            else
                local t = landing(a, pid, "last used on this screen", true)
                if t then return t end
            end
        end
    end

    if screen then
        for _, e in ipairs(Screens.normalWindowsOn(screen, nil, stack)) do
            local app = hs.application.applicationForPID(e.pid)
            local ok, bid = pcall(function() return app and app:bundleID() end)
            if app and ok and not skip(e.pid, bid) and not recentAppsTransient[bid] then
                local t = landing(app, e.pid, "front window on this screen")
                if t then return t end
            end
        end
    end

    for _, a in ipairs(recentAppsCandidates(function(bid, pid) return skip(pid, bid) end)) do
        if appReturnUsable(a) then
            local ok, bid = pcall(function() return a:bundleID() end)
            if panelMode and ok and bid == "net.kovidgoyal.kitty" then
                return { app = a, kittyPanel = true, why = "nothing else on this screen" }
            end
            return { app = a, why = "nothing else on this screen" }
        end
    end
    return nil
end

-- One console line per press, so a slow switch can be traced to where the
-- time went: waiting for hyper mode to be entered (a key pressed right after
-- hyper waits for all of it), the handler itself, or the target app, whose
-- activation macOS reports after the handler returns. appSwitchWatcher
-- finishes the line when the activation arrives, or a timer says it never
-- did. Global, like kittyFocusWatcher, so it is not collected.
local appSwitchPending = nil

local function appSwitchMs(from, to)
    return (to - from) / 1e6
end

-- "x 12.3 ms after hyper went down (entering took 4.5: keys 3.0, entered() 1.5)",
-- or "" when hyper is not down.
local function appSwitchHyperNote(t0)
    local m = hyper_mode
    if not (m and m.modality and m.modality.down_p and m.downAt) then return "" end
    local note = string.format("; %.1f ms after hyper went down (entering took %.1f",
                               appSwitchMs(m.downAt, t0), m.downMs or -1)
    if m.enteredAt and m.enteredAt >= m.downAt then
        note = note .. string.format(": keys %.1f, entered() %.1f", appSwitchMs(m.downAt, m.enteredAt),
                                     (m.downMs or 0) - appSwitchMs(m.downAt, m.enteredAt))
    end
    return note .. ")"
end

--- ** Not landing on a floating window
--
-- Bringing an app forward raises its front window, and that can be a
-- floating one: a browser's Picture-in-Picture video floats above its
-- normal windows, so hyper+/ handed the keyboard to the video rather than
-- to Brave. So once an app this file (or kitty's toggle) brought forward
-- has activated, if its focused window floats above CoreGraphics layer 0,
-- the app's front normal window (Screens.isNormalEntry) is focused instead.
--
-- The layer is the test. Chromium's PiP window reports itself as an
-- AXStandardWindow with the usual buttons (AeroSpace's recorded
-- Accessibility dumps of Brave, Chrome and Edge PiP windows, upstream), so
-- isStandard() passes it; its layer is 3 (Chromium's kFloatingWindow maps to
-- kCGFloatingWindowLevel). Its title is localized and differs between
-- browsers, so that is no test either.
--
-- Dialogs are left alone. A sheet should be: Accessibility treats it as a
-- child of its window, so the focused window is the parent (unmeasured
-- here). An app-modal dialog sits at the modal-panel level while its app
-- runs the modal session (NSModalPanelWindowLevel in AppKit; what layer a
-- real one reads at here is unmeasured), so windows at or above
-- kModalPanelLayer are left alone, and so is any window whose subrole says
-- it is a dialog, whatever its layer.
--
-- A check runs only while its app is still frontmost and no newer switch
-- has started (appFloatingSupersede): otherwise it would pull focus back to
-- an app the user has just left. Then it costs one Accessibility query to
-- that app alone (its focused window), after the activation and not in the
-- key handler. A window any earlier window-list read saw at layer 0 ends it
-- there; anything else costs one CoreGraphics read, and only a floating
-- window pays for more.
--
-- The delay lets the app settle its key window after macOS reports it
-- active.
local kFloatingCheckDelay = 0.05

-- kCGModalPanelWindowLevel, from the SDK's CGWindowLevel.h.
local kModalPanelLayer = 8

-- Apps whose floating window is the point: the kitty panel floats on purpose
-- (core/kitty-panel.lua), and a hide can return to it.
appFloatingIntended = appFloatingIntended or { ["net.kovidgoyal.kitty"] = true }

-- Bumped by every switch this config starts; a check scheduled under an
-- older value is dropped.
local appFloatingGen = 0

--- Drops every floating check still waiting: call it when a key moves focus
--- some other way (hyper+z, hyper+;).
function appFloatingSupersede()
    appFloatingGen = appFloatingGen + 1
end

-- Focuses `w' and says whether `app''s focused window is now `w', trying
-- once more after raising it: the floating window may keep the keyboard.
local function appFocusWindowChecked(app, w)
    local id = w:id()
    local function took()
        local ok, fw = pcall(function() return app:focusedWindow() end)
        return ok and fw ~= nil and fw:id() == id
    end
    pcall(function() w:focus() end)
    if took() then return true end
    pcall(function() w:raise() w:focus() end)
    return took()
end

--- `gen' is appFloatingGen when the check was scheduled; nil (from the
--- console) checks regardless.
function appFocusOffFloating(app, label, gen)
    if gen ~= nil and gen ~= appFloatingGen then return end
    -- NSRunningApplication's own flag: asks the app nothing.
    local okf, front = pcall(function() return app:isFrontmost() end)
    if not (okf and front) then return end
    local okb, bid = pcall(function() return app:bundleID() end)
    if not okb or appFloatingIntended[bid] then return end
    local ok, fw = pcall(function() return app:focusedWindow() end)
    if not ok or not fw then return end
    local id = fw:id()
    if Screens.layerOf(id) == 0 then return end

    local stack = Screens.windowStack()
    local layer = nil
    for _, e in ipairs(stack) do
        if e.id == id then layer = e.layer break end
    end
    if not layer or layer == 0 or layer >= kModalPanelLayer then return end
    local oks, sub = pcall(function() return fw:subrole() end)
    if oks and (sub == "AXDialog" or sub == "AXSystemDialog") then return end

    local pid = app:pid()
    for _, e in ipairs(stack) do
        if e.pid == pid and e.id ~= id and Screens.isNormalEntry(e) then
            local w = Screens.entryWindow(e)
            if w then
                local took = appFocusWindowChecked(app, w)
                print(string.format("%s: %s: focus was on a floating window (layer %d); %s", label,
                                    app:name() or "?", layer,
                                    took and "moved to its front normal window"
                                         or "its front normal window would not take focus"))
                return
            end
        end
    end
end

-- pid -> { label, gen, by }: activations this file caused, to check once
-- they arrive. An entry lapses after a second, so a later activation of the
-- same app (the user clicking its PiP video, say) is left alone.
local appFloatingPending = {}
local kFloatingPendingSeconds = 1

--- Check `app' with appFocusOffFloating once it has activated. An app that
--- is in front already gets no activation to wait for, so it is checked
--- straight away: kitty's own hide hands focus to the app that was in front
--- before kitty, which is often the one its toggle then returns to.
function appCheckFloatingOnActivation(app, label)
    appFloatingSupersede()
    local gen = appFloatingGen
    local ok, pid = pcall(function() return app:pid() end)
    if not ok or not pid then return end
    local okf, front = pcall(function() return hs.application.frontmostApplication():pid() end)
    if okf and front == pid then
        hsAfter(kFloatingCheckDelay, function() appFocusOffFloating(app, label, gen) end)
        return
    end
    appFloatingPending[pid] = { label = label, gen = gen, by = hs.timer.secondsSinceEpoch() + kFloatingPendingSeconds }
end

-- Global, so it is not collected.
appFloatingWatcher = hs.application.watcher.new(function(_, event, app)
    if event ~= hs.application.watcher.activated or not app then return end
    local pid = app:pid()
    local p = appFloatingPending[pid]
    if not p then return end
    appFloatingPending[pid] = nil
    if hs.timer.secondsSinceEpoch() > p.by then return end
    hsAfter(kFloatingCheckDelay, function() appFocusOffFloating(app, p.label, p.gen) end)
end)
appFloatingWatcher:start()

appSwitchWatcher = hs.application.watcher.new(function(_, event, app)
    local p = appSwitchPending
    if not p or event ~= hs.application.watcher.activated or not app or app:pid() ~= p.pid then return end
    appSwitchPending = nil
    print(string.format("appHotkey: %s: activated %.1f ms after the handler started (handler %.1f ms%s)",
                        p.name, appSwitchMs(p.t0, hs.timer.absoluteTime()), p.handlerMs, p.hyperNote))
end)
appSwitchWatcher:start()

-- How an app hotkey brings its app forward.
--
-- false (the default): straight to the window server. unhide() first, since
-- the second press of a hotkey hides its app; it is a no-op on a visible app.
-- Then _bringtofront(false), the call hs.application:activate() itself ends
-- with (SetFrontProcessWithOptions, front window only).
--
-- true: hs.application:activate() as it is, which before that asks the
-- target app over Accessibility for its focused window and makes it main.
-- That is 3 to 8 ms of round trips to an app that answers promptly, and
-- unbounded when it does not. Switch this on if an app with several windows
-- (on several spaces, say) ever comes forward with the wrong one.
app_hotkey_activate_via_ax = app_hotkey_activate_via_ax or false

local function appBringForward(app)
    if app_hotkey_activate_via_ax then return app:activate() end
    app:unhide()
    return app:_bringtofront(false)
end

--- Carries out a screenReturnTarget: the kitty panel through kittyPanelShow,
--- a window on the screen through hs.window:focus (one app's Accessibility),
--- and otherwise the app through appBringForward, which asks it nothing and
--- so may raise a floating window: that is checked once the app activates.
function screenReturnFocus(t, label)
    if t.kittyPanel then return kittyPanelShow(label) end
    if t.window and pcall(function() t.window:focus() end) then return end
    appCheckFloatingOnActivation(t.app, label)
    appBringForward(t.app)
end

-- The second press of an app's hotkey hides the app and returns you to the
-- app you were in before it on the same screen. Left to itself, macOS
-- activates an app of its own choosing when the frontmost app hides:
-- hyper+x, hyper+k, hyper+k landed in Telegram rather than Emacs. So the
-- return target (screenReturnTarget, above) is brought forward first, and
-- the app is hidden only once the target has activated (or after a second,
-- if it never does): hiding the app while it is still frontmost would let
-- macOS choose again. In panel mode the kitty panel comes back through
-- kittyPanelShow, since kitty activating shows nothing by itself. With no
-- target the app is simply hidden, as before.
--
-- appHidePending is the hide waiting for its target; the activation watcher
-- below finishes it. Global, like appSwitchPending.
appHidePending = nil

-- `why' is nil when the return target activated, else why the hide went
-- ahead without that.
local function appHideFinish(why)
    local p = appHidePending
    if not p then return end
    appHidePending = nil
    hsCancel(p.timer)
    pcall(function() p.app:hide() end)
    print(string.format("appHotkey: %s: hidden %.1f ms after the handler started, %s",
                        p.name, appSwitchMs(p.t0, hs.timer.absoluteTime()),
                        why and (why .. "; " .. p.backName .. " was the return target")
                            or ("once " .. p.backName .. " had activated")))
end

local function appHideReturning(app, t0, hyperNote)
    appHideFinish("superseded by another hide")

    local name = app:name() or "?"
    -- The app is frontmost, so the active screen is the one it is on.
    local tq = hs.timer.absoluteTime()
    local target = screenReturnTarget(hs.screen.mainScreen(), app:pid())
    local queryMs = appSwitchMs(tq, hs.timer.absoluteTime())
    local back = target and target.app
    if not back then
        app:hide()
        print(string.format("appHotkey: %s: hidden, no app to return to (handler %.1f ms%s)", name,
                            appSwitchMs(t0, hs.timer.absoluteTime()), hyperNote))
        return
    end

    local pending = { app = app, name = name, backPid = back:pid(), backName = back:name() or "?", t0 = t0 }
    appHidePending = pending
    pending.timer = hsAfter(1, function()
        if appHidePending == pending then appHideFinish("no activation within 1 s") end
    end)

    screenReturnFocus(target, "appHotkey")
    print(string.format("appHotkey: %s: returning to %s, %s%s (handler %.1f ms, %.1f of it choosing%s)", name,
                        pending.backName, target.why, target.window and ", its window here" or "",
                        appSwitchMs(t0, hs.timer.absoluteTime()), queryMs, hyperNote))
end

appHideWatcher = hs.application.watcher.new(function(_, event, app)
    local p = appHidePending
    if not p or event ~= hs.application.watcher.activated or not app then return end
    if app:pid() ~= p.backPid then return end
    appHideFinish()
end)
appHideWatcher:start()

local function toggleFocusApp(app)
    local t0 = hs.timer.absoluteTime()
    local hyperNote = appSwitchHyperNote(t0)

    if app:isFrontmost() then
        appHideReturning(app, t0, hyperNote)
        return
    end

    appCheckFloatingOnActivation(app, "appHotkey")
    appBringForward(app)
    local pending = { pid = app:pid(), name = app:name() or "?", t0 = t0,
                      handlerMs = appSwitchMs(t0, hs.timer.absoluteTime()), hyperNote = hyperNote }
    appSwitchPending = pending
    hsAfter(1, function()
        if appSwitchPending ~= pending then return end
        appSwitchPending = nil
        print(string.format("appHotkey: %s: no activation within 1 s (handler %.1f ms%s)",
                            pending.name, pending.handlerMs, pending.hyperNote))
    end)
end

function toggleFocus(appName)
    local app = getApp(appName)
    if app then
        toggleFocusApp(app)
    end
end

function appHotkey(o)
    -- Specialise once when the binding is registered so scalar hotkeys retain
    -- their direct path. Only an ordered candidate list pays for the search.
    local pressedfn
    if type(o.appName) == "table" then
        local appNames = o.appName
        pressedfn = function()
            for i = 1, #appNames do
                local app = getApp(appNames[i])
                if app then
                    toggleFocusApp(app)
                    return
                end
            end
        end
    else
        local appName = o.appName
        pressedfn = function()
            toggleFocus(appName)
        end
    end

    hyper_bind_v2{
        key = o.key,
        mods = o.mods or {}, -- Use provided mods, or default to an empty table
        pressedfn = pressedfn
    }
end
-- function appHotkey(o)
--     function h_appHotkey()
--         toggleFocus(o.appName)
--         -- use `sleep 2 ; reval-copy frontapp-get ; fsay hi` to get this
--     end

--     mods = o.modifiers
--     -- If mods == "hyper", use =hyper_bind_v1=:
--     if mods == "hyper" or mods == hyper or not mods then
--         hyper_bind_v1(o.key, h_appHotkey)
--     else
--         -- hs.hotkey.bind(mods, o.key, h_appHotkey)
--         hs.alert("impossible 8170")
--     end
-- end
-- @upstreamBug https://github.com/Hammerspoon/hammerspoon/issues/2879 hs.hotkey.bind cannot bind punctuation keys such as /

appHotkey{
    key='/',
    appName={
        'com.brave.Browser',
        'company.thebrowser.Browser',
        'com.vivaldi.Vivaldi',
        'com.microsoft.edgemac',
        'com.google.Chrome',
        'com.apple.Safari',
    }
}
appHotkey{
    key='/',
    mods={'shift'},
    appName={
        'company.thebrowser.Browser',
        'com.interversehq.qView',
    }
}

appHotkey{
    key="'",
    mods={'shift'},
    appName='com.apple.Safari'
}
appHotkey{
    key='.',
    -- mods={'shift'},
    appName='com.google.Chrome'
}
appHotkey{
    key='.',
    mods={'shift'},
    appName='com.microsoft.edgemac'
}
-- appHotkey{ key='.', mods={'shift'}, appName='com.openai.atlas' }
-- appHotkey{ key='.', appName='com.openai.atlas' }
-- appHotkey{ key='m', appName='com.google.Chrome.app.ahiigpfcghkbjfcibpojancebdfjmoop' } -- https://devdocs.io/offline ; 'm' is also set as a search engine in Chrome
-- appHotkey{ key='m', appName='com.kapeli.dashdoc' } -- dash can bind itself in its pref
appHotkey{
    -- Was hyper+;, which now moves focus between screens (below).
    key='y',
    appName={
        'chat.delta.desktop.electron',
        'com.microsoft.Excel',
    }
}

-- hyper+; focuses the next screen's frontmost window, hyper+shift+; moves the
-- focused window to the next screen; both bring the pointer along. Screens go
-- left to right and wrap. See Screens.focusNext in core/screens.lua.
hyper_bind_v2{ key=';', pressedfn=function() Screens.focusNext(1) end }
hyper_bind_v2{ key=';', mods={'shift'}, pressedfn=function() Screens.moveWindowNext(1) end }

-- appHotkey{ key='c', appName='com.microsoft.VSCodeInsiders' }
-- appHotkey{ key='c', appName='com.apple.Terminal' }
appHotkey{ key='c', appName='com.apple.iCal' }
-- appHotkey{ key='c', appName='com.todesktop.230313mzl4w4u92' } -- Cursor VSCode App

emacsAppName = 'org.gnu.Emacs'
appHotkey{ key='x', appName=emacsAppName }

appHotkey{ key='l',
           appName={
               'com.tdesktop.PurpleTelegram',
               'com.tdesktop.Telegram',
           }
}

appHotkey{ key='\\', appName='com.anthropic.claudefordesktop' }
appHotkey{
    mods={'shift'},
    key='\\',
    appName='com.claudecode.context' }
-- appHotkey{ key='\\', appName='moe.Throne.macosx' }
-- appHotkey{ key='\\', appName='com.apple.iCal' }

-- appHotkey{ key='b', appName='com.apple.Preview' }
-- appHotkey{ key='b', appName='zathura' }
-- appHotkey{ key='a', appName='com.adobe.Reader' }

-- appHotkey{ key=']', appName='org.jdownloader.launcher' }

appHotkey{
    key='k',
    appName={
        'info.sioyek.sioyek',
        'net.sourceforge.skim-app.skim',
        'com.apple.Preview',
    }
}
-- appHotkey{ key='n', appName='net.sourceforge.skim-app.skim' }
-- appHotkey{ key='[', appName='info.sioyek.sioyek' }
-- appHotkey{ key=']', appName='net.sourceforge.skim-app.skim' }

appHotkey{ key='f', appName='com.apple.finder' }
-- appHotkey{ key='o', appName='com.operasoftware.Opera' }
-- appHotkey{ key='l', appName='notion.id' }

appHotkey{
    key='m',
    appName={
        'io.mpv',
        'com.openai.codex',
    }
}
-- appHotkey{ key='m', appName='com.adobe.Reader' }

appHotkey{ key='n', appName='com.apple.MobileSMS' } -- Apple Messages
-- appHotkey{ key='n', appName='com.appilous.Chatbot' } -- Pal ChatGPT app
-- appHotkey{ key='/', appName='com.quora.app.Experts' }
appHotkey{ key='b', appName='com.parallels.desktop.console' }

appHotkey{
    key='p',
    appName={
        'com.jetbrains.pycharm',
        'com.apple.Preview',
    }
}
appHotkey{
    key='p',
    mods={'shift'},
    appName={
        'com.microsoft.Powerpoint',
        'com.apple.iWork.Keynote',
    }
}
-- appHotkey{ key='w', appName='com.microsoft.Word' }

appHotkey{ key='=', appName='com.fortinet.FortiClient' }

appHotkey{ key='t', appName='org.mozilla.thunderbird' }


-- hyper+d: dismiss every notification, with the script zsh uses, run here
-- rather than through BrishGarden so it works while the garden is down. It
-- drives NotificationCenter through System Events, so the first run from
-- Hammerspoon has macOS ask once whether Hammerspoon may control System
-- Events.
-- @duplicateCode/b88a767cde3e47cb1bad4b855a8989b8: notif-os-dismiss-all in
-- zshlang/auto-load/others/notifications.zsh, which runs the same script.
hyper_bind_v1("d", function()
    local script = (nightdir or (os.getenv("HOME") .. "/scripts")) .. "/applescript/notif-dismiss-v2.jxa"
    gardenTask("/usr/bin/osascript", { "-l", "JavaScript", script }, function(code, _, err)
        if code == 0 then return end
        print("hyper+d: notif-dismiss-v2.jxa exited " .. tostring(code) .. ": " .. err)
        alert_gateway("Could not dismiss notifications: " .. err, { color = "warn" })
    end, 30, nil, "hyper+d")
end)

--- * Maccy's popup on the active screen
--
-- hyper+v passes ctrl+alt+cmd+shift+v through to Maccy (core/hyper-mode.lua),
-- and Maccy places its popup itself. Set to "screen center", Maccy 0.31
-- reads its `popupScreen' setting at every popup (Maccy/Menu/PopupLocation.swift
-- and Extensions/NSScreen+ForPopup.swift upstream): 0 means NSScreen.main
-- inside Maccy, which has no key window when its hotkey fires, and the popup
-- opened on the laptop while you worked on the monitor; n means
-- NSScreen.screens[n - 1], the order hs.screen.allScreens() lists them in.
-- Its "window center" setting is no better: it centres on the front app's
-- first CoreGraphics window at any layer, which for Brave is a 24 px strip.
--
-- So popupScreen is kept pointing at the screen named by the spec
-- `maccy_popup_screens' (default "active"; false leaves Maccy alone),
-- rewritten through `defaults' whenever that screen changes, and a press
-- costs nothing extra.
--
-- Maccy is sandboxed, so its settings live in its container, and on macOS
-- 14 the first write there from a process Hammerspoon starts makes macOS ask
-- whether Hammerspoon may access data from other apps (seen 2026-10-03). The
-- write waits for that answer, hence the long timeout; a failed write is
-- forgotten, so the next screen change tries again.
if maccy_popup_screens == nil then maccy_popup_screens = "active" end

local maccyBundleID = "org.p0deje.Maccy"

if maccy_popup_screens and Screens then
    local forget
    forget = Screens.onTargetChange(maccy_popup_screens, function(screen)
        if not screen then return end
        local index = nil
        for i, s in ipairs(hs.screen.allScreens()) do
            if s:id() == screen:id() then index = i break end
        end
        if not index then return end
        gardenTask("/usr/bin/defaults", { "write", maccyBundleID, "popupScreen", "-int", tostring(index) },
                   function(code, _, err)
                       if code == 0 then return end
                       print("Maccy popupScreen: defaults exited " .. code .. ": " .. err)
                       if forget then forget() end
                   end, 120, nil, "maccy-popup-screen")
    end)
end
