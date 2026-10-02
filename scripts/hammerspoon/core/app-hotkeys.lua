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

-- Global, so it is not collected.
recentAppsWatcher = hs.application.watcher.new(function(_, event, app)
    if event == hs.application.watcher.activated and app then recentAppsPush(app) end
end)
recentAppsWatcher:start()
do
    local front = hs.application.frontmostApplication()
    if front then recentAppsPush(front) end
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

-- The second press of an app's hotkey hides the app and returns you to the
-- app you were in before it. Left to itself, macOS activates an app of its
-- own choosing when the frontmost app hides: hyper+x, hyper+k, hyper+k
-- landed in Telegram rather than Emacs. So the return target, the newest
-- entry of recentApps that is still running and not hidden, is brought
-- forward first, and the app is hidden only once the target has activated
-- (or after a second, if it never does): hiding the app while it is still
-- frontmost would let macOS choose again. In panel mode the kitty panel
-- comes back through kittyPanelShow, since kitty activating shows nothing by
-- itself. With no target the app is simply hidden, as before.
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

local function appReturnTarget(app)
    local pid = app:pid()
    for _, a in ipairs(recentAppsCandidates(function(_, apid) return apid == pid end)) do
        if appReturnUsable(a) then return a end
    end
    return nil
end

local function appHideReturning(app, t0, hyperNote)
    appHideFinish("superseded by another hide")

    local name = app:name() or "?"
    local back = appReturnTarget(app)
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

    if back:bundleID() == "net.kovidgoyal.kitty" and kitty_hotkey_mode == "panel" and kittyPanelShow then
        kittyPanelShow("appHotkey")
    else
        appBringForward(back)
    end
    print(string.format("appHotkey: %s: returning to %s (handler %.1f ms%s)", name, pending.backName,
                        appSwitchMs(t0, hs.timer.absoluteTime()), hyperNote))
end

appHideWatcher = hs.application.watcher.new(function(_, event, app)
    local p = appHidePending
    if not p or event ~= hs.application.watcher.activated or not app then return end
    if app:pid() == p.backPid then appHideFinish() end
end)
appHideWatcher:start()

local function toggleFocusApp(app)
    local t0 = hs.timer.absoluteTime()
    local hyperNote = appSwitchHyperNote(t0)

    if app:isFrontmost() then
        appHideReturning(app, t0, hyperNote)
        return
    end

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
