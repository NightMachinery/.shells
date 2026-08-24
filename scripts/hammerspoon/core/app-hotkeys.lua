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
            -- brishzeval("yabai -m window --focus " .. windowID)
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

function toggleFocus(appName)
    local launch_p = false

    local app = nil
    app = getApp(appName)

    if app then
        if app:isFrontmost() then
            app:hide()
        else
            app:activate()
            -- focusAppYabai(appName)
        end
    else
        if launch_p then
            hs.application.launchOrFocus(appName)
            app = getApp(appName)
        end
    end
end

function appHotkey(o)
    -- This function now acts as a wrapper for hyper_bind_v2,
    -- making it easy to create app-toggling hotkeys.
    -- It accepts an 'o.mods' table for additional modifiers.
    hyper_bind_v2{
        key = o.key,
        mods = o.mods or {}, -- Use provided mods, or default to an empty table
        pressedfn = function()
            toggleFocus(o.appName)
        end
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

-- appHotkey{ key='/', appName='company.thebrowser.Browser' } -- Arc
appHotkey{ key='/', appName='com.brave.Browser' }
-- appHotkey{ key='/', appName='com.vivaldi.Vivaldi' }
appHotkey{ key='/', mods={'shift'}, appName='company.thebrowser.Browser' }
-- appHotkey{ key='/', mods={'shift'}, appName='com.interversehq.qView' }

appHotkey{
    key="'",
    mods={'shift'},
    appName='com.apple.Safari'
}
appHotkey{
    key='.',
    mods={'shift'},
    appName='com.google.Chrome'
}
appHotkey{
    key='.',
    -- mods={'shift'},
    appName='com.microsoft.edgemac'
}
-- appHotkey{ key='.', mods={'shift'}, appName='com.openai.atlas' }
-- appHotkey{ key='.', appName='com.openai.atlas' }
-- appHotkey{ key='m', appName='com.google.Chrome.app.ahiigpfcghkbjfcibpojancebdfjmoop' } -- https://devdocs.io/offline ; 'm' is also set as a search engine in Chrome
-- appHotkey{ key='m', appName='com.kapeli.dashdoc' } -- dash can bind itself in its pref
-- appHotkey{ key=';', appName='com.microsoft.Excel' }
appHotkey{ key=';', appName='chat.delta.desktop.electron' }

-- appHotkey{ key='c', appName='com.microsoft.VSCodeInsiders' }
-- appHotkey{ key='c', appName='com.apple.Terminal' }
-- appHotkey{ key='c', appName='com.openai.codex' }
appHotkey{ key='c', appName='com.apple.iCal' }
-- appHotkey{ key='c', appName='com.todesktop.230313mzl4w4u92' } -- Cursor VSCode App

emacsAppName = 'org.gnu.Emacs'
appHotkey{ key='x', appName=emacsAppName }

appHotkey{ key='l',
           appName='com.tdesktop.PurpleTelegram'
           -- appName='com.tdesktop.Telegram'
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

appHotkey{ key='k', appName='info.sioyek.sioyek' }
-- appHotkey{ key='k', appName='net.sourceforge.skim-app.skim' }
-- appHotkey{ key='n', appName='net.sourceforge.skim-app.skim' }
-- appHotkey{ key='[', appName='info.sioyek.sioyek' }
-- appHotkey{ key=']', appName='net.sourceforge.skim-app.skim' }

appHotkey{ key='f', appName='com.apple.finder' }
-- appHotkey{ key='o', appName='com.operasoftware.Opera' }
-- appHotkey{ key='l', appName='notion.id' }

appHotkey{ key='m', appName='io.mpv' }
-- appHotkey{ key='m', appName='com.adobe.Reader' }

appHotkey{ key='n', appName='com.apple.MobileSMS' } -- Apple Messages
-- appHotkey{ key='n', appName='com.appilous.Chatbot' } -- Pal ChatGPT app
-- appHotkey{ key='/', appName='com.quora.app.Experts' }
appHotkey{ key='b', appName='com.parallels.desktop.console' }

-- appHotkey{ key='p', appName='com.jetbrains.pycharm' }
appHotkey{ key='p', appName='com.apple.Preview' }
-- appHotkey{ key='p', appName='com.apple.iWork.Keynote' }
-- appHotkey{ key='w', appName='com.microsoft.Powerpoint' }
-- appHotkey{ key='w', appName='com.microsoft.Word' }

appHotkey{ key='=', appName='com.fortinet.FortiClient' }

appHotkey{ key='t', appName='org.mozilla.thunderbird' }


hyper_bind_v1("d", function()
                  brishzeval2bg("notif-os-dismiss-all")
end)
