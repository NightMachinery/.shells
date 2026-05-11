function focusAppYabai(appName)
    local app = hs.application.get(appName)
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
    app = hs.application.get(appName)

    if app then
        if app:isFrontmost() then
        else
            app:activate()
        end
    else
        if launch_p then
            hs.application.launchOrFocus(appName)
            app = hs.application.get(appName)
        end
    end
end

function toggleFocus(appName)
    local launch_p = false

    local app = nil
    app = hs.application.get(appName)

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
            app = hs.application.get(appName)
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

appHotkey{ key='/', appName='company.thebrowser.Browser' }
appHotkey{ key='.', appName='com.google.Chrome' }
appHotkey{ key='.', mods={'shift'}, appName='com.microsoft.edgemac' }
-- appHotkey{ key='.', mods={'shift'}, appName='com.openai.atlas' }
appHotkey{ key='/', mods={'shift'}, appName='com.interversehq.qView' }
-- appHotkey{ key='.', appName='com.openai.atlas' }
-- appHotkey{ key='m', appName='com.google.Chrome.app.ahiigpfcghkbjfcibpojancebdfjmoop' } -- https://devdocs.io/offline ; 'm' is also set as a search engine in Chrome
-- appHotkey{ key='m', appName='com.kapeli.dashdoc' } -- dash can bind itself in its pref
-- appHotkey{ key=';', appName='com.microsoft.Excel' }
appHotkey{ key=';', appName='chat.delta.desktop.electron' }

-- appHotkey{ key='c', appName='com.microsoft.VSCodeInsiders' }
-- appHotkey{ key='c', appName='com.apple.Terminal' }
appHotkey{ key='c', appName='com.openai.codex' }
-- appHotkey{ key='c', appName='com.todesktop.230313mzl4w4u92' } -- Cursor VSCode App

emacsAppName = 'org.gnu.Emacs'
appHotkey{ key='x', appName=emacsAppName }

appHotkey{ key='l', appName='com.tdesktop.Telegram' }
-- appHotkey{ key='\\', appName='com.tdesktop.Telegram' }

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
-- appHotkey{ key='\\', appName='com.apple.iCal' }

appHotkey{ key='m', appName='mpv' }
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

appHotkey{ key='t', appName='Thunderbird' }


hyper_bind_v1("d", function()
                  brishzeval2bg("notif-os-dismiss-all")
end)
