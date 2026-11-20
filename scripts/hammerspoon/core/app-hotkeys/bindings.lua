--- * App hotkeys: the bindings
--- Which hyper key brings which app forward, through appHotkey (main.lua).
--- Loaded after main.lua, which defines it. appName is a bundle id, or a
--- list tried in order, of which the first running app wins; a press with
--- none running does nothing. mods adds modifiers on top of hyper.

appHotkey{
    key='/',
    appName={
        -- 'company.thebrowser.dia',
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
    appName={
        -- 'com.brave.Browser',
        'company.thebrowser.dia',
        'com.google.Chrome',
        'com.apple.Safari',
    }
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

appHotkey{ key='\\', appName={
               'moe.Throne.macosx',
               'com.anthropic.claudefordesktop',
               'com.claudecode.context',
                             }
}
appHotkey{
    mods={'shift'},
    key='\\',
    appName={
        'com.anthropic.claudefordesktop',
        'com.claudecode.context',
        'moe.Throne.macosx',
    }
}
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
        'mpv',
        'io.mpv',
        'com.openai.codex',
        'sh.paseo.ypaseo.desktop', -- YPaseo, the personal Paseo build
        'sh.paseo.desktop'
    }
}
-- Hyper+Shift+G opens YPaseo or Paseo.
appHotkey{ key='g', mods={'shift'}, appName={ 'sh.paseo.ypaseo.desktop', 'sh.paseo.desktop' } }

-- Hyper+Shift+M opens Music through Purple's local window capability.
local purpleMusicTask
local purpleMusicRetry
local purpleAppPath = "/Applications/Purple Telegram.app"
local purpleMusicCLI = os.getenv("HOME") .. "/.local/bin/tdesktop-cli"

local function purpleMusicTry(remaining)
    purpleMusicTask = hs.task.new(purpleMusicCLI, function(code, stdout)
        purpleMusicTask = nil
        if code == 0 then return end
        if code == 3 and remaining > 0 then
            purpleMusicRetry = hs.timer.doAfter(0.5, function()
                purpleMusicRetry = nil
                purpleMusicTry(remaining - 1)
            end)
        elseif code == 2 then
            local ok, reply = pcall(hs.json.decode, stdout)
            local reason = ok and reply and reply.error and reply.error.code
            if reason == "locked" then
                hs.alert.show("Unlock Purple to open Music")
            else
                hs.alert.show("Enable the Window capability in Purple Local automation to open Music")
            end
        else
            hs.alert.show("Could not open Purple Music")
        end
    end, {"music", "--json"})
    if not purpleMusicTask or not purpleMusicTask:start() then
        purpleMusicTask = nil
        hs.alert.show("tdesktop-cli is required to open Purple Music")
    end
end

hyper_bind_v2{
    key="m",
    mods={"shift"},
    pressedfn=function()
        if purpleMusicTask or purpleMusicRetry then return end
        local matchingApp = hs.fnutils.find(
            hs.application.applicationsForBundleID("com.tdesktop.PurpleTelegram") or {},
            function(application) return application:path() == purpleAppPath end
        )
        if not matchingApp and not hs.application.launchOrFocus(purpleAppPath) then
            hs.alert.show("Could not launch Purple Telegram")
            return
        end
        purpleMusicTry(12)
    end,
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
