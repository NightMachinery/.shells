local inputEnglish = "com.apple.keylayout.US"
local inputPersian = "com.apple.keylayout.Persian-ISIRI2901"
--
hs.window.animationDuration = 0;
hs.dockicon.hide() -- to appear above full-screen windows you must hide the Hammerspoon Dock icon
---
function langSetPer()
    hs.keycodes.currentSourceID(inputPersian)
end

function langSetEn()
    hs.keycodes.currentSourceID(inputEnglish)
end

function langEnIs()
    return hs.keycodes.currentSourceID() == inputEnglish
end

function langGet()
    if langEnIs() then
        return "En"
    else
        return "Fa"
    end
end

function langSetToggle()
    if langEnIs() then
        langSetPer()
    else
        langSetEn()
    end
end
---
enOnly = { "iTerm2", "Terminal", "kitty", "Code", "Code - Insiders", "Emacs", "mpv", "zathura", "sioyek", "Maccy" } --
if false then
    function appWatch(appName, event, app)
        -- @deprecated as it was too slow. In general, calling Zsh functions that will then call Hammerspoon functions is a bad idea.
        ---
        -- alert.show("appWatch: " .. appName .. ", event: " .. tostring(event) .. ", app: " .. tostring(app), 7)
        local bshcode = ''
        if event == hs.application.watcher.activated then
            bshcode = bshcode .. 'nightshift-auto ; '
            if has_value(enOnly, appName) then
                bshcode = bshcode .. "input-lang-push en ; "
            else
                bshcode = bshcode .. "input-lang-pop ; "
            end
        end
        if bshcode ~= '' then
            bshcode = "{ " .. bshcode .. " } &>/dev/null & # appName: " .. appName .. ", event: " .. tostring(event)
            brishzeval(bshcode)
        end
    end
else
    input_lang_push_lang = nil
    function inputLangPop()
        if not (input_lang_push_lang == nil) then
            hs.keycodes.currentSourceID(input_lang_push_lang)
            input_lang_push_lang = nil
        end
    end
    function inputLangPush()
        if input_lang_push_lang == nil then
            input_lang_push_lang = hs.keycodes.currentSourceID()
        end
    end

    function appWatch(appName, event, app)
        -- alert.show("appWatch: " .. appName .. ", event: " .. tostring(event) .. ", app: " .. tostring(app), 7)
        if event == hs.application.watcher.activated then
            if has_value(enOnly, appName) then
                inputLangPush()
                langSetEn()
            else
                inputLangPop()
            end
        end
    end
end
appWatcher = hs.application.watcher.new(appWatch)
appWatcher:start()

hyper_bind_v1("i", function()
                  langSetToggle()
                  hs.alert(langGet(), 1)
                  brishzeval('input_lang_push_lang_del')
                  input_lang_push_lang = nil
end)
