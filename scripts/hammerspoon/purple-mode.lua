--- * Purple Mode
-- Purple is a secondary modal layer entered from Hyper+Cmd+P. It is mostly
-- used for mouse/keyboard-mouse actions and app-local editing shortcuts.
purple_mode = ModalMode.create{name="purple"}
ModalMode.installGlobals(purple_mode, "purple")

-- Which screens show the purple banner: "all" | "primary" | "internal" |
-- "all_external" | "active" | "mouse" (see ModalMode.targetScreens)
purple_overlay_screens = purple_overlay_screens or "all"

-- Entering with Secure Input on is a transient warning, not the indicator; the
-- id keeps repeated entries from stacking copies of it.
local kPurpleSIMAlertId = "purple-secure-input"

local purpleStyle = {
    -- [[https://github.com/Hammerspoon/hammerspoon/blob/master/extensions/alert/alert.lua#L17][hammerspoon/extensions/alert/alert.lua at master · Hammerspoon/hammerspoon]]
    -- strokeWidth  = 2,
    -- strokeColor = { white = 1, alpha = 1 },
    -- fillColor   = { white = 0, alpha = 0.75 },
    -- textColor = { white = 1, alpha = 1 },
    -- textFont  = ".AppleSystemUIFont",
    -- textSize  = 27,
    -- radius = 27,
    atScreenEdge = 1,
    fadeInDuration = 0.001,
    fadeOutDuration = 0.001,
    -- padding = nil,
    fillColor = { red = 0.5, blue = 0.5, alpha = 2 / 3 },
    radius = 24,
    strokeColor = { red = 19 / 255, green = 182 / 255, blue = 133 / 255, alpha = 1},
    strokeWidth = 16,
    textColor = { white = 0.125 },
    textSize = 48,
    text = "Purple",
    -- text = "Purple Mode 🌟"
    -- text = "Purple Mode ✈"
    overlayScreens = purple_overlay_screens,
}

-- A canvas group, the same as Hyper's indicator. The per-screen hs.alert loop
-- this replaces could not draw over fullscreen spaces:
-- [[https://github.com/Hammerspoon/hammerspoon/issues/3586][How do I show an alert on all fullscreen spaces? · Issue #3586 · Hammerspoon/hammerspoon]]
-- The group also rebuilds its canvases when the screen layout changes, which
-- the loop -- built once per entry from hs.screen.allScreens() -- did not.
local purpleModeIndicator = ModalMode.createIndicatorGroup(purpleStyle)

function purple_modality:entered()
    purple_modality.entered_p = true

    if hs.eventtap.isSecureInputEnabled() then
        alert_gateway("⚠️ Secure Input is on. Our Purple Mode commands might not work.",
                { id = kPurpleSIMAlertId, color = "warn" })
        -- [[https://github.com/Hammerspoon/hammerspoon/issues/3555][Hammerspoon hangs spradically when entering hyper mode and displaying a modal window · Issue #3555 · Hammerspoon/hammerspoon]]
    end

    purpleModeIndicator:show()
end

function purple_modality:exited()
    purple_modality.entered_p = false
    purple_modality.exit_on_release_p = false

    purpleModeIndicator:hide()
end

function purple_triggered()
    purple_mode.triggered()
end

function purple_bind_v1(key, pressedfn)
    return purple_mode.bindV1(key, pressedfn)
end

function purple_bind_v2(o)
    return purple_mode.bindV2(o)
end

--- ** Purple Hotkeys
purple_toggler_1 = hyper_bind_v2{mods={"cmd"}, key="p", pressedfn=purple_down, releasedfn=purple_up}
purple_bind_v2{ mods={"shift"}, auto_trigger_p=false, key="escape", pressedfn=purple_exit }

purple_bind_v2{ auto_trigger_p=false, key="q", pressedfn=timerifyFn{enabled_p=false, delay=0, fn=function()
                                                                        -- hs.alert("purple_modality: undo")

                                                                        -- purple_modality:exit()
                                                                        -- Somehow the =h= here would be in conflict with the h hotkey in purple_modality. This can be worked around by exiting and re-entering the mode.
                                                                        -- Using a timer also works
                                                                        -- [[https://stackoverflow.com/questions/53320589/hs-eventtap-keystroke-with-modifier-works-only-after-double-press][hammerspoon - hs.eventtap.keyStroke with modifier works only after double press - Stack Overflow]]
                                                                        -- We just need the key of this hotkey not to be in conflict with itself.
                                                                        -- Or we can change it to a released callback, so the real key isn't being held anymore.
                                                                        hs.eventtap.keyStroke({"cmd"}, "z")
                                                                        -- purple_modality:enter()
end}}
purple_bind_v2{ auto_trigger_p=false, key="a", pressedfn=(function()
                        hs.eventtap.keyStroke({"cmd", "ctrl"}, "h")
end)}
purple_bind_v2{ auto_trigger_p=false, key="d", pressedfn=(function()
                        hs.eventtap.keyStroke({"cmd", "ctrl"}, "m")
end)}
purple_bind_v2{ auto_trigger_p=false, key="s", pressedfn=(function()
                        hs.eventtap.keyStroke({"cmd", "shift"}, "m")
end)}
