---- * Hyper Modifier Key
hyper = {"cmd","ctrl","alt","shift"}

if false then
    -- Here we were trying to make F7 press other modifier keys.
    -- [[id:6fcee871-a0f9-46b5-af2f-a9b767c48422][@me How can I make Hammerspoon press modifier keys? · Issue #3582 · Hammerspoon/hammerspoon]]

    hs.hotkey.bind({}, 'F7', nil, function()
            -- hs.eventtap.keyStroke({"cmd","alt","shift","ctrl"}, "")

            hs.alert('F7 pressed')
            -- hs.eventtap.event.newKeyEvent("shift", true):post()
            -- hs.eventtap.event.newKeyEvent("shift", false):post()
            -- hs.eventtap.event.newKeyEvent(hs.keycodes.map.alt, true):post()
            -- hs.eventtap.event.newKeyEvent(hs.keycodes.map.alt, false):post()

            -- hs.osascript.applescript('tell application "System Events" to key code 58')
            -- hs.eventtap.event.newKeyEvent({}, " ", true):setKeyCode(61):post()
            -- hs.eventtap.event.newKeyEvent({}, " ", false):setKeyCode(61):post()
    end)
end

-- Which screens show the hyper banner: "all" | "primary" | "internal" | "all_external" | "active" | "mouse"
-- (see ModalMode.targetScreens)
hyper_overlay_screens = hyper_overlay_screens or "all"

local hyperStyle = {
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
    fillColor = { white = 1, alpha = 2 / 3 },
    radius = 24,
    strokeColor = { red = 19 / 255, green = 182 / 255, blue = 133 / 255, alpha = 1},
    strokeWidth = 16,
    textColor = { white = 0.125 },
    textSize = 48,
    text = "🌟",
    overlayScreens = hyper_overlay_screens,
}
local secureInputStyle = tableShallowCopy(hyperStyle)
secureInputStyle.fillColor = { red = 1, green = 0, blue = 0, alpha = 0.5 }

local isSecureInputEnabled = hs.eventtap.isSecureInputEnabled
---
-- The indicator is a canvas group, not an hs.alert. The hs.alert route this
-- replaced could not show over fullscreen spaces at all:
-- [[https://github.com/Hammerspoon/hammerspoon/issues/3586][How do I show an alert on all fullscreen spaces? · Issue #3586 · Hammerspoon/hammerspoon]]
-- Canvas mode has been seen to occasionally not show up; if that resurfaces,
-- the fix belongs in ModalMode.createIndicatorGroup rather than in a fallback
-- here.
local hyperModeIndicatorOrig = ModalMode.createIndicatorGroup(hyperStyle)
local hyperModeIndicatorSI = ModalMode.createIndicatorGroup(secureInputStyle)
local hyperModeIndicator = hyperModeIndicatorOrig
---
hyper_mode = ModalMode.create{name="hyper"}
ModalMode.installGlobals(hyper_mode, "hyper")

prevFocusedElement = nil
function hyper_modality:entered()
    -- When hs.hotkey.modal has finished enabling the mode's keys and hands
    -- over to this; see the timing lines in core/app-hotkeys/main.lua.
    hyper_mode.enteredAt = hs.timer.absoluteTime()
    hyper_modality.entered_p = true

    -- First, before the Secure Input dance below, which does synchronous AX
    -- work and can take its time: the blackout chords are dispatched from this
    -- tap rather than from hs.hotkey (see the header of core/blackout-lock.lua)
    -- and they must be live for the whole of the mode, not most of it.
    if blackoutChordTapStart then blackoutChordTapStart() end

    -- Peek: fade any alert band down to a whisper for as long as hyper is
    -- held, so a band lying over a tab bar or a title bar stops hiding the
    -- thing you are about to run a command on. Visual only -- the alerts keep
    -- their own timers and die on schedule even while invisible. Guarded
    -- because hyper mode must still load if the alert engine did not.
    -- Deliberately not on purple, which is a mode you sit in for minutes.
    if alertV2PeekBegin then alertV2PeekBegin() end

    -- I have not yet added the redis updaters for purple_modality.
    redisActivateMode("hyper_modality")

    if isSecureInputEnabled() then
        -- [[https://github.com/Hammerspoon/hammerspoon/issues/3555][Hammerspoon hangs spradically when entering hyper mode and displaying a modal window · Issue #3555 · Hammerspoon/hammerspoon]]

        hyperModeIndicator = hyperModeIndicatorSI
        -- Unfocus the app's focused element over Accessibility; exited()
        -- focuses it again. Two older tries, unused for a while, are in git
        -- history: an Escape, and focusing a Hammerspoon webview at the
        -- primary screen's corner, with a wait for Secure Input to end and a
        -- warning band when it did not.
        local axApp = hs.axuielement.applicationElement(hs.application.frontmostApplication())
        prevFocusedElement = axApp and axApp.AXFocusedUIElement
        if prevFocusedElement then
            prevFocusedElement.AXFocused = false
        end
    else
        hyperModeIndicator = hyperModeIndicatorOrig
    end

    hyperModeIndicator:show()
end

function hyper_modality:exited()
    hyper_modality.entered_p = false
    hyper_modality.exit_on_release_p = false

    if blackoutChordTapStop then blackoutChordTapStop() end

    if alertV2PeekEnd then alertV2PeekEnd() end

    if prevFocusedElement and prevFocusedElement:isValid() then
        prevFocusedElement.AXFocused = true
    end
    prevFocusedElement = nil

    hyperModeIndicator:hide()

    redisDeactivateMode("hyper_modality")
end

function hyper_triggered()
    hyper_mode.triggered()
end

function hyper_bind_v1(key, pressedfn)
    return hyper_mode.bindV1(key, pressedfn)
end

function hyper_bind_v2(o)
    return hyper_mode.bindV2(o)
end

--- ** Hyper Hotkeys (Main Section)
hyper_bind_v2({
    key = 'escape',
    auto_trigger_p=false,
    pressedfn = function()
        hyper_exit() -- Exit the modality
        -- hs.eventtap.keyStroke({}, 'escape') -- Simulate escape key press
    end
})

hyper_toggler_1 = hs.hotkey.bind({}, "F18", hyper_down, hyper_up, nil)

-- Keys that re-emit the real modifier chord, so an existing
-- ctrl+alt+cmd+shift+<key> shortcut keeps working from hyper mode.
--
-- "o",
--
-- If you can't bind a key here, it's most probably because you have bound it later in the code.
---
-- hs.keycodes.map['left'], hs.keycodes.map['right'], "right"
-- [[https://github.com/Hammerspoon/hammerspoon/issues/2282][Having trouble sending arrow key events · Issue #2282 · Hammerspoon/hammerspoon]]
---
hyper_passthrough_keys = hyper_passthrough_keys or {"v", "\\", "delete", "j"}

---
-- What hyper+space does: "emit_key" | "neru"
hyper_space_mode = "emit_key"
-- hyper_space_mode = "neru"

neru_bin = "/Applications/Neru.app/Contents/MacOS/neru"
-- hs.task takes the argv directly, so no shell is spawned. Built once.
neru_hints_args = {"hints", "--action", "left_click"}

if hyper_space_mode == "emit_key" then
    table.insert(hyper_passthrough_keys, "space")

    hyper_bind_v2{mods={"shift"}, key="space", pressedfn=function()
                      hs.task.new(neru_bin, nil, neru_hints_args):start()
    end}

else
    hyper_bind_v2{key="space", pressedfn=function()
                      hs.task.new(neru_bin, nil, neru_hints_args):start()
    end}

    -- Fallback: hyper+shift+space still emits the original chord.
    hyper_bind_v2{mods={"shift"}, key="space", pressedfn=function()
                      hs.eventtap.keyStroke({"cmd","alt","shift","ctrl"}, "space")
    end}
end

neru_recursive_grid_left_click_args = {"recursive_grid", "--action", "left_click"}

---

for _, key in ipairs(hyper_passthrough_keys) do
    hyper_bind_v2{key=key, pressedfn=function()
                      hs.eventtap.keyStroke({"cmd","alt","shift","ctrl"}, key)
    end}
end
-- Binding hyper+cmd+v so that we can access the clipboard manager even when Secure Input is on. (Individual letters won't be readable by Hammerspoon, but modifier+letters are readable.)
hyper_bind_v2{mods={"cmd"}, key="v", pressedfn=function()
                    hs.eventtap.keyStroke({"cmd","alt","shift","ctrl"}, "v")
end}

function bindToKey(params)
    local binder = params.binder or hyper_bind_v2
    local from_mods = params.from_mods or {}
    local from = params.from
    local to = params.to
    local to_mods = params.to_mods or {}

    binder{
        mods = from_mods,
        key = from,
        pressedfn = function()
            hs.eventtap.event.newKeyEvent(to_mods, to, true):post()
        end,
        releasedfn = function()
            hs.eventtap.event.newKeyEvent(to_mods, key, false):post()
        end
    }
end

for _, key in ipairs({"["}) do -- "-" is no longer used by our AltTab config after it made having more than one shortcut a Pro feature.
    -- AltTab Window Switcher:
    -- Its proper functions needs the modifier keys to be kept pressed while the hyper mode is active. I.e., it really needs the hyper mode to be the same thing as having the modifiers pressed.
    -- I don't know of a way to do that (see [[id:6fcee871-a0f9-46b5-af2f-a9b767c48422][@me How can I make Hammerspoon press modifier keys? · Issue #3582 · Hammerspoon/hammerspoon]]), but we could add `hyper_modality.press_on_exit = {"space"}`. This would make the common usage of the window switcher painless, but it might break the more advanced usage of it; like pressing =w= to close a window.
    bindToKey{
        binder = hyper_bind_v2,
        from = key,
        to = key,
        to_mods = hyper,
    }
end

---
-- [[https://github.com/Hammerspoon/hammerspoon/issues/1946][Mission control related shortcuts do not work on macOS Mojave · Issue #1946 · Hammerspoon/hammerspoon]]
-- Trigger existing hyper key shortcuts
for _, key in ipairs({"left", "right"}) do
    -- hs.keycodes.map['left'], hs.keycodes.map['right']
    -- [[https://github.com/Hammerspoon/hammerspoon/issues/2282][Having trouble sending arrow key events · Issue #2282 · Hammerspoon/hammerspoon]]
    ---
    hyper_bind_v2{key=key, pressedfn=function()
                      hyper_triggered()

                      hs.eventtap.keyStroke({"fn", "cmd","alt","shift","ctrl"}, key)
    end}
end
