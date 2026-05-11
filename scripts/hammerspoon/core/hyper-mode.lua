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

local hyperAlerts

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
}
local secureInputStyle = tableShallowCopy(hyperStyle)
secureInputStyle.fillColor = { red = 1, green = 0, blue = 0, alpha = 0.5 }

local realCurrentWindow
local maxSIMWaitTime <const> = 0.75 -- seconds

local focusStealingWebview = hs.webview.new{x=0, y=0, w=500, h=500}
-- local focusStealingWebview = hs.webview.new{x=0, y=0, w=0, h=0}

local hyperSIMAlerts
local isSecureInputEnabled = hs.eventtap.isSecureInputEnabled
---
-- hyper_alert_canvas_p = false
hyper_alert_canvas_p = true
-- canvas mode seems to be more buggy though? It sometimes just doesn't show up.

function hyperModeIndicatorCreate(hyperStyle)
    local strokeWidth = hyperStyle.strokeWidth / 1.5
    local text = hyperStyle.text or ""

    local hyperModeIndicator = hs.canvas.new{x=0, y=0, w=0, h=0}:insertElement{
        id = 'background',
        type = 'rectangle',
        action = 'strokeAndFill',
        fillColor = hyperStyle.fillColor,
        roundedRectRadii = { xRadius = hyperStyle.radius, yRadius = hyperStyle.radius },
        strokeColor = hyperStyle.strokeColor,
        strokeWidth = strokeWidth,
        padding = strokeWidth / 2,
}:insertElement{
        id = 'textBox',
        type = 'text',
        text = text,
        textAlignment = 'center',
        textColor = hyperStyle.textColor,
        textSize = hyperStyle.textSize,
}
    local hyperTextBoxSize = hyperModeIndicator:minimumTextSize(2, text)
    local screenFrame = hs.screen.primaryScreen():fullFrame()
    local hyperFrame = {}
    hyperFrame.w = hyperTextBoxSize.w + hyperStyle.strokeWidth*2 + hyperStyle.textSize -- default alert padding is 1/2 of font size, but we need it on both sides
    hyperFrame.h = hyperTextBoxSize.h + hyperStyle.strokeWidth*2 + hyperStyle.textSize -- ditto
    ModalMode.positionedFrame(screenFrame, hyperFrame, hyperStyle.overlayPosition or "center-top", hyperStyle.overlayMargin)
    -- hyperFrame.y = hyperFrame.y + 35 -- to be below the notch

    -- Hammerspoon can automatically center the text horizontally, but not vertically, so:
    hyperModeIndicator.textBox.frame = {
        x = (hyperFrame.w - hyperTextBoxSize.w)/2,
        y = ((hyperFrame.h - hyperTextBoxSize.h)/2) + 5,
        -- I have added =5= to make it look better on the notch.
        w = hyperTextBoxSize.w,
        h = hyperTextBoxSize.h,
    }
    hyperModeIndicator:frame(hyperFrame)

    hyperModeIndicator:behavior{'canJoinAllSpaces', 'transient', 'fullScreenAuxiliary'} -- canvas will appear in all spaces
    -- OR
    -- hyperModeIndicator:behavior{'canJoinAllSpaces', 'stationary', 'fullScreenAuxiliary'} -- canvas will appear in all spaces AND Mission Control

    return hyperModeIndicator
end


local hyperModeIndicator
local hyperModeIndicatorOrig
local hyperModeIndicatorSI
if hyper_alert_canvas_p then
    hyperModeIndicatorOrig = hyperModeIndicatorCreate(hyperStyle)
    hyperModeIndicator = hyperModeIndicatorOrig

    hyperModeIndicatorSI = hyperModeIndicatorCreate(secureInputStyle)
end
---
hyper_mode = ModalMode.create{name="hyper"}
ModalMode.installGlobals(hyper_mode, "hyper")

if false then
    -- For debugging:
    hs.hotkey.bind('ctrl', "'",
                   function() -- pressed
                       focusStealingWebview:allowTextEntry(true)
                       focusStealingWebview:windowStyle(hs.webview.windowMasks.titled)
                       focusStealingWebview:show():hswindow():focus()
                   end,
                   function() -- released
                       focusStealingWebview:hide()
                   end
    )
end

prevFocusedElement = nil
function hyper_modality:entered()
    hyper_modality.entered_p = true

    -- I have not yet added the redis updaters for purple_modality.
    redisActivateMode("hyper_modality")

    if isSecureInputEnabled() then
        -- [[https://github.com/Hammerspoon/hammerspoon/issues/3555][Hammerspoon hangs spradically when entering hyper mode and displaying a modal window · Issue #3555 · Hammerspoon/hammerspoon]]
        -- hs.alert("⚠️ Secure Input is on. Our Hyper Mode commands might not work.", 0.7)

        hyperModeIndicator = hyperModeIndicatorSI
        ---
        if true then
            local axApp = hs.axuielement.applicationElement(hs.application.frontmostApplication())
            if axApp then
                -- hs.alert("axApp found")
                prevFocusedElement = axApp.AXFocusedUIElement

                -- hs.alert("axApp.AXFocusedUIElement: " .. prevFocusedElement)
                prevFocusedElement.AXFocused = false
            else
                -- hs.alert("no axApp")
            end
        elseif true then
            doEscape()
            -- An escape makes the password input bar unfocused in Arc.
        else
            realCurrentWindow = hs.window.focusedWindow()

            -- focusStealingWebview:allowTextEntry(true)
          focusStealingWebview:windowStyle(hs.webview.windowMasks.titled)
            focusStealingWebview:show():hswindow():focus()

            if false then
                -- Whichever app is enabling SIM might not disable it immediately.
                -- Watch for SIM to shut off, giving up after `maxSIMWaitTime` seconds.
                local endTime = hs.timer.absoluteTime() + maxSIMWaitTime*1000000000 -- convert to nanoseconds
                while isSecureInputEnabled() and hs.timer.absoluteTime() < endTime do
                    -- Normally I try to avoid hs.timer.usleep, because it basically hangs Hammerspoon.
                    -- But for really short periods like this, it's probably cleaner than rewriting with timers or coroutines.
                    hs.timer.usleep(1000)
                end

                if isSecureInputEnabled() then
                    -- Still in Secure Input Mode - give up and show alerts about it.
                    local secureInputInfo = hs.execute[[ps -c -o pid=,command= -p $(ioreg -l -w 0 | grep -Eo '"kCGSSessionSecureInputPID"=[0-9]+' | cut -d= -f2 | sort | uniq]]

                    local msg = "⚠️ Secure Input is on. Hyper Mode commands might not work.\nEnabled by:\n"..secureInputInfo
                    msg = msg:gsub('loginwindow', 'unknown (supposedly loginwindow)')
                    msg = msg:gsub('^%s*(.-)%s*$', '%1')

                    print(msg) -- leave a copy of the message in the console, so you can still see it after the alert goes away
                    hyperSIMAlerts = {}
                    for i, screen in pairs(hs.screen.allScreens()) do
                        hyperSIMAlerts[i] = hs.alert(msg, screen, "")
                    end
                end
            end
        end
    else
        realCurrentWindow = nil

        hyperModeIndicator = hyperModeIndicatorOrig
    end

    if hyper_alert_canvas_p then
        hyperModeIndicator:show()
    else
        -- @todo Use secureInputStyle if secure input is enabled.
        ---
        hyperAlerts = {}
        -- WAIT @me [[https://github.com/Hammerspoon/hammerspoon/issues/3586][How do I show an alert on all fullscreen spaces? · Issue #3586 · Hammerspoon/hammerspoon]]
        for i, screen in pairs(hs.screen.allScreens()) do
            msg = "🌟"
            -- msg = "Hyper Mode 🌟"
            -- msg = "Hyper Mode ✈"

            alert = hs.alert(msg, hyperStyle, screen, "")
            hyperAlerts[i] = alert
        end
    end
end

function hyper_modality:exited()
    hyper_modality.entered_p = false
    hyper_modality.exit_on_release_p = false

    if hyperSIMAlerts then
        for i, alert in pairs(hyperSIMAlerts) do
            hs.alert.closeSpecific(alert, 0.25)
        end
    end
    if realCurrentWindow then
        realCurrentWindow:focus()
        realCurrentWindow = nil
        focusStealingWebview:hide()
    end

    if prevFocusedElement and prevFocusedElement:isValid() then
        prevFocusedElement.AXFocused = true
    end
    prevFocusedElement = nil

    if hyper_alert_canvas_p then
        hyperModeIndicator:hide()
    else
        for i, alert in pairs(hyperAlerts) do
            hs.alert.closeSpecific(alert, 0.25)
        end
    end

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
-- Trigger existing hyper key shortcuts
for _, key in ipairs({"v", "\\", "delete", "space", "j"}) do
    -- "o",
    --
    -- If you can't bind a key here, it's most probably because you have bound it later in the code.
    ---
    -- hs.keycodes.map['left'], hs.keycodes.map['right'], "right"
    -- [[https://github.com/Hammerspoon/hammerspoon/issues/2282][Having trouble sending arrow key events · Issue #2282 · Hammerspoon/hammerspoon]]
    ---
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

for _, key in ipairs({"[", "-"}) do
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
