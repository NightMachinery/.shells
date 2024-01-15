nightdir = os.getenv("NIGHTDIR") or (os.getenv("HOME") .. "/scripts")
-- require("luarocks.loader")
-- `luarocks path`
package.path = package.path .. ';/usr/local/Cellar/luarocks/3.3.1/share/lua/5.4/?.lua;/usr/local/share/lua/5.4/?.lua;/usr/local/share/lua/5.4/?/init.lua;/usr/local/lib/lua/5.4/?.lua;/usr/local/lib/lua/5.4/?/init.lua;./?.lua;./?/init.lua;/Users/evar/.luarocks/share/lua/5.4/?.lua;/Users/evar/.luarocks/share/lua/5.4/?/init.lua;' .. nightdir ..  '/lua/?.lua'
package.cpath = package.cpath .. ';/usr/local/lib/lua/5.4/?.so;/usr/local/lib/lua/5.4/loadall.so;./?.so;/Users/evar/.luarocks/lib/lua/5.4/?.so'

require "pipe"
ipc = require "hs.ipc"
popclick = require "hs.noises"
application = require "hs.application"
window = require "hs.window"
hotkey = require "hs.hotkey"
keycodes = require "hs.keycodes"
fnutils = require "hs.fnutils"
alert = require "hs.alert"
screen = require "hs.screen"
grid = require "hs.grid"
hints = require "hs.hints"
timer = require "hs.timer"
appfinder = require "hs.appfinder"
applescript = require "hs.applescript"
eventtap = require "hs.eventtap"
json = require("hs.json")
-- chooser = require "hs.chooser"
plp = require 'pl.pretty'
---
function nop()
    hs.alert("repeating")
end
---- * Redis
local redis = require("redis")
local maxRetries = 300
local retryDelay = 10 -- seconds

function connectToRedis()
    return redis.connect('127.0.0.1', 6379)
end

function connectToRedisWithRetries()
    local redisClient
    local success, errMsg
    for attempt = 1, maxRetries do
        success, redisClient = pcall(connectToRedis)
        if success then
            return true, redisClient
        else
            print("Attempt " .. attempt .. " failed: " .. errMsg)
            print("Retrying in " .. retryDelay .. " seconds...")
            hs.timer.usleep(retryDelay * 1000000) -- usleep takes microseconds
        end
    end
    return false, nil
end

local success, redisClient = connectToRedisWithRetries()
if success then
    -- print("Connected to Redis successfully!")
    -- You can now use redisClient to interact with Redis
else
    print("Failed to connect to Redis after " .. maxRetries .. " attempts.")
end
---
local function copyToClipboard(text)
    hs.pasteboard.setContents(text)
end

local function doCopy()
    hs.eventtap.keyStroke({"cmd"}, "c")
end

local function doPaste()
    hs.eventtap.keyStroke({"cmd"}, "v")
end
---
local function timerifyFn(params)
    -- We need to create a new timer for each call/press and make sure it
    -- doesn't get garbage-collected:
    local enabled_p = params.enabled_p
    if enabled_p == nil then
        enabled_p = true
    end
    local fn = params.fn
    local delay = params.delay or 0

    if enabled_p then
        return function()
            local timer
            timer = hs.timer.doAfter(delay, function()
                                         timer = nil
                                         fn()
            end)
        end
    else
        return fn
    end
end
---- * Hyper Modifier Key
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

-- A global variable for the Hyper Mode
redisModalityUpdateP = false
-- @works but not needed for now
function redisActivateMode(mode)
    if redisActivateMode then
        redisClient:set(mode, true)
    end
end
function redisDeactivateMode(mode)
    if redisActivateMode then
        redisClient:set(mode, false)
    end
end

hyper_modality = hs.hotkey.modal.new()
hyper_modality.exit_on_release_p = false
hyper_modality.down_p = false
hyper_modality.entered_p = false

function hyper_enter()
    hyper_modality:enter()
end

function hyper_exit()
    hyper_modality:exit()
end

function hyper_toggle()
    -- hs.alert("toggle")

    if hyper_modality.entered_p then
        hyper_exit()
    else
        hyper_enter()
    end
end

function hyper_down()
    -- hs.alert("hyper down")

    hyper_modality.down_p = true

    hyper_toggle()
end

function hyper_up()
    -- hs.alert("hyper up")

    hyper_modality.down_p = false

    if hyper_modality.exit_on_release_p then
        hyper_exit()
    end
    -- hyper_modality.exit_on_release_p = false
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
}

function hyper_modality:entered()
    hyper_modality.entered_p = true

    -- I have not yet added the redis updaters for purple_modality.
    redisActivateMode("hyper_modality")

    if hs.eventtap.isSecureInputEnabled() then
        hs.alert("⚠️ Secure Input is on. Our Hyper Mode commands might not work.")
        -- [[https://github.com/Hammerspoon/hammerspoon/issues/3555][Hammerspoon hangs spradically when entering hyper mode and displaying a modal window · Issue #3555 · Hammerspoon/hammerspoon]]
    end

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

function hyper_modality:exited()
    hyper_modality.entered_p = false
    hyper_modality.exit_on_release_p = false

    for i, alert in pairs(hyperAlerts) do
        hs.alert.closeSpecific(alert, 0.25)
    end

    redisDeactivateMode("hyper_modality")
end

function hyper_triggered()
    hyper_modality.exit_on_release_p = true
    if not hyper_modality.down_p then
        -- This is to handle the race conditions.
        -- The key release handler might be run before we have set =exit_on_release_p=.
        hyper_exit()
    end
end

function hyper_bind_v1(key, pressedfn)
    function h_pressedfn()
        hyper_triggered()

        pressedfn()
    end

    return hyper_modality:bind({}, key, h_pressedfn, nil, nil)
end

function hyper_bind_v2(o)
    local hotkey_holder = { my_hotkey = nil }

    if o.auto_trigger_p == nil then
        o.auto_trigger_p = true
    end

    if o.auto_trigger_p then
        if o.pressedfn == nil then
            h_pressedfn = nil
        else
            function h_pressedfn()
                hyper_triggered()

                o.pressedfn()
            end
        end

        if o.releasedfn == nil then
            h_releasedfn = nil
        else
            function h_releasedfn()
                hyper_triggered()

                o.releasedfn()
            end
        end

        if o.repeatfn == nil then
            h_repeatfn = nil
        else
            function h_repeatfn()
                -- hs.alert("repeating: " .. o.key)
                -- hs.alert("repeating: " .. o.key .. "down: " .. tostring(hyper_modality.down_p) .. "hotkey: " .. tostring(hotkey_holder.my_hotkey))

                if hyper_modality.down_p == false then
                    -- @upstreamBug @raceCondition [[id:c27a51c9-d4ea-4714-8c15-72840c1fb933][=repeatfn= is buggy]]

                    -- hyper_exit()

                    if true then
                        hyper_enter()
                        hyper_exit()
                        return
                    elseif false then
                        error("workaround for infinitely repeating hotkeys (id: NIGHT_817124)")
                        -- No need for the buggy code down, we can just throw an explicit error ourselves.

                        -- return
                        -- Somehow the return statment above causes a syntax error!
                    else
                        if not (hotkey_holder.my_hotkey == nil) then
                            -- hs.alert("disabling")

                            -- Disable the hotkey
                            hotkey_holder.my_hotkey:disable()
                            -- This throws an exception which successfully stops the repeat loop.
                            -- `callback: /Users/evar/.hammerspoon/init.lua:209: attempt to call a nil value (method 'disable')`

                            hotkey_holder.my_hotkey:enable()
                            -- We should re-enable the hotkey so that it can be triggered again. Since an exception happens, everything magically works out even without this line.

                            hs.alert("disabled")
                        end

                        hs.alert("Repeat Bug Encountered (id: NIGHT_817123)")
                        return 0
                    end
                end

                hyper_triggered()

                o.repeatfn()
            end
        end
    else
        h_pressedfn = o.pressedfn
        h_releasedfn = o.releasedfn
        h_repeatfn = o.repeatfn
    end

    hotkey_holder.my_hotkey = hyper_modality:bind(o.mods or {}, o.key, h_pressedfn, h_releasedfn, h_repeatfn)
    -- hs.hotkey.bind(mods, key, [message,] pressedfn, releasedfn, repeatfn)

    return hotkey_holder.my_hotkey
end

--- ** Hyper Hotkeys (Main Section)
hyper_toggler_1 = hs.hotkey.bind({}, "F18", hyper_down, hyper_up, nil)
-- Trigger existing hyper key shortcuts
for _, key in ipairs({"v", "\\", "delete", "space", "h"}) do
    -- "o",
    --
    -- If you can't bind a key here, it's most probably because you have bound it later in the code.
    ---
    -- hs.keycodes.map['left'], hs.keycodes.map['right'], "right"
    -- [[https://github.com/Hammerspoon/hammerspoon/issues/2282][Having trouble sending arrow key events · Issue #2282 · Hammerspoon/hammerspoon]]
    ---
    hyper_bind_v2{key=key, pressedfn=function()
                      hyper_triggered()

                      hs.eventtap.keyStroke({"cmd","alt","shift","ctrl"}, key)
    end}
end

for _, key in ipairs({"[", "-"}) do
    -- AltTab Window Switcher:
    -- Its proper functions needs the modifier keys to be kept pressed while the hyper mode is active. I.e., it really needs the hyper mode to be the same thing as having the modifiers pressed.
    -- I don't know of a way to do that (see [[id:6fcee871-a0f9-46b5-af2f-a9b767c48422][@me How can I make Hammerspoon press modifier keys? · Issue #3582 · Hammerspoon/hammerspoon]]), but we could add `hyper_modality.press_on_exit = {"return"}`. This would make the common usage of the window switcher painless, but it might break the more advanced usage of it; like pressing =w= to close a window.
    hyper_bind_v2{
        key = key,
        pressedfn = function()
            hyper_triggered()
            hs.eventtap.event.newKeyEvent({"cmd", "alt", "shift", "ctrl"}, key, true):post()
        end,
        releasedfn = function()
            hs.eventtap.event.newKeyEvent({"cmd", "alt", "shift", "ctrl"}, key, false):post()
        end
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
--- * Purple Mode
-- A global variable for the Purple Mode
purple_modality = hs.hotkey.modal.new()
purple_modality.exit_on_release_p = false
purple_modality.down_p = false
purple_modality.entered_p = false

function purple_enter()
    purple_modality:enter()
end

function purple_exit()
    purple_modality:exit()
end

function purple_toggle()
    -- hs.alert("toggle")

    if purple_modality.entered_p then
        purple_exit()
    else
        purple_enter()
    end
end

function purple_down()
    -- hs.alert("purple down")

    purple_modality.down_p = true

    purple_toggle()
end

function purple_up()
    -- hs.alert("purple up")

    purple_modality.down_p = false

    if purple_modality.exit_on_release_p then
        purple_exit()
    end
    -- purple_modality.exit_on_release_p = false
end

local purpleAlerts

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
}

function purple_modality:entered()
    purple_modality.entered_p = true

    if hs.eventtap.isSecureInputEnabled() then
        hs.alert("⚠️ Secure Input is on. Our Purple Mode commands might not work.")
        -- [[https://github.com/Hammerspoon/hammerspoon/issues/3555][Hammerspoon hangs spradically when entering hyper mode and displaying a modal window · Issue #3555 · Hammerspoon/hammerspoon]]
    end

    purpleAlerts = {}
    for i, screen in pairs(hs.screen.allScreens()) do
        msg = "Purple"
        -- msg = "Purple Mode 🌟"
        -- msg = "Purple Mode ✈"

        alert = hs.alert(msg, purpleStyle, screen, "")
        purpleAlerts[i] = alert
    end
end

function purple_modality:exited()
    purple_modality.entered_p = false
    purple_modality.exit_on_release_p = false

    for i, alert in pairs(purpleAlerts) do
        hs.alert.closeSpecific(alert, 0.25)
    end
end

function purple_triggered()
    purple_modality.exit_on_release_p = true
    if not purple_modality.down_p then
        -- This is to handle the race conditions.
        -- The key release handler might be run before we have set =exit_on_release_p=.
        purple_exit()
    end
end

function purple_bind_v1(key, pressedfn)
    function h_pressedfn()
        purple_triggered()

        pressedfn()
    end

    return purple_modality:bind({}, key, h_pressedfn, nil, nil)
end

function purple_bind_v2(o)
    local hotkey_holder = { my_hotkey = nil }

    if o.auto_trigger_p == nil then
        o.auto_trigger_p = true
    end

    if o.auto_trigger_p then
        if o.pressedfn == nil then
            h_pressedfn = nil
        else
            function h_pressedfn()
                purple_triggered()

                o.pressedfn()
            end
        end

        if o.releasedfn == nil then
            h_releasedfn = nil
        else
            function h_releasedfn()
                purple_triggered()

                o.releasedfn()
            end
        end

        if o.repeatfn == nil then
            h_repeatfn = nil
        else
            function h_repeatfn()
                -- hs.alert("repeating: " .. o.key)
                -- hs.alert("repeating: " .. o.key .. "down: " .. tostring(purple_modality.down_p) .. "hotkey: " .. tostring(hotkey_holder.my_hotkey))

                if purple_modality.down_p == false then
                    -- @upstreamBug @raceCondition [[id:c27a51c9-d4ea-4714-8c15-72840c1fb933][=repeatfn= is buggy]]

                    -- purple_exit()

                    if true then
                        purple_enter()
                        purple_exit()
                        return
                    elseif false then
                        error("workaround for infinitely repeating hotkeys (id: NIGHT_817124)")
                        -- No need for the buggy code down, we can just throw an explicit error ourselves.

                        -- return
                        -- Somehow the return statment above causes a syntax error!
                    else
                        if not (hotkey_holder.my_hotkey == nil) then
                            -- hs.alert("disabling")

                            -- Disable the hotkey
                            hotkey_holder.my_hotkey:disable()
                            -- This throws an exception which successfully stops the repeat loop.
                            -- `callback: /Users/evar/.hammerspoon/init.lua:209: attempt to call a nil value (method 'disable')`

                            hotkey_holder.my_hotkey:enable()
                            -- We should re-enable the hotkey so that it can be triggered again. Since an exception happens, everything magically works out even without this line.

                            hs.alert("disabled")
                        end

                        hs.alert("Repeat Bug Encountered (id: NIGHT_817123)")
                        return 0
                    end
                end

                purple_triggered()

                o.repeatfn()
            end
        end
    else
        h_pressedfn = o.pressedfn
        h_releasedfn = o.releasedfn
        h_repeatfn = o.repeatfn
    end

    hotkey_holder.my_hotkey = purple_modality:bind(o.mods or {}, o.key, h_pressedfn, h_releasedfn, h_repeatfn)
    -- hs.hotkey.bind(mods, key, [message,] pressedfn, releasedfn, repeatfn)

    return hotkey_holder.my_hotkey
end

-- Everything above is just copied from the hyper modality code with a simple s/hyper/purple/gc.
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
--- * Mouse
function cursorHide()
    -- Get the main screen's frame
    local mainScreen = hs.screen.mainScreen()
    local mainFrame = mainScreen:frame()

    -- Calculate the position at the rightmost edge of the screen
    -- We subtract a small amount (like 1) to ensure the cursor is still on the screen
    local xPosition = mainFrame.w - 1
    local yPosition = mainFrame.h / 2 -- This will place the cursor vertically at the center

    -- Move the mouse to the top center (so that the top app bar gets hidden next)
    hs.mouse.absolutePosition(hs.geometry.point(mainFrame.w / 2, 0))

    -- Move the mouse to the center right
    hs.mouse.absolutePosition(hs.geometry.point(xPosition, yPosition))
end
hyper_bind_v2{pressedfn=cursorHide, mods={"ctrl"}, key="space"}
-- ** Keyboard Mouse Mode
-- @warning Moving the mouse using Hammerspoon doesn't register for certain for some events. Doing a right click forces the new position to be registered in these cases.
---
-- Variables for momentum
local min_speed = 10
local max_speed = 1000
local acceleration = 30
local current_speed = min_speed

-- Table to keep track of which keys are currently pressed

local keysPressed = { up = false, down = false, left = false, right = false }

-- Update the cursorMoveRelative function to handle diagonal movement
function cursorMoveRelative()
    local dx = 0
    local dy = 0

    if keysPressed.up then dy = dy - 1 end

    if keysPressed.down then dy = dy + 1 end
    if keysPressed.left then dx = dx - 1 end
    if keysPressed.right then dx = dx + 1 end

    if dx ~= 0 or dy ~= 0 then
        local currentPos = hs.mouse.absolutePosition()
        local newPos = hs.geometry.point(currentPos.x + dx * current_speed, currentPos.y + dy * current_speed)
        hs.mouse.absolutePosition(newPos)
        current_speed = math.min(max_speed, current_speed + acceleration)

    end
end

-- Function to update the key state and move the cursor
function updateKeyStateAndMove(key, isPressed)
    keysPressed[key] = isPressed
    cursorMoveRelative()
end

function areAnyKeysPressed()
    for _, isPressed in pairs(keysPressed) do
        if isPressed then

            return true        end
    end
    return false
end

-- Function to reset the speed and key state when the key is released
function resetSpeedAndKeyState(key)
    keysPressed[key] = false

    if not areAnyKeysPressed() then
        current_speed = min_speed
    end
end

-- Bindings with updated functions for key press and release
purple_bind_v2{mods={}, key="right", pressedfn=function() updateKeyStateAndMove('right', true) end, repeatfn=cursorMoveRelative, releasedfn=function() resetSpeedAndKeyState('right') end}
purple_bind_v2{mods={}, key="left", pressedfn=function() updateKeyStateAndMove('left', true) end, repeatfn=cursorMoveRelative, releasedfn=function() resetSpeedAndKeyState('left') end}
purple_bind_v2{mods={}, key="up", pressedfn=function() updateKeyStateAndMove('up', true) end, repeatfn=cursorMoveRelative, releasedfn=function() resetSpeedAndKeyState('up') end}
purple_bind_v2{mods={}, key="down", pressedfn=function() updateKeyStateAndMove('down', true) end, repeatfn=cursorMoveRelative, releasedfn=function() resetSpeedAndKeyState('down') end}

hyper_bind_v2{auto_trigger_p=false, mods={"shift"}, key="right", pressedfn=function() updateKeyStateAndMove('right', true) end, repeatfn=cursorMoveRelative, releasedfn=function() resetSpeedAndKeyState('right') end}
hyper_bind_v2{auto_trigger_p=false, mods={"shift"}, key="left", pressedfn=function() updateKeyStateAndMove('left', true) end, repeatfn=cursorMoveRelative, releasedfn=function() resetSpeedAndKeyState('left') end}
hyper_bind_v2{auto_trigger_p=false, mods={"shift"}, key="up", pressedfn=function() updateKeyStateAndMove('up', true) end, repeatfn=cursorMoveRelative, releasedfn=function() resetSpeedAndKeyState('up') end}
hyper_bind_v2{auto_trigger_p=false, mods={"shift"}, key="down", pressedfn=function() updateKeyStateAndMove('down', true) end, repeatfn=cursorMoveRelative, releasedfn=function() resetSpeedAndKeyState('down') end}

-- *** Mouse Avy
-- **** showOverlay
function showOverlay(params)
    -- Default values for the parameters
    local defaults = {
        text = "",
        x = 100,
        y = 100,
        backgroundColor = { white = 1, alpha = 0.5 },
        foregroundColor = { black = 1, alpha = 1.0 }
    }

    -- Merge default values with the provided parameters
    for k, v in pairs(defaults) do
        if params[k] == nil then
            params[k] = v
        end
    end

    -- Convert color tables to hs.drawing.color objects
    local bgColor = hs.drawing.color.asRGB(params.backgroundColor)
    local fgColor = hs.drawing.color.asRGB(params.foregroundColor)

    -- Create a styled text object for the text element
    local styledText = hs.styledtext.new(params.text, {
                                             color = fgColor,
                                             paragraphStyle = {
                                                 alignment = "center"
                                             }
    })

    -- Create a canvas
    local canvas = hs.canvas.new({x = params.x, y = params.y, w = 0, h = 0})

    -- Add a background rectangle element to the canvas
    canvas:insertElement({
            type = "rectangle",
            action = "fill",
            fillColor = bgColor,
            frame = { x = "0%", y = "0%", w = "100%", h = "100%" }
                         }, 1) -- Insert as the first element

    -- Add a text element to the canvas
    canvas:insertElement({
            type = "text",
            text = styledText,
            frame = { x = "0%", y = "0%", w = "100%", h = "100%" }

                         }, 2) -- Insert as the second element

    -- Calculate the size of the text
    local textSize = canvas:minimumTextSize(styledText)
    -- [[https://www.hammerspoon.org/docs/hs.canvas.html#minimumTextSize][Hammerspoon docs: hs.canvas]]

    height = textSize.h + 0
    width = textSize.w + 5

    -- Adjust the size of the canvas to fit the text with some padding
    canvas:size({w = width, h = height})

    -- Center the canvas frame on the provided x, y coordinates
    local frame = canvas:frame()
    canvas:frame({
            x = params.x - (width) / 2,
            y = params.y - (height) / 2,
            w = width,
            h = height
    })

    -- Show the canvas
    canvas:show(0)
    -- fadeInTime - An optional number of seconds over which to fade in the canvas object. Defaults to zero.

    canvas:bringToFront(true)
    -- @redundant? if true, place the canvas on top of all windows (including the dock and menubar and fullscreen windows).

    -- Return the canvas object in case the caller wants to manipulate it further.
    -- The caller must store a reference to the canvas or it will be garbage collected.
    -- [[https://github.com/Hammerspoon/hammerspoon/issues/2730][Auto collect hs.canvas objects? · Issue #2730 · Hammerspoon/hammerspoon]]
    return {
        canvas=canvas,
    }
end

-- my_canvas = showOverlay({
--     text = "a",
--     x = 200,
--     y = 200,
--     backgroundColor = { red = 0, green = 0, blue = 1, alpha = 0.5 },
--     foregroundColor = { red = 1, green = 1, blue = 1, alpha = 1.0 }
-- })
-- ***** avy
-- Define a function to generate two-letter combinations
local function generateTwoLetterCombinations()
    local lettersFirst = "abcdefghijklmnopqrstuvwxyz/.,;'[]\\=-0987654321`"
    -- letters = letters .. "1234567890-="

    local lettersSecond = "abcdefghijklmnopqrstuvwxyz/.,;'[]\\"

    local combinations = {}
    for i = 1, #lettersFirst do
        for j = 1, #lettersSecond do
            table.insert(combinations, lettersFirst:sub(i, i) .. lettersSecond:sub(j, j))
        end
    end
    return combinations
end

function screenPositionAvy(params)
    if params == nil then
        params = {}
    end

    -- Default values for the parameters
    local defaults = {
        callback = function(x, y)
            hs.mouse.absolutePosition({ x = x, y = y })
        end,

        backgroundColor = { white = 1, alpha = 0.5 },
        fgColor = { red = 0, green = 0, blue = 0, alpha = 1.0 },
        secondModalBgColor = { red = 0.9, green = 1.0, blue = 1.0, alpha = 0.3 },
        secondModalFgColor = { red = 0.8, green = 0.0, blue = 0.0, alpha = 1.0 },

        overlayHeight = 20,
        overlayWidth = 50,
        fontSize = 18,
    }

    -- Merge default values with the provided parameters
    for k, v in pairs(defaults) do
        if params[k] == nil then
            params[k] = v
        end
    end
    ---
    local callback = params.callback
    local fgColor = hs.drawing.color.asRGB(params.fgColor)
    local bgColor = hs.drawing.color.asRGB(params.backgroundColor)
    local secondModalBgColor = hs.drawing.color.asRGB(params.secondModalBgColor)
    local secondModalFgColor = hs.drawing.color.asRGB(params.secondModalFgColor)

    local combinations = generateTwoLetterCombinations()

    local screenWidth = hs.screen.mainScreen():frame().w
    local screenHeight = hs.screen.mainScreen():frame().h

    local overlayHeight = params.overlayHeight
    local overlayWidth = params.overlayWidth
    local fontSize = params.fontSize

    local canvas = hs.canvas.new({x = 0, y = 0, w = screenWidth, h = screenHeight + 200})
    local index = 1

    -- Calculate the number of overlays to fit the screen
    local columns = math.ceil(screenWidth / overlayWidth)
    local rows = math.ceil(screenHeight / overlayHeight) + 9

    local function cleanup()
        canvas:hide()
        canvas:delete()
    end

    -- Define a function to handle key presses
    local function handleKeyPress(modal, x, y)
        -- hs.mouse.absolutePosition({ x = x, y = y })
        modal:exit()
        cleanup()

        callback(x, y) -- Call the callback function with the selected position
    end

    -- Set up the hotkey modal
    local mouse_avy_modality = hs.hotkey.modal.new()

    -- Create a table to store the second modal for each first character
    local second_modals = {}

    canvas:insertElement({
            type = "rectangle",
            action = "fill",
            fillColor = bgColor,
            frame = { x = "0%", y = "0%", w = "100%", h = "100%" }
                         }, 1) -- Insert as the first element

    local function updateStyledTextColor(element_index, combo_index, color)
        local text = combinations[combo_index]

        local newStyledText = hs.styledtext.new(
            text,
            {

                color = color,
                paragraphStyle = { alignment = "center" },
                font = { size = fontSize },
        })
        canvas[element_index].text = newStyledText
    end

    -- Create text elements for each position and add them to the canvas
    local text_elements_by_first_char = {}
    for row = 0, rows - 1 do
        for col = 0, columns - 1 do
            local combo = combinations[index]

            -- if not combo then
            --    combo = "??"
            -- end

            if combo then
                local x = col * overlayWidth + overlayWidth / 2
                local y = row * overlayHeight + overlayHeight / 2
                local x_canvas = x - overlayWidth / 2
                local y_canvas = y - overlayHeight / 2

                local styledText = hs.styledtext.new(
                    combo,
                    {
                        color = fgColor,
                        paragraphStyle = { alignment = "center" },
                        font = { size = fontSize },
                })
                canvas:insertElement({

                        type = "text",
                        text = styledText,
                        frame = { x = x_canvas, y = y_canvas, w = overlayWidth, h = overlayHeight }

                })
                local canvas_index = #canvas
                local combo_index = index

                local first_char = combo:sub(1, 1)
                local second_char = combo:sub(2, 2)

                -- Store the text element index for the first character
                if not text_elements_by_first_char[first_char] then
                    text_elements_by_first_char[first_char] = {}
                end
                table.insert(text_elements_by_first_char[first_char], {canvas_index= canvas_index, combo_index = combo_index})


                if not second_modals[first_char] then
                    second_modals[first_char] = hs.hotkey.modal.new()
                    mouse_avy_modality:bind(
                        {},
                        first_char,
                        function()
                            mouse_avy_modality:exit()
                            second_modals[first_char]:enter()

                            -- Change the background color when entering the second modal
                            canvas[1].fillColor = secondModalBgColor

                            -- Change the text color when entering the second modal
                            if text_elements_by_first_char[first_char] then
                                for _, el in ipairs(text_elements_by_first_char[first_char]) do

                                    updateStyledTextColor(el.canvas_index, el.combo_index, secondModalFgColor)
                                end
                            end
                    end)
                end

                second_modals[first_char]:bind({}, second_char, (function()
                            handleKeyPress(second_modals[first_char], x, y_canvas + 10)
                                              end))

                index = index + 1
            end
        end
    end

    -- Show the canvas
    canvas:show()
    canvas:bringToFront(true)

    -- Exit the second modal and re-enter the first modal when the escape key is pressed
    -- [[https://github.com/Hammerspoon/hammerspoon/issues/848][How to bind hs.hotkey.modal to any key press? · Issue #848 · Hammerspoon/hammerspoon]]
    for first_char, second_modal in pairs(second_modals) do
        second_modal:bind({}, "escape", function()
                second_modal:exit()
                mouse_avy_modality:enter()

                -- Revert the background color when exiting the second modal
                canvas[1].fillColor = hs.drawing.color.asRGB(bgColor)

                -- Revert the text color when exiting the second modal
                if text_elements_by_first_char[first_char] then
                    for _, el in ipairs(text_elements_by_first_char[first_char]) do

                        updateStyledTextColor(el.canvas_index, el.combo_index, fgColor) -- Revert to original color
                    end
                end
        end)
    end

    mouse_avy_modality:bind({}, "escape", function()
            mouse_avy_modality:exit()

            cleanup()
    end)

    -- Pressing =enter= will reuse the current mouse position.
    mouse_avy_modality:bind({}, "return", function()
            local mousePosition = hs.mouse.absolutePosition()
            local x = mousePosition.x
            local y = mousePosition.y

            handleKeyPress(mouse_avy_modality, x, y)
    end)

    -- Enter the modal state
    mouse_avy_modality:enter()
end
-- *** Mouse Clicks
function leftClick()
    hs.eventtap.leftClick(hs.mouse.absolutePosition())
end

function rightClick()
    -- hs.alert("right click")

    hs.eventtap.rightClick(hs.mouse.absolutePosition())
end

function mouseClick(params)
    -- Set defaults for optional parameters
    params.actions = params.actions or {"down", "up"}

    params.position = params.position or hs.mouse.absolutePosition()
    params.mods = params.mods or {}
    params.sleep = params.sleep or 0

    -- Define the event types based on the action names
    local eventTypes = {
        left_down = hs.eventtap.event.types.leftMouseDown,
        left_up = hs.eventtap.event.types.leftMouseUp,
        right_down = hs.eventtap.event.types.rightMouseDown,
        right_up = hs.eventtap.event.types.rightMouseUp,

        left_drag = hs.eventtap.event.types.leftMouseDragged,
    }

    -- Alias mapping for shorthand actions
    local actionAliases = {
        down = "left_down",
        up = "left_up",
        -- rd = "right_down",
        -- ru = "right_up",
    }

    -- Generate and post the events
    for _, action in ipairs(params.actions) do
        -- Translate shorthand actions to their full counterparts
        local fullAction = actionAliases[action] or action
        local eventType = eventTypes[fullAction]
        if eventType then
            local event = hs.eventtap.event.newMouseEvent(eventType, params.position, params.mods)
            event:post()

            -- Sleep between actions if specified
            if params.sleep > 0 then
                hs.timer.usleep(params.sleep * 1000000)
            end
        else
            error("Unsupported mouse action: " .. tostring(action))
        end
    end
end

function leftClickAvy()
    screenPositionAvy({
            callback = function(x, y)
            ---
             hs.mouse.absolutePosition({ x = x, y = y })
             leftClick()
            ---
                -- This also moves the cursor and doesn't just click the location like Shortcat can do.
                -- mouseClick{position={ x = x, y = y}}
            ---
        end,
    })
end
function leftDrag()
    mouseClick{actions={"left_down", "left_drag"}, sleep=0}
end
function leftDragAvy()
    screenPositionAvy({
            callback = function(x, y)
                mouseClick{position={ x = x, y = y}, actions={"left_down", "left_drag"}, sleep=0}
                -- This works with the real cursor (i.e., moving the cursor selects stuff), but not with us moving the cursor. This same thing happened with just =left_down=.
                -- Update: This no longer works with the real cursor. I have no idea what I changed. It just worked a minute ago!
                -- This somewhat works (on Skim only) if you press leftDrag both on the start of the text and on its end. It's not at all reliable though.
                -- I guess the only way to make this work reliably is as =textSelectAvyV2= does: do a left_down on the start, then a left_drag and left_up at the destination. Even that doesn't work reliably on Skim.

                -- mouseClick{position={ x = x, y = y}, actions={"left_drag", "left_up"}, sleep=0}
            end,
    })
end
function rightClickAvy()
    screenPositionAvy({
            callback = function(x, y)
            hs.mouse.absolutePosition({ x = x, y = y })
            rightClick()
        end,
    })
end

function textSelectAvy()
    local function second(x, y)
        mouseClick{mods={"shift"}, position={ x = x, y = y }}

        doCopy()
    end

    local function first(x, y)
        hs.mouse.absolutePosition({ x = x, y = y })
        leftClick()

        screenPositionAvy({
                callback = second,
        })
    end

    screenPositionAvy({
            callback = first,
    })
end

function textSelectAvyV2()
    local function dragTo(x, y)
        mouseClick{position={ x = x, y = y}, actions={"left_drag", "left_up"}, sleep=0}

        doCopy()
    end

    local function startDrag(x, y)
        -- Move the mouse to the starting position
        -- hs.mouse.absolutePosition({ x = x, y = y })

        -- Create a mouse event for pressing down
        mouseClick{position={ x = x, y = y}, actions={"left_down"}, sleep=0}

        screenPositionAvy({
            callback = dragTo,
        })
    end

    screenPositionAvy({
        callback = startDrag,
    })
end

for _, binder in ipairs({purple_bind_v2, hyper_bind_v2}) do
    binder{mods={"ctrl"}, key="return", pressedfn=screenPositionAvy,}
    binder{mods={}, key="return", pressedfn=leftClickAvy,}
    -- binder{mods={"shift"}, key="return", pressedfn=leftDrag,}
    binder{mods={}, key="]", pressedfn=rightClickAvy,}
    binder{mods={}, key="o", pressedfn=textSelectAvyV2,}

    binder{mods={}, key="'", pressedfn=leftClick}
    binder{mods={"shift"}, key="'", pressedfn=function()
               mouseClick{mods={"shift"}}
    end}
    binder{mods={"ctrl"}, key="'", pressedfn=function()
               -- This works partially; if you move the mouse manually, the drag/select action is performed correctly. But moving the mouse using Hammerspoon doesn't trigger this.
               mouseClick{actions={"down"}}
    end}
    binder{mods={}, key=";", pressedfn=rightClick}
end
--- **** Screenshot Avy
-- Function to take a screenshot given two points
function takeScreenshot(topLeft, bottomRight)
    local x = topLeft.x
    local y = topLeft.y
    local width = bottomRight.x - topLeft.x
    local height = bottomRight.y - topLeft.y
    local rectString = string.format("%f,%f,%f,%f", x, y, width, height)

    -- -c      Force screen capture to go to the clipboard.
    -- -R      <rectangle> Capture rectangle using format x,y,width,height.
    hs.task.new("/usr/sbin/screencapture", nil, {"-c", "-R" .. rectString}):start()
end

-- Function to capture two points on the screen and take a screenshot
function screenshotAvy()
    local alpha = 0.1

    local points = {}
    local function capturePoint(x, y)
        table.insert(points, {x = x, y = y})

        if #points == 2 then
            takeScreenshot(points[1], points[2])
        else
            -- Call screenPositionAvy again to capture the second point
            screenPositionAvy({
            callback = capturePoint,
            backgroundColor = { red=255/255, green=140/255, blue=0 , alpha = alpha },
            secondModalBgColor = { red = 0.9, green = 1.0, blue = 1.0, alpha = alpha },
            })
        end
    end

    -- Start capturing the first point
    screenPositionAvy({
            callback = capturePoint,
            backgroundColor = { red=255/255, green=215/255, blue=0 , alpha = alpha },
            secondModalBgColor = { red = 0.9, green = 1.0, blue = 1.0, alpha = alpha },
    })
end

hyper_bind_v2{key="s", pressedfn=screenshotAvy}
--- * _
function has_value (tab, val)
    for index, value in ipairs(tab) do
        if value == val then
            return true
        end
    end

    return false
end

-- Scroll functionality forked from https://github.com/trishume/dotfiles/blob/master/hammerspoon/hammerspoon.symlink/init.lua
function newScroller(delay, tick)
    return { delay = delay, tick = tick, timer = nil, mode = "pixel" }
end

function startScroll(scroller)
    if scroller.timer == nil then
        scroller.timer = timer.doEvery(scroller.delay, function()
                                           eventtap.scrollWheel({0,scroller.tick},{}, scroller.mode)
        end)
    end
end

function stopScroll(scroller)
    if scroller.timer then
        scroller.timer:stop()
        scroller.timer = nil
    end
end

listener = nil
popclickListening = false
popclickScrollEnabled = false
tssScrollDown = newScroller(0.02, -10)
scrollExcluded = { "iTerm2", "Terminal", "Code", "Code - Insiders" } -- "Emacs",
lastS = 0
timerS = nil
gradS = 0
-- gradMode = true
gradMode = false
timerSR = timer.doEvery(0.1, function()
                            gradS = gradS - 1
                            if gradS < 0 then
                                gradS = 0
                            end
end)
function stopTimerS()
    if timerS ~= nil then
        timerS:stop()
        timerS = nil
    end
end
function scrollHandler(evNum)
    -- hs.alert.show("Listening: " .. tostring(popclickListening) .. ", NH: " .. evNum)
    if not popclickListening then
        return
    end
    local durS = 0
    if evNum == 1 then
        lastS = os.time()
    else
        if lastS ~= 0 then
            durS = os.time() - lastS
        end
        lastS = 0
    end
    stopTimerS()
    appName = application.frontmostApplication():name()
    iterm_focus = exec_raw('/usr/local/bin/redis-cli --raw get iterm_focus')
    -- hs.alert.show("App: " .. appName .. ", iterm_focus: " .. iterm_focus .. ", NH: " .. evNum)
    iterm_focus = (iterm_focus == 'TERMINAL_WINDOW_BECAME_KEY\n')

    if popclickScrollEnabled then
        if evNum == 1 then
            if iterm_focus or has_value(scrollExcluded, appName) then
                return
            elseif appName == "Emacs" then
                tssScrollDown.tick = -1
                -- tssScrollDown.mode = "line"
                tssScrollDown.mode = "pixel"
                tssScrollDown.delay  = 0.1
            elseif appName == "mpv" then
                tssScrollDown.tick = -10
                tssScrollDown.mode = "pixel"
                tssScrollDown.delay  = 0.001
            else
                tssScrollDown.tick = -10
                tssScrollDown.mode = "pixel"
                tssScrollDown.delay  = 0.02
            end
            startScroll(tssScrollDown)
        elseif evNum == 2 then
            stopScroll(tssScrollDown) -- Don't exclude apps here or we'll have infinite scroll
        elseif evNum == 3 then
            if iterm_focus or has_value(scrollExcluded, appName) then
                return
            elseif appName == "python" then
                for i = 1,10 do
                    eventtap.keyStroke({}, hs.keycodes.map['up'])
                end
            elseif appName == "Emacs" then
                -- eventtap.scrollWheel({0,100},{}, "line") -- works accurately for text modes
                -- it seems any amount of scrolling seems the same to pdf-mode
                -- eventtap.scrollWheel({0,40000},{}, "pixel")
                for i = 1,3 do
                    eventtap.scrollWheel({0,250},{}, "line") -- works accurately for text modes
                end
                -- eventtap.keyStroke({}, 'k')
            else
                eventtap.scrollWheel({0,250},{}, "pixel")
            end
        end
    elseif gradMode then
        if evNum == 3 then
            -- if appName == "mpv" then
            --   hs.eventtap.keyStroke({}, hs.keycodes.map['space'])
            -- end
        elseif evNum == 2 then
            -- if durS >= 1 then
            --   if appName == "mpv" then
            --     hs.eventtap.keyStroke({}, hs.keycodes.map['space'])
            --   end
            -- end
        elseif evNum == 1 then
            -- brishzeval("bello")
            if appName == "mpv" then
                -- brishzeval("bell-lm-timetoparty")
                -- timerS = hs.timer.doAfter(0.9, function() hs.eventtap.keyStroke({}, hs.keycodes.map['space']) end)
                timerS = timer.doEvery(0.01, function()
                                           gradS = gradS + 0.2
                                           if gradS > 30 then
                                               stopTimerS()
                                               gradS = 0
                                               hs.eventtap.keyStroke({}, hs.keycodes.map['space'])
                                           end
                end)
            end
        end
    end
end

function popclickPlayPause()
    -- alert.show("toggling popclick")
    if not popclickListening then
        lastS = 0
        listener:start() -- @workaround_lisflag
        alert.show("listening")
    else
        listener:stop() -- @workaround_lisflag
        lastS = 0
        scrollHandler(2) -- stop
        alert.show("stopped listening")
    end
    popclickListening = not popclickListening
    return popclickListening
end
function popclickScrollToggle()
    if popclickScrollEnabled then
        lastS = 0
        scrollHandler(2) -- stop
        -- alert.show("stopped popclickScroll")
    else
        -- alert.show("enabled popclickScroll")
    end
    popclickScrollEnabled = not popclickScrollEnabled
    return popclickScrollEnabled
end
function popclickBttToggle()
    -- return popclickScrollToggle()
    return popclickPlayPause()
end
function popclickBttGet()
    -- return popclickScrollEnabled
    return popclickListening
end
function popclickInit()
    popclickListening = false
    -- popclickScrollEnabled = false
    popclickScrollEnabled = true
    local fn = scrollHandler
    listener = popclick.new(fn)
    -- popclickPlayPause()
    --- @workaround_lisflag we use the flag to control acting on the events to work around the bug that sometimes starting the listener can take ~6 seconds. Update: When that bug happens, it doesn't seem that listener can listen at all! Restarting hammerspoon completely seems to solve this issue.
    -- listener:start()
    ---
    -- brishzeval('awaysh hs-popclick-btt-refresh')
end
---
local inputEnglish = "com.apple.keylayout.US"
local inputPersian = "com.apple.keylayout.Persian-ISIRI2901"
--
hyper = {"cmd","ctrl","alt","shift"}
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
    function appWatch(appName, event, app)
        -- alert.show("appWatch: " .. appName .. ", event: " .. tostring(event) .. ", app: " .. tostring(app), 7)
        if event == hs.application.watcher.activated then
            if has_value(enOnly, appName) then
                if input_lang_push_lang == nil then
                    input_lang_push_lang = hs.keycodes.currentSourceID()
                end

                langSetEn()
            else
                if not (input_lang_push_lang == nil) then
                    hs.keycodes.currentSourceID(input_lang_push_lang)
                    input_lang_push_lang = nil
                end
            end
        end
    end
end
appWatcher = hs.application.watcher.new(appWatch)
appWatcher:start()
---
popclickInit()
-- popclickPlayPause()
-- hs.urlevent.bind("popclickPlayPause", function(eventName, params)
--                    popclickPlayPause()
--                    -- alert.show("popclickListening: " .. tostring(popclickListening))
-- end)
---
-- * [[https://www.hammerspoon.org/docs/hs.eventtap.event.html#newSystemKeyEvent][Hammerspoon docs: hs.eventtap.event]]
-- ** DONE [[https://github.com/Hammerspoon/hammerspoon/issues/3350][`hs.eventtap.event.newSystemKeyEvent`: keeps pressing the keys indefintely · Issue #3350 · Hammerspoon/hammerspoon]]

function systemKey(key, repeatDelay)
    -- REPEAT_FASTER = 10 * 1000
    REPEAT_FASTER = 5

    hs.eventtap.event.newSystemKeyEvent(key, true):post()

    if not repeatDelay or repeatDelay <= 0 then
        repeatDelay = REPEAT_FASTER
    end
    hs.timer.usleep(repeatDelay)
    hs.eventtap.event.newSystemKeyEvent(key, false):post()
end

function mediaPlayPauseKey()
    systemKey("PLAY")
end

function mediaPreviousKey()
    systemKey("PREVIOUS")
end

function mediaNextKey()
    systemKey("NEXT")
end

function volumeIncKey()
    systemKey("SOUND_UP")
end

function volumeDecKey()
    systemKey("SOUND_DOWN")
end

function volumeMuteKey()
    systemKey("MUTE")
end

function brightnessIncKey()
    systemKey("BRIGHTNESS_UP")
end

function brightnessDecKey()
    systemKey("BRIGHTNESS_DOWN")
end
---
if false then
    hyper_bind_v1("p", function()
                      appName = application.frontmostApplication():name()
                      -- brishzeval("true " .. hs.pasteboard.getContents())
                      if appName == "Screen Sharing" then
                          -- Doesn't work, test with `sleep 2 ; hs -c 'hs.eventtap.keyStrokes("hi jungle")'`
                          -- hs.eventtap.keyStrokes(hs.pasteboard.getContents())
                      else
                          -- This was slow even with input-lang-get-darwin-fast, so the fault is probably with hammerspoon itself?

                          -- Warning:hs.keycode: key 'c' not found in active keymap; using ANSI-standard US keyboard layout as fallback, returning '8'
                          -- eventtap.keyStroke({"cmd"}, 'a')
                          -- doCopy()
                          eventtap.keyStroke({"cmd"}, 0)
                          eventtap.keyStroke({"cmd"}, 8)
                          local res
                          -- res = brishzeval2(("lang-toggle %q"):format(hs.pasteboard.getContents())) -- got into deadlocks since we hammerspoon in getting and setting the input lang now
                          if hs.keycodes.currentSourceID() == inputEnglish then
                              res = brishzeval2(("ecn %q | en2per"):format(hs.pasteboard.getContents()))
                              langSetPer()
                          else
                              res = brishzeval2(("ecn %q | per2en"):format(hs.pasteboard.getContents()))
                              langSetEn()
                          end
                          hs.eventtap.keyStrokes(tostring(res))
                      end
    end)
end
---
function chis()
    -- https://www.hammerspoon.org/docs/hs.chooser.html
    -- @todo it'd probably be better if we put the URLs as `subtext`, and their titles as `text`. hs.chooser has this better than fzf.
    local tab = nil
    c = hs.chooser.new(function(x)
            if tab then tab:delete() end
            if not x then return end
            brishzeval2bg(("chis_clean %q | inargsf open"):format(x.text))
    end)
    c:placeholderText("Search history ...")
    c:width(95)
    c:rows(15)
    tab = hs.hotkey.bind('', 'tab', function()
                             local x = c:selectedRowContents()
                             if not x then
                                 return
                             end
                             brishzeval2bg(("bell-lm-mhm ; chis_clean %q | inargsf open ; "):format(x.text))
    end)
    chis_first = true
    local timer
    c:choices(function()
            local q = c:query()
            local cmd = ("chis_find %q"):format(q)
            if chis_first then
                chis_first = false
                cmd = "deus " .. cmd
            end
            local res = brishzeval2(cmd)
            local out = {}
            for l in res:gmatch("([^\r\n]+)\r?\n?") do
                table.insert(out, {["text"] = l})
                -- table.insert(out, {["text"] = hs.styledtext.ansi(l)}) -- so slow it's completely broken
            end
            return out
    end)
    c:queryChangedCallback(function(query)
            if timer and timer:running() then
                timer:stop()
            end
            timer = hs.timer.doAfter(0.2, function() c:refreshChoicesCallback() end)
    end)
    c:show()
end
-- hyper_bind_v1("o", chis)
--
function ntagFinder()
    -- allows you to add (use enter or tab) or remove (shift+tab) tags from the selected files in Finder.
    ---
    local timer
    local tab = nil
    local antitab = nil
    c = hs.chooser.new(function(x)
            if tab then tab:delete() end
            if antitab then antitab:delete() end
            if not x then return end
            brishzeval2_out(("ntag-finder-sel-add %q"):format(x.text))
    end)
    c:placeholderText("ntag ...")
    -- c:width(95)
    c:rows(11)
    tab = hs.hotkey.bind('', 'tab', function()
                             local x = c:selectedRowContents()
                             if not x then
                                 return
                             end
                             -- brishzeval2bg(("ntag-finder-sel-add %q ; bell-lm-mhm"):format(x.text))
                             brishzeval2bg(("bell-lm-mhm ; ntag-finder-sel-add %q ; "):format(x.text))
    end)
    antitab = hs.hotkey.bind('shift', 'tab', function()
                                 local x = c:selectedRowContents()
                                 if not x then
                                     return
                                 end
                                 -- brishzeval2bg(("ntag-finder-sel-rm %q ; bell-pp-piece"):format(x.text))
                                 brishzeval2bg(("bell-pp-piece ; ntag-finder-sel-rm %q ; "):format(x.text))
    end)
    c:choices(function()
            local q = c:query()
            local cmd = ("ntag-select %q"):format(q)
            local res = brishzeval2(cmd)
            local out = {}
            for l in res:gmatch("([^\r\n]+)\r?\n?") do
                -- @upstreambug https://github.com/Hammerspoon/hammerspoon/issues/2574
                -- table.insert(out, {["text"] = hs.styledtext.ansi(l, {font={size=25}})})
                table.insert(out, {["text"] = l})
            end
            return out
    end)
    c:queryChangedCallback(function(query)
            if timer and timer:running() then
                timer:stop()
            end
            timer = hs.timer.doAfter(0.0, function() c:refreshChoicesCallback() end)
    end)
    c:show()
end
hyper_bind_v2{mods={"cmd"}, key='n', pressedfn=ntagFinder}
--- * Emoji Chooser
-- Reusable function to filter choices based on space-separated regexp patterns
local function filterChoicesByPatterns(params)
    local query = params.query
    local choices = params.choices
    local filterKey = params.on or "text" -- Default to "text" if no key is provided

    local patterns = {}
    for pattern in query:gmatch("%S+") do -- Split query into space-separated patterns
        table.insert(patterns, pattern)
    end

    local filteredChoices = {}
    for _, choice in ipairs(choices) do
        local match = true
        for _, pattern in ipairs(patterns) do
            if not string.match(choice[filterKey], pattern) then

                match = false
                break
            end
        end
        if match then
            table.insert(filteredChoices, choice)
        end
    end
    return filteredChoices
end


local emojiData = {}
local function loadEmojiData()
    local filePath = os.getenv("HOME") .. "/code/misc/unicode-emoji-json/data-by-emoji.json"
    local file = io.open(filePath, "r")
    if not file then
        hs.alert.show("Emoji data file not found")
        return
    end

    local data = file:read("*a")

    file:close()
    emojiData = json.decode(data)
end
loadEmojiData()

local emojiAlert -- Reference to the alert

local emojiStyle = {
    -- Define your custom style here, similar to hyperStyle
    atScreenEdge = 1,
    fadeInDuration = 0.001,
    fadeOutDuration = 0.001,
    fillColor = { white = 1, alpha = 2 / 3 },

    radius = 24,
    strokeColor = { red = 19 / 255, green = 182 / 255, blue = 133 / 255, alpha = 1},
    strokeWidth = 16,
    textColor = { white = 0.125 },
    textSize = 48,
}

function emojiChooser()
    local selectedEmojis = {} -- Table to store selected emojis

    local function updateAlert()
        if emojiAlert then
            hs.alert.closeSpecific(emojiAlert)
        end

        if #selectedEmojis >= 1 then
            local message = table.concat(selectedEmojis)
            emojiAlert = hs.alert.show(message, emojiStyle, hs.screen.mainScreen(), 'infinite')
        end
    end

    local function clearAlert()
        if emojiAlert then
            hs.alert.closeSpecific(emojiAlert)
            emojiAlert = nil
        end
    end

    local chooser = hs.chooser.new(function(choice)
            if not choice then
                -- canceled
                return
            end

            if #selectedEmojis == 0 then
                table.insert(selectedEmojis, choice.text)
            end

            local emojiString = table.concat(selectedEmojis)
            ---
            -- hs.eventtap.keyStrokes(emojiString)
            -- =keyStrokes= cannot insert some emojis.
            ---
            copyToClipboard(emojiString)
            doPaste()
            ---

            selectedEmojis = {}
            clearAlert()
            return
    end)
    chooser:placeholderText("Choose an emoji...")

    local choices = {}
    for emoji, info in pairs(emojiData) do
        table.insert(choices, {
                         text = emoji,
                         subText = info.name
                         ---
                         -- subText = emoji,
                         -- text = info.name
                         ---
                         -- image property can be added here if you have images for emojis
        })
    end

    table.sort(choices, function(a, b) return a.subText < b.subText end)

    chooser:choices(choices)

    -- Update the chooser choices based on the query
    chooser:queryChangedCallback(function(query)
            local filteredChoices = filterChoicesByPatterns{query=query, choices=choices, on="subText"}
        chooser:choices(filteredChoices)
    end)

    local function addCurrent()
        local choice = chooser:selectedRowContents()

        if choice then
            table.insert(selectedEmojis, choice.text)
            updateAlert()
        end
    end

    local function removeLast()
        if #selectedEmojis > 0 then
            table.remove(selectedEmojis)
            updateAlert()
        end
    end

    local hotkeys = {
        shiftEnter = hs.hotkey.bind('shift', 'return', addCurrent),
        tab = hs.hotkey.bind('', 'tab', addCurrent),
        backspace = hs.hotkey.bind('shift', 'delete', removeLast),
        backspace = hs.hotkey.bind('', '\\', removeLast),
        shiftTab = hs.hotkey.bind('shift', 'tab', removeLast)
    }

    local function cleanup()
        -- hs.alert("emojiChooser: cleanup")

        for _, hk in pairs(hotkeys) do hk:delete() end
        clearAlert()
    end

    chooser:hideCallback(cleanup)

    local function main()
        chooser:show()
    end

    -- Wrap the main execution in pcall to catch any errors
    local success, err = pcall(main)

    -- If an error occurred, clean up and rethrow the error
    if not success then
        hs.alert("emojiChooser error:" .. err)

        cleanup()
        error(err)
    end

    -- Use the defer function to ensure cleanup happens on garbage collection
    -- This will act as a "finally" block
    -- local function defer(func)
    --     return setmetatable({}, { __gc = func })
    -- end
    -- local _ = defer(cleanup)
end
hyper_bind_v2{mods={}, key="a", pressedfn=emojiChooser}
---
function anycomplete()
    local timer
    local myTask = nil
    local tab = nil
    local antitab = nil

    if hs.keycodes.currentSourceID() == inputEnglish then
        eventtap.keyStroke({"shift", "alt"}, hs.keycodes.map['left'])
    else
        eventtap.keyStroke({"shift", "alt"}, hs.keycodes.map['right'])
    end

    doCopy()

    c = hs.chooser.new(function(x)
            if tab then tab:delete() end
            if antitab then antitab:delete() end
            if not x then return end

            hs.eventtap.keyStrokes(x.text)
    end)
    c:placeholderText("anycomplete ...")
    c:width(70)
    c:rows(11)
    tab = hs.hotkey.bind('', 'tab', function()
                             local x = c:selectedRowContents()
                             if not x then
                                 return
                             end
                             c:query(x.text)
                             refreshChoices()
    end)
    antitab = hs.hotkey.bind('shift', 'tab', function()
                                 local x = c:selectedRowContents()
                                 if not x then
                                     return
                                 end
                                 hs.pasteboard.setContents(x.text)
                                 c:hide()
    end)
    -- c:choices(function()
    --     local q = c:query()
    --     local cmd = ("autosuggestions-gateway %q"):format(q)
    --     print("cmd: " .. cmd)
    --     local res = brishzeval2(cmd)
    --     local out = {}
    --     for l in res:gmatch("([^\r\n]+)\r?\n?") do
    --       table.insert(out, {["text"] = l})
    --     end
    --     return out
    -- end)
    function refreshChoices()
        if myTask then
            myTask:terminate()
            myTask = nil
        end
        local q = c:query()
        local cmd = ("autosuggestions-gateway %q"):format(q)
        -- print("cmd: " .. cmd)
        myTask = hs.task.new("/usr/local/bin/brishz2.dash",
                             function(exitCode, stdOut, stdErr)
                                 local res = stdOut
                                 local out = {}
                                 for l in res:gmatch("([^\r\n]+)\r?\n?") do
                                     table.insert(out, {["text"] = l})
                                 end
                                 c:choices(out)
                             end, {cmd})
        if myTask  then
            myTask:start()
        end
    end
    c:queryChangedCallback(function(query)
            if timer and timer:running() then
                timer:stop()
            end
            -- timer = hs.timer.doAfter(0.3, function() c:refreshChoicesCallback() end)
            timer = hs.timer.doAfter(0.0, refreshChoices)
    end)
    c:query(hs.pasteboard.getContents())
    c:show()
    if hs.keycodes.currentSourceID() == inputEnglish then
        eventtap.keyStroke({}, hs.keycodes.map['right'])
    else
        eventtap.keyStroke({}, hs.keycodes.map['left'])
    end
end
hyper_bind_v1("g", anycomplete)
---
-- https://github.com/kovidgoyal/kitty/issues/45

-- hyper_bind_v1("k", function()
--   local app = hs.application.get("kitty")

--   if app then
--       if not app:mainWindow() then
--           app:selectMenuItem({"kitty", "New OS window"})
--       elseif app:isFrontmost() then
--           app:hide()
--       else
--           app:activate()
--       end
--   else
--       hs.application.launchOrFocus("kitty")
--       app = hs.application.get("kitty")
--   end

--   -- app:mainWindow():moveToUnit'[100,50,0,0]'
--   -- app:mainWindow().setShadows(false)
-- end)
---
hyper_bind_v1("i", function()
                  langSetToggle()
                  hs.alert(langGet(), 1)
                  brishzeval('input_lang_push_lang_del')
                  input_lang_push_lang = nil
end)
---
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

function toggleFocus(appName)
    local launch_p = false

    local app = hs.application.get(appName)

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
    function h_appHotkey()
        toggleFocus(o.appName)
        -- use `sleep 2 ; reval-copy frontapp-get ; fsay hi` to get this
    end

    mods = o.modifiers
    -- If mods == "hyper", use =hyper_bind_v1=:
    if mods == "hyper" or mods == hyper or not mods then
        hyper_bind_v1(o.key, h_appHotkey)
    else
        -- hs.hotkey.bind(mods, o.key, h_appHotkey)
        hs.alert("impossible 8170")
    end
end
-- @upstreamBug https://github.com/Hammerspoon/hammerspoon/issues/2879 hs.hotkey.bind cannot bind punctuation keys such as /

-- appHotkey{ key='.', appName='com.microsoft.edgemac' }
appHotkey{ key='/', appName='company.thebrowser.Browser' }
appHotkey{ key='.', appName='com.google.Chrome' }
-- appHotkey{ key='m', appName='com.google.Chrome.app.ahiigpfcghkbjfcibpojancebdfjmoop' } -- https://devdocs.io/offline ; 'm' is also set as a search engine in Chrome
-- appHotkey{ key='m', appName='com.kapeli.dashdoc' } -- dash can bind itself in its pref

appHotkey{ key='c', appName='com.microsoft.VSCodeInsiders' }

emacsAppName = 'org.gnu.Emacs'
appHotkey{ key='x', appName=emacsAppName }

appHotkey{ key='l', appName='com.tdesktop.Telegram' }
-- appHotkey{ key='\\', appName='com.tdesktop.Telegram' }

-- appHotkey{ key='b', appName='com.apple.Preview' }
-- appHotkey{ key='b', appName='zathura' }
-- appHotkey{ key='a', appName='com.adobe.Reader' }

-- appHotkey{ key='p', appName='com.jetbrains.pycharm' }
appHotkey{ key='p', appName='com.apple.Preview' }

-- appHotkey{ key=']', appName='org.jdownloader.launcher' }

appHotkey{ key='k', appName='info.sioyek.sioyek' }
appHotkey{ key='j', appName='net.sourceforge.skim-app.skim' }
-- appHotkey{ key='[', appName='info.sioyek.sioyek' }
-- appHotkey{ key=']', appName='net.sourceforge.skim-app.skim' }

appHotkey{ key='f', appName='com.apple.finder' }
-- appHotkey{ key='o', appName='com.operasoftware.Opera' }
-- appHotkey{ key='l', appName='notion.id' }
-- appHotkey{ key='\\', appName='com.apple.iCal' }
appHotkey{ key='m', appName='mpv' }
-- appHotkey{ key='/', appName='com.quora.app.Experts' }
-- appHotkey{ key='n', appName='com.appilous.Chatbot' }
appHotkey{ key='b', appName='com.parallels.desktop.console' }
appHotkey{ key='w', appName='com.microsoft.Powerpoint' }
appHotkey{ key='=', appName='com.fortinet.FortiClient' }


hyper_bind_v1("d", function()
                  brishzeval("notif-dismiss.as")
end)
---
acrobatScrollStep = 20

acrobatHotkeyDown = hs.hotkey.new({}, 'b', function()
        ---
        hs.eventtap.scrollWheel({0, -acrobatScrollStep}, {}, "line")
        ---
        -- for i = 1, 5 do -- @slow
        --   hs.eventtap.keyStroke({}, "down")
        -- end
        ---
        -- hs.eventtap.keyStroke({}, 'b') -- @todo1 @infLoop
        -- [[https://github.com/Hammerspoon/hammerspoon/discussions/3130][How do I make the keybinding be detected but not intercepted? · Discussion #3130 · Hammerspoon/hammerspoon]]
end)

acrobatHotkeyUp = hs.hotkey.new({}, 'v', function()
        hs.eventtap.scrollWheel({0, acrobatScrollStep}, {}, "line")
        ---
        -- hs.eventtap.keyStroke({}, 'v') -- @infLoop
end)

hs.window.filter.new('Acrobat Reader')
    :subscribe(hs.window.filter.windowFocused,function()
                   acrobatHotkeyDown:enable()
                   acrobatHotkeyUp:enable()
              end)
    :subscribe(hs.window.filter.windowUnfocused,function()
                   acrobatHotkeyDown:disable()
                   acrobatHotkeyUp:disable()
              end)
---
function bindWithRepeat(mods, key, pressedfn)
    if mods == "hyper" or mods == hyper or not mods then
        hyper_bind_v2{
            key=key,
            pressedfn=pressedfn,
            repeatfn=pressedfn,
        }
    else
        hs.alert("bindWithRepeat: with mods")
        hs.hotkey.bind(mods, key, pressedfn, nil, pressedfn)
    end
end
----
function pressPageUp()
    eventtap.keyStroke({}, hs.keycodes.map['pageup'])
end

bindWithRepeat(hyper, "up", pressPageUp)

bindWithRepeat(hyper, "down", function()
                   eventtap.keyStroke({}, hs.keycodes.map['pagedown'])
end)

-- You can set hyper+F5 to the dictation command in macOS settings.
hyper_bind_v1("5", function()
                  brishzeval('awaysh-fast input-volume-mute-toggle')
                  -- @needed awaysh-fast
end)

bindWithRepeat(hyper, "F1", function()
                   brishzeval('awaysh-fast brightness-dec')
end)

bindWithRepeat(hyper, "F2", function()
                   brishzeval('awaysh-fast brightness-inc')
end)

hyper_bind_v1("F6", function()
                  brishzeval('awaysh-fast focus-do-not-disturb-toggle')
end)

hyper_bind_v1("F7", function()
                  -- brishzeval('awaysh-fast hear-prev')
                  mediaPreviousKey()
end)

hyper_bind_v1("F8", function()
                  -- brishzeval('awaysh-fast hear-play-toggle')
                  mediaPlayPauseKey()
end)

hyper_bind_v1("F9", function()
                  -- brishzeval('awaysh-fast hear-next')
                  mediaNextKey()
end)

hyper_bind_v1("F10", function()
                  -- brishzeval('awaysh-fast volume-mute-toggle')
                  volumeMuteKey()
end)

function volumeInc(v, device)
    v = v or 5

    d = device or hs.audiodevice.defaultOutputDevice()
    v = d:outputVolume() + v
    if v > 100 then
        v = 100
    end
    if v < 0 then
        v = 0
    end

    d:setOutputVolume(v)

    hs.alert("vol: " .. math.floor(v + 0.5), 0.4)
end

bindWithRepeat(hyper, "F11", function()
                   ---
                   -- brishzeval('awaysh-fast volume-dec')
                   ---
                   -- volumeInc(-5)
                   volumeDecKey()
                   ---
end)

bindWithRepeat(hyper, "F12", function()
                   ---
                   -- brishzeval('awaysh-fast volume-inc')
                   ---
                   -- volumeInc(5)
                   volumeIncKey()
                   ---
end)
--
kitty_prev_app = nil
function kittyHandler()
    local app = hs.application.get("kitty")
    local win = app:focusedWindow()
    local appscreen = win:screen()
    local mousescreen = hs.mouse.getCurrentScreen()
    local mouseSpace = hs.spaces.focusedSpace()

    if app then
        if appscreen == mousescreen then
            if app:isFrontmost() then
                app:hide()
                kitty_prev_app:activate()
                -- sometimes works without this, too, but it's better to explicitly include this.
            else
                kitty_prev_app = application.frontmostApplication()

                space_res = hs.spaces.moveWindowToSpace(win, mouseSpace)
                -- https://www.hammerspoon.org/docs/hs.spaces.html#moveWindowToSpace
                -- a window can only be moved from a user space to another user space -- you cannot move the window of a full screen (or tiled) application to another space and you cannot move a window to the same space as a full screen application.
                -- @toFuture/1401/12 This merged PR solves this: https://github.com/Hammerspoon/hammerspoon/pull/3298

                -- hs.alert.show(string.format("moving to space %s: %s", mouseSpace, space_res))

                app:activate()
                app:mainWindow():moveToUnit'[100,0,0,100]'
            end
        else
            win:moveToScreen(mousescreen)

            if app:isHidden() then
                app:activate()
                app:mainWindow():moveToUnit'[100,0,0,100]'
            end

        end
    end

    -- app:mainWidnow().setShadows(false)
end

hyper_bind_v1('z', kittyHandler)
hs.hotkey.bind({}, 'F12', kittyHandler)
---
function pasteBlockified()
    -- Get the clipboard content
    local clipboardContent = hs.pasteboard.getContents()
    if clipboardContent then
        -- Remove trailing whitespace
        clipboardContent = string.gsub(clipboardContent, "%s*$", "")
    end

    -- Check if the clipboard content contains multiple lines.
    -- [[https://stackoverflow.com/questions/55586867/how-to-put-in-markdown-an-inline-code-block-that-only-contains-a-backtick-char][How to put (in markdown) an inline code block that only contains a backtick character (`) - Stack Overflow]]
    if true or clipboardContent:match("\n") then
        -- Multiple lines
        -- Update: I have made this codepath be taken even for single line inputs, as that behavior is more often wanted.
        markdownContent = "```\n" .. clipboardContent .. "\n```\n"
    else
        -- Single line
        clipboardContent = string.gsub(clipboardContent, "^%s*", "")

        if clipboardContent:match("`") then
            markdownContent = "```" .. clipboardContent .. "```"
        else
            markdownContent = "`" .. clipboardContent .. "`"
        end
    end

    -- Put the markdownContent back to clipboard
    hs.pasteboard.setContents(markdownContent)

    -- Trigger a "paste" event
    doPaste()

    -- Set the original clipboard content back to clipboard after some delay (in seconds)
    hs.timer.doAfter(0.2, function() hs.pasteboard.setContents(clipboardContent) end)
end

hyper_bind_v1(",", pasteBlockified)
---
function install()
    -- @bootstrap installs the CLI binary
    -- https://www.hammerspoon.org/docs/hs.ipc.html#cliInstall
    -- This needs some dirs to be user-writable (see the docs), so using `ln -s /Applications/Hammerspoon.app/Contents/Frameworks/hs/hs ~/bin/` directly is better,
    hs.ipc.cliUninstall()
    res = hs.ipc.cliInstall()
    -- res = hs.ipc.cliInstall('/Users/evar/bin', false)
    -- brishzeval(string.format("echo hs cli result: %s", res))
end
-- install()
---
function reloadConfig(files)
    doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
    end
end
myWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
brishzeval("bell-lm-eternalhappiness")
--- @end
