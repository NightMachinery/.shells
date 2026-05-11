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
-- backspace_symbol = "⌫"
backspace_symbol = "⎌" -- good, as it is small enough not to disturb the grid, also we can see behind it better
-- backspace_symbol = "␈" -- bad, big

fn_symbol = "ϟ"
-- fn_symbol = "★"
-- fn_symbol = "🌟"
-- fn_symbol = "☆"
-- fn_symbol = "✯"

tab_symbol = "↪"
-- tab_symbol = "↪"
-- tab_symbol = "⇥"
-- tab_symbol = "⎞"

-- I_symbol = "ℐ"
-- I_symbol = "ℑ"
-- I_symbol = "𐒻"
-- I_symbol = "𐒻"
I_symbol = "𝐈"
-- I_symbol = "I"

function strToList(str)
    local list = {}
    for i = 1, #str do
        table.insert(list, str:sub(i, i))
    end
    return list
end

function flatten1(list_of_lists)
    local flat_list = {}
    for _, sublist in ipairs(list_of_lists) do
        for _, item in ipairs(sublist) do
            table.insert(flat_list, item)
        end
    end
    return flat_list
end

function generateTwoLetterCombinations()
    -- @todo Change the order of lettersFirstList so that hard-to-press keys are in positions usually not needed.
    local lettersFirstList = flatten1({
            strToList("r"),
            {
                backspace_symbol,
                fn_symbol,
                -- "↑", "↓", "←", "→"
                -- We are using the arrow keys to move the canvas grid, so we can't use them here.
            },
            strToList("abcdeijklmnopqsuvwxyz/.,;'[]\\=-09"),
            {
                tab_symbol,
            },
            strToList("|\":?<>LKJNM{}POIUB"),
            strToList("hfgt"),
            strToList("8HZXCASDFV"),
            strToList("`1234567"),
            -- 654321
    })

    local lettersSecondList = flatten1({
            {
                backspace_symbol,
                fn_symbol,
                "↑", "↓", "←", "→",
            },
            strToList("abcijklmnopsuvxz/.,;'[]\\=-0"),
            {
                tab_symbol,
            },
            -- strToList("qwertydfgh"),
    })

    local combinations = {}
    for i, first in ipairs(lettersFirstList) do

        for j, second in ipairs(lettersSecondList) do
            local combo = {text=(first .. second), first=first, second=second}
            table.insert(combinations, combo)
        end
    end
    return combinations
end

avy_combinations = generateTwoLetterCombinations()
-- No need to recompute this every time

function charToKeybinding(char)
    local mods = {}
    local char_base = char
    local shift_map = {
        ["~"] = "`", ["!"] = "1", ["@"] = "2", ["#"] = "3",
        ["$"] = "4", ["%"] = "5", ["^"] = "6", ["&"] = "7",

        ["*"] = "8", ["("] = "9", [")"] = "0", ["_"] = "-",

        ["+"] = "=", ["<"] = ",", [">"] = ".", [":"] = ";",
        ["\""] = "'", ["?"] = "/", ["{"] = "[", ["}"] = "]",
        ["|"] = "\\",
    }
    local icon_map = {
        ["↑"] = "up",
        ["↓"] = "down",
        ["←"] = "left",
        ["→"] = "right",
        [backspace_symbol] = "delete",
        [fn_symbol] = "F18",
        [tab_symbol] = "tab",
        -- We can add more stuff. Even the `fn` key can be bound.
    }

    -- Check if the character is an arrow key
    if icon_map[char] then
        char_base = icon_map[char]
        -- Check if the character is uppercase or a special character
    elseif char == I_symbol then
        char_base = "i"
        mods = {"shift"}
    elseif char:match("%u") or shift_map[char] then
        mods = {"shift"}
        -- If it's a special character, get its base version
        if shift_map[char] then
            char_base = shift_map[char]
        else
            -- If it's an uppercase letter, convert it to lowercase

            char_base = char:lower()

        end
    elseif char:match("%l") then
    else
        -- print("charToKeybinding: unknown: " .. char)
    end

    return {mods=mods, key=char_base}
end

function screenPositionAvy(params)
    -- @todo2 Cache the canvas object etc. to make this function faster
    ---
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
        overlayWidth = 33,
        fontSize = 18,
        fontName = "Fira Code Retina",
        fontAlignment = "center",

        -- Define the amount to move the canvas with each arrow key press
        moveAmount = 5,
    }

    -- Merge default values with the provided parameters
    for k, v in pairs(defaults) do
        if params[k] == nil then
            params[k] = v
        end
    end
    ---
    local moveAmount = params.moveAmount
    local callback = params.callback
    local fgColor = hs.drawing.color.asRGB(params.fgColor)
    local bgColor = hs.drawing.color.asRGB(params.backgroundColor)
    local secondModalBgColor = hs.drawing.color.asRGB(params.secondModalBgColor)
    local secondModalFgColor = hs.drawing.color.asRGB(params.secondModalFgColor)

    local combinations = avy_combinations
    -- local combinations = generateTwoLetterCombinations()

    local screenWidth = hs.screen.mainScreen():frame().w
    local screenHeight = hs.screen.mainScreen():frame().h

    local overlayHeight = params.overlayHeight
    local overlayWidth = params.overlayWidth

    local y_overlay_offset = 17
    local overlayHeightOffset = 10
    -- These two depend on the font in question.

    local fontName = params.fontName
    local fontSize = params.fontSize
    local fontAlignment = params.fontAlignment

    local canvas_initial_x = -overlayWidth
    local canvas_initial_y = -overlayHeight
    local canvas_width = screenWidth - 2 * canvas_initial_x
    local canvas_height = screenHeight - 2 * canvas_initial_y + 30
    local canvas = hs.canvas.new({x = canvas_initial_x, y = canvas_initial_y, w = canvas_width, h = canvas_height})
    local index = 1

    -- Variable to store the current offset of the canvas
    local canvasOffset = { x = canvas_initial_x, y = canvas_initial_y }

    -- Calculate the number of overlays to fit the screen
    local columns = math.ceil(canvas_width / overlayWidth)
    local rows = math.ceil(canvas_height / overlayHeight) + 0

    local function cleanup()
        canvas:hide()
        canvas:delete()
    end

    local function handleKeyPress(modal, x, y, adjust_p)
        if adjust_p == nil then
            adjust_p = true
        end

        local adjustedX = x
        local adjustedY = y
        if adjust_p then
            -- Adjust the position by the current canvas offset
            adjustedX = x + canvasOffset.x
            adjustedY = y + canvasOffset.y
        end

        modal:exit()
        cleanup()

        callback(adjustedX, adjustedY) -- Call the callback function with the adjusted position
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
        local combo_obj = combinations[combo_index]
        local text = combo_obj.text

        local newStyledText = hs.styledtext.new(
            text,
            {

                color = color,
                paragraphStyle = { alignment = fontAlignment },
                font = { name = fontName, size = fontSize },
        })
        canvas[element_index].text = newStyledText
    end

    -- Create text elements for each position and add them to the canvas
    local text_elements_by_first_char = {}
    for row = 0, rows - 1 do
        for col = 0, columns - 1 do
            local combo_obj = combinations[index]
            if not combo_obj then
                print("screenPositionAvy: insufficient combinations")
                break
            end

            local combo = combo_obj.text

            if combo then
                local x = col * overlayWidth + overlayWidth / 2
                local y = row * overlayHeight + overlayHeight / 2
                local x_canvas = x - overlayWidth / 2
                local y_canvas = y - overlayHeight / 2

                local styledText = hs.styledtext.new(
                    combo,
                    {
                        color = fgColor,
                        paragraphStyle = { alignment = fontAlignment },
                        font = { name = fontName, size = fontSize },
                })
                canvas:insertElement({

                        type = "text",
                        text = styledText,
                        frame = { x = x_canvas, y = y_canvas, w = overlayWidth, h = (overlayHeight + overlayHeightOffset) }

                })
                local canvas_index = #canvas
                local combo_index = index

                local first_char = combo_obj.first
                local second_char = combo_obj.second
                -- local first_char = combo:sub(1, 1)
                -- local second_char = combo:sub(2, 2)

                -- Store the text element index for the first character
                if not text_elements_by_first_char[first_char] then
                    text_elements_by_first_char[first_char] = {}
                end
                table.insert(text_elements_by_first_char[first_char], {canvas_index= canvas_index, combo_index = combo_index})


                if not second_modals[first_char] then
                    second_modals[first_char] = hs.hotkey.modal.new()
                    first_char_keybinding = charToKeybinding(first_char)

                    mouse_avy_modality:bind(
                        first_char_keybinding.mods,
                        first_char_keybinding.key,
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


                second_char_keybinding = charToKeybinding(second_char)
                second_modals[first_char]:bind(
                    second_char_keybinding.mods,
                    second_char_keybinding.key,
                    (function()
                            handleKeyPress(second_modals[first_char], x, y_canvas + y_overlay_offset)
                                              end))

                index = index + 1
            end
        end
    end

    ---
    -- Functions to move the canvas
    -- Function to move the canvas
    local function moveCanvas(dx, dy)
        canvasOffset.x = canvasOffset.x + dx
        canvasOffset.y = canvasOffset.y + dy

        local currentPos = canvas:frame()

        canvas:frame({
                x = currentPos.x + dx,
                y = currentPos.y + dy,
                w = currentPos.w,
                h = currentPos.h
        })
    end

    local function moveCanvasUp()
        moveCanvas(0, -moveAmount)
    end

    local function moveCanvasDown()
        moveCanvas(0, moveAmount)
    end

    local function moveCanvasLeft()
        moveCanvas(-moveAmount, 0)
    end

    local function moveCanvasRight()
        moveCanvas(moveAmount, 0)
    end
    -- Bind arrow keys to move the canvas with repeat functionality

    mouse_avy_modality:bind({}, "up", moveCanvasUp, nil, moveCanvasUp)
    mouse_avy_modality:bind({}, "down", moveCanvasDown, nil, moveCanvasDown)

    mouse_avy_modality:bind({}, "left", moveCanvasLeft, nil, moveCanvasLeft)
    mouse_avy_modality:bind({}, "right", moveCanvasRight, nil, moveCanvasRight)
    ---

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

            handleKeyPress(mouse_avy_modality, x, y, false)
    end)

    -- Enter the modal state
    mouse_avy_modality:enter()

    -- Show the canvas
    canvas:show()
    canvas:bringToFront(true)
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
    binder{mods={}, key="return", auto_trigger_p=false, pressedfn=(function()
                   prevFocusedElement = nil -- We do not want the focus to return to a SecureInput element.
                   hyper_triggered()

                   leftClickAvy()
               end),}
    -- binder{mods={"shift"}, key="return", pressedfn=leftDrag,}
    binder{mods={}, key="]", pressedfn=rightClickAvy,}
    binder{mods={}, key="o", pressedfn=textSelectAvyV2,}

    -- binder{mods={}, key="'", pressedfn=leftClick} -- overridden by STT
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

    local fg_alpha = 1.0
    -- local fg_alpha = 0.5

    -- local overlayHeight = 20
    local overlayWidth = 50

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

                    fgColor = { red = 0, green = 0, blue = 0, alpha = fg_alpha },
                    secondModalFgColor = { red = 0.8, green = 0.0, blue = 0.0, alpha = fg_alpha },

                    overlayWidth = overlayWidth,
            })
        end
    end

    -- Start capturing the first point
    screenPositionAvy({
            callback = capturePoint,
            backgroundColor = { red=255/255, green=215/255, blue=0 , alpha = alpha },
            secondModalBgColor = { red = 0.9, green = 1.0, blue = 1.0, alpha = alpha },

            fgColor = { red = 0, green = 0, blue = 0, alpha = fg_alpha },
            secondModalFgColor = { red = 0.8, green = 0.0, blue = 0.0, alpha = fg_alpha },

            overlayWidth = overlayWidth,
    })
end

hyper_bind_v2{key="s", pressedfn=screenshotAvy}

function screenshotAll()
    hs.task.new("/usr/sbin/screencapture", nil, {"-c"}):start()
end
function hScreenshotAll()
    hyper_exit()
    hs.timer.usleep(300000)
    -- to wait for the hyper alert and the OS sticky modifier alerts to fade out

    screenshotAll()
end
hyper_bind_v2{mods={}, key="3", pressedfn=screenshotAll}
hyper_bind_v2{mods={"shift"}, key="s", pressedfn=hScreenshotAll}
