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
