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
popclickInit()
