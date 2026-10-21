-- Browser chooser query actions. Modified Return is captured before the
-- chooser's native Cmd+Return handler, and only while this chooser is visible.
return function(config)
    local Q = {}
    local chooser = config.chooser
    local function encode(value)
        return (value:gsub("[^A-Za-z0-9_.~-]", function(char)
            return string.format("%%%02X", string.byte(char))
        end))
    end

    function Q.url(query, destination)
        if not query or not query:find("%S") then return nil end
        local base = destination == "chatgpt" and "https://chatgpt.com/?q="
            or "https://www.google.com/search?q="
        return base .. encode(query)
    end

    function Q.submit(destination, query)
        local url = Q.url(query or chooser:query(), destination)
        if not url or Q.stopped then return false end
        if chooser:isVisible() then chooser:cancel() end
        -- Exit the event callback and let chooser focus restoration finish.
        Q.pending = hs.timer.doAfter(0, function()
            Q.pending = nil
            if not Q.stopped then config.engine.openURL(url) end
        end)
        return true
    end

    function Q.accept(choice)
        if not choice or choice.kind or choice.url then return false end
        -- enableDefaultForQuery supplies {text=query} when no result remains.
        Q.submit("google", choice.text)
        return true
    end

    function Q.key(code, flags)
        if not chooser:isVisible() or (code ~= 36 and code ~= 76) then return false end
        if flags.alt or flags.ctrl or (flags.cmd and flags.shift) then return false end
        if flags.cmd then Q.submit("chatgpt"); return true end
        if flags.shift then Q.submit("google"); return true end
        local first = chooser:selectedRowContents(1)
        if not first or not first.text then Q.submit("google"); return true end
        return false
    end

    Q.tap = hs.eventtap.new({hs.eventtap.event.types.keyDown}, function(event)
        return Q.key(event:getKeyCode(), event:getFlags())
    end)
    chooser:enableDefaultForQuery(true)
    chooser:showCallback(function() if not Q.stopped then Q.tap:start() end end)
    chooser:hideCallback(function() Q.tap:stop() end)

    function Q.stop()
        Q.stopped = true
        Q.tap:stop()
        if Q.pending then Q.pending:stop(); Q.pending=nil end
    end
    return Q
end
