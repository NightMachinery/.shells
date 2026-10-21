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

    local prefixes = { ["?"]="google", [".a"]="amazon", [".k"]="keepa", [".c"]="chatgpt",
        [".g"]="gemini", [".l"]="claude", [".s"]="semanticscholar",
        [".gh"]="github", [".y"]="youtube", [".w"]="wikipedia",
        [".r"]="reddit", [".gs"]="googlescholar", [".ni"]="ninisite" }

    function Q.magic(query)
        if not config.magic or type(query)~="string" then return nil end
        -- Anchor before any normalization. Only an ASCII space is a separator.
        local token, body = query:match("^(%S+) (.*)$")
        return prefixes[token], body
    end

    function Q.url(query, destination)
        if not query or not query:find("%S") then return nil end
        if destination=="gemini" then return "https://gemini.google.com/app" end
        local bases = {
            google="https://www.google.com/search?q=",
            amazon="https://www.amazon.de/s?k=",
            semanticscholar="https://www.semanticscholar.org/search?sort=relevance&q=",
            github="https://github.com/search?type=repositories&q=",
            youtube="https://www.youtube.com/results?search_query=",
            wikipedia="https://en.wikipedia.org/w/index.php?title=Special:Search&search=",
            reddit="https://www.google.com/search?q=site%3Areddit.com%20",
            googlescholar="https://scholar.google.com/scholar?q=",
            ninisite="https://www.google.com/search?q=site%3Aninisite.com%20",
            chatgpt="https://chatgpt.com/?q=",
            keepa="https://keepa.com/#!search/3-",
            claude="https://claude.ai/new?q=",
        }
        local base=bases[destination]
        return base and base .. encode(query) or nil
    end

    function Q.submit(destination, query)
        query = query or chooser:query()
        local url = Q.url(query, destination)
        if not url or Q.stopped then return false end
        if chooser:isVisible() then chooser:cancel() end
        -- Exit the event callback and let chooser focus restoration finish.
        Q.pending = hs.timer.doAfter(0, function()
            Q.pending = nil
            if not Q.stopped then
                if destination=="chatgpt" then config.engine.openChatGPT(url, query)
                elseif destination=="gemini" then config.engine.openGemini(query)
                else config.engine.openURL(url) end
            end
        end)
        return true
    end

    function Q.accept(choice)
        if not choice or choice.kind or choice.url then return false end
        -- enableDefaultForQuery supplies {text=query} when no result remains.
        local destination, query=Q.magic(choice.text)
        Q.submit(destination or "google", destination and query or choice.text)
        return true
    end

    function Q.key(code, flags)
        if not chooser:isVisible() or (code ~= 36 and code ~= 76) then return false end
        if flags.alt or flags.ctrl or (flags.cmd and flags.shift) then return false end
        if flags.cmd then Q.submit("chatgpt"); return true end
        if flags.shift then Q.submit("google"); return true end
        local destination, query=Q.magic(chooser:query())
        if destination then Q.submit(destination, query); return true end
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
