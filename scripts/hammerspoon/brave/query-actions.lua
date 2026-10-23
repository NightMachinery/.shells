-- Browser chooser query actions. Modified Return is captured before the
-- chooser's native Cmd+Return handler, and only while this chooser is visible.
return function(config)
    local Q = {version="1.1.0",replace=false}
    local chooser = config.chooser
    local function encode(value)
        return (value:gsub("[^A-Za-z0-9_.~-]", function(char)
            return string.format("%%%02X", string.byte(char))
        end))
    end

    local magics = {
        {"?", "google", "Google", "Web search"},
        {".a", "amazon", "Amazon Germany", "Product search"},
        {".k", "keepa", "Keepa", "Amazon Germany price search"},
        {".c", "chatgpt", "ChatGPT", "New conversation, automatically submitted"},
        {".g", "gemini", "Gemini", "Argon High, automatically submitted · Alt+Enter"},
        {".l", "claude", "Claude", "New conversation, automatically submitted · Ctrl+Enter"},
        {".s", "semanticscholar", "Semantic Scholar", "Research papers"},
        {".gs", "googlescholar", "Google Scholar", "Scholarly literature"},
        {".gh", "github", "GitHub", "Repositories"},
        {".y", "youtube", "YouTube", "Videos"},
        {".w", "wikipedia", "Wikipedia", "English encyclopedia"},
        {".r", "reddit", "Reddit", "Google with site:reddit.com"},
        {".ni", "ninisite", "Ninisite", "Google with site:ninisite.com"},
    }
    local prefixes = {}
    for _, magic in ipairs(magics) do prefixes[magic[1]]=magic[2] end

    function Q.helpChoices()
        local choices = {}
        for _, magic in ipairs(magics) do
            choices[#choices+1] = {text=magic[1] .. " · " .. magic[3],
                subText=magic[4], prefix=magic[1], kind="magic"}
        end
        return choices
    end

    local function defer(action)
        if chooser:isVisible() then chooser:cancel() end
        -- Exit the event callback and let chooser focus restoration finish.
        Q.pending = hs.timer.doAfter(0, function()
            Q.pending = nil
            if not Q.stopped then action() end
        end)
    end

    function Q.help(query)
        if not config.magic or query~=".help" or not config.showHelp or Q.stopped then return false end
        defer(config.showHelp)
        return true
    end

    function Q.magic(query)
        if not config.magic or type(query)~="string" then return nil end
        -- Anchor before any normalization. Only an ASCII space is a separator.
        local token, body = query:match("^(%S+) (.*)$")
        if prefixes[token] then return prefixes[token],body end
        if not token or token:sub(1,1)~="." then return nil end
        local destinations,seen,reconstructed={},{},""
        for part in token:gmatch("%.([^%.]+)") do
            local prefix="." .. part
            if not prefixes[prefix] then return nil end
            reconstructed=reconstructed .. prefix
            if not seen[prefixes[prefix]] then
                seen[prefixes[prefix]]=true;destinations[#destinations+1]=prefixes[prefix]
            end
        end
        if reconstructed~=token or #destinations<1 then return nil end
        return destinations,body
    end

    function Q.url(query, destination)
        if not query or not query:find("%S") then return nil end
        if destination=="gemini" then return "https://gemini.google.com/app" end
        local bases = {
            google="https://www.google.com/search?q=",
            googleweb="https://www.google.com/search?udm=14&q=",
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
        if config.enabled==false then return false end
        query=query or chooser:query()
        local destinations=type(destination)=="table" and destination or {destination}
        local requests={}
        for _,name in ipairs(destinations) do
            local url=Q.url(query,name)
            if not url or Q.stopped then return false end
            requests[#requests+1]={destination=name,url=url,query=query}
        end
        if #requests==0 then return false end
        if ChooserSearch then ChooserSearch.recordQuery(chooser) end
        local options={replace=Q.replace,tabId=Q.targetTabId}
        defer(function()
            if config.engine.openQueries then config.engine.openQueries(requests,options);return end
            -- Compatibility for simple embedders without the batch engine.
            for _,request in ipairs(requests) do
                local name=request.destination
                if name=="chatgpt" then config.engine.openChatGPT(request.url,query,options)
                elseif name=="gemini" then config.engine.openGemini(query,options)
                elseif name=="claude" then config.engine.openClaude(request.url,query,options)
                else config.engine.openURL(request.url,options) end
            end
        end)
        return true
    end

    function Q.modeLabel()
        return Q.replace and "↺ Replace" or "New tab"
    end
    function Q.toggleMode()
        Q.replace=not Q.replace
        if config.modeChanged then config.modeChanged(Q.modeLabel()) end
    end

    function Q.accept(choice)
        if config.enabled==false then return false end
        if not choice or choice.kind or choice.url then return false end
        -- enableDefaultForQuery supplies {text=query} when no result remains.
        if Q.help(choice.text) then return true end
        local destination, query=Q.magic(choice.text)
        Q.submit(destination or "google", destination and query or choice.text)
        return true
    end

    function Q.key(code, flags)
        if config.enabled==false then return false end
        if not chooser:isVisible() then return false end
        if code==48 and not (flags.cmd or flags.alt or flags.ctrl or flags.shift or flags.fn) then Q.toggleMode();return true end
        if code~=36 and code~=76 then return false end
        local modifiers=(flags.ctrl and 1 or 0)+(flags.alt and 1 or 0)
            +(flags.cmd and 1 or 0)+(flags.shift and 1 or 0)
        if modifiers==2 and flags.ctrl and flags.shift then Q.submit("googleweb");return true end
        if modifiers>1 then return false end
        if flags.ctrl then Q.submit("claude"); return true end
        if flags.alt then Q.submit("gemini"); return true end
        if flags.cmd then Q.submit("chatgpt"); return true end
        if flags.shift then Q.submit("google"); return true end
        if Q.help(chooser:query()) then return true end
        local destination, query=Q.magic(chooser:query())
        if destination then Q.submit(destination, query); return true end
        local first = chooser:selectedRowContents(1)
        if config.waitForResults and (not first or (not first.kind and not first.url)) and config.waitForResults() then return true end
        if not first or not first.text then Q.submit("google"); return true end
        return false
    end

    Q.tap = hs.eventtap.new({hs.eventtap.event.types.keyDown}, function(event)
        return Q.key(event:getKeyCode(), event:getFlags())
    end)
    chooser:enableDefaultForQuery(config.enabled~=false)
    chooser:showCallback(function() if not Q.stopped and config.enabled~=false then Q.tap:start() end end)
    chooser:hideCallback(function() Q.tap:stop() end)

    function Q.stop()
        Q.stopped = true
        Q.tap:stop()
        if Q.pending then Q.pending:stop(); Q.pending=nil end
    end
    return Q
end
