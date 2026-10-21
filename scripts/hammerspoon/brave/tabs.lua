-- Serialized native scripting for tab-strip navigation. No page JavaScript.
return function(config)
    local E = {queue = {}}
    local script = nightdir .. "/hammerspoon/brave/tabs.js"

    local function pump()
        if E.stopped or E.task or #E.queue == 0 then return end
        local job = table.remove(E.queue, 1)
        local done = false
        local function finish(result)
            if done then return end
            done = true
            if E.timeout then E.timeout:stop(); E.timeout = nil end
            E.task = nil
            if not E.stopped then
                job.callback(result)
                pump()
            end
        end
        local task = hs.task.new("/usr/bin/osascript", function(code, out)
            local ok, result = pcall(hs.json.decode, out or "")
            finish(code == 0 and ok and type(result) == "table" and result or {ok=false,error="Browser automation failed"})
        end, {"-l", "JavaScript", script})
        if not task then finish({ok=false,error="Could not start browser automation"}); return end
        E.task = task
        task:setInput(hs.json.encode(job.request))
        E.timeout = hs.timer.doAfter(4, function()
            task:terminate()
            finish({ok=false,error="Browser automation timed out; action was not retried"})
        end)
        if not task:start() then finish({ok=false,error="Could not start browser automation"}) end
    end

    function E.request(request, callback)
        E.queue[#E.queue+1] = {request=request,callback=callback}
        pump()
    end

    local function report(result)
        E.lastResult = result
        if result.ok then config.clearError() else config.unavailable(result.error) end
    end

    function E.step(direction)
        if not config.braveInFront() then return false end
        E.request({action="step",direction=direction}, report)
        return true
    end

    function E.listTabs(callback)
        E.request({action="list"}, callback)
    end

    function E.selectTab(tabId)
        E.request({action="selectTab",tabId=tabId}, report)
    end

    function E.activeState(callback)
        E.request({action="activeState"}, callback)
    end

    function E.openChatGPT(url, query)
        E.request({action="openURL",url=url}, function(result)
            report(result)
            if result.ok then config.submitChatGPT(result, query) end
        end)
        return true
    end

    function E.openGemini(query)
        E.request({action="openURL",url="https://gemini.google.com/app"}, function(result)
            report(result)
            if result.ok then config.submitGemini(result, query) end
        end)
        return true
    end

    function E.openURL(url)
        E.request({action="openURL",url=url}, report)
        return true
    end

    function E.openBookmark(url)
        E.request({action="bookmark",url=url}, report)
        return true
    end

    function E.stop()
        E.stopped = true
        E.queue = {}
        if E.timeout then E.timeout:stop(); E.timeout=nil end
        if E.task then E.task:terminate(); E.task=nil end
    end
    return E
end
