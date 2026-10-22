-- Serialized native scripting plus optional stable-tab DOM submission.
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

    E.dom=dofile(nightdir .. "/hammerspoon/brave/dom-submit.lua")({engine=E})
    E.nativeQueue={};E.submissions={}
    local pumpNative
    pumpNative=function()
        if E.stopped or E.nativeActive or #E.nativeQueue==0 then return end
        local job=table.remove(E.nativeQueue,1);E.nativeActive=true
        local function finish(result)
            E.nativeActive=false
            E.lastSubmission={status=result.status,destination=job.request.destination,route="native"}
            E.submissions[job.opened.tabId]=E.lastSubmission
            pumpNative()
        end
        E.activeState(function(state)
            if not state.ok or not state.frontmost or state.tabId~=job.batch.anchor or not config.braveInFront() then
                config.unavailable("Native submission needs the original browser focus; query left in its tab")
                finish({status="cancelled"});return
            end
            local function submit()
                job.batch.anchor=job.opened.tabId
                local submitters={chatgpt=config.submitChatGPT,claude=config.submitClaude,gemini=config.submitGemini}
                local accepted=submitters[job.request.destination](job.opened,job.request.query,finish)
                if accepted==false then finish({status="cancelled"}) end
            end
            if job.opened.tabId==state.tabId then submit()
            else E.request({action="selectTab",tabId=job.opened.tabId,requireActiveTabId=state.tabId},function(selected)
                if selected.ok then submit() else report(selected);finish({status="cancelled"}) end
            end) end
        end)
    end
    function E.openQueries(requests,options)
        options=options or {}
        E.request({action="openMany",items=requests,replace=options.replace,tabId=options.tabId},function(result)
            if not result.ok then report(result);return end
            local batch={anchor=result.anchor}
            for index,opened in ipairs(result.opened) do
                local request=requests[index]
                report(opened)
                if opened.ok and (request.destination=="chatgpt" or request.destination=="claude" or request.destination=="gemini") then
                    E.dom.submit(opened,request.destination,request.query,function(status)
                        if status.status=="native-required" then
                            E.nativeQueue[#E.nativeQueue+1]={opened=opened,request=request,batch=batch};pumpNative()
                        else
                            E.lastSubmission={status=status.status,destination=request.destination,route="javascript",reason=status.reason}
                            E.submissions[opened.tabId]=E.lastSubmission
                            if status.reason then config.unavailable(status.reason) end
                        end
                    end)
                end
            end
        end)
        return true
    end
    function E.openChatGPT(url,query,options)
        return E.openQueries({{destination="chatgpt",url=url,query=query}},options)
    end
    function E.openClaude(url,query,options)
        return E.openQueries({{destination="claude",url=url,query=query}},options)
    end
    function E.openGemini(query,options)
        return E.openQueries({{destination="gemini",url="https://gemini.google.com/app",query=query}},options)
    end
    function E.openURL(url,options)
        return E.openQueries({{destination="url",url=url}},options)
    end
    function E.openBookmark(url,options)
        options=options or {}
        E.request({action="bookmark",url=url,replace=options.replace,tabId=options.tabId},report)
        return true
    end

    function E.stop()
        E.stopped = true
        E.queue = {};E.nativeQueue={};E.nativeActive=false
        E.dom.stop()
        if E.timeout then E.timeout:stop(); E.timeout=nil end
        if E.task then E.task:terminate(); E.task=nil end
    end
    return E
end
