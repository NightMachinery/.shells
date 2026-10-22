-- Detect support in the first real execution; never probe with a separate task.
return function(config)
    local D={version="1.2.0",jobs={},waiting={}}
    local file=assert(io.open(nightdir .. "/hammerspoon/brave/dom-submit.js","r"))
    local source=file:read("*a");file:close()
    function D.submit(opened,destination,query,callback)
        if D.stopped then return end
        if D.disabledUntil then
            if hs.timer.secondsSinceEpoch()<D.disabledUntil then callback({status="native-required"});return end
            D.disabledUntil=nil;D.supported=nil
        end
        if D.supported==nil and D.probing then
            D.waiting[#D.waiting+1]={opened,destination,query,callback};return
        end
        local job={deadline=hs.timer.secondsSinceEpoch()+45}
        D.jobs[job]=true
        if D.supported==nil then D.probing=job end
        local function releaseDetection()
            if D.probing~=job then return end
            D.probing=nil
            local waiting=D.waiting;D.waiting={}
            for _,args in ipairs(waiting) do D.submit(table.unpack(args)) end
        end
        local payload={nonce=hs.host.uuid(),destination=destination,query=query}
        local function finish(result)
            if not D.jobs[job] then return end
            D.jobs[job]=nil
            releaseDetection()
            if job.timer then job.timer:stop();job.timer=nil end
            D.lastResult={status=result.status,destination=destination,reason=result.reason}
            callback(result)
        end
        local check
        check=function()
            job.timer=nil
            if D.stopped or not D.jobs[job] then return end
            local started=hs.timer.absoluteTime()
            payload.initial=not job.targetSeen
            local code="(" .. source .. ")(" .. hs.json.encode(payload) .. ")"
            config.engine.request({action="dom",tabId=opened.tabId,destination=destination,initial=payload.initial,code=code},function(response)
                if D.stopped or not D.jobs[job] then return end
                D.lastCheckMs=(hs.timer.absoluteTime()-started)/1e6
                if response.jsUnavailable then
                    D.disabledUntil=hs.timer.secondsSinceEpoch()+72*60*60;D.supported=false
                    releaseDetection()
                    return finish({status="native-required"})
                end
                if not response.ok then releaseDetection();return finish({status="failed",reason=response.error}) end
                if response.jsExecuted~=false then D.supported=true;D.disabledUntil=nil;releaseDetection() end
                local result=response.result
                if type(result)~="table" or type(result.status)~="string" then return finish({status="failed",reason="Invalid DOM submission response"}) end
                if not result.navigationPending then job.targetSeen=true end
                if result.status~="waiting" then return finish(result) end
                if hs.timer.secondsSinceEpoch()>=job.deadline then return finish({status="timeout",reason="Chat composer did not become ready"}) end
                job.timer=hs.timer.doAfter(.15,check)
            end)
        end
        check()
    end
    function D.stop()
        D.stopped=true
        for job in pairs(D.jobs) do if job.timer then job.timer:stop();job.timer=nil end end
        D.jobs={};D.waiting={};D.probing=nil
    end
    return D
end
