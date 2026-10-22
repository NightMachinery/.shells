-- A managed Go service owns snapshots/indexing; Lua receives bounded matches.
return function(config)
    local H={version="2.0.1",waiting={},requests={}}
    local source=nightdir .. "/golang/omni-history"
    local function finishWaiting(ok)
        local waiting=H.waiting;H.waiting={}
        for _,callback in ipairs(waiting) do callback(ok) end
    end
    local function fail(message)
        H.error=message
        finishWaiting(false)
    end
    local function launch(binary)
        if H.stopped then return end
        local frame=""
        H.task=hs.task.new(binary,function()
            H.task=nil;H.endpoint=nil
            if not H.stopped then fail("Browser history helper stopped") end
        end,function(_,out)
            if H.stopped then return false end
            frame=frame .. (out or "")
            if #frame>4096 then fail("Invalid history helper startup");return false end
            if frame:find("\n",1,true) then
                local ok,value=pcall(hs.json.decode,frame)
                if not ok or type(value)~="table" or type(value.token)~="string" or
                    type(value.url)~="string" or not value.url:match("^http://127%.0%.0%.1:%d+/search$") then
                    fail("Invalid history helper startup");return false
                end
                H.endpoint=value
                finishWaiting(true)
                return false
            end
            return true
        end,{"-parent",tostring(hs.processInfo.processID)})
        if not H.task or not H.task:start() then fail("Could not start browser history helper") end
    end
    function H.start()
        if H.started or H.stopped then return end
        H.started=true
        local content={}
        for _,name in ipairs({"go.mod","main.go","sqlite.go"}) do
            local file=io.open(source .. "/" .. name,"r")
            if not file then fail("Browser history helper source is missing");return end
            content[#content+1]=file:read("*a");file:close()
        end
        local cache=os.getenv("HOME") .. "/Library/Caches/hammerspoon"
        hs.fs.mkdir(cache);cache=cache .. "/omni-history";hs.fs.mkdir(cache)
        local binary=cache .. "/" .. hs.hash.SHA256(table.concat(content,"\n")):sub(1,16)
        if hs.fs.attributes(binary,"mode")=="file" then launch(binary);return end
        local compiler
        for _,path in ipairs({"/opt/homebrew/bin/go","/usr/local/bin/go","/usr/local/go/bin/go"}) do
            if hs.fs.attributes(path,"mode")=="file" then compiler=path;break end
        end
        if not compiler then fail("Go is required to build the browser history helper");return end
        H.build=hs.task.new(compiler,function(code)
            H.build=nil
            if H.buildTimer then H.buildTimer:stop();H.buildTimer=nil end
            if H.stopped then return end
            if code~=0 then fail("Browser history helper build failed");return end
            launch(binary)
        end,{"build","-trimpath","-o",binary,"."})
        if not H.build then fail("Could not build browser history helper");return end
        H.build:setWorkingDirectory(source)
        H.buildTimer=hs.timer.doAfter(60,function()
            H.buildTimer=nil
            if H.build then H.build:terminate() end
            fail("Browser history helper build timed out")
        end)
        if not H.build:start() then fail("Could not build browser history helper") end
    end
    function H.search(query,options,callback)
        if H.stopped then return end
        local request={started=hs.timer.absoluteTime()}
        H.requests[request]=true
        local function finish(value)
            if request.done then return end
            request.done=true;H.requests[request]=nil
            if request.timeout then request.timeout:stop();request.timeout=nil end
            if request.retry then request.retry:stop();request.retry=nil end
            if not H.stopped then callback(value) end
        end
        local function send()
            if H.stopped or request.done then return end
            if not H.endpoint then
                finish({ok=false,error=H.error or "Browser history helper is starting"});return
            end
            local body=hs.json.encode({query=query,aliases=options.aliases or {},exclude=options.exclude or {}})
            hs.http.asyncPost(H.endpoint.url,body,{["Content-Type"]="application/json",
                Authorization="Bearer " .. H.endpoint.token},function(status,raw)
                if H.stopped or request.done then return end
                local ok,result=pcall(hs.json.decode,raw or "")
                if status~=200 or not ok or type(result)~="table" or not result.ok then
                    finish({ok=false,error="Browser history search unavailable"});return
                end
                if not result.ready then
                    request.retry=hs.timer.doAfter(.1,send)
                else
                    H.lastResult={ok=true,total=result.total,returned=#result.history,revision=result.revision,
                        elapsed=(hs.timer.absoluteTime()-request.started)/1e9}
                    finish(result)
                end
            end)
        end
        request.timeout=hs.timer.doAfter(3,function()finish({ok=false,error="Browser history search timed out"}) end)
        if H.endpoint then send()
        elseif H.error then finish({ok=false,error=H.error})
        else H.waiting[#H.waiting+1]=function(ok)
            if request.done then return end
            if ok then send() else finish({ok=false,error=H.error}) end
        end;H.start() end
    end
    function H.stop()
        H.stopped=true;H.waiting={}
        for request in pairs(H.requests) do
            request.done=true
            if request.timeout then request.timeout:stop();request.timeout=nil end
            if request.retry then request.retry:stop();request.retry=nil end
        end
        H.requests={}
        if H.buildTimer then H.buildTimer:stop();H.buildTimer=nil end
        if H.build then H.build:terminate();H.build=nil end
        if H.task then H.task:terminate();H.task=nil end
    end
    return H
end
