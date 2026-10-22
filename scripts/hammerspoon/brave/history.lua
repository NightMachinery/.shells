-- A managed Go service owns snapshots/indexing; Lua receives bounded matches.
return function(config)
    local H={version="3.0.0",waiting={},requests={}}
    local stateFile=(config and config.stateFile) or os.getenv("HOME") .. "/.local/state/omni-history/server.json"
    local function endpoint()
        local attr=hs.fs.symlinkAttributes(stateFile)
        local parent=hs.fs.symlinkAttributes(stateFile:match("^(.*)/[^/]+$"))
        if not attr or attr.mode~="file" or attr.permissions~="rw-------" or attr.size>4096 or
            not parent or parent.mode~="directory" or parent.permissions~="rwx------" then
            H.endpoint=nil;H.error="Browser history service unavailable; run omni-history-boot";return
        end
        local stamp=table.concat({attr.ino,attr.modification,attr.change,attr.size},":")
        if H.endpoint and H.stateStamp==stamp then return H.endpoint end
        local file=io.open(stateFile,"r")
        if not file then H.endpoint=nil;return end
        local raw=file:read("*a");file:close()
        local ok,value=pcall(hs.json.decode,raw)
        if not ok or type(value)~="table" or type(value.password)~="string" or #value.password==0 or
            type(value.url)~="string" or not value.url:match("^http://127%.0%.0%.1:%d+/search$") then
            H.endpoint=nil;H.error="Invalid history service discovery";return
        end
        H.endpoint=value;H.stateStamp=stamp;H.error=nil
        return value
    end
    function H.start()
        if not H.stopped then endpoint() end
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
            local active=endpoint()
            if not active then
                finish({ok=false,error=H.error or "Browser history helper is starting"});return
            end
            local body=hs.json.encode({query=query,aliases=options.aliases or {},exclude=options.exclude or {}})
            hs.http.asyncPost(active.url,body,{["Content-Type"]="application/json",
                Authorization="Basic " .. hs.base64.encode("omni:" .. active.password)},function(status,raw)
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
        send()
    end
    function H.stop()
        H.stopped=true;H.waiting={}
        for request in pairs(H.requests) do
            request.done=true
            if request.timeout then request.timeout:stop();request.timeout=nil end
            if request.retry then request.retry:stop();request.retry=nil end
        end
        H.requests={}
        H.endpoint=nil;H.stateStamp=nil
    end
    return H
end
