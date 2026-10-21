-- Run from hammerspoon/: lua tests/chatgpt-submit.lua
local factory=dofile("brave/chatgpt-submit.lua")
local function node(attributes)
    return {attributeValue=function(_,key) return attributes[key] end,
        setAttributeValue=function(_,key,value) attributes[key]=value end,attributes=attributes}
end
local function setup(options)
    options=options or {}
    local state={sends=0,queue={},callbacks={},now=0,lookups=0,front=true}
    local input=node({AXRole="AXTextArea",AXValue=options.text or "query"})
    local send=node({AXRole="AXButton",AXTitle="Send",AXEnabled=not options.disabled})
    local web=node({AXRole=options.outsideWeb and "AXGroup" or "AXWebArea",AXChildren={input,send}})
    local root=node({AXRole="AXWindow",AXChildren={web}})
    hs={timer={secondsSinceEpoch=function() return state.now end,doAfter=function(delay,fn)
        local timer={fn=fn,delay=delay,stop=function(self) self.stopped=true end}
        state.queue[#state.queue+1]=timer; return timer
    end},eventtap={keyStroke=function(mods,key)
        assert(#mods==0 and key=="return"); state.sends=state.sends+1
    end}}
    local C=factory({
        braveInFront=function() if state.front then return {} end end,
        nativeWindow=function() return root end,
        unavailable=function(message) state.warning=message end,
        engine={activeState=function(callback)
            state.lookups=state.lookups+1
            local result={ok=true,frontmost=true,tabId="123",url=options.url or "https://chatgpt.com/?q=query"}
            if options.changeTab and state.lookups>1 then result.tabId="456" end
            if options.deferred then state.callbacks[#state.callbacks+1]=function() callback(result) end
            else callback(result) end
        end},
    })
    function state.tick()
        local queue=state.queue; state.queue={}
        for _,timer in ipairs(queue) do
            if not timer.stopped then state.now=state.now+timer.delay; timer.fn() end
        end
    end
    return C,state,input,send
end
local C,s=setup(); C.submit({tabId="123"},"query"); assert(s.sends==1 and C.lastResult.status=="submitted")
s.tick(); assert(s.sends==1) -- no send retry
local input,send
C,s,input=setup({text="loading"}); C.submit({tabId="123"},"query"); assert(s.sends==0)
input.attributes.AXValue="query"; s.tick(); assert(s.sends==1)
C,s,input,send=setup({disabled=true}); C.submit({tabId="123"},"query"); assert(s.sends==0)
send.attributes.AXEnabled=true; s.tick(); assert(s.sends==1)
C,s=setup({changeTab=true}); C.submit({tabId="123"},"query"); assert(s.sends==0 and C.lastResult.status=="cancelled")
for _,url in ipairs({"https://chatgpt.com.evil.test/","https://chatgpt.com/c/existing","https://example.org/"}) do
    C,s=setup({url=url}); C.submit({tabId="123"},"query"); assert(s.sends==0 and C.lastResult.status=="cancelled")
end
C,s=setup(); s.front=false; C.submit({tabId="123"},"query"); assert(s.sends==0 and C.lastResult.status=="cancelled")
C,s=setup({text="different query"}); C.submit({tabId="123"},"query"); s.now=46; s.tick()
assert(s.sends==0 and C.lastResult.status=="timeout")
C,s=setup({outsideWeb=true}); C.submit({tabId="123"},"query"); assert(s.sends==0); C.stop(); s.tick(); assert(s.sends==0)
C,s=setup({deferred=true}); C.submit({tabId="123"},"query"); C.stop()
for _,callback in ipairs(s.callbacks) do callback() end
assert(s.sends==0) -- a reload cancels an in-flight native inspection
for _,query in ipairs({"", "  "}) do
    C,s=setup(); assert(C.submit({tabId="123"},query)==false); assert(s.sends==0)
end
C,s=setup(); assert(C.submit({},"query")==false); assert(s.sends==0)
print("ChatGPT guarded submission tests passed")
