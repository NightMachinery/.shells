-- Run from hammerspoon/: lua tests/browser-query-actions.lua
local pending, opened, taps = {}, {}, {}
hs = {
    timer={doAfter=function(_,fn)
        local timer={fn=fn,stop=function(self) self.stopped=true end}
        pending[#pending+1]=timer; return timer
    end},
    eventtap={event={types={keyDown=10}},new=function(_,fn)
        local tap={fn=fn,start=function(self) self.active=true end,stop=function(self) self.active=false end}
        taps[#taps+1]=tap; return tap
    end},
}
local chooser={visible=false,text=""}
function chooser:query() return self.text end
function chooser:selectedRowContents() return self.row end
function chooser:isVisible() return self.visible end
function chooser:cancel() self.visible=false; self.hideFn() end
function chooser:enableDefaultForQuery(value) self.fallback=value end
function chooser:showCallback(fn) self.showFn=fn end
function chooser:hideCallback(fn) self.hideFn=fn end
local Q=dofile("brave/query-actions.lua")({chooser=chooser,engine={
    openURL=function(url) opened[#opened+1]=url end,
    openChatGPT=function(url,query)
        assert(query=="always chat")
        opened[#opened+1]=url
    end,
}})
local function show(query) chooser.text=query; chooser.row={text="Matching row"}; chooser.visible=true; chooser.showFn() end
local function flush()
    local work=pending; pending={}
    for _,timer in ipairs(work) do if not timer.stopped then timer.fn() end end
end
assert(chooser.fallback)
assert(Q.url("  ","google")==nil)
assert(Q.url("a+b & ü/#?", "google")=="https://www.google.com/search?q=a%2Bb%20%26%20%C3%BC%2F%23%3F")
assert(not Q.key(36,{cmd=true})) -- outside chooser
show("chosen query")
assert(taps[1].active)
assert(not Q.key(36,{})) -- regular Enter keeps native row selection
assert(not Q.accept({kind="tab",tabId="1"}))
assert(not Q.accept({url="https://example.org/"}))
assert(Q.key(36,{shift=true})); assert(not chooser.visible and not taps[1].active)
assert(#opened==0); flush(); assert(opened[1]==Q.url("chosen query","google"))
show("always chat"); assert(Q.key(76,{cmd=true})); flush()
assert(opened[2]=="https://chatgpt.com/?q=always%20chat")
-- Native no-match Enter closes first, then returns a query-only choice.
show("no match"); chooser.row={}; assert(Q.key(36,{})); flush()
assert(opened[3]==Q.url("no match","google"))
show("   "); assert(Q.key(36,{shift=true})); flush(); assert(#opened==3)
show("keep native combos"); assert(not Q.key(36,{cmd=true,shift=true}))
assert(not Q.key(36,{ctrl=true,shift=true})); assert(not Q.key(20,{cmd=true}))
show("cancel pending on reload"); Q.key(36,{cmd=true}); Q.stop(); flush(); assert(#opened==3)
print("browser query action tests passed")
