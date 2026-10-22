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
    openGemini=function(query) opened[#opened+1]='gemini:'..query end,
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
Q=dofile("brave/query-actions.lua")({chooser=chooser,engine={openGemini=function(query) opened[#opened+1]='gemini:'..query end}})
show("whole query");assert(Q.key(36,{alt=true}));flush();assert(opened[#opened]=="gemini:whole query")
show("numpad query");assert(Q.key(76,{alt=true}));flush();assert(opened[#opened]=="gemini:numpad query")
show("no combined modifiers");assert(not Q.key(36,{alt=true,cmd=true}))
assert(not Q.key(36,{alt=true,shift=true}));assert(not Q.key(36,{alt=true,ctrl=true}))
print("browser query action tests passed")

-- Magic routing is opt-in for Omni. Bookmarks retain literal query behavior.
assert(Q.magic(".g hello")==nil)
local routed={}
local magic=dofile("brave/query-actions.lua")({chooser=chooser,magic=true,engine={
    openURL=function(url) routed[#routed+1]={url=url} end,
    openChatGPT=function(url,query) routed[#routed+1]={url=url,query=query,kind="chatgpt"} end,
    openGemini=function(query) routed[#routed+1]={query=query,kind="gemini"} end,
    openClaude=function(url,query) routed[#routed+1]={url=url,query=query,kind="claude"} end,
}})
local routes={["?"]="google",[".a"]="amazon",[".k"]="keepa",[".c"]="chatgpt",[".g"]="gemini",[".l"]="claude",
    [".s"]="semanticscholar",[".gh"]="github",[".y"]="youtube",[".w"]="wikipedia",
    [".r"]="reddit",[".gs"]="googlescholar",[".ni"]="ninisite"}
for token,destination in pairs(routes) do
    local body="  a+b & ü\nsecond line"
    local actual,text=magic.magic(token.." "..body)
    assert(actual==destination and text==body) -- only one separator is removed
    for _,query in ipairs({" "..token.." q","\t"..token.." q",token,token.."q",token.."\tq",token.."\nq","before "..token.." q"}) do
        assert(magic.magic(query)==nil,query)
    end
    show(token.." "..body) -- even a highlighted matching row cannot steal it
    assert(magic.key(36,{}));flush()
    local action=routed[#routed]
    if destination=="gemini" then assert(action.kind=="gemini" and action.query==body)
    else assert(action.url==magic.url(body,destination)) end
    if destination=="chatgpt" or destination=="claude" then assert(action.query==body and action.kind==destination) end
end
assert(magic.magic(".G query")==nil and magic.magic(".unknown query")==nil)
assert(magic.url("a+b & ü","amazon")=="https://www.amazon.de/s?k=a%2Bb%20%26%20%C3%BC")
assert(magic.url("a b","keepa")=="https://keepa.com/#!search/3-a%20b")
assert(magic.url("a b","claude")=="https://claude.ai/new?q=a%20b")
assert(magic.url("a b","gemini")=="https://gemini.google.com/app")
assert(magic.url("a b","reddit")=="https://www.google.com/search?q=site%3Areddit.com%20a%20b")
assert(magic.url("a+b & ü","ninisite")=="https://www.google.com/search?q=site%3Aninisite.com%20a%2Bb%20%26%20%C3%BC")
assert(magic.url("a b","unsupported")==nil)
show(" .g literal");assert(not magic.key(36,{})) -- leading space keeps local selection
chooser.row={};assert(magic.key(36,{}));flush()
assert(routed[#routed].url==magic.url(" .g literal","google"))
show(".g full query");assert(magic.key(36,{shift=true}));flush()
assert(routed[#routed].url==magic.url(".g full query","google"))
show(".g full query");assert(magic.key(36,{cmd=true}));flush()
assert(routed[#routed].kind=="chatgpt" and routed[#routed].query==".g full query")
show(".g full query");assert(magic.key(36,{alt=true}));flush()
assert(routed[#routed].kind=="gemini" and routed[#routed].query==".g full query")
local count=#routed
show(".g ");assert(magic.key(76,{}));flush();assert(chooser.visible and #routed==count)
show(".c  \t");assert(magic.key(36,{}));flush();assert(chooser.visible and #routed==count)
assert(magic.accept({text=".l from callback"}));flush()
assert(routed[#routed].url==magic.url("from callback","claude") and routed[#routed].kind=="claude")
magic.stop()
print("strict Omni prefix tests passed")
