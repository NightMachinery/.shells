-- MRU keeps a frozen order while Cmd is held, learns external tab selections,
-- and commits on release even if native enumeration was asynchronous.
local held=true
hs={eventtap={checkKeyboardModifiers=function()return{cmd=held}end}}
local active='a';local callback;local selected={}
local engine={activeState=function(cb)cb({ok=true,frontmost=true,tabId=active})end,
 listTabs=function(cb)callback=cb end,selectTab=function(id)selected[#selected+1]=id;active=id end}
local r=dofile('browser/recency.lua')({engine=engine,inFront=function()return true end})
for _,id in ipairs({'c','b','a'})do active=id;r.observe()end
local function deliver()
 local cb=callback;callback=nil
 cb({ok=true,activeTabId=active,tabs={{tabId='a',windowId='1'},{tabId='b',windowId='1'},{tabId='c',windowId='1'},{tabId='other-window',windowId='2'}}})
end
assert(r.cycle(1));deliver();assert(selected[1]=='b')
r.cycle(1);assert(selected[2]=='c');r.cycle(-1);assert(selected[3]=='b')
r.observe();assert(r.order[1]=='a') -- cycling must not reorder the session
r.finish();assert(r.order[1]=='b'and not r.session)
r.cycle(1);r.finish();deliver();assert(selected[4]=='a'and not r.cycling and r.order[1]=='a')
held=false;r.cycle(1);deliver();assert(not r.cycling)
print('Native-independent tab MRU: event learning, frozen cycles, window scope and asynchronous release passed')

local before=#selected
r.finish();r.finish();assert(#selected==before)
print('Idle modifier releases do not trigger tab reads or selections')
