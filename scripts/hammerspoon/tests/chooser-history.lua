-- Run from hammerspoon/: lua tests/chooser-history.lua
local saved,reads,writes={},0,0
local function copy(t)local r={}for i,v in ipairs(t or {})do r[i]=v end;return r end
hs={settings={get=function(k)reads=reads+1;return copy(saved[k])end,set=function(k,v)writes=writes+1;saved[k]=copy(v)end},chooser={}}
local originalCalls=0
hs.chooser.globalCallback=function()originalCalls=originalCalls+1 end
function hs.chooser.new(done)
 local c={value='',visible=false,done=done}
 function c:query(value)if value==nil then return self.value end;self.value=value;return self end
 function c:queryChangedCallback(fn)self.changed=fn;return self end
 function c:choices(rows)self.rows=rows;return self end
 function c:isVisible()return self.visible end
 function c:show()self.visible=true;hs.chooser.globalCallback(self,'willOpen');return self end
 function c:hide()self.visible=false;hs.chooser.globalCallback(self,'didClose')end
 function c:accept()self:hide();self.done({text='choice'})end
 function c:cancel()self:hide();self.done(nil)end
 return c
end
dofile('core/chooser-search.lua')
local S=ChooserSearch
local c=S.new(function()end,nil,{historyId='test'})
c:show();c:query('first');c:accept();c:show();c:query('second');c:accept()
c:show();c:query('second');c:accept();assert(writes==2)
c:show();c:query('cancelled');c:cancel();assert(writes==2)
c:show();c:query('draft')
local changed={};S.onQueryChanged(c,function(q)changed[#changed+1]=q end)
assert(S.historyKey(35,{alt=true}));assert(c:query()=='second')
assert(S.historyKey(35,{alt=true}));assert(c:query()=='first')
assert(S.historyKey(35,{alt=true}));assert(c:query()=='first')
assert(S.historyKey(45,{alt=true}));assert(c:query()=='second')
assert(S.historyKey(45,{alt=true}));assert(c:query()=='draft')
assert(#changed==5 and reads==1 and writes==2)
assert(not S.historyKey(35,{alt=true,cmd=true}))
-- Editing recalled input starts a fresh traversal with the edited draft.
S.historyKey(35,{alt=true});c:query('edited');S.historyKey(35,{alt=true});S.historyKey(45,{alt=true});assert(c:query()=='edited')
-- Reopening the same query resets traversal independently of consumer callbacks.
S.historyKey(35,{alt=true});c:hide();c:show();S.historyKey(35,{alt=true});assert(c:query()=='second')
c:hide();assert(not S.historyKey(35,{alt=true}))
local opt=S.remote(function()end,{historyId=false});opt:show();opt:query('private');opt:accept();opt:show()
assert(not S.historyKey(35,{alt=true}) and reads==1 and writes==2);opt:hide()
local other=S.new(function()end,nil,{historyId='other'});other:show();other:query('other query');other:accept()
c:show();c:query('');S.historyKey(35,{alt=true});assert(c:query()=='second');c:hide()
-- Persistent namespace survives a module reload, without stacking lifecycle hooks.
local beforeCalls=originalCalls
dofile('core/chooser-search.lua');S=ChooserSearch
local d=S.remote(function()end,{historyId='test'});d:show();d:query('new draft');S.historyKey(35,{alt=true});assert(d:query()=='second');d:hide()
assert(originalCalls==beforeCalls+2)
for i=1,105 do d:show();d:query('query '..i);d:accept()end
assert(#saved['chooser.queries.test.v1']==100 and saved['chooser.queries.test.v1'][1]=='query 105')
print('Chooser history: accepted queries, draft restoration, isolation, opt-out, bounds, callbacks and reload passed')
