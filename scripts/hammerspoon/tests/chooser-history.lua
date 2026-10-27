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
 function c:hideCallback(fn)self.onHide=fn;return self end
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

-- Accessibility text ranges use UTF-16 units, not UTF-8 bytes.
local field={setTimeout=function(self)return self end,attributeValue=function(_,key)
 if key=='AXRole'then return 'AXTextField'elseif key=='AXValue'then return d:query()end
end,setAttributeValue=function(_,key,value)assert(key=='AXSelectedTextRange');assert(value.location==6 and value.length==0);return true end}
hs.processInfo={processID=1}
hs.axuielement={applicationElement=function()return {setTimeout=function(self)return self end,
 attributeValue=function(_,key)assert(key=='AXFocusedUIElement');return field end}end}
d:show();d:query('🧪 متن');d:accept();d:show();d:query('draft');assert(S.historyKey(35,{alt=true}))
print('Recalled Unicode input leaves the caret at its UTF-16 end')

-- A native callback can supply another userdata wrapper for the same chooser.
local shared={}
local mt={__eq=function(a,b)return a.identity==b.identity end}
local canonical=setmetatable(S.remote(function()end,{historyId='wrapper'}),mt)
canonical.identity=shared
local wrapper=setmetatable({identity=shared},mt)
canonical.visible=true;hs.chooser.globalCallback(wrapper,'willOpen')
assert(S.activeChoosers[canonical])
canonical:query('wrapper query');canonical.visible=false
hs.chooser.globalCallback(wrapper,'didClose');canonical.done({text='chosen'})
assert(not S.activeChoosers[canonical]and saved['chooser.queries.wrapper.v1'][1]=='wrapper query')
print('Native-equivalent chooser wrappers retain visible state and release it after close')

d:hide()
local paging=S.remote(function()end,{historyId=false})
local reserved=S.remote(function()end,{reservedPageKeys={down=true}})
local keys,posted,system,bound={}, {},false,{}
local function boundKeys()local r={}for k in pairs(bound)do r[#r+1]=k end;table.sort(r);return table.concat(r,',')end
hs.hotkey={getHotkeys=function()return keys end,systemAssigned=function()return system end,
 bind=function(mods,key,pressed,released,repeated)
  assert(#mods==1 and mods[1]=='alt' and repeated==pressed and not bound[key])
  bound[key]=pressed;return {delete=function()bound[key]=nil end}
 end}
hs.eventtap={keyStroke=function(mods,key)posted[#posted+1]=(#mods>0 and table.concat(mods,'+')..'+' or '')..key end}
paging:show();assert(boundKeys()=='down,up','historyId=false leaves Alt+P/N unbound')
bound.down();assert(posted[#posted]=='pagedown')
bound.up();assert(posted[#posted]=='pageup')
assert(S.pageKey(125,{alt=true,fn=true}) and posted[#posted]=='pagedown')
assert(not S.pageKey(125,{alt=true,cmd=true}))
paging:hide();assert(boundKeys()=='')
keys={{idx='⌥DOWN'}};paging:show();assert(boundKeys()=='up');paging:hide();keys={}
system={enabled=true};paging:show();assert(boundKeys()=='');paging:hide();system=false
reserved:show();assert(boundKeys()=='n,p,up')
local sent=#posted;bound.p();assert(#posted==sent)
reserved:hide();assert(boundKeys()=='' and not S.pageKey(125,{alt=true}))
print('Chooser Alt keys are hotkeys bound only while open, around hotkey, system and per-chooser conflicts')

paging:show();reserved:show();reserved:hide();assert(boundKeys()=='down,up','Closing one rebinds for the other');paging:hide()
paging:show();local up=bound.up;paging.visible=false
up();assert(boundKeys()=='' and posted[#posted]=='alt+up')
paging.visible=true;paging:hide()
print('A hotkey left bound by a missed close hands its key back')

-- A deleted hs.chooser stays a key with no metatable; a coroutine indexes the same way.
local states;for i=1,20 do local n,v=debug.getupvalue(S.pageKey,i);if n=='states' then states=v;break end end
local dead=coroutine.create(function()end);states[dead]={historyId='test'}
assert(not S.pageKey(126,{alt=true,fn=true}) and states[dead]==nil)
states[dead]={historyId='test'};assert(not S.historyKey(35,{alt=true}) and states[dead]==nil)
paging:show();assert(S.pageKey(126,{alt=true}) and posted[#posted]=='pageup');paging:hide()
print('Deleted choosers leave Alt+arrows to the focused app and are pruned')

hs.eventtap=nil
local closing=S.new(function()end,nil,{historyId=false})
local cleanups=0;S.onHide(closing,function()cleanups=cleanups+1 end)
closing:show();closing.onHide();closing.onHide();closing:hide()
assert(cleanups==1,'A doubled native hide callback runs the cleanup once')
closing:show();closing.onHide();assert(cleanups==2,'The next opening gets its own cleanup')
print('A doubled native hide callback runs chooser cleanup once per opening')
