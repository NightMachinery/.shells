-- Run from hammerspoon/: lua tests/omni-prompt.lua
nightdir='..'
local pending, opened, selected = {}, {}, {}
local front=true
local activeChooser
local field={setTimeout=function(self)return self end}
function field:attributeValue(k)
 if k=='AXRole' then return 'AXTextField' end
 if k=='AXValue' then return activeChooser and activeChooser.text end
end
function field:setAttributeValue(k,v)
 assert(k=='AXSelectedTextRange');activeChooser.range=v;return self
end
hs={
 processInfo={processID=123},
 axuielement={applicationElement=function(pid)
  assert(pid==123)
  return {setTimeout=function(self)return self end,attributeValue=function(_,key)
   assert(key=='AXFocusedUIElement');return activeChooser and field
  end}
 end},
 timer={doAfter=function(_,fn)
  local t={fn=fn,stop=function(self)self.stopped=true end}
  pending[#pending+1]=t;return t
 end},
 eventtap={event={types={keyDown=10}},new=function()
  return {start=function()end,stop=function()end}
 end},
 chooser={new=function(completion)
  local c={text='',visible=false,items={},completion=completion}
  function c:query(v)if v==nil then return self.text end;self.text=v;return self end
  function c:choices(v)self.items=v;return self end
  function c:queryChangedCallback(fn)self.changed=fn;return self end
  function c:showCallback(fn)self.onShow=fn;return self end
  function c:hideCallback(fn)self.onHide=fn;return self end
  function c:placeholderText(v)self.placeholder=v;return self end
  function c:width()return self end
  function c:rows()return self end
  function c:enableDefaultForQuery()return self end
  function c:selectedRowContents()return self.items[1] end
  function c:isVisible()return self.visible end
  function c:show()self.visible=true;activeChooser=self;self.range={location=0,length=#self.text};if self.onShow then self.onShow() end;return self end
  function c:hide()self.visible=false;if self.onHide then self.onHide() end;return self end
  function c:cancel()self:hide();self.completion(nil) end
  function c:delete()self.deleted=true end
  return c
 end},
}
dofile('core/chooser-search.lua')
local function flush()
 local work=pending;pending={}
 for _,t in ipairs(work)do if not t.stopped then t.fn() end end
end
local function typeQuery(c,q)c:query(q);c.changed(q) end
local function make()
 return dofile('brave/omni-prompt.lua')({
  braveInFront=function()return front end,
  bookmarks={read=function()return {{text='Bookmark',url='https://example.org/bookmark'}} end},
  searchAliases={apply=function(rows)return rows end},unavailable=error,
  engine={
   listTabs=function(fn)fn({ok=true,tabs={{title='Help tab',index=3,windowNumber=1,tabId='42',url='https://example.org/'}}})end,
   selectTab=function(id)selected[#selected+1]=id end,
   openBookmark=function(url)opened[#opened+1]=url end,
   openURL=function(url)opened[#opened+1]=url end,
  },
 })
end
local o=make();assert(o.show())
assert(o.chooser.items[1].subText=='Tab 3 · Window 1 · https://example.org/')
typeQuery(o.chooser,'.help')
assert(o.queryActions.key(36,{}));assert(not o.chooser.visible and not o.helpChooser.visible)
flush();assert(o.helpChooser.visible and #o.helpChooser.items==13 and #opened==0)
-- Every displayed magic resolves through the same registry as routing.
for _,row in ipairs(o.helpChooser.items)do assert(o.queryActions.magic(row.prefix..' query')) end
typeQuery(o.helpChooser,'scholar semantic')
assert(#o.helpChooser.items==1 and o.helpChooser.items[1].prefix=='.s')
local choice=o.helpChooser.items[1];o.helpChooser:hide();o.helpChooser.completion(choice)
assert(not o.chooser.visible);flush()
assert(o.chooser.visible and o.chooser:query()=='.s ' and #opened==0 and #selected==0)
assert(o.chooser.range.location==3 and o.chooser.range.length==0)
-- Typing after choosing a magic appends to its prefix instead of replacing it.
local range=o.chooser.range
local value=o.chooser:query()
typeQuery(o.chooser,value:sub(1,range.location)..'papers'..value:sub(range.location+range.length+1))
assert(o.chooser:query()=='.s papers')
-- Exact .help is the only command; modified Enter still uses the whole input.
assert(not o.queryActions.help(' .help') and not o.queryActions.help('.help '))
typeQuery(o.chooser,'.help');assert(o.queryActions.key(36,{shift=true}));flush()
assert(opened[1]=='https://www.google.com/search?q=.help')
-- Escape from help performs no navigation or prefill.
assert(o.show());typeQuery(o.chooser,'.help');assert(o.queryActions.key(36,{}));flush()
o.helpChooser:cancel();flush();assert(not o.chooser.visible and #opened==1)
-- Deferred help cannot steal focus after switching apps, or survive a reload.
assert(o.show());typeQuery(o.chooser,'.help');o.queryActions.key(36,{})
front=false;flush();assert(not o.helpChooser.visible);front=true
assert(o.show());typeQuery(o.chooser,'.help');o.queryActions.key(36,{})
o.stop();flush();assert(not o.helpChooser.visible and o.helpChooser.deleted)
local other=make();other.showHelp();local row=other.helpChooser.items[1]
other.helpChooser:hide();other.helpChooser.completion(row);other.stop();flush()
assert(not other.chooser.visible and other.pendingHelp==nil)
print('Omni magic cheatsheet tests passed')
