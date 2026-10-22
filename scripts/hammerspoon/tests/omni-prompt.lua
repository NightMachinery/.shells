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

-- Disabled sources do no I/O. History results arrive after local rows.
local tabCallback,historyCallback,tabReads,historyReads,bookmarkReads
local warnings={}
tabReads,historyReads,bookmarkReads=0,0,0
local function variant(sources,features)
 return dofile('brave/omni-prompt.lua')({
  sources=sources,name='Test',magic=not features or features.magic,queryActions=not features or features.actions,
  braveInFront=function()return front end,
  bookmarks={read=function()bookmarkReads=bookmarkReads+1;return {{text='Saved',url='https://example.org/saved'}} end},
  history={search=function(query,options,fn)
   historyReads=historyReads+1;historyCallback=fn
   assert(type(options.aliases)=='table' and type(options.exclude)=='table')
  end},
  searchAliases={apply=function(rows)return rows end},unavailable=function(err)warnings[#warnings+1]=err end,
  engine={listTabs=function(fn)tabReads=tabReads+1;tabCallback=fn end,
   openBookmark=function(url)opened[#opened+1]=url end,selectTab=function(id)selected[#selected+1]=id end,
   openURL=function(url)opened[#opened+1]=url end},
 })
end
local v=variant({tabs=true,bookmarks=true,history=true})
assert(v.show());assert(not v.chooser.visible and historyReads==0)
tabCallback({ok=true,tabs={{title='Current',url='https://example.org/',index=1,windowNumber=1,tabId='9'}}})
assert(v.chooser.visible and #v.chooser.items==2 and v.historyPending)
historyCallback({ok=true,total=5000,history={{title='Historical paper',url='https://archive.example.org/paper'}}})
assert(#v.chooser.items==3 and not v.historyPending and v.historyTotal==5000)
assert(v.chooser.items[3].kind=='history' and v.chooser.items[3].subText=='History · https://archive.example.org/paper')
typeQuery(v.chooser,'historical archive.example');assert(#v.chooser.items==1)
v.chooser.completion(v.chooser.items[1]);assert(opened[#opened]=='https://archive.example.org/paper')
flush() -- stale remote completion remains harmless
local reads=historyReads
local b=variant({bookmarks=true},{magic=false,actions=false});b.show()
assert(#b.chooser.items==1 and tabReads==1 and historyReads==reads and bookmarkReads==2)
assert(not b.queryActions.key(36,{cmd=true}) and not b.queryActions.magic('.c test'))
-- Plain no-match Enter waits for the latest history response instead of Googling early.
local h=variant({history=true});h.show();local old=historyCallback
local count=#opened
typeQuery(h.chooser,'deep paper');assert(h.queryActions.key(36,{}));assert(#opened==count)
flush();old({ok=true,history={}});assert(h.historyPending and #opened==count)
historyCallback({ok=true,total=5000,history={{title='Deep paper',url='https://example.org/deep'}}})
flush();assert(opened[#opened]=='https://example.org/deep' and not h.chooser.visible)
local no=variant({history=true});no.show()
typeQuery(no.chooser,'no match');assert(no.queryActions.key(36,{}));flush()
historyCallback({ok=true,total=5000,history={}});flush()
assert(opened[#opened]=='https://www.google.com/search?q=no%20match')
local stale=variant({history=true});stale.show();old=historyCallback
stale.cancel();old({ok=true,history={{title='Old',url='https://example.org/old'}}});assert(not stale.chooser.visible)
stale.show();old=historyCallback;typeQuery(stale.chooser,'new');flush()
old({ok=true,history={{title='Wrong new',url='https://example.org/wrong'}}});assert(#stale.chooser.items==0)
historyCallback({ok=false,error='History unavailable'});assert(stale.chooser.visible and #warnings==1)
v.stop();b.stop();h.stop();no.stop();stale.stop()
print('Omni asynchronous variants, stale responses and pending Enter tests passed')
