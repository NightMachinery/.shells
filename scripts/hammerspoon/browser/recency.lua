-- Event-driven MRU for browsers without Brave's native recency controller.
return function(config)
 local R={version="1.0.0",order={},position=1}
 local function record(id)
  if not id then return end
  for i=#R.order,1,-1 do if R.order[i]==id then table.remove(R.order,i) end end
  table.insert(R.order,1,id)
  while #R.order>500 do table.remove(R.order) end
 end
 function R.observe()
  if R.stopped or R.cycling or not config.inFront() or R.reading then return end
  R.reading=true
  config.engine.activeState(function(state)
   R.reading=false
   if not R.stopped and not R.cycling and state.ok and state.frontmost then record(state.tabId) end
  end)
 end
 function R.finish()
  if not R.cycling and not R.loading then return end
  if R.loading then R.released=true;return end
  R.cycling=false;R.session=nil;R.selected=nil;R.observe()
 end
 local function move(direction)
  local rows=R.session
  if not rows or #rows<2 then return end
  R.position=(R.position-1+direction+#rows)%#rows+1
  R.selected=rows[R.position]
  config.engine.selectTab(R.selected)
 end
 function R.cycle(direction)
  if not config.inFront() then return false end
  if R.loading then R.pending=(R.pending or 0)+direction;return true end
  if R.session then move(direction);return true end
  R.loading=true;R.cycling=true;R.pending=direction;R.released=false
  config.engine.listTabs(function(result)
   R.loading=false
   if R.stopped or not config.inFront() or not result.ok then R.finish();return end
   local current,available,strip=nil,{},{}
   for _,row in ipairs(result.tabs) do if row.tabId==result.activeTabId then current=row end end
   for _,row in ipairs(result.tabs) do
    if not current or row.windowId==current.windowId then available[row.tabId]=true;strip[#strip+1]=row.tabId end
   end
   local ordered,seen={},{}
   local function add(id)if id and available[id] and not seen[id] then ordered[#ordered+1]=id;seen[id]=true end end
   add(result.activeTabId)
   for _,id in ipairs(R.order)do add(id)end
   for _,id in ipairs(strip)do add(id)end
   R.session=ordered;R.position=1
   move(R.pending);R.pending=0
   if R.released or not hs.eventtap.checkKeyboardModifiers().cmd then R.finish() end
  end)
  return true
 end
 function R.watch()
  if R.stopped or not config.inFront() then return end
  local app=config.inFront()
  local function changed(_,_,notification)
   if R.debounce then return end
   R.debounce=hs.timer.doAfter(.08,function()R.debounce=nil;if notification=="AXFocusedWindowChanged" or notification=="AXWindowCreated" then R.watch() else R.observe() end end)
  end
  if R.observer then R.observer:stop() end
  local ok,observer=pcall(hs.axuielement.observer.new,app:pid())
  if not ok then R.observe();return end
  R.observer=observer:callback(changed)
  local ax=hs.axuielement.applicationElement(app)
  if not ax then observer:stop();R.observer=nil;return end
  ax:setTimeout(.1)
  for _,event in ipairs({"AXFocusedUIElementChanged","AXFocusedWindowChanged","AXWindowCreated"})do
   pcall(function()observer:addWatcher(ax,event)end)
  end
  local root=config.nativeWindow(app)
  local queue,index=root and {root} or {},1
  while queue[index] and index<=180 do
   local e=queue[index];index=index+1;e:setTimeout(.1)
   local role=e:attributeValue("AXRole")
   if role=="AXTabGroup" or role=="AXRadioButton" or role=="AXTab" then
    for _,event in ipairs({"AXValueChanged","AXSelectedChildrenChanged"})do pcall(function()observer:addWatcher(e,event)end)end
   end
   if role~="AXWebArea" and role~="AXMenuBar" then
    for _,child in ipairs(e:attributeValue("AXChildren") or {})do queue[#queue+1]=child end
    if role=="AXTabGroup" then for _,tab in ipairs(e:attributeValue("AXTabs") or {})do queue[#queue+1]=tab end end
   end
  end
  observer:start();R.observe()
 end
 function R.stop()
  R.stopped=true
  if R.observer then R.observer:stop();R.observer=nil end
  if R.debounce then R.debounce:stop();R.debounce=nil end
 end
 return R
end
