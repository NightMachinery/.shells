-- Firefox exposes native AX tabs but no AppleScript tab/DOM dictionary.
return function(config)
 local F={version="1.0.0",known={},knownOrder={},timers={}}
 local function bounded(e)if e and e:setTimeout(.1)then return e end end
 local function token(element)
  for id,e in pairs(F.known)do
   if e==element then return id end
  end
  local id=hs.host.uuid();F.known[id]=element;F.knownOrder[#F.knownOrder+1]=id
  while #F.knownOrder>1000 do F.known[table.remove(F.knownOrder,1)]=nil end
  return id
 end
 local function url(root)
  local q,i={root},1
  local deadline=hs.timer.absoluteTime()+6e8
  while q[i] and i<=150 and hs.timer.absoluteTime()<deadline do
   local e=bounded(q[i]);i=i+1
   if e then
    if e:attributeValue("AXRole")=="AXWebArea" then
     local value=e:attributeValue("AXURL");if type(value)=="table"then value=value.url end
     if type(value)=="string"then return value end
    else for _,child in ipairs(e:attributeValue("AXChildren")or{})do q[#q+1]=child end end
   end
  end
  return ""
 end
 function F.list()
  local app=hs.application.get(config.bundleID)
  if not app then return {ok=false,error="Firefox is not running"}end
  local rows,active={},nil
  local ax=bounded(hs.axuielement.applicationElement(app))
  local focusedAX=ax and bounded(ax:attributeValue("AXFocusedWindow"))
  if focusedAX and focusedAX:attributeValue("AXRole")~="AXWindow"then focusedAX=bounded(focusedAX:attributeValue("AXWindow"))end
  local focused=focusedAX and focusedAX:asHSWindow()or app:focusedWindow()
  local windows=app:allWindows()
  -- Some browser AX bridges expose the focused window while AXWindows is empty.
  if #windows==0 and focused then windows={focused}end
  for wi,window in ipairs(windows)do
   local root=bounded(hs.axuielement.windowElement(window))
   local q,i,seen=root and {root}or{},1,{}
   local deadline=hs.timer.absoluteTime()+8e8
   while q[i] and i<=250 and hs.timer.absoluteTime()<deadline do
    local e=bounded(q[i]);i=i+1
    if e then
     local role=e:attributeValue("AXRole")
     local parent=bounded(e:attributeValue("AXParent"))
     if role=="AXTab"or(role=="AXRadioButton"and parent and parent:attributeValue("AXRole")=="AXTabGroup")then
      local id=token(e)
      if not seen[id]then
       seen[id]=true
       local selected=e:attributeValue("AXValue")==1 or e:attributeValue("AXSelected")==true
       local title=e:attributeValue("AXTitle")or e:attributeValue("AXDescription")or"Tab"
       local row={tabId=id,windowId=tostring(window:id()),windowNumber=wi,index=1,title=title,
        url=selected and url(root)or"",element=e,selected=selected,root=root}
       for _,previous in ipairs(rows)do if previous.windowId==row.windowId then row.index=row.index+1 end end
       rows[#rows+1]=row
       if selected and focused and focused:id()==window:id()then active=id end
      end
     elseif role~="AXWebArea"and role~="AXMenuBar"then
      for _,child in ipairs(e:attributeValue("AXChildren")or{})do q[#q+1]=child end
      if role=="AXTabGroup"then for _,child in ipairs(e:attributeValue("AXTabs")or{})do q[#q+1]=child end end
     end
    end
   end
  end
  return {ok=true,tabs=rows,activeTabId=active}
 end
 local function public(result)
  local rows={}
  for _,row in ipairs(result.tabs or{})do rows[#rows+1]={tabId=row.tabId,windowId=row.windowId,windowNumber=row.windowNumber,index=row.index,title=row.title,url=row.url}end
  return {ok=result.ok,error=result.error,tabs=rows,activeTabId=result.activeTabId}
 end
 local function select(row)
  if not config.inFront()then return {ok=false,error="Browser focus changed"}end
  if not row or not row.element:isValid()then return {ok=false,error="Firefox tab closed"}end
  local accepted=row.element:performAction("AXPress")
  return {ok=accepted~=nil and accepted~=false,tabId=row.tabId,windowId=row.windowId,error=not accepted and "Firefox tab action not confirmed"or nil}
 end
 function F.request(request,callback)
  if F.stopped then return end
  if request.action=="dom"then callback({ok=false,jsUnavailable=true,error="Firefox has no native tab JavaScript API"});return end
  local list=F.list()
  if not list.ok then callback(list);return end
  if request.action=="list"then callback(public(list));return end
  local active,target
  for _,row in ipairs(list.tabs)do
   if row.tabId==list.activeTabId then active=row end
   if row.tabId==request.tabId then target=row end
  end
  if request.action=="activeState"or request.action=="snapshot"then
   callback(active and {ok=true,tabId=active.tabId,windowId=active.windowId,url=active.url,
    frontmost=config.inFront()~=nil}or{ok=false,error="Firefox active tab is not exposed"});return
  end
  if request.action=="selectTab"then
   if request.requireActiveTabId and request.requireActiveTabId~=list.activeTabId then callback({ok=false,error="Browser focus changed"})
   else callback(select(target))end;return
  end
  if request.action=="step"then
   local rows={};for _,row in ipairs(list.tabs)do if active and row.windowId==active.windowId then rows[#rows+1]=row end end
   if active and #rows>0 then callback(select(rows[(active.index-1+request.direction+#rows)%#rows+1]))
   else callback({ok=false,error="Firefox tab strip unavailable"})end;return
  end
  if request.action=="bookmark"then
   if not config.inFront()then callback({ok=false,error="Browser focus changed"});return end
   -- Only the selected tab's actual URL is available, never infer a background
   -- tab URL from its title or reuse a cached URL after navigation.
   for _,row in ipairs(list.tabs)do if row.url==request.url then local result=select(row);result.reused=true;callback(result);return end end
  end
  if request.action~="openMany"and request.action~="bookmark"then callback({ok=false,error="Unsupported Firefox action"});return end
  if request.replace then callback({ok=false,error="Firefox does not expose safe native URL replacement; toggle New tab"});return end
  local items=request.items or{{url=request.url}}
  local args={"-g","-b",config.bundleID}
  for _,item in ipairs(items)do
   if type(item.url)~="string"or not item.url:match("^https?://[^%s]+$")then callback({ok=false,error="URL must be HTTP or HTTPS"});return end
   args[#args+1]=item.url
  end
  local before={};for _,row in ipairs(list.tabs)do before[row.tabId]=true end
  F.task=hs.task.new("/usr/bin/open",function(code)
   F.task=nil
   if F.stopped then return end
   if code~=0 then callback({ok=false,error="Firefox URL open failed; not retried"});return end
   local deadline=hs.timer.secondsSinceEpoch()+2
   local function check()
    if F.stopped then return end
    local current=F.list();local fresh={}
    for _,row in ipairs(current.tabs or{})do if not before[row.tabId]then fresh[#fresh+1]=row end end
    if #fresh==#items then
     local opened={};for i,row in ipairs(fresh)do opened[i]={ok=true,tabId=row.tabId,windowId=row.windowId,url=items[i].url,reused=false}end
     if request.action=="bookmark"then callback(opened[1])else callback({ok=true,opened=opened,anchor=current.activeTabId})end
    elseif hs.timer.secondsSinceEpoch()<deadline then F.timer=hs.timer.doAfter(.1,check)
    else callback({ok=false,error="Firefox opened URLs but new tab identity was not confirmed; no submission attempted"})end
   end
   check()
  end,args)
  if not F.task or not F.task:start()then F.task=nil;callback({ok=false,error="Could not start Firefox URL opener"})end
 end
 function F.stop()
  F.stopped=true
  if F.timer then F.timer:stop()end
  if F.task then F.task:terminate()end
 end
 return F
end
