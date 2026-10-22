nightdir='..'
local front=true
local active='original'
local opens,waits,native={}, {}, {}
local E=dofile('brave/tabs.lua')({braveInFront=function()return front end,clearError=function()end,unavailable=function()end,
 submitGemini=function(opened,q,done)native[#native+1]={opened=opened,done=done};return true end})
E.request=function(r,cb)
 if r.action=='openMany' then
  local opened={}
  for i,item in ipairs(r.items)do local id=tostring(#opens+1);opens[#opens+1]={url=item.url,replace=i==1 and r.replace,tabId=r.tabId};opened[#opened+1]={ok=true,tabId=id}end
  cb({ok=true,anchor=active,opened=opened})
 elseif r.action=='activeState' then cb({ok=true,frontmost=front,tabId=active})
 elseif r.action=='selectTab' then assert(r.requireActiveTabId==active);active=r.tabId;cb({ok=true,tabId=active})
 else error(r.action)end
end
E.dom.submit=function(opened,destination,q,cb)waits[#waits+1]={cb=cb,destination=destination}end
E.openQueries({{destination='chatgpt',url='https://chatgpt.com/',query='one'},
 {destination='claude',url='https://claude.ai/new',query='one'},
 {destination='gemini',url='https://gemini.google.com/app',query='one'}},{replace=true,tabId='original'})
assert(#opens==3 and #waits==3 and opens[1].replace and not opens[2].replace and active=='original')
front=false;waits[1].cb({status='submitted'});waits[2].cb({status='submitted'});waits[3].cb({status='native-required'})
assert(#native==0 and E.submissions['3'].status=='cancelled')
front=true;active='before'
E.openQueries({{destination='gemini',url='https://gemini.google.com/app',query='two'},
 {destination='gemini',url='https://gemini.google.com/app',query='three'}})
assert(#opens==5 and #waits==5 and active=='before')
waits[4].cb({status='native-required'});waits[5].cb({status='native-required'});assert(#native==1 and active=='4')
native[1].done({status='submitted'});assert(#native==2 and active=='5')
native[2].done({status='submitted'});assert(not E.nativeActive)
E.stop();print('Concurrent browser opens/DOM submissions preserve focus; native fallbacks remain serialized and guarded')
