-- Run from hammerspoon/: lua tests/claude-submit.lua
local factory=dofile('brave/claude-submit.lua')
local function setup(options)
 options=options or {}
 local s={now=0,pending={},sends=0,attempts=0,front=true,callbacks={}}
 local function node(attrs)
  local e={attrs=attrs}
  function e:setTimeout()return self end
  function e:attributeValue(k)return self.attrs[k] end
  function e:setAttributeValue(k,v)
   if options.deferFocus then s.applyFocus=function()self.attrs[k]=v end
   else self.attrs[k]=v end
   return self
  end
  function e:actionNames()return options.press and {'AXPress'} or {'AXShowMenu'} end
  function e:performAction(action)
   assert(action=='AXPress');s.attempts=s.attempts+1
   if options.failedPress then return nil,'AX timeout' end
   s.sends=s.sends+1;return self
  end
  return e
 end
 local composer=node({AXRole='AXTextArea',AXValue=options.text or 'query'})
 local send=node({AXRole='AXButton',AXTitle='Send message',AXEnabled=not options.disabled})
 local web=node({AXRole='AXWebArea',AXURL={url=options.axURL or 'https://claude.ai/new?q=query'},AXChildren={composer,send}})
 local root=node({AXRole='AXWindow',AXChildren={web}})
 hs={timer={secondsSinceEpoch=function()return s.now end,doAfter=function(delay,fn)
  local t={fn=fn,delay=delay,stop=function(self)self.stopped=true end};s.pending[#s.pending+1]=t;return t
 end},eventtap={keyStroke=function(mods,key)
  assert(#mods==0 and key=='space' and send.attrs.AXFocused==true)
  assert(composer.attrs.AXValue=='query' and send.attrs.AXEnabled==true)
  s.sends=s.sends+1;s.attempts=s.attempts+1
 end}}
 local c=factory({braveInFront=function()if s.front then return {} end end,
  nativeWindow=function()return root end,unavailable=function(reason)s.warning=reason end,
  engine={activeState=function(cb)
   local state={ok=true,frontmost=true,tabId=s.tabId or 'owned',url=options.url or 'https://claude.ai/new?q=query'}
   if options.deferred then s.callbacks[#s.callbacks+1]=function()cb(state)end else cb(state)end
  end}})
 function s.tick()
  s.now=s.now+.2
  local work=s.pending;s.pending={}
  for _,t in ipairs(work)do if not t.stopped then t.fn()end end
 end
 return c,s,composer,send
end
local c,s=setup();assert(c.submit({tabId='owned'},'query'));assert(s.sends==0)
s.tick();assert(c.lastResult.status=='submitted' and s.sends==1);s.tick();assert(s.sends==1)
c,s=setup({press=true});c.submit({tabId='owned'},'query');assert(s.sends==1 and c.lastResult.status=='submitted')
c,s=setup({press=true,failedPress=true});c.submit({tabId='owned'},'query');s.tick()
assert(c.lastResult.status=='failed' and s.sends==0 and s.attempts==1)
local input,send
c,s,input=setup({text='loading'});c.submit({tabId='owned'},'query');s.tick();assert(s.sends==0)
input.attrs.AXValue='query';s.tick();s.tick();assert(s.sends==1)
c,s,input,send=setup({disabled=true});c.submit({tabId='owned'},'query');s.tick();assert(s.sends==0)
send.attrs.AXEnabled=true;s.tick();s.tick();assert(s.sends==1)
c,s=setup({deferFocus=true});c.submit({tabId='owned'},'query');s.tick();assert(s.sends==0)
s.applyFocus();s.tick();assert(s.sends==1)
c,s=setup();c.submit({tabId='owned'},'query');s.tabId='other';s.tick();assert(c.lastResult.status=='cancelled' and s.sends==0)
c,s=setup();c.submit({tabId='owned'},'query');s.front=false;s.tick();assert(c.lastResult.status=='cancelled' and s.sends==0)
c,s,input=setup();c.submit({tabId='owned'},'query');input.attrs.AXValue='edited';s.tick();assert(s.sends==0)
for _,url in ipairs({'https://claude.ai.evil/new','https://claude.ai/chat/existing','https://example.org/'})do
 c,s=setup({url=url});c.submit({tabId='owned'},'query');assert(c.lastResult.status=='cancelled' and s.sends==0)
end
c,s=setup({axURL='https://example.org/'});c.submit({tabId='owned'},'query');s.tick();assert(s.sends==0)
c.stop();s.tick();assert(s.sends==0)
c,s=setup({deferred=true});c.submit({tabId='owned'},'query');c.stop()
for _,cb in ipairs(s.callbacks)do cb()end;assert(s.sends==0)
c,s=setup({text='different'});c.submit({tabId='owned'},'query');s.now=46;s.tick()
assert(c.lastResult.status=='timeout' and s.sends==0)
c,s=setup();assert(not c.submit({},'query'));assert(not c.submit({tabId='owned'},'  '));assert(s.sends==0)
print('Claude guarded Send-button tests passed')
