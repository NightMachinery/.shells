-- Run from hammerspoon/: lua tests/hyper-focus-restore.lua
local front,ax
hs={axuielement={applicationElement=function(app)return app.ax end},application={frontmostApplication=function()return front end}}
local function node(role,window,pid)
 local e={role=role,window=window,pidValue=pid or 1,valid=true,focused=true,writes=0,attempts=0}
 function e:attributeValue(k)if k=='AXRole' then return self.role elseif k=='AXWindow' then return self.window end end
 function e:setTimeout(value)self.timeout=value;if not self.failTimeout then return self end end
 function e:isValid()return self.valid end
 function e:pid()return self.pidValue end
 function e:setAttributeValue(k,v)
  assert(k=='AXFocused')
  self.attempts=self.attempts+1
  if (v and self.failRestore) or (not v and self.failUnfocus) then
   if not v and self.applyFailedUnfocus then self.focused=false;self.writes=self.writes+1 end
   return nil,'AX write failed'
  end
  self.focused=v;self.writes=self.writes+1;return self
 end
 return e
end
local function fixture(proxy)
 local win=node('AXWindow')
 local e=node('AXTextField',win)
 front={pidValue=1,pid=function(self)return self.pidValue end}
 ax={setTimeout=function(self,value)self.timeout=value;return self end,attributeValue=function(_,k)if k=='AXFocusedUIElement' then return e elseif k=='AXFocusedWindow' then return ax.window end end}
 ax.window=proxy and node('AXGroup',win) or win
 front.ax=ax
 return dofile('core/hyper-focus-restore.lua')({isTransient=function(app)return app.borrower end}),e,win
end
local f,e,w=fixture();assert(f.capture(front)==e and not e.focused);assert(f.restore(e) and e.focused)
assert(not f.restore(e)) -- state consumed exactly once
f,e,w=fixture(true);f.capture(front);assert(f.restore(e)) -- native window behind a proxy
f,e,w=fixture();f.capture(front);f.cancel();assert(not f.restore(e) and not e.focused) -- pending switch, old owner still front
f,e,w=fixture();f.capture(front);front={pid=function()return 2 end};assert(not f.restore(e) and not e.focused)
f,e,w=fixture();f.capture(front);ax.window=node('AXWindow');assert(not f.restore(e)) -- same app, another window
f,e,w=fixture();f.capture(front);e.valid=false;assert(not f.restore(e))
f,e,w=fixture();f.capture(front);w.valid=false;assert(not f.restore(e))
f,e,w=fixture();f.capture(front);assert(not f.restore(nil)) -- legacy caller cleared saved element
f,e,w=fixture();e.window=nil;ax.window=nil;assert(f.capture(front)==nil and e.focused);assert(not f.restore(e))
f,e,w=fixture();f.capture(front);front=nil;assert(not f.restore(e))
f,e,w=fixture();e.pidValue=2;assert(f.capture(front)==nil and e.focused)
f,e,w=fixture();e.failUnfocus=true;assert(f.capture(front)==e and e.focused and f.element==e)
assert(f.lastUnfocusError and f.restore(e) and e.attempts==2 and e.focused) -- failure without effect
f,e,w=fixture();f.capture(front);e.failRestore=true;assert(not f.restore(e) and not f.element and f.lastRestoreError)
assert(not f.restore(e) and e.attempts==2) -- failed restore consumed once
f,e,w=fixture();front.ax=nil;assert(f.capture(front)==nil and e.focused)
f,e,w=fixture();f.capture(front);front.ax=nil;assert(not f.restore(e))
f,e,w=fixture(true);e.window=nil;assert(f.capture(front)==e);assert(f.restore(e)) -- proxy fallback at capture
f,e,w=fixture();f.capture(front);assert(ax.timeout==.15 and e.timeout==.15 and w.timeout==.15)
f,e,w=fixture();e.failTimeout=true;assert(f.capture(front)==nil and e.focused)
-- Capture must be verifiable through the owner's focused window before any write.
f,e,w=fixture();ax.window=nil;assert(f.capture(front)==nil and e.focused and e.writes==0)
f,e,w=fixture();ax.window=node('AXWindow');assert(f.capture(front)==nil and e.focused and e.writes==0)
f,e,w=fixture();e.window=node('AXGroup');assert(f.capture(front)==nil and e.focused and e.writes==0)
-- Borrowers have no AX window: restoration must query the captured owner.
f,e,w=fixture();f.capture(front);front={pid=function()return 2 end,borrower=true};assert(f.restore(e) and e.focused)
f,e,w=fixture();f.capture(front);ax.window=node('AXWindow');front={pid=function()return 2 end,borrower=true};assert(not f.restore(e))
f,e,w=fixture();f.capture(front);front.ax=nil;front={pid=function()return 2 end,borrower=true};assert(not f.restore(e))
f,e,w=fixture();f.capture(front);front.pidValue=2;assert(not f.restore(e)) -- captured app identity changed
f,e,w=fixture();f.capture(front);f.cancel();front={pid=function()return 2 end,borrower=true};assert(not f.restore(e))
-- Failed blur results do not reveal whether the remote write took effect.
f,e,w=fixture();e.failUnfocus=true;e.applyFailedUnfocus=true;assert(f.capture(front)==e and not e.focused)
assert(f.restore(e) and e.focused and e.attempts==2)
f,e,w=fixture();e.failUnfocus=true;f.capture(front);e.focused=false -- delayed effect before release
assert(f.restore(e) and e.focused and e.attempts==2)
f,e,w=fixture();e.failUnfocus=true;f.capture(front);f.cancel();assert(not f.restore(e) and e.attempts==1)
f,e,w=fixture();e.failUnfocus=true;f.capture(front);front={pid=function()return 2 end};assert(not f.restore(e) and e.attempts==1)
f,e,w=fixture();e.failUnfocus=true;f.capture(front);front={pid=function()return 2 end,borrower=true};assert(f.restore(e) and e.attempts==2)
f,e,w=fixture();e.failUnfocus=true;f.capture(front);ax.window=node('AXWindow');front={pid=function()return 2 end,borrower=true};assert(not f.restore(e) and e.attempts==1)
print('Hyper focus ownership tests passed')
