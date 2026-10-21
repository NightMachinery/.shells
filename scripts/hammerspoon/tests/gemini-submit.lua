-- Run from hammerspoon/: lua tests/gemini-submit.lua
local now,pending,updates=0,{},{}
local ui,state,focused,front,sends,typed,blocked
hs={timer={secondsSinceEpoch=function() return now end,doAfter=function(_,fn)
    local t={fn=fn,stop=function(self) self.stopped=true end};pending[#pending+1]=t;return t
end},eventtap={keyStroke=function(_,key)
    assert(focused,'keystroke before confirmed focus')
    if focused.kind=='picker' and key=='return' then ui.menu='model'
    elseif focused.kind=='argon' and key=='return' then ui.argon=true;ui.menu=nil
    elseif focused.kind=='thinking' and key=='return' then ui.menu='high'
    elseif focused.kind=='high' and key=='return' then ui.high=true;ui.menu=nil
    elseif key=='escape' then ui.menu=nil
    elseif focused.kind=='composer' and key=='return' then sends=sends+1
    else error('unexpected '..key..' on '..focused.kind) end
end}}
local function element(kind,role,title)
    local e={kind=kind}
    function e:attributeValue(name)
        if name=='AXRole' then return role end
        if name=='AXTitle' then return title end
        if name=='AXEnabled' then return true end
        if name=='AXFocused' then return focused==self end
        if name=='AXValue' and kind=='composer' then return ui.text end
        if name=='AXChildren' then return {} end
    end
    function e:setAttributeValue(name,value)
        updates[#updates+1]=function()
            if name=='AXFocused' then
                focused=self
                if ui.autoOpen and kind=='picker' then ui.menu='model' end
                if ui.autoOpen and kind=='thinking' then ui.menu='high' end
            elseif name=='AXValue' and not blocked then ui.text=value;typed=typed+1 end
        end
    end
    function e:isValid() return true end
    return e
end
local picker=element('picker','AXPopUpButton','Open mode picker, currently Argon')
local composer=element('composer','AXTextArea','Enter a prompt for Gemini')
local thinking=element('thinking','AXMenuItem','Thinking level')
local function root()
    local children={picker,composer}
    if ui.menu then
        if not ui.noArgon then children[#children+1]=element('argon','AXMenuItem',(ui.argon and 'Selected ' or '')..'4 Argon Complex tasks') end
        children[#children+1]=thinking
        if ui.menu=='high' then children[#children+1]=element('high','AXMenuItem',(ui.high and 'Selected ' or '')..'High Extra thorough') end
    end
    local web=element('web','AXWebArea','')
    function web:attributeValue(name) if name=='AXRole' then return 'AXWebArea' elseif name=='AXChildren' then return children end end
    local outside=element('outside','AXTextArea','Enter a prompt for Gemini')
    local r=element('window','AXWindow','')
    function r:attributeValue(name) if name=='AXRole' then return 'AXWindow' elseif name=='AXChildren' then return {outside,web} end end
    return r
end
local function fixture(options)
    options=options or {};now=0;pending={};updates={};focused=nil;sends=0;typed=0;blocked=options.blocked
    front=true;state={ok=true,frontmost=true,tabId='owned',url='https://gemini.google.com/app'}
    ui={argon=options.argon or false,high=options.high or false,text=options.text or 'Ask Gemini\n',noArgon=options.noArgon,autoOpen=options.autoOpen}
    return dofile('brave/gemini-submit.lua')({engine={activeState=function(cb) cb(state) end},
        braveInFront=function() if front then return {} end end,nativeWindow=root,unavailable=function() end})
end
local function tick()
    now=now+.2;local work=updates;updates={};for _,fn in ipairs(work) do fn() end
    work=pending;pending={};for _,t in ipairs(work) do if not t.stopped then t.fn() end end
end
local function complete(c)
    for _=1,250 do if c.lastResult.status~='waiting' then return end;tick() end
    error('did not finish')
end
local c=fixture();assert(c.submit({tabId='owned'},'  hello\nü & +'))
assert(c.lastResult.status=='waiting' and sends==0)
complete(c);assert(c.lastResult.status=='submitted');assert(ui.argon and ui.high)
assert(ui.text=='  hello\nü & +' and typed==1 and sends==1);tick();assert(sends==1)
c=fixture({argon=true,high=true});c.submit({tabId='owned'},'hello');complete(c)
assert(c.lastResult.status=='submitted' and typed==1 and sends==1)
c=fixture({autoOpen=true});c.submit({tabId='owned'},'hello');complete(c)
assert(c.lastResult.status=='submitted' and sends==1)
for _,bad in ipairs({'https://gemini.google.com.evil/app','https://gemini.google.com/app/conversation','https://aistudio.google.com/app'}) do
    c=fixture();state.url=bad;c.submit({tabId='owned'},'hello');assert(c.lastResult.status=='cancelled' and sends==0)
end
c=fixture();c.submit({tabId='owned'},'hello');state.tabId='another';tick();assert(c.lastResult.status=='cancelled' and typed==0)
c=fixture();c.submit({tabId='owned'},'hello');front=false;tick();assert(c.lastResult.status=='cancelled' and sends==0)
c=fixture({text='my draft'});c.submit({tabId='owned'},'hello');complete(c);assert(c.lastResult.status=='cancelled' and typed==0)
c=fixture({noArgon=true});c.submit({tabId='owned'},'hello');complete(c);assert(c.lastResult.status=='cancelled' and sends==0)
c=fixture({blocked=true});c.submit({tabId='owned'},'hello');complete(c);assert(c.lastResult.status=='timeout' and sends==0)
c=fixture();c.submit({tabId='owned'},'hello');c.stop();tick();assert(sends==0 and typed==0)
c=fixture();assert(not c.submit({tabId='owned'},' \t'));assert(not c.submit({},'hello'))
c=fixture();c.submit({tabId='owned'},'hello')
for _=1,100 do if c.lastResult.stage=='send' then break end;tick() end
ui.text='edited';tick();assert(c.lastResult.status=='cancelled' and sends==0)
print('Gemini native Argon High submission tests passed')
