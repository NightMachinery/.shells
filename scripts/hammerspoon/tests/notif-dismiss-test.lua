-- Run from scripts: lua hammerspoon/tests/notif-dismiss-test.lua
local function element(role, children, actions)
    local e={role=role,children=children or {},actions=actions or {},pressed={}}
    function e:setTimeout() end
    function e:attributeValue(key)
        if key=='AXRole' then return self.role end
        if key=='AXChildren' then return self.children end
        if key=='AXWindows' then return self.windows end
    end
    function e:actionNames() return self.actions end
    function e:performAction(action) self.pressed[#self.pressed+1]=action end
    return e
end
local close='Name:Close\nTarget:0x0\nSelector:(null)'
local clear='Name:Clear All\nTarget:0x0\nSelector:(null)'
for _,wrappers in ipairs({2,3}) do
    local single=element('AXGroup',nil,{'AXPress',close})
    local nested=element('AXGroup',nil,{close})
    local stack=element('AXGroup',{nested},{close,clear})
    local generic=element('AXGroup',nil,{'AXPress'})
    local list=element('AXGroup',{single,stack,generic})
    for _=1,wrappers do list=element('AXGroup',{list}) end
    local app=element('AXApplication');app.windows={element('AXWindow',{list})}
    local menu=element('AXGroup',nil,{close});app.children={menu}
    local env=setmetatable({hs={application={get=function()return app end},axuielement={applicationElement=function(a)return a end}},hsAfter=function()end,alert_gateway=function(msg)error(msg)end},{__index=_G})
    assert(loadfile('hammerspoon/core/notif-dismiss.lua','t',env))()
    env.notifDismissAll()
    assert(single.pressed[1]==close)
    assert(stack.pressed[1]==clear)
    assert(#nested.pressed==0,'Do not close a stack and its children twice')
    assert(#generic.pressed==0,'Do not activate notification bodies')
    assert(#menu.pressed==0,'Do not traverse application menu actions')
end
print('NOTIFICATION_DISMISS_TESTS_PASSED')
