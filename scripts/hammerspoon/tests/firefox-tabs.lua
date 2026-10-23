-- Firefox AX transport: stable element identity, focused-window fallback,
-- selected URL only, no traversal into page tab imitations, no JS/replace.
local presses=0
local function element(attrs)
 local e={attrs=attrs or{}}
 function e:setTimeout()return self end
 function e:attributeValue(k)return self.attrs[k]end
 function e:isValid()return true end
 function e:performAction(action)assert(action=='AXPress');presses=presses+1;return self end
 return e
end
local group=element({AXRole='AXTabGroup'})
local first=element({AXRole='AXRadioButton',AXTitle='One',AXSelected=true,AXParent=group})
local second=element({AXRole='AXRadioButton',AXTitle='Two',AXSelected=false,AXParent=group})
group.attrs.AXChildren={first,second};group.attrs.AXTabs={first,second}
local spoof=element({AXRole='AXTabGroup',AXChildren={element({AXRole='AXRadioButton',AXTitle='Fake tab',AXParent=group})}})
local web=element({AXRole='AXWebArea',AXURL={url='https://example.org/current'},AXChildren={spoof}})
local root=element({AXRole='AXWindow',AXChildren={group,web}})
local window={id=function()return 42 end}
function root:asHSWindow()return window end
local ax=element({AXFocusedWindow=root})
local app={allWindows=function()return{}end,focusedWindow=function()return nil end}
local front=true;local nextID=0
hs={host={uuid=function()nextID=nextID+1;return tostring(nextID)end},application={get=function()return app end},
 axuielement={applicationElement=function()return ax end,windowElement=function()return root end},
 timer={absoluteTime=function()return 0 end}}
local F=dofile('browser/firefox.lua')({bundleID='org.mozilla.firefox',inFront=function()return front and app or nil end})
local r=F.list();assert(r.ok and #r.tabs==2 and r.activeTabId==r.tabs[1].tabId)
assert(r.tabs[1].url=='https://example.org/current'and r.tabs[2].url=='')
local id=r.tabs[2].tabId;assert(F.list().tabs[2].tabId==id)
local result;local function receive(v)result=v end
F.request({action='selectTab',tabId=id},receive);assert(result.ok and presses==1)
front=false;F.request({action='selectTab',tabId=id},receive);assert(not result.ok and presses==1)
F.request({action='activeState'},receive);assert(result.ok and not result.frontmost)
F.request({action='dom'},receive);assert(result.jsUnavailable and presses==1)
front=true;F.request({action='bookmark',url='https://example.org/current'},receive);assert(result.ok and result.reused and presses==2)
F.request({action='openMany',replace=true,items={{url='https://example.org/new'}}},receive);assert(not result.ok)
F.request({action='openMany',items={{url='javascript:alert(1)'}}},receive);assert(not result.ok)
F.stop();print('Firefox AX adapter: stable identities, fallback, page boundary, URL visibility and safe unsupported actions passed')
