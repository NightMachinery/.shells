-- Shared native browser shortcuts; retain the established Brave console alias.
if BrowserShortcuts then BrowserShortcuts.stop()
elseif BraveShortcuts then BraveShortcuts.stop() end
BrowserShortcuts={version="2.0.0",contexts={}}
local B=BrowserShortcuts
local registry=dofile(nightdir .. "/hammerspoon/browser/registry.lua")
local factory=dofile(nightdir .. "/hammerspoon/browser/controller.lua")
function B.context(id)
    for _,browser in ipairs(registry.browsers) do
        if browser.id==id then
            if not B.contexts[id] then B.contexts[id]=factory(browser) end
            return B.contexts[id]
        end
    end
end
function B.sync()
    local app=hs.application.frontmostApplication()
    local browser=app and registry.find(app:bundleID())
    -- Disable the outgoing context before registering an incoming chord.
    for id,context in pairs(B.contexts) do if not browser or id~=browser.id then context.sync() end end
    if browser then B.context(browser.id).sync();B.activeBrowser=browser.id else B.activeBrowser=nil end
end
function B.stop()
    if B.watcher then B.watcher:stop() end
    for _,context in pairs(B.contexts) do context.stop() end
end
BraveShortcuts=B.context("brave")
B.watcher=hs.application.watcher.new(function(_,event)
    if event==hs.application.watcher.activated or event==hs.application.watcher.deactivated or
        event==hs.application.watcher.terminated then B.sync() end
end):start()
B.sync()
