nightdir='..'
local realDofile=dofile
local front='com.brave.Browser'
local order={}
local registry=realDofile('browser/registry.lua')
assert(registry.find('com.apple.Safari').kind=='safari'and registry.find('org.mozilla.firefox').kind=='firefox')
assert(registry.find('com.google.Chrome').kind=='chromium'and registry.find('company.thebrowser.dia').kind=='dia')
local ids={};for _,b in ipairs(registry.browsers)do assert(not ids[b.bundleID]);ids[b.bundleID]=true end
local function factory(browser)
 local c={browser=browser}
 function c.sync()c.enabled=front==browser.bundleID;order[#order+1]=(c.enabled and 'enable:'or'disable:')..browser.id end
 function c.stop()c.stopped=true end
 return c
end
dofile=function(path)
 if path:match('/browser/registry.lua$')then return registry end
 if path:match('/browser/controller.lua$')then return factory end
 error(path)
end
hs={application={frontmostApplication=function()return {bundleID=function()return front end}end,
 watcher={activated=1,deactivated=2,terminated=3,new=function(fn)return {start=function(self)return self end,stop=function()end}end}}}
realDofile('auto-load/brave-shortcuts.lua')
assert(BrowserShortcuts.contexts.brave.enabled and BraveShortcuts==BrowserShortcuts.contexts.brave)
assert(not BrowserShortcuts.contexts.safari)
front='org.mozilla.firefox';order={};BrowserShortcuts.sync()
assert(order[1]=='disable:brave'and order[2]=='enable:firefox')
assert(not BraveShortcuts.enabled and BrowserShortcuts.contexts.firefox.enabled)
front='unrelated.editor';BrowserShortcuts.sync()
for _,c in pairs(BrowserShortcuts.contexts)do assert(not c.enabled)end
local old=BrowserShortcuts.contexts
realDofile('auto-load/brave-shortcuts.lua');assert(old.brave.stopped and old.firefox.stopped)
assert(BrowserShortcuts.context('unknown')==nil)
print('Browser manager: allowlist, lazy contexts, outgoing-first hotkey scoping, console alias and reload cleanup passed')
