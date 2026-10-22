nightdir='..'
-- Run from hammerspoon/: lua tests/browser-search-aliases.lua
local A=dofile("brave/search-aliases.lua")({unavailable=function() error("unexpected warning") end})
local rows={
 {text="Channel",url="https://chat.example.org/channel/one"},
 {text="Other channel",url="https://CHAT.EXAMPLE.ORG:443/channel/two"},
 {text="Lookalike",url="https://chat.example.org.evil.test/"},
 {text="Path only",url="https://example.org/chat.example.org"},
 {text="Credentials",url="https://chat.example.org@evil.test/"},
}
A.apply(rows,{["chat.example.org"]={"Team chat", 42}})
dofile("core/chooser-search.lua")
assert(#ChooserSearch.filter(rows,"team chat")==2)
assert(#ChooserSearch.filter(rows,"team two")==1)
A.apply(rows,{})
assert(#ChooserSearch.filter(rows,"team")==0)
print("browser search alias tests passed")
