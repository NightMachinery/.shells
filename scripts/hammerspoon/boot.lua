-- hs.alert("loading")
---
nightdir = os.getenv("NIGHTDIR") or (os.getenv("HOME") .. "/scripts")
-- require("luarocks.loader")
-- `luarocks path`
package.path = package.path .. ';/usr/local/Cellar/luarocks/3.3.1/share/lua/5.4/?.lua;/usr/local/share/lua/5.4/?.lua;/usr/local/share/lua/5.4/?/init.lua;/usr/local/lib/lua/5.4/?.lua;/usr/local/lib/lua/5.4/?/init.lua;./?.lua;./?/init.lua;/Users/evar/.luarocks/share/lua/5.4/?.lua;/Users/evar/.luarocks/share/lua/5.4/?/init.lua;' .. nightdir ..  '/lua/?.lua'
package.cpath = package.cpath .. ';/usr/local/lib/lua/5.4/?.so;/usr/local/lib/lua/5.4/loadall.so;./?.so;/Users/evar/.luarocks/lib/lua/5.4/?.so'

require "pipe"

rex = require("rex_pcre2")

inspect = require "hs.inspect"
location = require "hs.location"
wifi = require "hs.wifi"
ipc = require "hs.ipc"
popclick = require "hs.noises"
application = require "hs.application"
window = require "hs.window"
hotkey = require "hs.hotkey"
keycodes = require "hs.keycodes"
fnutils = require "hs.fnutils"
alert = require "hs.alert"
screen = require "hs.screen"
grid = require "hs.grid"
hints = require "hs.hints"
timer = require "hs.timer"
appfinder = require "hs.appfinder"
applescript = require "hs.applescript"
eventtap = require "hs.eventtap"
json = require("hs.json")

-- chooser = require "hs.chooser"
plp = require 'pl.pretty'
--- * Hammerspoon module loader
function loadHammerspoonFile(path)
    dofile(nightdir .. "/hammerspoon/" .. path)
end

local hammerspoonCoreFiles = {
    "core/helpers.lua",
    "modal-mode.lua",
    "core/redis.lua",
    "core/wifi-watcher.lua",
    "core/hyper-mode.lua",
    "purple-mode.lua",
    "core/mouse.lua",
    "core/input-language.lua",
    "core/popclick.lua",
    "core/system-keys.lua",
    "core/choosers.lua",
    "core/app-hotkeys.lua",
    "core/window-media-bindings.lua",
    "core/stt.lua",
    "core/reload.lua",
}

for _, file in ipairs(hammerspoonCoreFiles) do
    loadHammerspoonFile(file)
end
