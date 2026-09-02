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
--- Immediately after hs.ipc, and before anything else can print: hs.ipc has
--- just replaced the global `print` with one that recurses without bound
--- whenever a print happens while an `hs -c` command is being handled, wedging
--- the client. See hammerspoon/core/ipc-fix.lua. Loaded here with an explicit
--- dofile rather than from the core list below, because "before anything else
--- prints" is the whole point and loadHammerspoonFile is not defined yet.
dofile(nightdir .. "/hammerspoon/core/ipc-fix.lua")
popclick = require "hs.noises"
application = require "hs.application"
window = require "hs.window"
hotkey = require "hs.hotkey"
keycodes = require "hs.keycodes"
fnutils = require "hs.fnutils"
-- Stock hs.alert, kept reachable by hand for the console. Named for what it is
-- so that nothing reaches for it by accident: alerts go through alert_gateway,
-- which is v2. Mirrors [agfi:hs-alert-v1] on the zsh side.
alert_v1 = require "hs.alert"
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
    -- After modal-mode.lua: reuses its screen watcher and targetScreens().
    -- alert/ is one module in six files, listed rather than globbed so the
    -- order is visible here. state.lua first: it declares the AlertEngine
    -- table the rest hang off, and the alpha knob colors.lua bakes in.
    "alert/state.lua",
    "alert/colors.lua",
    "alert/markup.lua",
    "alert/layout.lua",
    "alert/render.lua",
    "alert/api.lua",
    -- After alert/: the banner is a wrapper over it.
    "core/agent-banner.lua",
    "core/redis.lua",
    "core/wifi-watcher.lua",
    -- Before core/audio-watcher.lua only for readability; there is no
    -- dependency between them.
    "core/audio-devices.lua",
    "core/audio-watcher.lua",
    "core/power-watcher.lua",
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
