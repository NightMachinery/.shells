local ipc = require "hs.ipc"
local popclick = require "hs.noises"
local application = require "hs.application"
local window = require "hs.window"
local hotkey = require "hs.hotkey"
local keycodes = require "hs.keycodes"
local fnutils = require "hs.fnutils"
local alert = require "hs.alert"
local screen = require "hs.screen"
local grid = require "hs.grid"
local hints = require "hs.hints"
local timer = require "hs.timer"
local appfinder = require "hs.appfinder"
local applescript = require "hs.applescript"
local eventtap = require "hs.eventtap"
---
function has_value (tab, val)
  for index, value in ipairs(tab) do
    if value == val then
      return true
    end
  end

  return false
end
function exec_raw(cmd)
  local f = assert(io.popen(cmd, 'r'))
  local s = assert(f:read('*a'))
  f:close()
  return (s)
end
function exec(cmd)
  return trim1(exec_raw(cmd))
end
function trim1(s)
  return (s:gsub("^%s*(.-)%s*$", "%1"))
end
function brishz(cmd)
  exec("/usr/local/bin/brishzq.zsh " .. cmd)
end
-- Scroll functionality forked from https://github.com/trishume/dotfiles/blob/master/hammerspoon/hammerspoon.symlink/init.lua
function newScroller(delay, tick)
  return { delay = delay, tick = tick, timer = nil }
end

function startScroll(scroller)
  if scroller.timer == nil then
    scroller.timer = timer.doEvery(scroller.delay, function()
                                     eventtap.scrollWheel({0,scroller.tick},{}, "pixel")
    end)
  end
end

function stopScroll(scroller)
  if scroller.timer then
    scroller.timer:stop()
    scroller.timer = nil
  end
end

listener = nil
popclickListening = false
local tssScrollDown = newScroller(0.02, -10)
local scrollExcluded = { "iTerm2", "Terminal", "Code", "Code - Insiders" } -- "Emacs",
function scrollHandler(evNum)
  if not popclickListening then
    return
  end
  appName = application.frontmostApplication():name()
  iterm_focus = exec_raw('/usr/local/bin/redis-cli --raw get iterm_focus')
  -- hs.alert.show("App: " .. appName .. ", iterm_focus: " .. iterm_focus .. ", NH: " .. evNum)
  iterm_focus = (iterm_focus == 'TERMINAL_WINDOW_BECAME_KEY\n')
  if evNum == 1 then
    if iterm_focus or has_value(scrollExcluded, appName) then
      return
    end
    startScroll(tssScrollDown)
  elseif evNum == 2 then
    stopScroll(tssScrollDown) -- Don't exclude apps here or we'll have infinite scroll
  elseif evNum == 3 then
    if iterm_focus or has_value(scrollExcluded, appName) then
      return
    elseif appName == "python" then
      for i = 1,10 do
        eventtap.keyStroke({}, hs.keycodes.map['up'])
      end
    else
      eventtap.scrollWheel({0,250},{}, "pixel")
    end
  end
end

function popclickPlayPause()
  if not popclickListening then
    listener:start()
    alert.show("listening")
  else
    listener:stop()
    scrollHandler(2) -- stop
    alert.show("stopped listening")
  end
  popclickListening = not popclickListening
  return popclickListening
end

function popclickInit()
  popclickListening = false
  local fn = scrollHandler
  listener = popclick.new(fn)
  exec_raw('brishz.dash awaysh hs-popclick-btt-refresh')
end
function install()
  -- @bootstrap
  hs.ipc.cliInstall()
end
---
hyper = {"cmd","ctrl","alt","shift"}
hs.window.animationDuration = 0;
---
function appWatch(appName, event, app)
 -- alert.show("appWatch: " .. appName .. ", event: " .. tostring(event) .. ", app: " .. tostring(app), 7)
  if event == hs.application.watcher.activated then
    if appName ~= 'mpv' then
      brishz("nightshift-on")
    else
      brishz("nightshift-off")
    end
  end
end
appWatcher = hs.application.watcher.new(appWatch)
appWatcher:start()
---
popclickInit()
-- popclickPlayPause()
-- hs.urlevent.bind("popclickPlayPause", function(eventName, params)
--                    popclickPlayPause()
--                    -- alert.show("popclickListening: " .. tostring(popclickListening))
-- end)
