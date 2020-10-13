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
  return trim1(exec_raw(s))
end
function trim1(s)
   return (s:gsub("^%s*(.-)%s*$", "%1"))
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
local scrollExcluded = { "iTerm2", "Terminal", "Emacs", "Code", "Code - Insiders" }
function scrollHandler(evNum)
  appName = application.frontmostApplication():name()
  iterm_focus = exec_raw('/usr/local/bin/redis-cli --raw get iterm_focus')
  hs.alert.show("App: " .. appName .. ", iterm_focus: " .. iterm_focus .. ", NH: " .. evNum)
  iterm_focus = (iterm_focus == 'TERMINAL_WINDOW_BECAME_KEY\n')
  if evNum == 1 then
    if iterm_focus or has_value(scrollExcluded, appName) then
      return
    end
    startScroll(tssScrollDown)
  elseif evNum == 2 then
    stopScroll(tssScrollDown) -- Don't exclude apps here or we'll have infinite scroll
  elseif evNum == 3 then
    if appName == "ReadKit" then
      eventtap.keyStroke({}, "j")
    elseif iterm_focus or has_value(scrollExcluded, appName) then
      return
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
    alert.show("stopped listening")
  end
  popclickListening = not popclickListening
end

function popclickInit()
  popclickListening = false
  local fn = scrollHandler
  listener = popclick.new(fn)
end
---
hyper = {"cmd","ctrl","alt","shift"}
hs.window.animationDuration = 0;
---
popclickInit()
popclickPlayPause()
