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
function scrollHandler(evNum)
  -- hs.alert.show("NH: " .. evNum)
  if evNum == 1 then
    startScroll(tssScrollDown)
  elseif evNum == 2 then
    stopScroll(tssScrollDown)
  elseif evNum == 3 then
    if application.frontmostApplication():name() == "ReadKit" then
      eventtap.keyStroke({}, "j")
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
