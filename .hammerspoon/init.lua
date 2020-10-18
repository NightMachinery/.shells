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
  return exec("/usr/local/bin/brishzq.zsh " .. cmd)
end
function brishzeval(cmd)
  return exec(("/usr/local/bin/brishz.dash %q"):format(cmd))
end
-- Scroll functionality forked from https://github.com/trishume/dotfiles/blob/master/hammerspoon/hammerspoon.symlink/init.lua
function newScroller(delay, tick)
  return { delay = delay, tick = tick, timer = nil, mode = "pixel" }
end

function startScroll(scroller)
  if scroller.timer == nil then
    scroller.timer = timer.doEvery(scroller.delay, function()
                                     eventtap.scrollWheel({0,scroller.tick},{}, scroller.mode)
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
  -- hs.alert.show("Listening: " .. tostring(popclickListening) .. ", NH: " .. evNum)
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
    elseif appName == "Emacs" then
      tssScrollDown.tick = -1
      -- tssScrollDown.mode = "line"
      tssScrollDown.mode = "pixel"
      tssScrollDown.delay  = 0.1
    elseif appName == "mpv" then
      tssScrollDown.tick = -10
      tssScrollDown.mode = "pixel"
      tssScrollDown.delay  = 0.001
    else
      tssScrollDown.tick = -10
      tssScrollDown.mode = "pixel"
      tssScrollDown.delay  = 0.02
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
    elseif appName == "Emacs" then
      -- eventtap.scrollWheel({0,100},{}, "line") -- works accurately for text modes
      -- it seems any amount of scrolling seems the same to pdf-mode
      -- eventtap.scrollWheel({0,40000},{}, "pixel")
      for i = 1,3 do
        eventtap.scrollWheel({0,250},{}, "line") -- works accurately for text modes
      end
      -- eventtap.keyStroke({}, 'k')
    else
      eventtap.scrollWheel({0,250},{}, "pixel")
    end
  end
end

function popclickPlayPause()
  -- alert.show("toggling popclick")
  if not popclickListening then
    listener:start() -- @workaround_lisflag
    alert.show("listening")
  else
    listener:stop() -- @workaround_lisflag
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
  --- @workaround_lisflag we use the flag to control acting on the events to work around the bug that sometimes starting the listener can take ~6 seconds. Update: When that bug happens, it doesn't seem that listener can listen at all! Restarting hammerspoon completely seems to solve this issue.
  -- listener:start()
  ---
  brishzeval('awaysh hs-popclick-btt-refresh')
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
      brishzeval("input-lang-pop ; nightshift-on")
    else
      brishzeval("input-lang-push en ; nightshift-off")
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
---
hs.hotkey.bind(hyper, "p", function()
                 -- Warning:hs.keycode: key 'c' not found in active keymap; using ANSI-standard US keyboard layout as fallback, returning '8'
                 -- eventtap.keyStroke({"cmd"}, 'a')
                 -- eventtap.keyStroke({"cmd"}, 'c')
                 eventtap.keyStroke({"cmd"}, 0)
                 eventtap.keyStroke({"cmd"}, 8)
                 local res = brishz(("lang-toggle %q"):format(hs.pasteboard.getContents()))
                 hs.eventtap.keyStrokes(tostring(res))
end)
---
function reloadConfig(files)
  doReload = false
  for _,file in pairs(files) do
    if file:sub(-4) == ".lua" then
      doReload = true
    end
  end
  if doReload then
    hs.reload()
  end
end
myWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
brishz("bell-lm-eternalhappiness")
---
