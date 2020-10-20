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
popclickScrollEnabled = false
tssScrollDown = newScroller(0.02, -10)
scrollExcluded = { "iTerm2", "Terminal", "Code", "Code - Insiders" } -- "Emacs",
lastS = 0
timerS = nil
gradS = 0
timerSR = timer.doEvery(0.1, function()
                          gradS = gradS - 1
                          if gradS < 0 then
                            gradS = 0
                          end
end)
function stopTimerS()
  if timerS ~= nil then
    timerS:stop()
    timerS = nil
  end
end
function scrollHandler(evNum)
  -- hs.alert.show("Listening: " .. tostring(popclickListening) .. ", NH: " .. evNum)
  if not popclickListening then
    return
  end
  local durS = 0
  if evNum == 1 then
    lastS = os.time()
  else
    if lastS ~= 0 then
      durS = os.time() - lastS
    end
    lastS = 0
  end
  stopTimerS()
  appName = application.frontmostApplication():name()
  iterm_focus = exec_raw('/usr/local/bin/redis-cli --raw get iterm_focus')
  -- hs.alert.show("App: " .. appName .. ", iterm_focus: " .. iterm_focus .. ", NH: " .. evNum)
  iterm_focus = (iterm_focus == 'TERMINAL_WINDOW_BECAME_KEY\n')

  if popclickScrollEnabled then
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
  else
    if evNum == 3 then
      -- if appName == "mpv" then
      --   hs.eventtap.keyStroke({}, hs.keycodes.map['space'])
      -- end
    elseif evNum == 2 then
      -- if durS >= 1 then
      --   if appName == "mpv" then
      --     hs.eventtap.keyStroke({}, hs.keycodes.map['space'])
      --   end
      -- end
    elseif evNum == 1 then
      -- brishz("bello")
      if appName == "mpv" then
        -- brishz("bell-lm-timetoparty")
        -- timerS = hs.timer.doAfter(0.9, function() hs.eventtap.keyStroke({}, hs.keycodes.map['space']) end)
        timerS = timer.doEvery(0.01, function()
                                 gradS = gradS + 0.2
                                 if gradS > 30 then
                                   stopTimerS()
                                   gradS = 0
                                   hs.eventtap.keyStroke({}, hs.keycodes.map['space'])
                                 end
        end)
      end
    end
  end
end

function popclickPlayPause()
  -- alert.show("toggling popclick")
  if not popclickListening then
    lastS = 0
    listener:start() -- @workaround_lisflag
    alert.show("listening")
  else
    listener:stop() -- @workaround_lisflag
    lastS = 0
    scrollHandler(2) -- stop
    alert.show("stopped listening")
  end
  popclickListening = not popclickListening
  return popclickListening
end
function popclickScrollToggle()
  if popclickScrollEnabled then
    lastS = 0
    scrollHandler(2) -- stop
  end
  popclickScrollEnabled = not popclickScrollEnabled
  return popclickScrollEnabled
end
function popclickBttToggle()
  return popclickScrollToggle()
end
function popclickBttGet()
  return popclickScrollEnabled
end
function popclickInit()
  popclickListening = false
  popclickScrollEnabled = false
  local fn = scrollHandler
  listener = popclick.new(fn)
  popclickPlayPause()
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
enOnly = { "iTerm2", "Terminal", "Code", "Code - Insiders", "Emacs", "mpv" } -- "Emacs",
function appWatch(appName, event, app)
  -- alert.show("appWatch: " .. appName .. ", event: " .. tostring(event) .. ", app: " .. tostring(app), 7)
  local bshcode = ''
  if event == hs.application.watcher.activated then
    bshcode = bshcode .. 'nightshift-auto ; '
    if has_value(enOnly, appName) then
      bshcode = bshcode .. "input-lang-push en ; "
    else
      bshcode = bshcode .. "input-lang-pop ; "
    end
  end
  if bshcode ~= '' then
    brishzeval(bshcode)
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
                 appName = application.frontmostApplication():name()
                 -- brishz("true " .. hs.pasteboard.getContents())
                 if appName == "Screen Sharing" then
                   -- Doesn't work, test with `sleep 2 ; hs -c 'hs.eventtap.keyStrokes("hi jungle")'`
                   -- hs.eventtap.keyStrokes(hs.pasteboard.getContents())
                 else
                   -- This was slow even with input-lang-get-darwin-fast, so the fault is probably with hammerspoon itself?

                   -- Warning:hs.keycode: key 'c' not found in active keymap; using ANSI-standard US keyboard layout as fallback, returning '8'
                   -- eventtap.keyStroke({"cmd"}, 'a')
                   -- eventtap.keyStroke({"cmd"}, 'c')
                   eventtap.keyStroke({"cmd"}, 0)
                   eventtap.keyStroke({"cmd"}, 8)
                   local res = brishz(("lang-toggle %q"):format(hs.pasteboard.getContents()))
                   hs.eventtap.keyStrokes(tostring(res))
                 end
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
