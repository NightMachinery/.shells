nightdir = os.getenv("NIGHTDIR") or (os.getenv("HOME") .. "/scripts")
-- require("luarocks.loader")
-- `luarocks path`
package.path = package.path .. ';/usr/local/Cellar/luarocks/3.3.1/share/lua/5.4/?.lua;/usr/local/share/lua/5.4/?.lua;/usr/local/share/lua/5.4/?/init.lua;/usr/local/lib/lua/5.4/?.lua;/usr/local/lib/lua/5.4/?/init.lua;./?.lua;./?/init.lua;/Users/evar/.luarocks/share/lua/5.4/?.lua;/Users/evar/.luarocks/share/lua/5.4/?/init.lua;' .. nightdir ..  '/lua/?.lua'
package.cpath = package.cpath .. ';/usr/local/lib/lua/5.4/?.so;/usr/local/lib/lua/5.4/loadall.so;./?.so;/Users/evar/.luarocks/lib/lua/5.4/?.so'

require "pipe"
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
-- chooser = require "hs.chooser"
plp = require 'pl.pretty'
---
function has_value (tab, val)
  for index, value in ipairs(tab) do
    if value == val then
      return true
    end
  end

  return false
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
-- gradMode = true
gradMode = false
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
  elseif gradMode then
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
      -- brishzeval("bello")
      if appName == "mpv" then
        -- brishzeval("bell-lm-timetoparty")
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
    -- alert.show("stopped popclickScroll")
  else
    -- alert.show("enabled popclickScroll")
  end
  popclickScrollEnabled = not popclickScrollEnabled
  return popclickScrollEnabled
end
function popclickBttToggle()
  -- return popclickScrollToggle()
  return popclickPlayPause()
end
function popclickBttGet()
  -- return popclickScrollEnabled
  return popclickListening
end
function popclickInit()
  popclickListening = false
  -- popclickScrollEnabled = false
  popclickScrollEnabled = true
  local fn = scrollHandler
  listener = popclick.new(fn)
  -- popclickPlayPause()
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
local inputEnglish = "com.apple.keylayout.US"
local inputPersian = "com.apple.keylayout.Persian-ISIRI2901"
--
hyper = {"cmd","ctrl","alt","shift"}
hs.window.animationDuration = 0;
hs.dockicon.hide() -- to appear above full-screen windows you must hide the Hammerspoon Dock icon
---
function langSetPer()
  hs.keycodes.currentSourceID(inputPersian)
end
function langSetEn()
  hs.keycodes.currentSourceID(inputEnglish)
end
function langSetToggle()
  if hs.keycodes.currentSourceID() == inputEnglish then
    langSetPer()
  else
    langSetEn()
  end
end
---
enOnly = { "iTerm2", "Terminal", "kitty", "Code", "Code - Insiders", "Emacs", "mpv" } -- "Emacs",
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
    bshcode = "{ " .. bshcode .. " } &>/dev/null & # appName: " .. appName .. ", event: " .. tostring(event)
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
                 -- brishzeval("true " .. hs.pasteboard.getContents())
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
                   local res
                   -- res = brishzeval2(("lang-toggle %q"):format(hs.pasteboard.getContents())) -- got into deadlocks since we hammerspoon in getting and setting the input lang now
                   if hs.keycodes.currentSourceID() == inputEnglish then
                     res = brishzeval2(("ecn %q | en2per"):format(hs.pasteboard.getContents()))
                     langSetPer()
                   else
                     res = brishzeval2(("ecn %q | per2en"):format(hs.pasteboard.getContents()))
                     langSetEn()
                   end
                   hs.eventtap.keyStrokes(tostring(res))
                 end
end)
---
function chis()
  -- https://www.hammerspoon.org/docs/hs.chooser.html
  -- @todo it'd probably be better if we put the URLs as `subtext`, and their titles as `text`. hs.chooser has this better than fzf.
  local tab = nil
  c = hs.chooser.new(function(x)
      if tab then tab:delete() end
      if not x then return end
      brishzeval2bg(("chis_clean %q | inargsf open"):format(x.text))
  end)
  c:placeholderText("Search history ...")
  c:width(95)
  c:rows(15)
  tab = hs.hotkey.bind('', 'tab', function()
                         local x = c:selectedRowContents()
                         if not x then
                           return
                         end
                         brishzeval2bg(("bell-lm-mhm ; chis_clean %q | inargsf open ; "):format(x.text))
  end)
  chis_first = true
  local timer
  c:choices(function()
      local q = c:query()
      local cmd = ("chis_find %q"):format(q)
      if chis_first then
        chis_first = false
        cmd = "deus " .. cmd
      end
      local res = brishzeval2(cmd)
      local out = {}
      for l in res:gmatch("([^\r\n]+)\r?\n?") do
        table.insert(out, {["text"] = l})
        -- table.insert(out, {["text"] = hs.styledtext.ansi(l)}) -- so slow it's completely broken
      end
      return out
  end)
  c:queryChangedCallback(function(query)
      if timer and timer:running() then
        timer:stop()
      end
      timer = hs.timer.doAfter(0.2, function() c:refreshChoicesCallback() end)
  end)
  c:show()
end
-- hs.hotkey.bind(hyper, "o", chis)
--
function ntagFinder()
  local timer
  local tab = nil
  local antitab = nil
  c = hs.chooser.new(function(x)
      if tab then tab:delete() end
      if antitab then antitab:delete() end
      if not x then return end
      brishzeval2bg(("ntag-finder-sel-add %q"):format(x.text))
  end)
  c:placeholderText("ntag ...")
  -- c:width(95)
  c:rows(11)
  tab = hs.hotkey.bind('', 'tab', function()
                         local x = c:selectedRowContents()
                         if not x then
                           return
                         end
                         -- brishzeval2bg(("ntag-finder-sel-add %q ; bell-lm-mhm"):format(x.text))
                         brishzeval2bg(("bell-lm-mhm ; ntag-finder-sel-add %q ; "):format(x.text))
  end)
  antitab = hs.hotkey.bind('shift', 'tab', function()
                         local x = c:selectedRowContents()
                         if not x then
                           return
                         end
                         -- brishzeval2bg(("ntag-finder-sel-rm %q ; bell-pp-piece"):format(x.text))
                         brishzeval2bg(("bell-pp-piece ; ntag-finder-sel-rm %q ; "):format(x.text))
  end)
  c:choices(function()
      local q = c:query()
      local cmd = ("ntag-select %q"):format(q)
      local res = brishzeval2(cmd)
      local out = {}
      for l in res:gmatch("([^\r\n]+)\r?\n?") do
        -- @upstreambug https://github.com/Hammerspoon/hammerspoon/issues/2574
        -- table.insert(out, {["text"] = hs.styledtext.ansi(l, {font={size=25}})})
        table.insert(out, {["text"] = l})
      end
      return out
  end)
  c:queryChangedCallback(function(query)
      if timer and timer:running() then
        timer:stop()
      end
      timer = hs.timer.doAfter(0.0, function() c:refreshChoicesCallback() end)
  end)
  c:show()
end
hs.hotkey.bind(hyper, "n", ntagFinder)
---
function anycomplete()
  local timer
  local myTask = nil
  local tab = nil
  local antitab = nil

  if hs.keycodes.currentSourceID() == inputEnglish then
    eventtap.keyStroke({"shift", "alt"}, hs.keycodes.map['left'])
  else
    eventtap.keyStroke({"shift", "alt"}, hs.keycodes.map['right'])
  end
  eventtap.keyStroke({"cmd"}, hs.keycodes.map['c'])
  c = hs.chooser.new(function(x)
      if tab then tab:delete() end
      if antitab then antitab:delete() end
      if not x then return end

      hs.eventtap.keyStrokes(x.text)
  end)
  c:placeholderText("anycomplete ...")
  c:width(70)
  c:rows(11)
  tab = hs.hotkey.bind('', 'tab', function()
                         local x = c:selectedRowContents()
                         if not x then
                           return
                         end
                         c:query(x.text)
                         refreshChoices()
  end)
  antitab = hs.hotkey.bind('shift', 'tab', function()
                         local x = c:selectedRowContents()
                         if not x then
                           return
                         end
                         hs.pasteboard.setContents(x.text)
                         c:hide()
  end)
  -- c:choices(function()
  --     local q = c:query()
  --     local cmd = ("autosuggestions-gateway %q"):format(q)
  --     print("cmd: " .. cmd)
  --     local res = brishzeval2(cmd)
  --     local out = {}
  --     for l in res:gmatch("([^\r\n]+)\r?\n?") do
  --       table.insert(out, {["text"] = l})
  --     end
  --     return out
  -- end)
  function refreshChoices()
    if myTask then
      myTask:terminate()
      myTask = nil
    end
    local q = c:query()
    local cmd = ("autosuggestions-gateway %q"):format(q)
    -- print("cmd: " .. cmd)
    myTask = hs.task.new("/usr/local/bin/brishz2.dash",
                function(exitCode, stdOut, stdErr)
                  local res = stdOut
                  local out = {}
                  for l in res:gmatch("([^\r\n]+)\r?\n?") do
                    table.insert(out, {["text"] = l})
                  end
                  c:choices(out)
                end, {cmd})
    if myTask  then
      myTask:start()
    end
  end
  c:queryChangedCallback(function(query)
      if timer and timer:running() then
        timer:stop()
      end
      -- timer = hs.timer.doAfter(0.3, function() c:refreshChoicesCallback() end)
      timer = hs.timer.doAfter(0.0, refreshChoices)
  end)
  c:query(hs.pasteboard.getContents())
  c:show()
  if hs.keycodes.currentSourceID() == inputEnglish then
    eventtap.keyStroke({}, hs.keycodes.map['right'])
  else
    eventtap.keyStroke({}, hs.keycodes.map['left'])
  end
end
hs.hotkey.bind(hyper, "g", anycomplete)
---
-- https://github.com/kovidgoyal/kitty/issues/45

-- hs.hotkey.bind(hyper, "k", function()
--   local app = hs.application.get("kitty")

--   if app then
--       if not app:mainWindow() then
--           app:selectMenuItem({"kitty", "New OS window"})
--       elseif app:isFrontmost() then
--           app:hide()
--       else
--           app:activate()
--       end
--   else
--       hs.application.launchOrFocus("kitty")
--       app = hs.application.get("kitty")
--   end

--   -- app:mainWindow():moveToUnit'[100,50,0,0]'
--   -- app:mainWindow().setShadows(false)
-- end)
---
function appHotkey(o)
  hs.hotkey.bind(o.modifiers or hyper, o.key, function()
                   appName = o.appName
                   -- use `sleep 2 ; reval-copy frontapp-get ; fsay hi` to get this

                   local app = hs.application.get(appName)

                   if app then
                     if app:isFrontmost() then
                       app:hide()
                     else
                       app:activate()
                     end
                   else
                     if o.launch then
                       hs.application.launchOrFocus(appName)
                       app = hs.application.get(appName)
                     end
                   end
  end)
end

appHotkey{ key='w', appName='com.google.Chrome' }
appHotkey{ key='o', appName='com.operasoftware.Opera' }
appHotkey{ key='c', appName='com.microsoft.VSCodeInsiders' }
appHotkey{ key='t', appName='com.tdesktop.Telegram' }
appHotkey{ key='l', appName='notion.id' }

hs.hotkey.bind(hyper, "d", function()
                 brishzeval("notif-dismiss.as")
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
brishzeval("bell-lm-eternalhappiness")
---
