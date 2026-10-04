--- * Maccy's popup on the active screen (retired)
--- Retired 2026-10-04: Maccy's own "Screen center" -> "Active screen"
--- (popupScreen 0) does the job, and this code overwrote that choice at every
--- load and focus change. Kept for when Maccy's choice falls short again; to
--- use it, add "disabled/maccy-popup-screen.lua" to boot.lua's list after
--- core/app-hotkeys/main.lua. It needs core/screens.lua and gardenTask.
--
-- hyper+v passes ctrl+alt+cmd+shift+v through to Maccy (core/hyper-mode.lua),
-- and Maccy places its popup itself. Set to "screen center", Maccy 2.7.1
-- reads its `popupScreen' setting at every popup (Maccy/PopupPosition.swift
-- and Extensions/NSScreen+ForPopup.swift upstream): 0 falls back to
-- NSScreen.main inside Maccy, and with 0 the popup opened on the laptop
-- while you worked on the monitor (NSScreen.mainScreen falls behind focus
-- inside Hammerspoon, see "The focused screen" in core/screens.lua; inside
-- Maccy that is unmeasured); n means NSScreen.screens[n - 1], the order
-- hs.screen.allScreens() lists them in. Its "window center" setting is no
-- better: it centres on the front app's first CoreGraphics window at any
-- layer, which for Brave is a 24 px strip.
--
-- So popupScreen is kept pointing at the screen named by the spec
-- `maccy_popup_screens' (below; false leaves Maccy alone),
-- rewritten through `defaults' when Screens.onTargetChange sees that screen
-- change (on a focus or display change; nothing watches the pointer), and a
-- press costs nothing extra. One write runs at a time, and a change that
-- arrives meanwhile is written after it, newest only: two writes running at
-- once could finish in either order and leave the older screen.
--
-- Maccy is sandboxed, so its settings live in its container, and on macOS
-- 14 the first write there from a process Hammerspoon starts makes macOS ask
-- whether Hammerspoon may access data from other apps (seen 2026-10-03). The
-- write waits for that answer, hence the long timeout; a failed write is
-- forgotten, so the next screen change tries again.
if maccy_popup_screens == nil then maccy_popup_screens = "active" end

local maccyBundleID = "org.p0deje.Maccy"

if maccy_popup_screens and Screens then
    -- written: the index Maccy was last given; wanted: the newest asked for;
    -- running: a write is under way.
    local written, wanted, running = nil, nil, false
    local forget
    local function pump()
        if running or wanted == nil or wanted == written then return end
        local index = wanted
        running = true
        gardenTask("/usr/bin/defaults", { "write", maccyBundleID, "popupScreen", "-int", tostring(index) },
                   function(code, _, err)
                       running = false
                       if code == 0 then
                           written = index
                       else
                           print("Maccy popupScreen: defaults exited " .. tostring(code) .. ": " .. tostring(err))
                           written = nil
                           -- Not retried here, where a write that keeps
                           -- failing would loop; the next change tries again.
                           if wanted == index then wanted = nil end
                           if forget then forget() end
                       end
                       pump()
                   end, 120, nil, "maccy-popup-screen")
    end
    forget = Screens.onTargetChange(maccy_popup_screens, function(screen)
        if not screen then return end
        for i, s in ipairs(hs.screen.allScreens()) do
            if s:id() == screen:id() then
                wanted = i
                return pump()
            end
        end
    end)
end
