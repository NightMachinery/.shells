--- * Screens: one registry for every display decision
--- Every module that has to pick a screen asks here, rather than calling
--- hs.screen itself. Before this, each module picked its own: overlays went to
--- the primary screen, the mouse code to the focused window's, the level keys
--- to whatever zsh called `main' -- which is the *primary* screen again, under
--- the name Hammerspoon uses for the focused one. Nothing agreed, and a fix in
--- one module taught the next nothing. See docs/multi-monitor.md.
---
--- A record per attached screen:
---   screen    the hs.screen object
---   uuid      hs.screen:getUUID(), upper-cased: the stable identity. It is the
---             same value zsh reads from m1ddc as "System UUID", so both
---             sides can key state on it. The CGDirectDisplayID is not
---             guaranteed stable across a hotplug.
---   cgid      hs.screen:id(), the CGDirectDisplayID: what zsh's id:<n>
---             selector takes, valid until the next layout change.
---   name, internal, frame, fullFrame
---   role      `laptop' for the built-in panel, `external-1..N' for the rest
---             from left to right, unless a per-screen pref overrides it.
---
--- Per-screen prefs live in redis under `screen_prefs', a JSON object keyed by
--- UUID -- {"<UUID>": {"role": "desk"}} -- and never in this repository: a
--- UUID identifies a particular monitor. Only `role' is read so far.
---
--- Events, through Screens.on(event, fn):
---   layout    screens were added, removed, moved or resized
---   added     fn(record), once per screen that was not there before
---   removed   fn(uuid), once per screen that went away
---   active    the focused window's screen changed
---   focus     fn(app, screen, fromWindow) once focus has settled after an
---             activation or an active-screen change: the frontmost app and
---             its screen, and whether that screen came from one of the
---             app's own windows (see "The focused screen")
---
--- Layout changes come from hs.screen.watcher. The focused screen is read
--- from CoreGraphics' window list, not from hs.screen.mainScreen(), which
--- lags (see "The focused screen"), so that overlays that follow focus move
--- with it instead of staying where they first opened.

Screens = Screens or {}

Screens.subscribers = Screens.subscribers or { layout = {}, added = {}, removed = {}, active = {}, focus = {} }
Screens.subscribers.focus = Screens.subscribers.focus or {}

--- The `working' intent: which screen a user-directed action lands on when
--- it is not tied to typing or to the pointer. An enum, not a boolean:
---   "active"   the focused window's screen
---   "pointer"  the screen under the mouse
screens_working_policy = screens_working_policy or "active"

local cache = nil
local lastActiveUUID = nil
local knownUUIDs = Screens.knownUUIDs or {}
Screens.knownUUIDs = knownUUIDs

local function screenIsInternal(screen)
    return (screen:name() or ""):lower():match("built%-in") ~= nil
end
Screens.screenIsInternal = screenIsInternal

local function screenUUID(screen)
    local ok, u = pcall(function() return screen:getUUID() end)
    if ok and type(u) == "string" and u ~= "" then return u:upper() end
    return nil
end

--- The prefs by UUID, and whether redis answered. This file loads before
--- core/redis.lua, so the first build cannot read them at all.
local function prefsAll()
    if not redisGet then return {}, false end
    local raw, ok = redisGet("screen_prefs")
    if not ok then return {}, false end
    if type(raw) ~= "string" or raw == "" then return {}, true end
    local okj, decoded = pcall(hs.json.decode, raw)
    if not okj or type(decoded) ~= "table" then
        print("Screens: screen_prefs is not a JSON object; ignored")
        return {}, true
    end
    local out = {}
    for k, v in pairs(decoded) do out[tostring(k):upper()] = v end
    return out, true
end

local function build()
    local prefs, prefsRead = prefsAll()
    local records = {}
    for _, screen in ipairs(hs.screen.allScreens()) do
        records[#records + 1] = {
            screen = screen,
            uuid = screenUUID(screen),
            cgid = screen:id(),
            name = screen:name() or "",
            internal = screenIsInternal(screen),
            frame = screen:frame(),
            fullFrame = screen:fullFrame(),
        }
    end
    --- Left to right, then top to bottom: the order the focus keys cycle in,
    --- and the order the external roles are numbered in.
    table.sort(records, function(a, b)
        if a.fullFrame.x ~= b.fullFrame.x then return a.fullFrame.x < b.fullFrame.x end
        return a.fullFrame.y < b.fullFrame.y
    end)

    local n = 0
    for _, r in ipairs(records) do
        if r.internal then
            r.role = "laptop"
        else
            n = n + 1
            r.role = "external-" .. n
        end
        local p = r.uuid and prefs[r.uuid]
        if type(p) == "table" and type(p.role) == "string" and p.role ~= "" then
            r.role = p.role
        end
    end
    return records, prefsRead
end

--- Every attached screen, as records, left to right. A build made without
--- the prefs (redis not up yet) is not kept, so roles set in screen_prefs
--- apply as soon as redis answers rather than at the next display change.
local cachePrefsRead = false
function Screens.list()
    if not (cache and cachePrefsRead) then cache, cachePrefsRead = build() end
    return cache
end

--- Call after editing screen_prefs; nothing watches the key.
function Screens.invalidate()
    cache = nil
end

function Screens.byUUID(uuid)
    if not uuid then return nil end
    uuid = uuid:upper()
    for _, r in ipairs(Screens.list()) do
        if r.uuid == uuid then return r end
    end
    return nil
end

function Screens.record(screen)
    if not screen then return nil end
    local id = screen:id()
    for _, r in ipairs(Screens.list()) do
        if r.cgid == id then return r end
    end
    return nil
end

--- The screen `delta' places after `screen' in left-to-right order, wrapping.
function Screens.neighbour(screen, delta)
    local list = Screens.list()
    if #list == 0 then return nil end
    local at = 1
    local id = screen and screen:id()
    for i, r in ipairs(list) do
        if r.cgid == id then at = i break end
    end
    return list[((at - 1 + (delta or 1)) % #list) + 1].screen
end

--- The selector zsh's display commands take for this screen: id:<n>. The id,
--- not the UUID, because it is what zsh resolves fastest and it is only
--- carried for the length of one call.
function Screens.zshSelector(screen)
    return "id:" .. tostring(screen:id())
end

function Screens.on(event, fn)
    local list = Screens.subscribers[event]
    if not list then
        print("Screens.on: unknown event " .. tostring(event))
        return
    end
    table.insert(list, fn)
end

local function emit(event, ...)
    for _, fn in ipairs(Screens.subscribers[event]) do
        local ok, err = pcall(fn, ...)
        if not ok then print("Screens: " .. event .. " subscriber failed: " .. tostring(err)) end
    end
end

for _, r in ipairs(Screens.list()) do
    if r.uuid then knownUUIDs[r.uuid] = true end
end

--- ** Resolving a spec to screens
--- The one place a screen spec means something. Specs name an intent where
--- one exists, so a module says what it wants rather than how to find it:
---   all                     every screen
---   primary                 the menu-bar display (zsh calls this `main')
---   internal                built-in panel(s)
---   external, all_external  the rest
---   active, main            the focused window's screen (Screens.focusedScreen).
---                           `main' only because hs.screen.mainScreen() is
---                           meant to answer the same; do not confuse it
---                           with zsh's `main', which is `primary' here
---   mouse, pointer          the screen under the mouse
---   typing                  where keyboard input goes: the active screen
---   working                 by screens_working_policy, active or pointer
---   uuid:<U>, id:<n>        one screen by UUID or CGDirectDisplayID
---   role:<name>             by role, e.g. role:laptop, role:external-1
--- A spec that matches nothing falls back to the primary screen (e.g.
--- `internal' in clamshell), and an unknown one to every screen.
local function activeScreen()
    return Screens.focusedScreen() or hs.screen.primaryScreen()
end

local function pointerScreen()
    return hs.mouse.getCurrentScreen() or activeScreen()
end

local function filterRecords(pred)
    local out = {}
    for _, r in ipairs(Screens.list()) do
        if pred(r) then out[#out + 1] = r.screen end
    end
    return out
end

function Screens.target(spec)
    spec = spec or "all"

    local screens
    if spec == "all" then
        screens = hs.screen.allScreens()
    elseif spec == "primary" then
        screens = { hs.screen.primaryScreen() }
    elseif spec == "internal" then
        screens = filterRecords(function(r) return r.internal end)
    elseif spec == "all_external" or spec == "external" then
        screens = filterRecords(function(r) return not r.internal end)
    elseif spec == "active" or spec == "main" or spec == "typing" then
        screens = { activeScreen() }
    elseif spec == "mouse" or spec == "pointer" then
        screens = { pointerScreen() }
    elseif spec == "working" then
        if screens_working_policy == "pointer" then
            screens = { pointerScreen() }
        else
            if screens_working_policy ~= "active" then
                print("Screens: unknown screens_working_policy " .. tostring(screens_working_policy) .. " (using active)")
            end
            screens = { activeScreen() }
        end
    elseif type(spec) == "string" and spec:match("^uuid:") then
        local r = Screens.byUUID(spec:sub(6))
        screens = r and { r.screen } or {}
    elseif type(spec) == "string" and spec:match("^id:%d+$") then
        local want = tonumber(spec:sub(4))
        screens = filterRecords(function(r) return r.cgid == want end)
    elseif type(spec) == "string" and spec:match("^role:") then
        local want = spec:sub(6)
        screens = filterRecords(function(r) return r.role == want end)
    else
        print("Screens.target: unknown spec: " .. tostring(spec) .. " (falling back to 'all')")
        screens = hs.screen.allScreens()
    end

    if #screens == 0 then
        screens = { hs.screen.primaryScreen() }
    end

    return screens
end

--- Whether a spec's answer can change without any screen being added or
--- removed: these are the overlays that must follow focus or the pointer.
local kMovingSpecs = { active = true, main = true, typing = true, mouse = true, pointer = true, working = true }
function Screens.specMoves(spec)
    return kMovingSpecs[spec or "all"] == true
end

--- ** Following a spec
--- fn(screen) now, and again when Screens.target(spec)[1] is found to be a
--- different screen. It is looked at on every display change, and for a
--- moving spec on every active-screen change; nothing watches the pointer,
--- so a pointer spec is only as fresh as the latest focus or display
--- change. For state that lives outside Hammerspoon and must
--- track a screen (an app preference, say), so it is set once per change
--- rather than at every use. Returns forget(): call it when applying failed,
--- so the next event applies again even if the screen is the same.
function Screens.onTargetChange(spec, fn)
    local last = nil
    local function check()
        local s = Screens.target(spec)[1]
        local u = s and screenUUID(s)
        if u == last then return end
        last = u
        local ok, err = pcall(fn, s)
        if not ok then print("Screens.onTargetChange(" .. tostring(spec) .. "): " .. tostring(err)) end
    end
    Screens.on("layout", function() last = nil check() end)
    if Screens.specMoves(spec) then Screens.on("active", check) end
    check()
    return function() last = nil end
end

--- ** Windows on a screen, without Accessibility
--- hs.window.orderedWindows() and hs.window.allWindows() ask every running
--- process over Accessibility, and some take 1.5 s each to answer (see
--- core/app-hotkeys.lua). CoreGraphics' window list asks none: hs.window.list
--- gives every on-screen window front to back with its owner's pid, bounds
--- and layer, in 19 to 40 ms (measured 2026-10-02, 38 windows). Only the one
--- app whose window is picked is then asked for its hs.window.
---
--- Layer 0 holds normal windows. Floating ones sit above it: the kitty panel
--- at 4 and Brave's 24 px strip at 26 (both measured), and Hammerspoon's
--- alert canvases at the overlay level (alert/render.lua), higher still.

--- Smallest normal window worth focusing; layer-0 helper strips are smaller.
local kMinNormalW, kMinNormalH = 100, 60

--- Window id -> layer, merged from every windowStack() read, so a window
--- seen once is still known after it leaves the screen: an app hidden by its
--- hotkey's second press is off screen at any read before its next
--- activation. Reset to the latest read once it holds more than
--- kLayerCacheMax ids, so closed windows do not pile up.
local layerOfId, layerOfCount = {}, 0
local kLayerCacheMax = 1000

--- Every on-screen window, front to back:
---   { id, pid, layer, alpha, frame = hs.geometry rect }
--- CG bounds are global, top-left origin, the same space as hs.screen:frame().
function Screens.windowStack()
    local out = {}
    local ok, list = pcall(hs.window.list, false)
    if not ok or type(list) ~= "table" then return out end
    for _, w in ipairs(list) do
        local b = w.kCGWindowBounds
        if b and w.kCGWindowOwnerPID then
            local e = {
                id = w.kCGWindowNumber,
                pid = w.kCGWindowOwnerPID,
                layer = w.kCGWindowLayer or 0,
                alpha = w.kCGWindowAlpha or 1,
                frame = hs.geometry.rect(b.X, b.Y, b.Width, b.Height),
            }
            out[#out + 1] = e
            if layerOfId[e.id] == nil then layerOfCount = layerOfCount + 1 end
            layerOfId[e.id] = e.layer
        end
    end
    if layerOfCount > kLayerCacheMax then
        layerOfId, layerOfCount = {}, 0
        for _, e in ipairs(out) do
            layerOfId[e.id] = e.layer
            layerOfCount = layerOfCount + 1
        end
    end
    return out
end

--- The CoreGraphics layer window `id' had at the latest windowStack() that
--- listed it, or nil when none has. It reads nothing, so it is a hint: a
--- window that changes layer keeps its old answer until it is listed again,
--- and whether the window server ever reuses an id within a session is
--- unmeasured. The floating-window check (core/app-hotkeys.lua) therefore
--- trusts only a 0, where a wrong answer just skips the check, and reads the
--- list afresh for anything else.
function Screens.layerOf(id)
    if id == nil then return nil end
    return layerOfId[id]
end

--- The screen holding the centre of `frame', else the one it overlaps most.
function Screens.screenOfFrame(frame)
    local cx, cy = frame.x + frame.w / 2, frame.y + frame.h / 2
    local best, bestArea = nil, 0
    for _, r in ipairs(Screens.list()) do
        local f = r.fullFrame
        if cx >= f.x and cx < f.x + f.w and cy >= f.y and cy < f.y + f.h then return r.screen end
        local i = f:intersect(frame)
        if i.area > bestArea then best, bestArea = r.screen, i.area end
    end
    return best
end

--- Whether a windowStack entry is a normal window: layer 0, visible, and
--- not a helper strip.
function Screens.isNormalEntry(e)
    return e.layer == 0 and e.alpha > 0 and e.frame.w >= kMinNormalW and e.frame.h >= kMinNormalH
end

--- The normal windows on `screen', front to back, whose pid skip(pid) does
--- not reject. `stack' is a windowStack() to reuse, so one press reads the
--- list once.
function Screens.normalWindowsOn(screen, skip, stack)
    local out, id = {}, screen:id()
    for _, e in ipairs(stack or Screens.windowStack()) do
        if Screens.isNormalEntry(e) and not (skip and skip(e.pid)) then
            local s = Screens.screenOfFrame(e.frame)
            if s and s:id() == id then out[#out + 1] = e end
        end
    end
    return out
end

--- The hs.window for a windowStack entry, asking only its owning app.
function Screens.entryWindow(e)
    local app = hs.application.applicationForPID(e.pid)
    if not app then return nil end
    local ok, wins = pcall(function() return app:allWindows() end)
    if not ok then return nil end
    for _, w in ipairs(wins) do
        if w:id() == e.id then return w end
    end
    return nil
end

--- ** The focused screen
--- The screen of the window with keyboard focus, from the window list
--- above: the frontmost app's front normal window, else its front visible
--- window floating below the menu bar (the kitty panel is at layer 4), else
--- the screen the latest read found from a window, else
--- hs.screen.mainScreen().
---
--- Not hs.screen.mainScreen() first, though it means the same: it is
--- NSScreen.mainScreen inside Hammerspoon, and it falls behind. Measured
--- 2026-10-03, with the load average near 46: 1.2 s after hyper+/ brought
--- Brave's fullscreen window forward on the external screen it still
--- answered the laptop, while the window list and Accessibility (Brave's
--- focused window) both had the external screen at 0.15 s; once focus went
--- back to kitty on the laptop, it answered the external screen. Waiting
--- longer did not help either: hyper+; kept choosing the same screen (seen
--- the same day). Whether it lags on an idle machine is unmeasured. While
--- this config read it, everything that follows focus followed it late or
--- the wrong way: activations were filed under the screen just left
--- (core/app-hotkeys.lua), so a hide returned to an app on the other
--- screen, and hyper+z, Maccy's popup and hyper+; all went to the screen
--- focus had left.
---
--- The answer is kept and reused while the same app stays frontmost, for at
--- most kFocusTrustSeconds (kFocusGuessSeconds when it had to fall back to
--- mainScreen), so the callers that ask often, an alert per level-key
--- press, read nothing. It is read afresh kFocusSettleSeconds after every
--- activation, every active-screen change hs.screen.watcher reports, and
--- every focus change made here, and that read emits `focus', and `active'
--- when the screen changed. A frontmost app with no window on screen then
--- (a Space still sliding in, a Finder with every window closed) is read
--- once more after kFocusRetrySeconds.

-- kCGMainMenuWindowLevel; the menu bar and status items sit at and above it.
local kMenuBarLayer = 24

local kFocusTrustSeconds, kFocusGuessSeconds = 2, 0.25
local kFocusSettleSeconds, kFocusRetrySeconds = 0.1, 0.4

--- The screen of `pid''s front window in `stack' (a windowStack(), read
--- now when nil): its front normal window, else its front visible window of
--- a normal size floating below the menu bar. nil when it has neither.
function Screens.windowScreenOf(pid, stack)
    if not pid then return nil end
    local floating = nil
    for _, e in ipairs(stack or Screens.windowStack()) do
        if e.pid == pid then
            if Screens.isNormalEntry(e) then return Screens.screenOfFrame(e.frame) end
            if not floating and e.layer > 0 and e.layer < kMenuBarLayer and e.alpha > 0
               and e.frame.w >= kMinNormalW and e.frame.h >= kMinNormalH then
                floating = e
            end
        end
    end
    return floating and Screens.screenOfFrame(floating.frame) or nil
end

-- { pid, screen, untilAt }: the latest read, and until when it is trusted.
local focusCache = nil

-- The screen the latest read found from a window. When the frontmost app
-- shows none, focus is taken to be still there: kitty is frontmost with its
-- panel hidden after a dialog closes, and hyper+z then sent the panel to the
-- screen mainScreen named, which was the other one (seen 2026-10-03).
local lastWindowScreen = nil

local function stillAttached(screen)
    return screen ~= nil and Screens.record(screen) ~= nil
end

local function frontmost()
    local ok, front = pcall(hs.application.frontmostApplication)
    if not (ok and front) then return nil, nil end
    local okp, pid = pcall(function() return front:pid() end)
    return front, okp and pid or nil
end

--- The focused screen, read now, and the frontmost app, and whether the
--- screen came from one of its windows rather than a fallback. `stack' is a
--- windowStack() to reuse.
function Screens.focusedScreenRead(stack)
    local front, pid = frontmost()
    local s = Screens.windowScreenOf(pid, stack)
    local fromWindow = s ~= nil
    if fromWindow then
        lastWindowScreen = s
    elseif stillAttached(lastWindowScreen) then
        s = lastWindowScreen
    else
        s = hs.screen.mainScreen() or hs.screen.primaryScreen()
    end
    focusCache = {
        pid = pid,
        screen = s,
        untilAt = hs.timer.secondsSinceEpoch() + (fromWindow and kFocusTrustSeconds or kFocusGuessSeconds),
    }
    return s, front, fromWindow
end

--- The focused screen, from the latest read while it still applies.
function Screens.focusedScreen()
    local c = focusCache
    if c and hs.timer.secondsSinceEpoch() < c.untilAt then
        local _, pid = frontmost()
        if pid == c.pid then return c.screen end
    end
    return (Screens.focusedScreenRead())
end

local function onLayout()
    Screens.invalidate()
    local now = {}
    for _, r in ipairs(Screens.list()) do
        if r.uuid then now[r.uuid] = r end
    end
    for u in pairs(knownUUIDs) do
        if not now[u] then
            knownUUIDs[u] = nil
            emit("removed", u)
        end
    end
    for u, r in pairs(now) do
        if not knownUUIDs[u] then
            knownUUIDs[u] = true
            emit("added", r)
        end
    end
    --- macOS can fire this several times per display change; every
    --- subscriber is idempotent and cheap, so no debouncing.
    emit("layout")
    local s = Screens.focusedScreenRead()
    lastActiveUUID = s and screenUUID(s)
end

local focusCheckPending = false

local function focusCheck(isRetry)
    if not isRetry then focusCheckPending = false end
    local s, front, fromWindow = Screens.focusedScreenRead()
    local u = s and screenUUID(s)
    if u ~= lastActiveUUID then
        lastActiveUUID = u
        emit("active")
    end
    emit("focus", front, s, fromWindow)
    if front and not fromWindow and not isRetry then
        hsAfter(kFocusRetrySeconds, function() focusCheck(true) end)
    end
end

--- Read the focused screen again once focus has settled. Activations and
--- hs.screen.watcher call it; so should anything that moves focus without
--- an activation, between two windows of one app.
function Screens.recheckFocus()
    if focusCheckPending then return end
    focusCheckPending = true
    hsAfter(kFocusSettleSeconds, function() focusCheck(false) end)
end

do
    local s = Screens.focusedScreenRead()
    lastActiveUUID = s and screenUUID(s)
end

if Screens.watcher then Screens.watcher:stop() end
Screens.watcher = hs.screen.watcher.newWithActiveScreen(function(activeChanged)
    if activeChanged then
        Screens.recheckFocus()
    else
        onLayout()
    end
end)
Screens.watcher:start()

if Screens.appWatcher then Screens.appWatcher:stop() end
Screens.appWatcher = hs.application.watcher.new(function(_, event)
    if event == hs.application.watcher.activated then Screens.recheckFocus() end
end)
Screens.appWatcher:start()

--- ** Moving focus between screens
--- hyper+; and hyper+shift+; (bound in core/app-hotkeys.lua). Screens are
--- taken left to right and wrap, so with two it is a toggle and with more it
--- walks across them.
---
--- Focus and the pointer move together. Half of this config follows the
--- focused window's screen and half the pointer's (see the spec list above),
--- and a keyboard jump that left the pointer behind would send the next
--- pointer-side action -- avy with screens_working_policy=pointer, a click --
--- back to the screen just left.

local kFocusBandId = "screen-focus"

local function focusBand(screen, text)
    alert_gateway(text, {
        id = kFocusBandId,
        seconds = 0.8,
        flashSeconds = 0,
        screens = "id:" .. tostring(screen:id()),
        peek = false,
    })
end

local function centreOf(frame)
    return hs.geometry.point(frame.x + frame.w / 2, frame.y + frame.h / 2)
end

--- The frontmost normal window on `screen' as an hs.window, or nil. It used
--- to be hs.window.orderedWindows() filtered by screen, which asks every app
--- over Accessibility (see "Windows on a screen" above).
function Screens.frontWindowOn(screen, stack)
    for _, e in ipairs(Screens.normalWindowsOn(screen, nil, stack)) do
        local w = Screens.entryWindow(e)
        if w then return w end
    end
    return nil
end
local frontWindowOn = Screens.frontWindowOn

--- What focusNext focuses before a screen's front normal window, by name:
---   fn(screen, stack) -> the frame it focused, or nil to pass.
--- The kitty panel registers one (core/kitty-panel.lua): it floats over its
--- screen's windows but is not a normal window.
Screens.focusHandlers = Screens.focusHandlers or {}

--- Focus the frontmost window on the next screen. With no window there,
--- nothing can take focus -- macOS focuses windows, not screens -- so the
--- pointer goes over anyway and the band says so; faking it by focusing the
--- Finder desktop moves focus somewhere unpredictable.
function Screens.focusNext(delta)
    local from = Screens.focusedScreen()
    local to = Screens.neighbour(from, delta or 1)
    if not to or (from and to:id() == from:id()) then
        focusBand(to or hs.screen.primaryScreen(), "only one screen")
        return
    end
    local r = Screens.record(to)
    local name = r and r.name or "?"

    -- A floating-window check still waiting for its timer would pull focus
    -- back to the app being left (core/app-hotkeys.lua).
    if appFloatingSupersede then appFloatingSupersede() end

    local stack = Screens.windowStack()
    for _, handler in pairs(Screens.focusHandlers) do
        local ok, frame = pcall(handler, to, stack)
        if ok and frame then
            Screens.recheckFocus()
            hs.mouse.absolutePosition(centreOf(frame))
            focusBand(to, "\u{2192} " .. name)
            return
        end
    end

    local w = frontWindowOn(to, stack)
    if w then
        w:focus()
        -- Two windows of one app activate nothing.
        Screens.recheckFocus()
        hs.mouse.absolutePosition(centreOf(w:frame()))
        focusBand(to, "\u{2192} " .. name)
    else
        hs.mouse.absolutePosition(centreOf(to:frame()))
        focusBand(to, "no windows on " .. name)
    end
end

local function onScreen(w, screen)
    local s = w:screen()
    return s ~= nil and s:id() == screen:id()
end

--- hs.window:moveToScreen, keeping the frame relative to the screen (it
--- scales it) and clamped inside it, and whether the window landed there:
--- Hammerspoon ignores the result of every Accessibility write, so a window
--- that refuses the move would otherwise pass for moved. Hammerspoon 1.1.1
--- already switches the app's AXEnhancedUserInterface off around the move
--- (-[HSwindow setFrame:] in HSuicore.m). An assistive app sets that
--- attribute on an app (Chromium's source names VoiceOver), and the app
--- reacts to it; Thunderbird had it on here (2026-10-02), set by something
--- not identified.
local function moveTo(w, to)
    w:moveToScreen(to, false, true, 0)
    return onScreen(w, to)
end

--- The newest window id CoreGraphics lists now, taken before a fullscreen
--- change so that its stand-ins (below) can be told apart from windows made
--- before it. The window server hands ids out in increasing order: every id
--- in the traces behind this grew with time (2026-10-03). A list of the
--- app's ids would not do, since it holds only the Spaces showing at the
--- time, and a leftover in another Space comes into view when the window
--- leaves fullscreen (seen: the wait then ran its full kSettleSeconds).
local function newestWindowId()
    local newest = 0
    for _, e in ipairs(Screens.windowStack()) do
        if e.id > newest then newest = e.id end
    end
    return newest
end

--- The stand-ins listed now: windows of `pid' newer than `sinceId', at
--- layer 0 and the exact size of a whole screen. While macOS animates a
--- window into or out of fullscreen, CoreGraphics does not list the window
--- itself on screen and the app shows such a stand-in on each screen
--- involved (measured on Brave 2026-10-03). Also whether the window itself
--- is listed.
local function standInsNow(w, pid, sinceId)
    local id = w:id()
    local seen, found = false, {}
    for _, e in ipairs(Screens.windowStack()) do
        if e.id == id then
            seen = true
        elseif e.pid == pid and e.id > sinceId and e.layer == 0 and e.alpha > 0 then
            for _, r in ipairs(Screens.list()) do
                if e.frame:equals(r.fullFrame) then found[#found + 1] = { id = e.id, screen = r.screen } break end
            end
        end
    end
    return found, seen
end

--- Where a leaked stand-in is put: outside every screen. Its position is
--- the one thing it lets be set, and it stayed where it was put (measured
--- 2026-10-03).
local kParkAt = { x = -20000, y = -20000 }

local function parkIfStandIn(w)
    local ok, sub = pcall(function() return w:subrole() end)
    if not (ok and sub == "AXUnknown") then return false end
    return (pcall(function()
        hs.axuielement.windowElement(w):setAttributeValue("AXPosition", kParkAt)
    end))
end

--- Moves `app''s leaked fullscreen stand-ins (see whenSettled) out of
--- sight: windows with the AXUnknown subrole, at layer 0, the exact size of
--- a whole screen. Only the Spaces showing now are searched: Brave lists
--- no window of the others over Accessibility, so a stand-in elsewhere
--- waits until its Space is shown. Returns how many it moved. For the
--- console, as Screens.parkStandIns("com.brave.Browser").
function Screens.parkStandIns(app)
    if type(app) == "string" then app = hs.application.get(app) end
    if not app then return 0 end
    local pid, n = app:pid(), 0
    for _, e in ipairs(Screens.windowStack()) do
        if e.pid == pid and e.layer == 0 then
            for _, r in ipairs(Screens.list()) do
                if e.frame:equals(r.fullFrame) then
                    local w = Screens.entryWindow(e)
                    if w and parkIfStandIn(w) then n = n + 1 end
                    break
                end
            end
        end
    end
    return n
end

--- done(true) once w:isFullScreen() == want, the frame has held still for
--- one poll, and the animation is over: the window is listed again and no
--- stand-in newer than `sinceId' (newestWindowId) is. done(false) after
--- kSettleSeconds, unless only the animation was left, which is printed and
--- then taken as done.
---
--- The animation is waited for because cutting it short leaks its stand-in.
--- With only the first two conditions, the move came 0.3 to 0.6 s after
--- leaving fullscreen while both stand-ins were still up, and the one on
--- the screen left behind stayed there, a still picture of the window, after
--- the window itself had been closed (measured 2026-10-03, a scratch Brave
--- window; leaving and entering fullscreen with nothing in between leaked
--- nothing). Such a leftover stays in its Space until the app quits
--- (assumed; the two seen here were still up 5 and 20 minutes later). It
--- has the AXUnknown subrole, no close button and no action but AXRaise,
--- so nothing here can close it, but its position can be set: see
--- Screens.parkStandIns.
local kSettlePoll, kSettleSeconds = 0.1, 3
local function whenSettled(w, want, pid, sinceId, done)
    local deadline = hs.timer.secondsSinceEpoch() + kSettleSeconds
    local last = nil
    local function poll()
        local ok, f = pcall(function() return w:isFullScreen() == want and w:frame() or nil end)
        if not ok then return done(false) end
        local still = f ~= nil and last ~= nil and f:equals(last)
        local animating = false
        if still then
            local okt, standIns, seen = pcall(standInsNow, w, pid, sinceId)
            animating = okt and (#standIns > 0 or not seen)
        end
        if still and not animating then return done(true) end
        last = f
        if hs.timer.secondsSinceEpoch() > deadline then
            if still then
                print("Screens.moveWindowNext: the fullscreen animation still seemed to run after "
                      .. kSettleSeconds .. " s; going on")
            end
            return done(still)
        end
        hsAfter(kSettlePoll, poll)
    end
    hsAfter(kSettlePoll, poll)
end

--- One move at a time: the fullscreen dance takes a second or two, and a
--- second press in the middle of it would move a window that is still
--- animating. The watchdog frees the key should a step die without calling
--- back, which would otherwise leave every later press saying "still
--- moving".
local moving = nil
local kMoveWatchdogSeconds = 2 * kSettleSeconds + 2

--- Windows that move some other way than Accessibility, by name:
---   fn(w, to, done) -> true when it took the move, then done(ok, note)
--- once it is over; false to leave it to the Accessibility move. The kitty
--- panel registers one (core/kitty-panel.lua): kitty keeps its own record of
--- the panel's screen, and lays the panel out on that screen again at its
--- next re-layout.
Screens.moveHandlers = Screens.moveHandlers or {}

--- Move the focused window to the next screen, then focus it there and
--- bring the pointer along. A native fullscreen window cannot be moved at
--- all (its AXPosition is not settable: measured on Thunderbird
--- 2026-10-02, where the old version of this silently did nothing), so it
--- leaves fullscreen, moves, and goes fullscreen again on the new screen.
function Screens.moveWindowNext(delta)
    if moving then
        focusBand(Screens.focusedScreen(), "still moving a window")
        return
    end
    -- Milliseconds since the press at each step, for the console line, so a
    -- slow move can be traced to the step that took the time.
    local t0 = hs.timer.absoluteTime()
    local marks = {}
    local function mark(step)
        marks[#marks + 1] = string.format("%s %.0f", step, (hs.timer.absoluteTime() - t0) / 1e6)
    end
    local w = hs.window.focusedWindow()
    if not w then
        focusBand(Screens.focusedScreen(), "no focused window")
        return
    end
    local from = w:screen()
    local to = Screens.neighbour(from, delta or 1)
    if not to or to:id() == from:id() then
        focusBand(from, "only one screen")
        return
    end
    local app = w:application()
    local appName = app and app:name() or "window"
    local r = Screens.record(to)
    local toName = r and r.name or "?"

    local token = {}
    moving = token
    local watchdog = hsAfter(kMoveWatchdogSeconds, function()
        if moving == token then
            moving = nil
            print("Screens.moveWindowNext: moving " .. appName .. " did not finish; key freed")
        end
    end)
    local function timings()
        return " (ms since the press: " .. table.concat(marks, ", ") .. ")"
    end
    local function finish(ok, note, how)
        if moving ~= token then return end
        moving = nil
        hsCancel(watchdog)
        if not ok then
            focusBand(from, "could not move " .. appName .. " to " .. toName .. (note or ""))
            mark("gave up")
            print("Screens.moveWindowNext: could not move " .. appName .. (note or "") .. timings())
            return
        end
        pcall(function()
            w:focus()
            hs.mouse.absolutePosition(centreOf(w:frame()))
        end)
        mark("focused")
        -- The window changed screens under the same frontmost app.
        Screens.recheckFocus()
        focusBand(to, "\u{2192} " .. toName .. "  (moved " .. appName .. ")")
        mark("band")
        print("Screens.moveWindowNext: moved " .. appName .. " to " .. toName .. (how or "") .. timings())
    end

    for name, handler in pairs(Screens.moveHandlers) do
        local ok, took = pcall(handler, w, to, function(moved, note) finish(moved, note, " (" .. name .. ")") end)
        if not ok then print("Screens.moveWindowNext: " .. name .. ": " .. tostring(took)) end
        if ok and took then return end
    end

    local okf, full = pcall(function() return w:isFullScreen() end)
    mark("read")
    if not (okf and full) then
        local okm, moved = pcall(moveTo, w, to)
        mark("moved")
        return finish(okm and moved)
    end

    focusBand(from, "leaving fullscreen to move " .. appName)
    local pid = app and app:pid()
    local sinceId = newestWindowId()
    if not pcall(function() w:setFullScreen(false) end) then
        return finish(false, ": it would not leave fullscreen")
    end
    whenSettled(w, false, pid, sinceId, function(left)
        mark("out of fullscreen")
        if not left then return finish(false, ": it did not leave fullscreen") end
        local okm, moved = pcall(moveTo, w, to)
        moved = okm and moved
        mark("moved")
        -- Back into fullscreen whether or not it moved, so a failed move
        -- leaves the window fullscreen where it was, as it was found.
        local sinceBack = newestWindowId()
        pcall(function() w:setFullScreen(true) end)
        whenSettled(w, true, pid, sinceBack, function(back)
            mark("fullscreen again")
            if not moved then
                return finish(false, back and " (it is fullscreen again where it was)"
                                          or " after leaving fullscreen, and it did not go fullscreen again")
            end
            -- macOS picks the screen a window goes fullscreen on, so the
            -- move is only done if it is still on the new one.
            local oks, there = pcall(onScreen, w, to)
            if not (oks and there) then return finish(false, ": it ended up on another screen") end
            finish(true, nil, back and ", fullscreen again" or ", but it did not go fullscreen again")
            -- Should a stand-in leak anyway, move it out of sight, and say
            -- so when that fails, rather than leave a still picture of the
            -- window to pass for a second window. Older leftovers of the
            -- same app that this move brought into view go too.
            hsAfter(1, function()
                local okl, left = pcall(standInsNow, w, pid, sinceId)
                local leaked = okl and #left or 0
                local okp, parked = pcall(Screens.parkStandIns, app)
                parked = okp and parked or 0
                if leaked == 0 and parked == 0 then return end
                print(string.format("Screens.moveWindowNext: %d stand-in window(s) of %s left by this move; "
                                    .. "%d stand-in(s) moved out of sight", leaked, appName, parked))
                if leaked > parked then
                    focusBand(left[1].screen, "a leftover picture of " .. appName .. " stayed here")
                end
            end)
        end)
    end)
end
--- @end
