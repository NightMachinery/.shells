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
---
--- The watcher is hs.screen.watcher.newWithActiveScreen, the variant that
--- also reports active-screen changes, so overlays that follow focus can move
--- with it instead of staying where they first opened.

Screens = Screens or {}

Screens.subscribers = Screens.subscribers or { layout = {}, added = {}, removed = {}, active = {} }

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

local function activeUUID()
    local s = hs.screen.mainScreen()
    return s and screenUUID(s)
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
    lastActiveUUID = activeUUID()
end

local function onActive()
    local u = activeUUID()
    if u == lastActiveUUID then return end
    lastActiveUUID = u
    emit("active")
end

for _, r in ipairs(Screens.list()) do
    if r.uuid then knownUUIDs[r.uuid] = true end
end
lastActiveUUID = activeUUID()

if Screens.watcher then Screens.watcher:stop() end
Screens.watcher = hs.screen.watcher.newWithActiveScreen(function(activeChanged)
    if activeChanged then
        onActive()
    else
        onLayout()
    end
end)
Screens.watcher:start()

--- ** Resolving a spec to screens
--- The one place a screen spec means something. Specs name an intent where
--- one exists, so a module says what it wants rather than how to find it:
---   all                     every screen
---   primary                 the menu-bar display (zsh calls this `main')
---   internal                built-in panel(s)
---   external, all_external  the rest
---   active, main            the focused window's screen. `main' only because
---                           hs.screen.mainScreen() is that; do not confuse
---                           it with zsh's `main', which is `primary' here
---   mouse, pointer          the screen under the mouse
---   typing                  where keyboard input goes: the active screen
---   working                 by screens_working_policy, active or pointer
---   uuid:<U>, id:<n>        one screen by UUID or CGDirectDisplayID
---   role:<name>             by role, e.g. role:laptop, role:external-1
--- A spec that matches nothing falls back to the primary screen (e.g.
--- `internal' in clamshell), and an unknown one to every screen.
local function activeScreen()
    return hs.screen.mainScreen() or hs.screen.primaryScreen()
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
--- fn(screen) now, and again whenever Screens.target(spec)[1] becomes a
--- different screen: on a display change, and for a moving spec on every
--- active-screen change. For state that lives outside Hammerspoon and must
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
--- at 4, Brave's 24 px strip at 26, the menu bar and Hammerspoon's canvases
--- higher still.

--- Smallest normal window worth focusing; layer-0 helper strips are smaller.
local kMinNormalW, kMinNormalH = 100, 60

--- Window id -> layer, from the latest windowStack(): every read replaces it
--- whole, so closed windows drop out.
local layerOfId = {}

--- Every on-screen window, front to back:
---   { id, pid, layer, alpha, frame = hs.geometry rect }
--- CG bounds are global, top-left origin, the same space as hs.screen:frame().
function Screens.windowStack()
    local out, layers = {}, {}
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
            layers[e.id] = e.layer
        end
    end
    layerOfId = layers
    return out
end

--- The CoreGraphics layer of window `id', or nil when it is not on screen.
--- Answered from the last windowStack() when that saw the window, so asking
--- about the same windows again costs nothing; a window it has not seen
--- costs one fresh read. A window that changes layer while staying open
--- keeps its old answer until the next read, which also replaces every
--- other answer, so an id is only ever answered for the window that held it
--- at the last read.
function Screens.layerOf(id)
    if id == nil then return nil end
    local l = layerOfId[id]
    if l ~= nil then return l end
    Screens.windowStack()
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
local function frontWindowOn(screen)
    for _, e in ipairs(Screens.normalWindowsOn(screen)) do
        local w = Screens.entryWindow(e)
        if w then return w end
    end
    return nil
end

--- Focus the frontmost window on the next screen. With no window there,
--- nothing can take focus -- macOS focuses windows, not screens -- so the
--- pointer goes over anyway and the band says so; faking it by focusing the
--- Finder desktop moves focus somewhere unpredictable.
function Screens.focusNext(delta)
    local from = hs.screen.mainScreen() or hs.mouse.getCurrentScreen()
    local to = Screens.neighbour(from, delta or 1)
    if not to or (from and to:id() == from:id()) then
        focusBand(to or hs.screen.primaryScreen(), "only one screen")
        return
    end
    local r = Screens.record(to)
    local name = r and r.name or "?"

    local w = frontWindowOn(to)
    if w then
        w:focus()
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

--- done(true) once w:isFullScreen() == want and the frame has held still
--- for one poll, or done(false) after kSettleSeconds. macOS animates the
--- way in and out of fullscreen; the move waits for that to finish rather
--- than racing it (whether a move made mid-animation fails is untested).
local kSettlePoll, kSettleSeconds = 0.1, 3
local function whenSettled(w, want, done)
    local deadline = hs.timer.secondsSinceEpoch() + kSettleSeconds
    local last = nil
    local function poll()
        local ok, f = pcall(function() return w:isFullScreen() == want and w:frame() or nil end)
        if not ok then return done(false) end
        if f and last and f:equals(last) then return done(true) end
        last = f
        if hs.timer.secondsSinceEpoch() > deadline then return done(false) end
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
        focusBand(hs.screen.mainScreen() or hs.screen.primaryScreen(), "still moving a window")
        return
    end
    local w = hs.window.focusedWindow()
    if not w then
        focusBand(hs.screen.mainScreen() or hs.screen.primaryScreen(), "no focused window")
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
    local function finish(ok, note, how)
        if moving ~= token then return end
        moving = nil
        hsCancel(watchdog)
        if not ok then
            focusBand(from, "could not move " .. appName .. " to " .. toName .. (note or ""))
            print("Screens.moveWindowNext: could not move " .. appName .. (note or ""))
            return
        end
        pcall(function()
            w:focus()
            hs.mouse.absolutePosition(centreOf(w:frame()))
        end)
        focusBand(to, "\u{2192} " .. toName .. "  (moved " .. appName .. ")")
        print("Screens.moveWindowNext: moved " .. appName .. " to " .. toName .. (how or ""))
    end

    for name, handler in pairs(Screens.moveHandlers) do
        local ok, took = pcall(handler, w, to, function(moved, note) finish(moved, note, " (" .. name .. ")") end)
        if not ok then print("Screens.moveWindowNext: " .. name .. ": " .. tostring(took)) end
        if ok and took then return end
    end

    local okf, full = pcall(function() return w:isFullScreen() end)
    if not (okf and full) then
        local okm, moved = pcall(moveTo, w, to)
        return finish(okm and moved)
    end

    focusBand(from, "leaving fullscreen to move " .. appName)
    if not pcall(function() w:setFullScreen(false) end) then
        return finish(false, ": it would not leave fullscreen")
    end
    whenSettled(w, false, function(left)
        if not left then return finish(false, ": it did not leave fullscreen") end
        local okm, moved = pcall(moveTo, w, to)
        moved = okm and moved
        -- Back into fullscreen whether or not it moved, so a failed move
        -- leaves the window fullscreen where it was, as it was found.
        pcall(function() w:setFullScreen(true) end)
        whenSettled(w, true, function(back)
            if not moved then
                return finish(false, back and " (it is fullscreen again where it was)"
                                          or " after leaving fullscreen, and it did not go fullscreen again")
            end
            -- macOS picks the screen a window goes fullscreen on, so the
            -- move is only done if it is still on the new one.
            local oks, there = pcall(onScreen, w, to)
            if not (oks and there) then return finish(false, ": it ended up on another screen") end
            finish(true, nil, back and ", fullscreen again" or ", but it did not go fullscreen again")
        end)
    end)
end
--- @end
