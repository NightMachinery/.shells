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

local function prefsAll()
    if not redisGet then return {} end
    local raw, ok = redisGet("screen_prefs")
    if not ok or type(raw) ~= "string" or raw == "" then return {} end
    local decoded = hs.json.decode(raw)
    if type(decoded) ~= "table" then return {} end
    local out = {}
    for k, v in pairs(decoded) do out[tostring(k):upper()] = v end
    return out
end

local function build()
    local prefs = prefsAll()
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
    return records
end

--- Every attached screen, as records, left to right.
function Screens.list()
    if not cache then cache = build() end
    return cache
end

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
--- @end
