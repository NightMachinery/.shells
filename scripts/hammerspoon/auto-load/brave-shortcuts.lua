-- Brave-only browser actions, ahead of webpage keyboard handlers.
-- Loaded by core/reload.lua's auto-load directory scan.
-- Cmd+. opens native Tab Search; Cmd+/ opens a new tab.
if BraveShortcuts then BraveShortcuts.stop() end

BraveShortcuts = { version = "1.0.2", hotkeys = {} }
local M = BraveShortcuts
local bundleID = "com.brave.Browser"
local log = hs.logger.new("brave-shortcuts", "warning")

local function braveInFront()
    local app = hs.application.frontmostApplication()
    if app and app:bundleID() == bundleID then return app end
end

local function unavailable(message)
    M.lastError = message
    log.w(message)
    alert_gateway("Brave shortcuts: " .. message, {
        id = "brave-shortcuts", color = "warn", seconds = 4,
    })
    return false
end

local function nativeWindow(app)
    local ax = hs.axuielement.applicationElement(app)
    ax:setTimeout(0.15)
    local win = ax:attributeValue("AXFocusedWindow")
    -- Chromium can return a focus proxy (AXGroup). Its AXWindow points to
    -- the real native window; searching the proxy would find no controls.
    if win and win:attributeValue("AXRole") ~= "AXWindow" then
        win = win:attributeValue("AXWindow")
    end
    if win and win:attributeValue("AXRole") == "AXWindow" then return win end
end

local function searchAlreadyFocused(app)
    local ax = hs.axuielement.applicationElement(app)
    ax:setTimeout(0.15)
    local element = ax:attributeValue("AXFocusedUIElement")
    if not element or element:attributeValue("AXRole") ~= "AXComboBox" then return false end
    local title = element:attributeValue("AXTitle")
    if type(title) ~= "string" or not title:lower():match("^search tabs") then return false end
    -- Tab Search renders browser WebUI inside a native alert-dialog window.
    -- An ARIA dialog in a page is *inside* its AXWebArea, so it cannot supply
    -- the native dialog ancestor outside that area required here.
    local sawWebArea, nativeDialog = false, false
    for _ = 1, 16 do
        local role = element:attributeValue("AXRole")
        if role == "AXWebArea" then sawWebArea = true end
        if sawWebArea and element:attributeValue("AXSubrole") == "AXApplicationAlertDialog" then
            nativeDialog = true
        end
        if role == "AXWindow" then return nativeDialog end
        element = element:attributeValue("AXParent")
        if not element then return false end
    end
    return false
end

local function isTabSearch(element, role)
    if role ~= "AXButton" and role ~= "AXPopUpButton" and role ~= "AXMenuButton" then return false end
    if element:attributeValue("AXEnabled") == false then return false end
    for _, attribute in ipairs({"AXTitle", "AXDescription"}) do
        local label = element:attributeValue(attribute)
        if type(label) == "string" then
            label = label:lower():gsub("%.%.%.$", ""):gsub("…$", "")
            if label == "tab search" or label == "search tabs" then return true end
        end
    end
    return false
end

local function findTabSearch(win)
    local cached = M.searchButton
    if M.searchWindow == win and cached and cached:isValid() and
        isTabSearch(cached, cached:attributeValue("AXRole")) then return cached end
    M.searchButton, M.searchWindow = nil, nil

    -- Stay in browser chrome. In particular, never descend into AXWebArea:
    -- a page button named "Tab search" must not become a browser action.
    local skipChildren = {
        AXWebArea = true, AXRadioButton = true, AXTab = true,
        AXRow = true, AXTextField = true, AXStaticText = true,
        AXMenu = true, AXMenuBar = true,
    }
    local queue, index = {{element = win, depth = 0}}, 1
    local started = hs.timer.absoluteTime()
    while index <= #queue and index <= 250 and hs.timer.absoluteTime() - started < 5e8 do
        local entry = queue[index]
        index = index + 1
        local element = entry.element
        local role = element:attributeValue("AXRole")
        if isTabSearch(element, role) then
            M.searchWindow, M.searchButton = win, element
            return element
        end
        if entry.depth < 14 and not skipChildren[role] then
            for _, child in ipairs(element:attributeValue("AXChildren") or {}) do
                queue[#queue + 1] = {element = child, depth = entry.depth + 1}
                if #queue >= 250 then break end
            end
        end
    end
end

function M.newTab()
    local app = braveInFront()
    if not app then return false end
    hs.axuielement.applicationElement(app):setTimeout(0.15)
    -- Select the native menu action, rather than sending Cmd+T to the page.
    if app:selectMenuItem({"File", "New Tab"}) then
        M.lastError = nil
        return true
    end
    return unavailable("New Tab menu action unavailable")
end

function M.tabSearch()
    local app = braveInFront()
    if not app then return false end
    if searchAlreadyFocused(app) then M.lastError = nil; return true end
    local win = nativeWindow(app)
    local button = win and findTabSearch(win)
    if button and button:performAction("AXPress") then
        M.lastError = nil
        return true
    end
    M.searchButton, M.searchWindow = nil, nil
    return unavailable("Tab Search button unavailable; show the browser tab strip and try again")
end

local function schedule(action)
    local app = braveInFront()
    if not app or M.pending then return end
    local pid = app:pid()
    -- Return from the hotkey handler before any accessibility round trips.
    -- Recheck focus at dispatch, so switching apps never acts on a stale one.
    M.pending = hs.timer.doAfter(0, function()
        M.pending = nil
        local current = braveInFront()
        if not current or current:pid() ~= pid then return end
        local ok, err = pcall(action)
        if not ok then unavailable(tostring(err)) end
    end)
end

function M.sync()
    local active = braveInFront() ~= nil
    for _, binding in ipairs(M.hotkeys) do
        if active then
            if not binding:enable() then unavailable("could not register a browser hotkey") end
        else
            binding:disable()
        end
    end
end

function M.stop()
    if M.pending then M.pending:stop(); M.pending = nil end
    if M.watcher then M.watcher:stop(); M.watcher = nil end
    for _, binding in ipairs(M.hotkeys) do binding:delete() end
    M.hotkeys = {}
    M.searchButton, M.searchWindow = nil, nil
end

M.hotkeys = {
    hs.hotkey.new({"cmd"}, ".", function() schedule(M.tabSearch) end),
    hs.hotkey.new({"cmd"}, "/", function() schedule(M.newTab) end),
}
M.watcher = hs.application.watcher.new(function(_, event)
    if event == hs.application.watcher.activated or
        event == hs.application.watcher.deactivated or
        event == hs.application.watcher.terminated then M.sync() end
end):start()
M.sync()
