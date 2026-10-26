--- * Dismissing every on-screen notification
--- hyper+d, and `notif-os-dismiss-all' in zsh, which calls notifDismissAll
--- here. Presses each notification's own "Close" button (a stack's "Clear All")
--- over Accessibility, straight from Hammerspoon, which already holds that
--- grant.
---
--- It replaced osascript on applescript/notif-dismiss-v2.jxa, which goes
--- through System Events: about 0.2 s to start osascript, 160 ms to list the
--- notifications and 25 ms or more per close, against about 2 ms each here
--- (2026-10-11). The script stays as zsh's fallback for when Hammerspoon is
--- not answering.
--- @duplicateCode/b88a767cde3e47cb1bad4b855a8989b8: applescript/notif-dismiss-v2.jxa
---
--- By default it closes one snapshot: what was on screen when it was called.
--- A notification that arrives mid-run is one nobody has read yet, so it stays.
--- `{ includeArrivals = true }' repeats passes until nothing new is left.
--- A closed notification stays in the tree for about 700 ms while it animates
--- out, so those passes cannot tell "still there" from "on its way out" by
--- counting; they remember what they pressed and press only the rest. Either
--- way, a last look once the animations are over warns if anything it pressed
--- did not go.
---
--- Shell interface:
---   hs -c 'notifDismissAll()'
---   hs -c 'notifDismissAll({ includeArrivals = true })'

--- Per call. NotificationCenter answers in about 2 ms; this only bounds how
--- long a hung NotificationCenter can stall Hammerspoon's main thread.
notifDismissAxTimeoutSeconds = notifDismissAxTimeoutSeconds or 0.5
notifDismissPassGapSeconds = notifDismissPassGapSeconds or 0.15
notifDismissMaxPasses = notifDismissMaxPasses or 10
--- After the last press: the ~700 ms close animation, with room to spare.
notifDismissSettleSeconds = notifDismissSettleSeconds or 1.5

local function notifAx(element)
    if element then
        pcall(function() element:setTimeout(notifDismissAxTimeoutSeconds) end)
    end
    return element
end

--- The name of the action that dismisses a group, or nil. Custom actions are
--- named "Name:Close\nTarget:0x0\nSelector:(null)"; reading the title from the
--- name saves an actionDescription round trip per action.
local function notifCloseAction(group)
    local close
    for _, name in ipairs(notifAx(group):actionNames() or {}) do
        local title = name:match("^Name:([^\n]*)")
        if title == "Clear All" then return name end
        if title == "Close" then close = name end
    end
    return close
end

--- Find dismissible groups without assuming a fixed Accessibility depth.
--- macOS 27 inserts another wrapper above the scroll area. Search windows
--- only, stop below a dismissible stack, and never press generic AXPress.
local function notifGroups()
    local app = hs.application.get("com.apple.notificationcenterui")
    if not app then return {} end
    local axApp = notifAx(hs.axuielement.applicationElement(app))
    local groups, visited = {}, 0
    local function walk(element, depth)
        if not element or depth > 12 or visited >= 256 then return end
        visited = visited + 1
        element = notifAx(element)
        if element:attributeValue("AXRole") == "AXGroup" and notifCloseAction(element) then
            groups[#groups + 1] = element
            return
        end
        for _, child in ipairs(element:attributeValue("AXChildren") or {}) do
            walk(child, depth + 1)
        end
    end
    for _, window in ipairs(axApp and axApp:attributeValue("AXWindows") or {}) do
        walk(window, 0)
    end
    return groups
end

local function notifContains(elements, element)
    for _, e in ipairs(elements) do
        if e == element then return true end
    end
    return false
end

--- Dismisses every notification on screen; opts.includeArrivals also those
--- that arrive while it runs. Returns at once; later passes and the final
--- check run on timers. Failures go to the console and an alert.
function notifDismissAll(opts)
    local includeArrivals = opts and opts.includeArrivals
    local pressed = {}
    local pass = 0

    local function fail(msg)
        print("notifDismissAll: " .. msg)
        alert_gateway("Could not dismiss notifications: " .. msg, { color = "warn" })
    end

    local function settle()
        local stuck = 0
        for _, group in ipairs(notifGroups()) do
            if notifContains(pressed, group) then stuck = stuck + 1 end
        end
        if stuck > 0 then fail(stuck .. " notification(s) did not close") end
    end

    local function step()
        pass = pass + 1
        local ok, err = pcall(function()
            local fresh = 0
            local groups = notifGroups()
            -- Bottom-up, like the JXA script, though AX element refs, unlike
            -- System Events' positional ones, survive the list shifting.
            for i = #groups, 1, -1 do
                local group = groups[i]
                if not notifContains(pressed, group) then
                    fresh = fresh + 1
                    local action = notifCloseAction(group)
                    if action then group:performAction(action) end
                    pressed[#pressed + 1] = group
                end
            end
            if fresh == 0 or not includeArrivals then
                if #pressed > 0 then hsAfter(notifDismissSettleSeconds, settle) end
            elseif pass >= notifDismissMaxPasses then
                fail("notifications kept arriving for " .. pass .. " passes")
            else
                hsAfter(notifDismissPassGapSeconds, step)
            end
        end)
        if not ok then fail(tostring(err)) end
    end

    step()
end
