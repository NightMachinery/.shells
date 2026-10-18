--- * Battery low banner
--- A pinned band across the top of every screen once the Mac is on battery and
--- macOS expects it to die within batteryLowMinutes. It stays up until the
--- machine is on AC power again, so the warning cannot be missed while there
--- is still time to save work or find a cable.
---
--- It is latched: once raised, only AC power takes it down. The estimate is a
--- running average that swings with load, so without the latch a machine
--- hovering around the threshold would raise and drop the band every minute,
--- and quitting a heavy app to buy a few minutes would hide a warning that is
--- still true.
---
--- Shell interface, for testing and inspection:
---   hs -c 'return batteryLowActive()'
---   hs -c 'batteryLowCheck({source = "Battery Power", minutes = 3, percent = 2})'
---   hs -c 'batteryLowCheck()'          -- re-read the real battery
---
--- A fake snapshot only lasts until the next real check, at most
--- kPollSeconds away, which on AC is what takes the test band down again.

--- Raise the band at or below this many minutes of estimated battery life.
batteryLowMinutes = batteryLowMinutes or 5

--- The estimate is unavailable (-1) for a minute or two after unplugging, and
--- tends to be optimistic in the last few percent. At or below this charge the
--- band goes up whatever the estimate says.
batteryLowPercentFloor = batteryLowPercentFloor or 2

--- The fullscreen wash when the band first appears. Longer than the agent
--- banner's, because this one is news the reader did not ask for.
batteryLowFlashSeconds = batteryLowFlashSeconds or 1

batteryLowState = batteryLowState or {
    latched = false,
}

local kBannerId = "battery-low"
local kClearedId = "battery-low-cleared"
local kPollSeconds = 30
--- Every check re-asserts the band, so this only matters if the checks stop:
--- then the band lapses on its own instead of claiming a stale reading for
--- hours.
local kBannerSeconds = 5 * 60

local function readBattery()
    return {
        source = hs.battery.powerSource(),
        -- Minutes; -1 while macOS is still calculating, -2 on AC.
        minutes = hs.battery.timeRemaining(),
        percent = hs.battery.percentage(),
    }
end

local function onBattery(b)
    return b.source == "Battery Power"
end

local function estimateKnown(b)
    return b.minutes ~= nil and b.minutes >= 0
end

local function isLow(b)
    if not onBattery(b) then
        return false
    end
    if estimateKnown(b) and b.minutes <= batteryLowMinutes then
        return true
    end
    return b.percent ~= nil and b.percent <= batteryLowPercentFloor
end

local function bannerText(b)
    local percent = b.percent and string.format("%d%%", math.floor(b.percent + 0.5)) or "?%"
    if estimateKnown(b) then
        return string.format("Battery: about %d min left (%s) - plug in now",
            math.floor(b.minutes), percent)
    end
    return string.format("Battery at %s - plug in now", percent)
end

function batteryLowActive()
    return alert_gateway_exists(kBannerId)
end

--- snapshot is for testing: a table shaped like readBattery's. Returns whether
--- the band is up afterwards.
function batteryLowCheck(snapshot)
    local b = snapshot or readBattery()

    if not onBattery(b) then
        local wasLatched = batteryLowState.latched
        batteryLowState.latched = false
        alert_gateway_dismiss(kBannerId)
        if wasLatched then
            -- Confirms the cable actually took. A loose one is exactly the
            -- failure this module is here to catch.
            alert_gateway("On AC power - battery warning cleared", {
                id = kClearedId,
                color = "success",
                seconds = 3,
                position = "top",
            })
        end
        return false
    end

    if not (batteryLowState.latched or isLow(b)) then
        return false
    end
    batteryLowState.latched = true
    alert_gateway_dismiss(kClearedId)

    -- Flash only when the band is appearing, not on every refresh: the minutes
    -- in the text change as the estimate moves, and the engine would wash the
    -- screen for every changed message.
    local appearing = not batteryLowActive()
    alert_gateway(bannerText(b), {
        id = kBannerId,
        color = "rust",
        seconds = kBannerSeconds,
        pinned = true,
        flashSeconds = appearing and batteryLowFlashSeconds or 0,
        position = "top",
    })
    return true
end

-- Globals on purpose: boot.lua loads modules with dofile, so a file-local
-- watcher or timer is collected and stops firing. The stops are for dofile'ing
-- this file into a live Hammerspoon, as in core/power-watcher.lua.
if batteryLowWatcher then batteryLowWatcher:stop() end
if batteryLowTimer then batteryLowTimer:stop() end

-- The watcher catches the cable going in or out at once. The poll is the
-- backstop for the estimate drifting past the threshold between whatever
-- moments macOS chooses to post a change; one read every kPollSeconds costs
-- nothing worth measuring.
batteryLowWatcher = hs.battery.watcher.new(function() batteryLowCheck() end)
batteryLowWatcher:start()
batteryLowTimer = hs.timer.doEvery(kPollSeconds, function() batteryLowCheck() end)

-- A reload starts with no bands, so a machine that is already low gets its
-- warning back straight away rather than at the next poll.
batteryLowCheck()
--- @end
