--- * Battery low banner
--- A pinned band across the top of every screen once the Mac is on battery and
--- macOS expects it to die soon, in two stages: amber "low" within
--- batteryLowMinutes, then pulsing red "critical" within
--- batteryCriticalMinutes. It stays up until the machine is on AC power again,
--- so the warning cannot be missed while there is still time to save work or
--- find a cable.
---
--- The stage is latched: it only ever escalates, and only AC power takes the
--- band down. The estimate is a running average that swings with load, so
--- without the latch a machine hovering around a threshold would raise and
--- drop the band (or flip its colour) every minute, and quitting a heavy app
--- to buy a few minutes would hide a warning that is still true.
---
--- Shell interface, for testing and inspection:
---   hs -c 'return batteryLowActive()'
---   hs -c 'return batteryLowState.stage'
---   hs -c 'batteryLowCheck({source = "Battery Power", minutes = 3, percent = 4})'
---   hs -c 'batteryLowCheck()'          -- re-read the real battery
---
--- A fake snapshot only lasts until the next real check, at most
--- kPollSeconds away, which on AC is what takes the test band down again.

--- Raise the band at or below this many minutes of estimated battery life.
batteryLowMinutes = batteryLowMinutes or 5
--- And turn it red at or below this many.
batteryCriticalMinutes = batteryCriticalMinutes or 2

--- The estimate is unavailable (-1) for a minute or two after unplugging, and
--- tends to be optimistic in the last few percent. At or below these charges
--- the stage is reached whatever the estimate says.
batteryLowPercentFloor = batteryLowPercentFloor or 2
batteryCriticalPercentFloor = batteryCriticalPercentFloor or 1

--- The fullscreen wash when the band appears or turns red. Longer than the
--- agent banner's, because this one is news the reader did not ask for.
batteryLowFlashSeconds = batteryLowFlashSeconds or 1

batteryLowState = batteryLowState or {
    -- nil, "low" or "critical"
    stage = nil,
}

local kBannerId = "battery-low"
local kClearedId = "battery-low-cleared"
local kPollSeconds = 30
--- Every check re-asserts the band, so this only matters if the checks stop:
--- then the band lapses on its own instead of claiming a stale reading for
--- hours.
local kBannerSeconds = 5 * 60

local kStages = {
    low = { rank = 1, label = "Battery low", color = "warn" },
    critical = { rank = 2, label = "Battery critical", color = "red-pulse-1" },
}

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

local function reached(b, minutes, percentFloor)
    if estimateKnown(b) and b.minutes <= minutes then
        return true
    end
    return b.percent ~= nil and b.percent <= percentFloor
end

--- The stage this reading alone calls for, before the latch.
local function stageOf(b)
    if not onBattery(b) then
        return nil
    end
    if reached(b, batteryCriticalMinutes, batteryCriticalPercentFloor) then
        return "critical"
    end
    if reached(b, batteryLowMinutes, batteryLowPercentFloor) then
        return "low"
    end
    return nil
end

local function bannerText(b, stage)
    local label = kStages[stage].label
    local percent = b.percent and string.format("%d%%", math.floor(b.percent + 0.5)) or "?%"
    if estimateKnown(b) then
        return string.format("%s: about %d min left (%s) - plug in now",
            label, math.floor(b.minutes), percent)
    end
    return string.format("%s: %s - plug in now", label, percent)
end

function batteryLowActive()
    return alert_gateway_exists(kBannerId)
end

--- snapshot is for testing: a table shaped like readBattery's. Returns the
--- stage the band is showing afterwards, or nil.
function batteryLowCheck(snapshot)
    local b = snapshot or readBattery()

    if not onBattery(b) then
        local wasUp = batteryLowState.stage ~= nil
        batteryLowState.stage = nil
        alert_gateway_dismiss(kBannerId)
        if wasUp then
            -- Confirms the cable actually took. A loose one is exactly the
            -- failure this module is here to catch.
            alert_gateway("On AC power - battery warning cleared", {
                id = kClearedId,
                color = "success",
                seconds = 3,
                position = "top",
            })
        end
        return nil
    end

    local previous = batteryLowState.stage
    local measured = stageOf(b)
    local stage = previous
    if measured and (not previous or kStages[measured].rank > kStages[previous].rank) then
        stage = measured
    end
    if not stage then
        return nil
    end
    batteryLowState.stage = stage
    alert_gateway_dismiss(kClearedId)

    -- Flash when the band appears or turns red, not on every refresh: the
    -- minutes in the text change as the estimate moves, and the engine would
    -- wash the screen for every changed message.
    local news = stage ~= previous or not batteryLowActive()
    alert_gateway(bannerText(b, stage), {
        id = kBannerId,
        color = kStages[stage].color,
        seconds = kBannerSeconds,
        pinned = true,
        flashSeconds = news and batteryLowFlashSeconds or 0,
        position = "top",
    })
    return stage
end

-- Globals on purpose: boot.lua loads modules with dofile, so a file-local
-- watcher or timer is collected and stops firing. The stops are for dofile'ing
-- this file into a live Hammerspoon, as in core/power-watcher.lua.
if batteryLowWatcher then batteryLowWatcher:stop() end
if batteryLowTimer then batteryLowTimer:stop() end

-- The watcher catches the cable going in or out at once. The poll is the
-- backstop for the estimate drifting past a threshold between whatever
-- moments macOS chooses to post a change; one read every kPollSeconds costs
-- nothing worth measuring.
batteryLowWatcher = hs.battery.watcher.new(function() batteryLowCheck() end)
batteryLowWatcher:start()
batteryLowTimer = hs.timer.doEvery(kPollSeconds, function() batteryLowCheck() end)

-- A reload starts with no bands, so a machine that is already low gets its
-- warning back straight away rather than at the next poll.
batteryLowCheck()
--- @end
