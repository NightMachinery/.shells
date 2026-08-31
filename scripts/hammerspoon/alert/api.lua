--- * Alert public interface
--- What callers touch: the alertV2* entry points and the alert_gateway* the rest
--- of the config goes through.

local kMinSeconds = 0.2
local kMaxSeconds = 4 * 60 * 60
local kDefaultSeconds = 5

--- ** Public interface
function alertV2Exists(id)
    return AlertEngine.findAlert(id) ~= nil
end

function alertV2Dismiss(id)
    local alert, index = AlertEngine.findAlert(id)
    if not alert then
        return false
    end
    if alert.timer then
        alert.timer:stop()
    end
    table.remove(alertEngineState.alerts, index)
    AlertEngine.syncTicker()
    AlertEngine.render()
    return true
end

function alertV2DismissAll()
    for _, alert in ipairs(alertEngineState.alerts) do
        if alert.timer then
            alert.timer:stop()
        end
    end
    alertEngineState.alerts = {}
    AlertEngine.syncTicker()
    AlertEngine.render()
    return true
end

--- opts (all optional):
---   id           re-showing the same id updates that alert in place
---   seconds      lifetime, default 5
---   markup       "plain" (default) or "md"; see ** Markup above
---   color        band colour, default dark slate. A table, or one of the
---                names default/warn/amber/crit/agent/free/notice
---   position     "top" (default), "center", "bottom"
---   flashSeconds fullscreen flash before settling into the band; 0 skips it
---   floodFade    fade the flash in and out, on by default. false for a hard
---                cut, a number for that many seconds on each ramp. Both ramps
---                fit inside flashSeconds rather than lengthening it
---   countdown    append "clears in M:SS", refreshed once a second
---   pinned       claim space before every unpinned alert, so a wall of text
---                elsewhere cannot push this one off the screen
---   screens      a ModalMode.targetScreens spec, default "all"
--- Returns the alert's id.
function alertV2(text, opts)
    opts = opts or {}
    text = tostring(text or "")

    -- Parse before anything else touches the text: everything downstream --
    -- the id heartbeat below included -- works on the marker-free string, so
    -- markup never leaks into layout or into a comparison.
    local runs = nil
    if opts.markup == "md" then
        text, runs = AlertEngine.parseMarkup(text)
    end

    local seconds = math.max(kMinSeconds,
        math.min(tonumber(opts.seconds) or kDefaultSeconds, kMaxSeconds))
    local flashSeconds = tonumber(opts.flashSeconds) or 0
    local fadeIn, fadeOut = AlertEngine.floodFadeRamps(opts.floodFade, flashSeconds)

    local id = opts.id
    if not id then
        alertEngineState.counter = alertEngineState.counter + 1
        id = "alert-" .. alertEngineState.counter
    end

    local alert, index = AlertEngine.findAlert(id)
    -- An unchanged message on an existing id is a heartbeat: push the deadline
    -- out, do not flash again. A long-running task would otherwise strobe.
    local unchanged = (alert ~= nil and alert.text == text)

    if not alert then
        alert = { id = id }
        table.insert(alertEngineState.alerts, alert)
    end
    alert.text = text
    alert.runs = runs
    alert.color = AlertEngine.resolveBandColor(opts.color)
    alert.position = AlertEngine.normalizePosition(opts.position)
    alert.countdown = opts.countdown and true or false
    alert.pinned = opts.pinned and true or false
    alert.screens = opts.screens
    alert.expiry = hs.timer.secondsSinceEpoch() + seconds

    if alert.timer then
        alert.timer:stop()
    end
    alert.timer = hs.timer.doAfter(seconds, function()
        alert.timer = nil
        alertV2Dismiss(id)
    end)

    if flashSeconds > 0 and not unchanged then
        -- Last flash wins: a second flashing alert replaces the flood rather
        -- than queueing behind it.
        if alertEngineState.floodTimer then
            alertEngineState.floodTimer:stop()
        end
        alertEngineState.flood = {
            color = alert.color,
            startedAt = hs.timer.secondsSinceEpoch(),
            duration = flashSeconds,
            fadeIn = fadeIn,
            fadeOut = fadeOut,
        }
        alertEngineState.floodTimer = hs.timer.doAfter(flashSeconds, function()
            alertEngineState.floodTimer = nil
            alertEngineState.flood = nil
            AlertEngine.destroyFlood()
        end)
    end

    AlertEngine.syncTicker()
    AlertEngine.hookScreenChange()
    AlertEngine.render()
    return id
end

--- Handy for a short message typed by hand, where quoting is the only problem:
---   hs -c 'alertV2FromBase64("aGVsbG8=")'
--- Not the shell entry point. `hammerspoon -c` wedges on payloads of a few
--- hundred characters - reproducibly, and it takes the whole ipc port down with
--- it - so anything that might be long goes through alertV2FromFile instead.
function alertV2FromBase64(encoded, opts)
    return alertV2(hs.base64.decode(encoded) or "", opts)
end

--- The shell entry point: the message is written to a file and only its path
--- travels over ipc, so length, quotes, newlines and unicode all stop being a
--- problem. The file is deleted here, which keeps the caller from having to
--- clean up after an alert that outlives it.
function alertV2FromFile(path, opts)
    local file = io.open(path, "r")
    if not file then
        return nil
    end
    local text = file:read("a")
    file:close()
    os.remove(path)
    -- Command output almost always ends in a newline, and an empty final band
    -- line is just wasted height.
    text = (text or ""):gsub("%s+$", "")
    return alertV2(text, opts)
end

--- ** Gateway
--- What the rest of the config calls. Nothing outside this file names an engine
--- directly, so changing which engine draws an alert -- or sending a subset of
--- them somewhere else entirely -- is an edit here rather than a sweep over
--- every caller. The zsh side has the same shape: [agfi:hs-alert] is a gateway
--- over [agfi:hs-alert-v2], with [agfi:hs-alert-v1] still beside it.
---
--- `opts' is passed through untouched, so callers use the option names
--- documented on alertV2 above.
function alert_gateway(text, opts)
    return alertV2(text, opts)
end

function alert_gateway_dismiss(id)
    return alertV2Dismiss(id)
end

function alert_gateway_exists(id)
    return alertV2Exists(id)
end
--- @end
