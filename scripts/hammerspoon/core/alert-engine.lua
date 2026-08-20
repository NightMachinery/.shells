--- * Alert engine (v2)
--- Coloured bands across the screen, stacking, one per live alert. This is the
--- generalisation of what used to be the single-purpose agent focus banner: the
--- banner is now just an alert with a crimson colour and a countdown.
---
--- Why bands instead of hs.alert's centred box:
---   - several alerts can be on screen at once, each with its own colour, so a
---     new one does not hide the one you were reading;
---   - long text wraps and the band grows, instead of being cut off or shrunk;
---   - an optional fullscreen flash makes one impossible to miss.
---
--- Nothing here takes focus and nothing swallows clicks: no canvas mouse events
--- are registered, so every canvas is inert to the pointer. Everything expires
--- on its own, so a caller that crashes cannot brand the screen permanently.
---
--- Shell interface:
---   hs -c 'alertV2("hello", { seconds = 5 })'
---   hs -c 'alertV2FromBase64("aGVsbG8=", { position = "bottom" })'
---   hs -c 'alertV2DismissAll()'
---
--- The base64 entry point exists because escaping quotes, newlines and unicode
--- through `hammerspoon -c` is a losing game; see hs-alert-v2 in
--- zshlang/auto-load/others/hammerspoon.zsh.

alertEngineState = alertEngineState or {
    alerts = {},         -- ordered oldest first; the render order too
    canvases = {},       -- { canvas, screen, position, stack }
    floodCanvases = {},
    flood = nil,         -- { color = ... } while a fullscreen flash is up
    floodTimer = nil,
    ticker = nil,
    hooked = false,
    counter = 0,
}

--- Dark slate. Quiet enough to live on screen for a few seconds without being
--- the loudest thing on the monitor.
alertV2DefaultColor = alertV2DefaultColor
    or { red = 0.16, green = 0.19, blue = 0.24, alpha = 0.95 }
--- Amber, the colour the agent banner shipped with, kept as an alternative:
-- alertV2DefaultColor = { red = 0.80, green = 0.36, blue = 0.02, alpha = 0.95 }

--- Crimson: "a machine is driving this screen, do not touch the keyboard".
alertV2AgentColor = alertV2AgentColor
    or { red = 0.62, green = 0.06, blue = 0.10, alpha = 1.0 }

--- Blue: the screen is yours again.
alertV2FreeColor = alertV2FreeColor
    or { red = 0.09, green = 0.055, blue = 0.42, alpha = 1.0 }

--- How opaque the fullscreen flash is. The alert's own colour, but see-through:
--- the flash has to be impossible to miss without blacking out the screen it
--- covers. The bands drawn on top of it keep their own opacity.
alertV2FloodAlpha = alertV2FloodAlpha or 0.33

--- Dimmed grey for the "earlier alerts hidden" notice, so it reads as chrome
--- rather than as another alert.
alertV2NoticeColor = alertV2NoticeColor
    or { red = 0.25, green = 0.25, blue = 0.28, alpha = 0.9 }

local kFont = "Menlo"
local kTextSize = 15
local kLineHeight = kTextSize * 1.4
local kPaddingX = 10
local kPaddingY = 6
local kMinBandHeight = 30
local kMinSeconds = 0.2
local kMaxSeconds = 4 * 60 * 60
local kDefaultSeconds = 5

--- No position may eat more than this fraction of a screen's usable height.
--- This is what makes hs-reval-alert (up to 30 lines of command output)
--- survivable: past the cap the text is truncated, and the band says so.
alertV2MaxStackFraction = alertV2MaxStackFraction or 0.45


local kPositions = { "top", "center", "bottom" }

--- ** Text measuring and wrapping
--- Menlo is monospaced, so wrapping is arithmetic on character counts rather
--- than a measuring call per candidate word. Measured once because point size
--- to advance width is not exactly 0.6 on every macOS release.
local cachedCharWidth = nil

local function charWidth()
    if cachedCharWidth then
        return cachedCharWidth
    end
    local ok, size = pcall(function()
        return hs.drawing.getTextDrawingSize(hs.styledtext.new(
            "MMMMMMMMMM",
            { font = { name = kFont, size = kTextSize } }))
    end)
    if ok and size and size.w and size.w > 0 then
        cachedCharWidth = size.w / 10
    else
        cachedCharWidth = kTextSize * 0.6
    end
    return cachedCharWidth
end

local function charLen(s)
    return utf8.len(s) or #s
end

local function charSub(s, i, j)
    if not utf8.len(s) then
        return s:sub(i, j)
    end
    local bi = utf8.offset(s, i)
    if not bi then
        return ""
    end
    local bj = #s
    if j then
        local after = utf8.offset(s, j + 1)
        bj = (after and (after - 1)) or #s
    end
    return s:sub(bi, bj)
end

--- Greedy word wrap, breaking mid-word only when a single word is longer than
--- the line. Leading whitespace is preserved, because command output is one of
--- the main callers and its indentation carries meaning.
local function wrapLine(raw, maxChars, out)
    if raw == "" then
        table.insert(out, "")
        return
    end
    while charLen(raw) > maxChars do
        local cut = nil
        for i = maxChars + 1, 2, -1 do
            if charSub(raw, i, i) == " " then
                cut = i
                break
            end
        end
        if cut then
            table.insert(out, charSub(raw, 1, cut - 1))
            raw = charSub(raw, cut + 1)
        else
            table.insert(out, charSub(raw, 1, maxChars))
            raw = charSub(raw, maxChars + 1)
        end
        if raw == "" then
            return
        end
    end
    table.insert(out, raw)
end

local function wrapText(text, maxChars)
    if maxChars < 1 then
        maxChars = 1
    end
    local out = {}
    for raw in (tostring(text) .. "\n"):gmatch("(.-)\n") do
        wrapLine(raw, maxChars, out)
    end
    if #out == 0 then
        out = { "" }
    end
    return out
end

--- ** Alert records
local function findAlert(id)
    for index, alert in ipairs(alertEngineState.alerts) do
        if alert.id == id then
            return alert, index
        end
    end
    return nil, nil
end

local function alertDisplayText(alert)
    if not alert.countdown then
        return alert.text
    end
    local left = math.max(0, math.floor(alert.expiry - hs.timer.secondsSinceEpoch()))
    return string.format("%s   -   clears in %d:%02d",
                         alert.text,
                         math.floor(left / 60),
                         left % 60)
end

local function normalizePosition(position)
    for _, known in ipairs(kPositions) do
        if position == known then
            return position
        end
    end
    return "top"
end

local function alertTargetsScreen(alert, screen)
    if not alert.screens or alert.screens == "all" then
        return true
    end
    for _, candidate in ipairs(ModalMode.targetScreens(alert.screens)) do
        if candidate:id() == screen:id() then
            return true
        end
    end
    return false
end

--- ** Layout
--- Returns nil when this position has nothing on this screen, otherwise
--- { x, w, bands = { { alert, lines, height, y, color } } } in draw order.
---
--- The stack always grows away from its anchor edge, oldest band at the anchor.
--- For a top stack that means the newest alert appears at the bottom and the
--- bands already on screen do not move, which is the whole point: an alert
--- arriving must not shift the words someone is mid-way through reading.
--- A bottom stack anchors at the bottom edge for the same reason, so there the
--- newest ends up on top.
local function layoutStack(screen, position)
    local work = screen:frame()
    local maxChars = math.max(1, math.floor((work.w - 2 * kPaddingX) / charWidth()))
    local budget = work.h * alertV2MaxStackFraction

    local list = {}
    for _, alert in ipairs(alertEngineState.alerts) do
        if alert.position == position and alertTargetsScreen(alert, screen) then
            table.insert(list, alert)
        end
    end
    if #list == 0 then
        return nil
    end

    -- Pinned alerts claim their space first, then the rest newest to oldest.
    -- Newest-first is so a burst never pushes off the one that just arrived;
    -- pinned-first is because the agent banner must survive somebody dumping a
    -- sixty-line command output on the same screen. Whatever budget is left
    -- goes to the older alerts, truncated if it has to be.
    local order = {}
    for index, alert in ipairs(list) do
        if alert.pinned then
            table.insert(order, index)
        end
    end
    for index = #list, 1, -1 do
        if not list[index].pinned then
            table.insert(order, index)
        end
    end

    -- Keep the first `fits` lines, with the last one saying what was cut. At
    -- one line the marker has to share it: a band that says only "(+59 more
    -- lines)" tells you nothing about which alert it was.
    local function truncate(lines, fits)
        if fits >= #lines then
            return lines
        end
        if fits > 1 then
            local kept = {}
            for line = 1, fits - 1 do
                table.insert(kept, lines[line])
            end
            table.insert(kept, string.format("... (+%d more lines)",
                                             #lines - (fits - 1)))
            return kept
        end
        local marker = string.format("  ... (+%d more lines)", #lines - 1)
        local head = lines[1]
        local room = maxChars - charLen(marker)
        if charLen(head) > room then
            head = charSub(head, 1, math.max(0, room))
        end
        return { head .. marker }
    end

    local oneLine = kLineHeight + 2 * kPaddingY

    local function allocate(remaining)
        -- Everyone visible gets one line before anyone gets two. Otherwise the
        -- newest alert, allocated first, eats the whole budget and the ones
        -- underneath it vanish entirely - and an alert reduced to a single line
        -- still tells you it happened, which a hidden one does not.
        local chosen, hidden = {}, 0
        for _, index in ipairs(order) do
            if remaining >= oneLine then
                remaining = remaining - oneLine
                table.insert(chosen, index)
            else
                hidden = hidden + 1
            end
        end

        -- Then the surplus, in the same order, so the pinned banner and the
        -- newest alert are the ones that get to be tall.
        local bands = {}
        for _, index in ipairs(chosen) do
            local alert = list[index]
            local lines = wrapText(alertDisplayText(alert), maxChars)
            local full = math.max(kMinBandHeight,
                                  #lines * kLineHeight + 2 * kPaddingY)
            local grant = math.max(0, math.min(full - oneLine, remaining))
            remaining = remaining - grant
            local height = oneLine + grant
            local fits = math.floor((height - 2 * kPaddingY) / kLineHeight)
            bands[index] = {
                alert = alert,
                lines = truncate(lines, fits),
                -- Centring is for a short message that fits on its line. A
                -- truncated one reads as a fragment of something longer, so it
                -- lines up with the multi-line bands instead.
                truncated = (fits < #lines),
                height = height,
                color = alert.color,
            }
        end

        -- Back into arrival order for drawing: allocation order is about who
        -- gets the pixels, not about where they end up on screen.
        local placed = {}
        for index = 1, #list do
            if bands[index] then
                table.insert(placed, bands[index])
            end
        end
        return placed, hidden
    end

    local bands, hidden = allocate(budget)
    if hidden > 0 then
        -- Re-run with room reserved for the notice, so that saying "N hidden"
        -- is not itself what pushed one more alert out.
        bands, hidden = allocate(budget - (kLineHeight + 2 * kPaddingY))
    end
    if hidden > 0 then
        table.insert(bands, 1, {
            lines = { string.format("... %d earlier alert(s) hidden", hidden) },
            height = kLineHeight + 2 * kPaddingY,
            color = alertV2NoticeColor,
        })
    end
    if #bands == 0 then
        return nil
    end

    local total = 0
    for _, band in ipairs(bands) do
        total = total + band.height
    end

    local top
    if position == "bottom" then
        top = work.y + work.h - total
        -- Oldest at the anchor edge means reversing: drawn top-down, the
        -- newest lands first and the oldest sits on the bottom edge.
        local reversed = {}
        for index = #bands, 1, -1 do
            table.insert(reversed, bands[index])
        end
        bands = reversed
    elseif position == "center" then
        -- The first band straddles the midline and the rest grow downward,
        -- rather than re-centring the whole stack every time one arrives.
        top = work.y + work.h / 2 - bands[1].height / 2
    else
        top = work.y
    end

    local y = top
    for _, band in ipairs(bands) do
        band.y = y
        y = y + band.height
    end

    return { x = work.x, w = work.w, bands = bands }
end

--- ** Rendering
local function canvasElements(stack, origin)
    local elements = {}
    for _, band in ipairs(stack.bands) do
        local x = stack.x - origin.x
        local y = band.y - origin.y
        table.insert(elements, {
            type = "rectangle",
            action = "fill",
            fillColor = band.color,
            frame = { x = x, y = y, w = stack.w, h = band.height },
        })
        -- hs.canvas has no vertical alignment for text, so the block is centred
        -- in the band by placing its own frame.
        local textHeight = #band.lines * kLineHeight
        table.insert(elements, {
            type = "text",
            text = table.concat(band.lines, "\n"),
            textColor = { white = 1 },
            textFont = kFont,
            textSize = kTextSize,
            textAlignment = (#band.lines == 1 and not band.truncated)
                and "center" or "left",
            frame = {
                x = x + kPaddingX,
                y = y + (band.height - textHeight) / 2,
                w = stack.w - 2 * kPaddingX,
                h = textHeight,
            },
        })
    end
    return elements
end

local function newCanvas(frame, level)
    local canvas = hs.canvas.new(frame)
    canvas:level(level)
    canvas:behavior(hs.canvas.windowBehaviors.canJoinAllSpaces
                        + hs.canvas.windowBehaviors.stationary
                        + hs.canvas.windowBehaviors.fullScreenAuxiliary)
    return canvas
end

local function destroyStrips()
    for _, record in ipairs(alertEngineState.canvases) do
        record.canvas:delete()
    end
    alertEngineState.canvases = {}
end

local function destroyFlood()
    for _, canvas in ipairs(alertEngineState.floodCanvases) do
        canvas:delete()
    end
    alertEngineState.floodCanvases = {}
end

--- The flood is a separate canvas per screen, one level above the strips, that
--- washes the whole display in the flashing alert's colour and then redraws
--- every band at exactly the geometry it already has. So when the flood is
--- deleted the words do not move, resize or reflow - only the colour drains
--- away from around them. A flash that re-centred its own text would yank it
--- out from under whoever started reading it.
---
--- The wash is deliberately see-through: it has to be impossible to miss, but
--- it covers every screen and it should not black out what you were looking at
--- to do it. The bands themselves keep their own opacity, so the alert stays
--- readable against whatever is underneath.
local function renderFlood()
    destroyFlood()
    local flood = alertEngineState.flood
    if not flood then
        return
    end
    local wash = {}
    for key, value in pairs(flood.color) do
        wash[key] = value
    end
    wash.alpha = alertV2FloodAlpha
    for _, screen in ipairs(ModalMode.targetScreens("all")) do
        local full = screen:fullFrame()
        local elements = {
            { type = "rectangle", action = "fill", fillColor = wash },
        }
        for _, position in ipairs(kPositions) do
            local stack = layoutStack(screen, position)
            if stack then
                for _, element in ipairs(canvasElements(stack, full)) do
                    table.insert(elements, element)
                end
            end
        end
        local canvas = newCanvas(full, hs.canvas.windowLevels.overlay + 1)
        canvas:appendElements(table.unpack(elements))
        canvas:show()
        table.insert(alertEngineState.floodCanvases, canvas)
    end
end

local function renderStrips()
    destroyStrips()
    if #alertEngineState.alerts == 0 then
        return
    end
    for _, screen in ipairs(ModalMode.targetScreens("all")) do
        for _, position in ipairs(kPositions) do
            local stack = layoutStack(screen, position)
            if stack then
                local canvas = newCanvas(
                    { x = stack.x, y = stack.bands[1].y, w = stack.w,
                      h = stack.bands[#stack.bands].y
                          + stack.bands[#stack.bands].height
                          - stack.bands[1].y },
                    hs.canvas.windowLevels.overlay)
                canvas:appendElements(table.unpack(
                    canvasElements(stack, { x = stack.x, y = stack.bands[1].y })))
                canvas:show()
                table.insert(alertEngineState.canvases, {
                    canvas = canvas,
                    screen = screen,
                    position = position,
                    stack = stack,
                })
            end
        end
    end
end

local function render()
    renderStrips()
    if alertEngineState.flood then
        renderFlood()
    end
end

--- ** Countdown ticker
local function anyCountdown()
    for _, alert in ipairs(alertEngineState.alerts) do
        if alert.countdown then
            return true
        end
    end
    return false
end

--- Rewrite the countdown digits without rebuilding the canvases, which would
--- flicker once a second. Falls back to a full render on the rare tick where
--- the new text wraps to a different number of lines.
local function tick()
    for _, record in ipairs(alertEngineState.canvases) do
        local work = record.screen:frame()
        local maxChars = math.max(1,
            math.floor((work.w - 2 * kPaddingX) / charWidth()))
        for index, band in ipairs(record.stack.bands) do
            if band.alert and band.alert.countdown then
                local lines = wrapText(alertDisplayText(band.alert), maxChars)
                if #lines ~= #band.lines then
                    render()
                    return
                end
                band.lines = lines
                record.canvas[index * 2].text = table.concat(lines, "\n")
            end
        end
    end
    if alertEngineState.flood then
        renderFlood()
    end
end

local function syncTicker()
    if anyCountdown() then
        if not alertEngineState.ticker then
            alertEngineState.ticker = hs.timer.doEvery(1, tick)
        end
    elseif alertEngineState.ticker then
        alertEngineState.ticker:stop()
        alertEngineState.ticker = nil
    end
end

local function hookScreenChange()
    if alertEngineState.hooked then
        return
    end
    -- A monitor arriving or leaving would otherwise leave bands on the old
    -- geometry, or missing from the new screen entirely. ModalMode already runs
    -- one screen watcher for every overlay in this config; do not start another.
    ModalMode.onScreenChange(function()
        if #alertEngineState.alerts > 0 then
            render()
        end
    end)
    alertEngineState.hooked = true
end

--- ** Public interface
function alertV2Exists(id)
    return findAlert(id) ~= nil
end

function alertV2Dismiss(id)
    local alert, index = findAlert(id)
    if not alert then
        return false
    end
    if alert.timer then
        alert.timer:stop()
    end
    table.remove(alertEngineState.alerts, index)
    syncTicker()
    render()
    return true
end

function alertV2DismissAll()
    for _, alert in ipairs(alertEngineState.alerts) do
        if alert.timer then
            alert.timer:stop()
        end
    end
    alertEngineState.alerts = {}
    syncTicker()
    render()
    return true
end

--- opts (all optional):
---   id           re-showing the same id updates that alert in place
---   seconds      lifetime, default 5
---   color        band colour, default dark slate
---   position     "top" (default), "center", "bottom"
---   flashSeconds fullscreen flash before settling into the band; 0 skips it
---   countdown    append "clears in M:SS", refreshed once a second
---   pinned       claim space before every unpinned alert, so a wall of text
---                elsewhere cannot push this one off the screen
---   screens      a ModalMode.targetScreens spec, default "all"
--- Returns the alert's id.
function alertV2(text, opts)
    opts = opts or {}
    text = tostring(text or "")

    local seconds = math.max(kMinSeconds,
        math.min(tonumber(opts.seconds) or kDefaultSeconds, kMaxSeconds))
    local flashSeconds = tonumber(opts.flashSeconds) or 0

    local id = opts.id
    if not id then
        alertEngineState.counter = alertEngineState.counter + 1
        id = "alert-" .. alertEngineState.counter
    end

    local alert, index = findAlert(id)
    -- An unchanged message on an existing id is a heartbeat: push the deadline
    -- out, do not flash again. A long-running task would otherwise strobe.
    local unchanged = (alert ~= nil and alert.text == text)

    if not alert then
        alert = { id = id }
        table.insert(alertEngineState.alerts, alert)
    end
    alert.text = text
    alert.color = opts.color or alertV2DefaultColor
    alert.position = normalizePosition(opts.position)
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
        alertEngineState.flood = { color = alert.color }
        alertEngineState.floodTimer = hs.timer.doAfter(flashSeconds, function()
            alertEngineState.floodTimer = nil
            alertEngineState.flood = nil
            destroyFlood()
        end)
    end

    syncTicker()
    hookScreenChange()
    render()
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
--- @end
