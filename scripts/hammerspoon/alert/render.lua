--- * Alert rendering
--- Turning the layout into canvases: the strips, the fullscreen flood, the
--- once-a-second countdown ticker, and the fade animation.

--- Neither fade ramp may eat more than this share of the flash, so there is
--- always a stretch at full opacity in the middle however short flashSeconds is.
--- Without it a 0.1s flash would be nothing but ramps and would never actually
--- reach the colour it is meant to be impossible to miss in.
local kFloodFadeMaxFraction = 0.4

--- ** Rendering
local function bandAlignment(band)
    -- Centring is for a short message that fits on its line. A truncated one
    -- reads as a fragment of something longer, so it lines up with the
    -- multi-line bands instead.
    return (#band.lines == 1 and not band.truncated) and "center" or "left"
end

local function runAttributes(attrs)
    local face = AlertEngine.kFont
    if attrs.bold and attrs.italic then
        face = AlertEngine.kFont .. "-BoldItalic"
    elseif attrs.bold then
        face = AlertEngine.kFont .. "-Bold"
    elseif attrs.italic then
        face = AlertEngine.kFont .. "-Italic"
    end
    local out = { font = { name = face, size = AlertEngine.kTextSize } }
    if attrs.color then
        out.color = alertV2MarkupColors[attrs.color]
    end
    if attrs.underline then
        out.underlineStyle = hs.styledtext.lineStyles.single
    end
    if attrs.strike then
        out.strikethroughStyle = hs.styledtext.lineStyles.single
    end
    return out
end

--- The band's text, as a plain string when there is no markup -- which is every
--- caller that predates it -- and as an hs.styledtext when there is.
---
--- hs.styledtext indexes by *byte*, the same as string offsets, so runs map
--- across without any utf8 conversion; a message with an emoji in it is not a
--- special case. A styledtext ignores the element's textAlignment, though, so
--- the alignment moves into the base attributes to keep short bands centred.
local function bandText(band)
    local plain = table.concat(band.lines, "\n")
    if not band.runs or #band.runs == 0 then
        return plain
    end

    local styled = hs.styledtext.new(plain, {
        font = { name = AlertEngine.kFont, size = AlertEngine.kTextSize },
        color = { white = 1 },
        paragraphStyle = { alignment = bandAlignment(band) },
    })

    -- Where each wrapped line ended up in the joined string.
    local placed, cursor = {}, 1
    for index, line in ipairs(band.lines) do
        placed[index] = cursor
        cursor = cursor + #line + 1 -- the "\n"
    end

    for _, run in ipairs(band.runs) do
        for index, span in ipairs(band.spans or {}) do
            local from = math.max(run.from, span.at)
            local to = math.min(run.to, span.at + span.len - 1)
            if to >= from then
                styled = styled:setStyle(runAttributes(run.attrs),
                                         placed[index] + (from - span.at),
                                         placed[index] + (to - span.at))
            end
        end
    end
    return styled
end

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
        local textHeight = #band.lines * AlertEngine.kLineHeight
        table.insert(elements, {
            type = "text",
            text = bandText(band),
            textColor = { white = 1 },
            textFont = AlertEngine.kFont,
            textSize = AlertEngine.kTextSize,
            textAlignment = bandAlignment(band),
            frame = {
                x = x + AlertEngine.kPaddingX,
                y = y + (band.height - textHeight) / 2,
                w = stack.w - 2 * AlertEngine.kPaddingX,
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

function AlertEngine.destroyFlood()
    if alertEngineState.floodFadeTimer then
        alertEngineState.floodFadeTimer:stop()
        alertEngineState.floodFadeTimer = nil
    end
    for _, canvas in ipairs(alertEngineState.floodCanvases) do
        canvas:delete()
    end
    alertEngineState.floodCanvases = {}
end

--- How long each ramp gets, given the caller's floodFade and the flash it has to
--- fit inside. Omitted means the module defaults, which is the on-by-default
--- case; false is the old hard cut; a number sets both ramps.
function AlertEngine.floodFadeRamps(floodFade, flashSeconds)
    if flashSeconds <= 0 or floodFade == false then
        return 0, 0
    end
    local fadeIn, fadeOut = alertV2FloodFadeInSeconds, alertV2FloodFadeOutSeconds
    local requested = tonumber(floodFade)
    if requested then
        fadeIn, fadeOut = requested, requested
    end
    local cap = flashSeconds * kFloodFadeMaxFraction
    return math.max(0, math.min(fadeIn, cap)), math.max(0, math.min(fadeOut, cap))
end

--- The wash's opacity right now, as a pure function of the clock. It has to be
--- derived rather than stored on the canvas, because renderFlood below throws
--- its canvases away and rebuilds them - from AlertEngine.render() on every new alert and
--- from tick() once a second while any countdown is alive. A fade held on the
--- canvas would be wiped by the first of those and snap back to full opacity.
---
--- Smoothstep rather than a straight line: a linear ramp reads as stopping
--- abruptly at both ends, and this is three multiplications.
local function floodWashAlpha()
    local flood = alertEngineState.flood
    if not flood then
        return 0
    end
    local peak = alertV2FloodAlpha
    local function ease(t)
        t = math.max(0, math.min(t, 1))
        return t * t * (3 - 2 * t)
    end

    local elapsed = hs.timer.secondsSinceEpoch() - flood.startedAt
    if flood.fadeIn > 0 and elapsed < flood.fadeIn then
        return peak * ease(elapsed / flood.fadeIn)
    end
    local remaining = flood.duration - elapsed
    if flood.fadeOut > 0 and remaining < flood.fadeOut then
        return peak * ease(remaining / flood.fadeOut)
    end
    return peak
end

--- The flashing alert's colour at the wash's current opacity. The alert's own
--- alpha is deliberately dropped: a band and the flash behind it are not the
--- same thing and do not want the same opacity.
local function floodWash()
    local flood = alertEngineState.flood
    if not flood then
        return nil
    end
    local wash = {}
    for key, value in pairs(flood.color) do
        wash[key] = value
    end
    wash.alpha = floodWashAlpha()
    return wash
end

--- Repaint the wash in place, the way tick() rewrites countdown digits in place:
--- rebuilding the canvases thirty times a second would re-lay-out every band on
--- every screen. Element 1 is the wash rectangle and the bands are drawn after
--- it, so only the colour moves - fading the whole canvas would fade the words
--- too and then pop them back to full opacity when the flood died.
---
--- hs.canvas hands back a copy of an element's fields, so mutating
--- `canvas[1].fillColor.alpha` writes to nothing; the whole table is assigned.
local function floodFadeStep()
    local wash = floodWash()
    if not wash then
        return
    end
    for _, canvas in ipairs(alertEngineState.floodCanvases) do
        canvas[1].fillColor = wash
    end
end

local function startFloodFade()
    if alertEngineState.floodFadeTimer then
        alertEngineState.floodFadeTimer:stop()
        alertEngineState.floodFadeTimer = nil
    end
    local flood = alertEngineState.flood
    if not flood or (flood.fadeIn <= 0 and flood.fadeOut <= 0) then
        return
    end
    -- One timer for the whole flood rather than stopping it across the hold:
    -- the flash is well under a second in every caller here, and a step is a
    -- colour assignment on at most a handful of canvases.
    alertEngineState.floodFadeTimer =
        hs.timer.doEvery(1 / alertV2FloodFadeFps, floodFadeStep)
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
    AlertEngine.destroyFlood()
    local wash = floodWash()
    if not wash then
        return
    end
    for _, screen in ipairs(ModalMode.targetScreens("all")) do
        local full = screen:fullFrame()
        local elements = {
            { type = "rectangle", action = "fill", fillColor = wash },
        }
        for _, position in ipairs(AlertEngine.kPositions) do
            local stack = AlertEngine.layoutStack(screen, position)
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
    -- AlertEngine.destroyFlood above stopped the ramp along with the old canvases, so every
    -- path that rebuilds the flood restarts it here. That keeps the fade correct
    -- through a mid-flash AlertEngine.render() or tick() without either of them knowing the
    -- flood is animating.
    startFloodFade()
end

local function renderStrips()
    destroyStrips()
    if #alertEngineState.alerts == 0 then
        return
    end
    for _, screen in ipairs(ModalMode.targetScreens("all")) do
        for _, position in ipairs(AlertEngine.kPositions) do
            local stack = AlertEngine.layoutStack(screen, position)
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

function AlertEngine.render()
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
            math.floor((work.w - 2 * AlertEngine.kPaddingX) / AlertEngine.charWidth()))
        for index, band in ipairs(record.stack.bands) do
            if band.alert and band.alert.countdown then
                local lines, spans = AlertEngine.wrapText(AlertEngine.alertDisplayText(band.alert), maxChars)
                if #lines ~= #band.lines then
                    AlertEngine.render()
                    return
                end
                band.lines, band.spans = lines, spans
                record.canvas[index * 2].text = bandText(band)
            end
        end
    end
    if alertEngineState.flood then
        renderFlood()
    end
end

function AlertEngine.syncTicker()
    if anyCountdown() then
        if not alertEngineState.ticker then
            alertEngineState.ticker = hs.timer.doEvery(1, tick)
        end
    elseif alertEngineState.ticker then
        alertEngineState.ticker:stop()
        alertEngineState.ticker = nil
    end
end

function AlertEngine.hookScreenChange()
    if alertEngineState.hooked then
        return
    end
    -- A monitor arriving or leaving would otherwise leave bands on the old
    -- geometry, or missing from the new screen entirely. ModalMode already runs
    -- one screen watcher for every overlay in this config; do not start another.
    ModalMode.onScreenChange(function()
        if #alertEngineState.alerts > 0 then
            AlertEngine.render()
        end
    end)
    alertEngineState.hooked = true
end
--- @end
