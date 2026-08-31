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
        color = AlertEngine.textColorFor(band.color),
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

--- Returns the elements, and alongside them the positions of any band whose
--- colour animates, so the caller can hand those to the animator without
--- having to work out the element numbering for itself. Two elements per band,
--- rectangle then text, is an arithmetic relation on a strip canvas but not on
--- the flood, which prepends a wash and concatenates three stacks.
local function canvasElements(stack, origin)
    local elements = {}
    local animated = {}
    for _, band in ipairs(stack.bands) do
        local x = stack.x - origin.x
        local y = band.y - origin.y
        table.insert(elements, {
            type = "rectangle",
            action = "fill",
            fillColor = AlertEngine.colorAt(band.color),
            frame = { x = x, y = y, w = stack.w, h = band.height },
        })
        if AlertEngine.isAnimated(band.color) then
            table.insert(animated, { offset = #elements, color = band.color })
        end
        -- hs.canvas has no vertical alignment for text, so the block is centred
        -- in the band by placing its own frame.
        local textHeight = #band.lines * AlertEngine.kLineHeight
        table.insert(elements, {
            type = "text",
            text = bandText(band),
            textColor = AlertEngine.textColorFor(band.color),
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
    return elements, animated
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

--- Deliberately does not stop the animator: a flood dying is not the end of
--- animation, because a band underneath it may still be animating. Callers that
--- destroy a flood follow with AlertEngine.syncAnimator, which decides.
function AlertEngine.destroyFlood()
    for _, entry in ipairs(alertEngineState.floodCanvases) do
        entry.canvas:delete()
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
local function floodWashAlpha(now)
    local flood = alertEngineState.flood
    if not flood then
        return 0
    end
    local peak = alertV2FloodAlpha
    local function ease(t)
        t = math.max(0, math.min(t, 1))
        return t * t * (3 - 2 * t)
    end

    local elapsed = (now or hs.timer.secondsSinceEpoch()) - flood.startedAt
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
---
--- Resolve first, then override the alpha, so an animated colour floods in
--- whatever shade it is wearing at this instant. The copy is not optional even
--- for a static colour: the table would otherwise be the palette entry itself,
--- and writing an alpha into it would repaint every band using that name.
local function floodWash(now)
    local flood = alertEngineState.flood
    if not flood then
        return nil
    end
    local wash = {}
    for key, value in pairs(AlertEngine.colorAt(flood.color, now)) do
        wash[key] = value
    end
    wash.alpha = floodWashAlpha(now)
    return wash
end

--- ** Animation
--- One timer drives everything that moves: the flood's fade ramps and any band
--- wearing an animated colour. They are the same job -- write a colour onto an
--- element that already exists -- and running two timers for it would mean two
--- clocks, two lifecycles, and colours computed at slightly different instants.
---
--- Repainting in place is the whole point, the way tick() rewrites countdown
--- digits in place: rebuilding canvases thirty times a second would re-lay-out
--- every band on every screen. And it is only ever the fill that moves, never
--- the canvas alpha -- fading a whole canvas would fade the words drawn on it
--- and pop them back the moment it died.
---
--- hs.canvas hands back a copy of an element's fields, so mutating
--- `canvas[1].fillColor.alpha` writes to nothing; the whole table is assigned.
local function animationStep()
    local now = hs.timer.secondsSinceEpoch()

    local wash = floodWash(now)
    for _, entry in ipairs(alertEngineState.floodCanvases) do
        if wash then
            entry.canvas[1].fillColor = wash
        end
        for _, animated in ipairs(entry.animated) do
            entry.canvas[animated.offset].fillColor =
                AlertEngine.colorAt(animated.color, now)
        end
    end

    for _, record in ipairs(alertEngineState.canvases) do
        for index, band in ipairs(record.stack.bands) do
            if AlertEngine.isAnimated(band.color) then
                -- Two elements per band, rectangle first.
                record.canvas[index * 2 - 1].fillColor =
                    AlertEngine.colorAt(band.color, now)
            end
        end
    end
end

--- A fading flood, an animated flood, or an animated band that layout actually
--- placed. The last of those matters: an alert whose band was squeezed off the
--- screen has nothing to repaint, and should not hold the timer open.
local function animationNeeded()
    local flood = alertEngineState.flood
    if flood and (flood.fadeIn > 0 or flood.fadeOut > 0
                      or AlertEngine.isAnimated(flood.color)) then
        return true
    end
    for _, record in ipairs(alertEngineState.canvases) do
        for _, band in ipairs(record.stack.bands) do
            if AlertEngine.isAnimated(band.color) then
                return true
            end
        end
    end
    return false
end

--- Same shape as syncTicker: called after anything that could have changed the
--- answer, and it decides whether a timer should be running rather than each
--- caller having to know. One timer for a whole flood rather than stopping it
--- across the hold, too -- a step is a colour assignment on a handful of
--- elements, and the flash is well under a second in every caller here.
function AlertEngine.syncAnimator()
    if animationNeeded() then
        if not alertEngineState.animator then
            alertEngineState.animator =
                hs.timer.doEvery(1 / alertV2AnimationFps, animationStep)
        end
    elseif alertEngineState.animator then
        alertEngineState.animator:stop()
        alertEngineState.animator = nil
    end
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
        -- Where each animated band ended up on this canvas. Unlike a strip, the
        -- flood prepends the wash and concatenates every position's stack, so
        -- there is no arithmetic that recovers these later.
        local animated = {}
        for _, position in ipairs(AlertEngine.kPositions) do
            local stack = AlertEngine.layoutStack(screen, position)
            if stack then
                local stackElements, stackAnimated = canvasElements(stack, full)
                local base = #elements
                for _, element in ipairs(stackElements) do
                    table.insert(elements, element)
                end
                for _, entry in ipairs(stackAnimated) do
                    table.insert(animated,
                        { offset = base + entry.offset, color = entry.color })
                end
            end
        end
        local canvas = newCanvas(full, hs.canvas.windowLevels.overlay + 1)
        canvas:appendElements(table.unpack(elements))
        canvas:show()
        table.insert(alertEngineState.floodCanvases,
                     { canvas = canvas, animated = animated })
    end
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
                -- Bound to a local first: canvasElements returns the animated
                -- offsets as a second value, and table.unpack would read that
                -- as its start index. A strip needs no offsets anyway -- its
                -- bands are elements 1,3,5..., which the animator derives.
                local elements = canvasElements(
                    stack, { x = stack.x, y = stack.bands[1].y })
                canvas:appendElements(table.unpack(elements))
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
    -- Last, because it is the freshly built canvases the animator has to paint,
    -- and because whether anything needs animating is only knowable once layout
    -- has decided which bands are actually on screen.
    AlertEngine.syncAnimator()
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
