--- * Alert layout
--- Measuring and wrapping text, finding alert records, and deciding where every
--- band lands on every screen. Nothing here draws.

--- ** Text measuring and wrapping
--- Menlo is monospaced, so wrapping is arithmetic on character counts rather
--- than a measuring call per candidate word. Measured once because point size
--- to advance width is not exactly 0.6 on every macOS release.
local cachedCharWidth = nil

function AlertEngine.charWidth()
    if cachedCharWidth then
        return cachedCharWidth
    end
    local ok, size = pcall(function()
        return hs.drawing.getTextDrawingSize(hs.styledtext.new(
            "MMMMMMMMMM",
            { font = { name = AlertEngine.kFont, size = AlertEngine.kTextSize } }))
    end)
    if ok and size and size.w and size.w > 0 then
        cachedCharWidth = size.w / 10
    else
        cachedCharWidth = AlertEngine.kTextSize * 0.6
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
---
--- Also records, per output line, where it came from in the input:
--- { at = byte offset, len = bytes taken from the input }. Markup runs are
--- offsets into that same input, so this is what lets them be mapped onto the
--- wrapped lines exactly, rather than by searching the wrapped text for the
--- substring -- which would style every other occurrence of it as well, and
--- would miss any run a line break happened to split. `len' is tracked
--- separately from the line's own length because truncation can append text
--- that came from nowhere.
local function wrapLine(raw, maxChars, out, spans, base)
    if raw == "" then
        out[#out + 1] = ""
        spans[#spans + 1] = { at = base, len = 0 }
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
        local piece, dropped
        if cut then
            piece = charSub(raw, 1, cut - 1)
            raw = charSub(raw, cut + 1)
            dropped = 1 -- the space we broke on
        else
            piece = charSub(raw, 1, maxChars)
            raw = charSub(raw, maxChars + 1)
            dropped = 0
        end
        out[#out + 1] = piece
        spans[#spans + 1] = { at = base, len = #piece }
        base = base + #piece + dropped
        if raw == "" then
            return
        end
    end
    out[#out + 1] = raw
    spans[#spans + 1] = { at = base, len = #raw }
end

--- Returns lines, spans. Callers that do not care about markup can ignore the
--- second value.
function AlertEngine.wrapText(text, maxChars)
    if maxChars < 1 then
        maxChars = 1
    end
    local out, spans = {}, {}
    local base = 1
    for raw in (tostring(text) .. "\n"):gmatch("(.-)\n") do
        wrapLine(raw, maxChars, out, spans, base)
        base = base + #raw + 1 -- the newline
    end
    if #out == 0 then
        out, spans = { "" }, { { at = 1, len = 0 } }
    end
    return out, spans
end

--- ** Alert records
function AlertEngine.findAlert(id)
    for index, alert in ipairs(alertEngineState.alerts) do
        if alert.id == id then
            return alert, index
        end
    end
    return nil, nil
end

function AlertEngine.alertDisplayText(alert)
    if not alert.countdown then
        return alert.text
    end
    local left = math.max(0, math.floor(alert.expiry - hs.timer.secondsSinceEpoch()))
    return string.format("%s   -   clears in %d:%02d",
                         alert.text,
                         math.floor(left / 60),
                         left % 60)
end

function AlertEngine.normalizePosition(position)
    for _, known in ipairs(AlertEngine.kPositions) do
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
function AlertEngine.layoutStack(screen, position)
    local work = screen:frame()
    local maxChars = math.max(1, math.floor((work.w - 2 * AlertEngine.kPaddingX) / AlertEngine.charWidth()))
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
    --- Source spans travel alongside so markup survives truncation. The
    --- synthetic marker line gets a zero-length span: it is text this file
    --- invented, so no run can possibly refer to it.
    local function truncate(lines, spans, fits)
        if fits >= #lines then
            return lines, spans
        end
        if fits > 1 then
            local kept, keptSpans = {}, {}
            for line = 1, fits - 1 do
                table.insert(kept, lines[line])
                table.insert(keptSpans, spans[line])
            end
            table.insert(kept, string.format("... (+%d more lines)",
                                             #lines - (fits - 1)))
            table.insert(keptSpans, { at = 1, len = 0 })
            return kept, keptSpans
        end
        local marker = string.format("  ... (+%d more lines)", #lines - 1)
        local head = lines[1]
        local room = maxChars - charLen(marker)
        if charLen(head) > room then
            head = charSub(head, 1, math.max(0, room))
        end
        -- The head is a prefix of line 1, so it keeps that line's offset; `len'
        -- shrinks to the head alone, which is what keeps the marker glued onto
        -- the end from being styled as if it came from the message.
        return { head .. marker }, { { at = spans[1].at, len = #head } }
    end

    local oneLine = AlertEngine.kLineHeight + 2 * AlertEngine.kPaddingY

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
            local lines, spans = AlertEngine.wrapText(AlertEngine.alertDisplayText(alert), maxChars)
            local full = math.max(AlertEngine.kMinBandHeight,
                                  #lines * AlertEngine.kLineHeight + 2 * AlertEngine.kPaddingY)
            local grant = math.max(0, math.min(full - oneLine, remaining))
            remaining = remaining - grant
            local height = oneLine + grant
            local fits = math.floor((height - 2 * AlertEngine.kPaddingY) / AlertEngine.kLineHeight)
            local kept, keptSpans = truncate(lines, spans, fits)
            bands[index] = {
                alert = alert,
                lines = kept,
                spans = keptSpans,
                runs = alert.runs,
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
        bands, hidden = allocate(budget - (AlertEngine.kLineHeight + 2 * AlertEngine.kPaddingY))
    end
    if hidden > 0 then
        table.insert(bands, 1, {
            lines = { string.format("... %d earlier alert(s) hidden", hidden) },
            height = AlertEngine.kLineHeight + 2 * AlertEngine.kPaddingY,
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
--- @end
