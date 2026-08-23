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
---   hs -c 'alertV2("**low** [12%]{red}", { markup = "md" })'
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
--- How opaque a band is. Slightly see-through, so a band lying across a window
--- does not read as a hole punched in it - you can still tell what it is
--- covering. One knob rather than an alpha buried in each colour below; a
--- colour passed in by a caller keeps whatever alpha it carries.
alertV2BandAlpha = alertV2BandAlpha or 0.8

--- How opaque the fullscreen flash is. The alert's own colour, but much more
--- see-through than a band: the flash has to be impossible to miss without
--- blacking out the screen it covers. The bands drawn on top of it keep their
--- own opacity.
alertV2FloodAlpha = alertV2FloodAlpha or 0.33

alertV2DefaultColor = alertV2DefaultColor
    or { red = 0.16, green = 0.19, blue = 0.24, alpha = alertV2BandAlpha }

--- Amber, the colour the agent banner shipped with: something wants attention
--- but nothing is on fire.
alertV2WarnColor = alertV2WarnColor
    or { red = 0.80, green = 0.36, blue = 0.02, alpha = alertV2BandAlpha }

--- Crimson: "a machine is driving this screen, do not touch the keyboard".
alertV2AgentColor = alertV2AgentColor
    or { red = 0.62, green = 0.06, blue = 0.10, alpha = alertV2BandAlpha }

--- Blue: the screen is yours again.
alertV2FreeColor = alertV2FreeColor
    or { red = 0.09, green = 0.055, blue = 0.42, alpha = alertV2BandAlpha }

--- Dimmed grey for the "earlier alerts hidden" notice, so it reads as chrome
--- rather than as another alert.
alertV2NoticeColor = alertV2NoticeColor
    or { red = 0.25, green = 0.25, blue = 0.28, alpha = alertV2BandAlpha }

--- Band colours by name, so a caller reachable only through a shell -- where a
--- table literal would have to survive `hammerspoon -c' quoting -- can say
--- `color = "warn"'. Resolved per call rather than cached in a table, so
--- reassigning one of the colours above takes effect immediately.
local function resolveBandColor(color)
    if type(color) == "table" then
        return color
    end
    if type(color) ~= "string" then
        return alertV2DefaultColor
    end
    return ({
        default = alertV2DefaultColor,
        warn    = alertV2WarnColor,
        amber   = alertV2WarnColor,
        crit    = alertV2AgentColor,
        agent   = alertV2AgentColor,
        free    = alertV2FreeColor,
        notice  = alertV2NoticeColor,
    })[color] or alertV2DefaultColor
end

--- Text colours for markup runs. Deliberately separate from the band colours
--- above: these sit *on* a band, so they are light and saturated rather than
--- dark and translucent.
alertV2MarkupColors = alertV2MarkupColors or {
    red    = { red = 1.00, green = 0.45, blue = 0.40 },
    amber  = { red = 1.00, green = 0.76, blue = 0.33 },
    green  = { red = 0.56, green = 0.87, blue = 0.52 },
    blue   = { red = 0.58, green = 0.76, blue = 1.00 },
    grey   = { white = 0.62 },
    dim    = { white = 0.55 },
    white  = { white = 1.00 },
}

--- ** Markup
--- Two input modes. `plain' is the default and is byte-for-byte what the caller
--- passed, so nothing that predates this renders differently.
---
--- `md' is a deliberately small markdown subset. A band is one font at one
--- size, so headings, lists and links have nothing to render into; what a band
--- *can* express is weight, slant, line decoration and colour, and that is
--- exactly the subset:
---
---   **bold**   *italic*   ~~strike~~   [text]{attrs}   \*  (escape)
---
--- `[text]{attrs}' is Pandoc's attribute-span syntax, borrowed because markdown
--- has no colour of its own. Attributes are space-separated and combine freely:
--- `[R 12%]{red bold}'. A leading dot is accepted too, so Pandoc's own
--- `{.red}' works. Colour names come from alertV2MarkupColors; the rest are
--- bold, italic, underline and strike.
---
--- Anything that does not parse -- an unclosed delimiter, an unknown attribute
--- name -- renders literally rather than being swallowed, so a typo is visible
--- instead of silently doing nothing.
---
--- Parsing produces a plain string plus runs of byte offsets into it, and
--- everything downstream keeps working on the plain string. That is what keeps
--- wrapping, truncation and the height budget untouched: the layout never
--- learns that markup exists.
local kMarkupFlags = { bold = true, italic = true, underline = true, strike = true }

local function parseAttrs(spec)
    local attrs = {}
    local any = false
    for token in spec:gmatch("%S+") do
        token = token:gsub("^%.", "")
        if kMarkupFlags[token] then
            attrs[token] = true
        elseif alertV2MarkupColors[token] then
            attrs.color = token
        else
            return nil -- unknown name: render the whole span literally
        end
        any = true
    end
    return any and attrs or nil
end

--- Returns plain, runs. `runs' is { from, to, attrs } with inclusive 1-based
--- byte offsets into `plain', outermost first so that a nested span's own
--- styling is applied last and wins.
local function parseMarkup(text)
    local chunks, runs = {}, {}
    local pos, len = 1, #text
    local plainLen = 0
    local parse

    local function emit(s)
        chunks[#chunks + 1] = s
        plainLen = plainLen + #s
    end

    local function merge(base, extra)
        local out = {}
        for k, v in pairs(base or {}) do out[k] = v end
        for k, v in pairs(extra or {}) do out[k] = v end
        return out
    end

    local function mark()
        return { pos = pos, chunks = #chunks, runs = #runs, plainLen = plainLen }
    end

    local function rewind(save)
        pos = save.pos
        plainLen = save.plainLen
        for i = #chunks, save.chunks + 1, -1 do chunks[i] = nil end
        for i = #runs, save.runs + 1, -1 do runs[i] = nil end
    end

    --- Record a span covering everything emitted since `save'. Inserted at the
    --- span's own position rather than appended, so runs stay outermost-first.
    local function record(save, from, attrs)
        if plainLen >= from then
            table.insert(runs, save.runs + 1, { from = from, to = plainLen, attrs = attrs })
        end
    end

    local function delimited(open, close, attrs, inherited)
        local save = mark()
        pos = pos + #open
        local from = plainLen + 1
        local merged = merge(inherited, attrs)
        if parse(close, merged) then
            pos = pos + #close
            record(save, from, merged)
            return true
        end
        rewind(save)
        return false
    end

    --- `[' ... `]{attrs}'. The attributes are only known after the closing
    --- bracket, so the span's extent is found first -- tracking bracket depth,
    --- and honouring `%b{}' for the attribute block so `{a {b}}' cannot end it
    --- early.
    local function attrSpan(inherited)
        local depth, i = 0, pos + 1
        local rb, attrEnd
        while i <= len do
            local c = text:sub(i, i)
            if c == "\\" then
                i = i + 2
            elseif c == "[" then
                depth = depth + 1
                i = i + 1
            elseif c == "]" then
                if depth == 0 then
                    local _, close = text:find("^%b{}", i + 1)
                    if close then rb, attrEnd = i, close end
                    break
                end
                depth = depth - 1
                i = i + 1
            else
                i = i + 1
            end
        end
        if not rb then
            return false
        end
        local attrs = parseAttrs(text:sub(rb + 2, attrEnd - 1))
        if not attrs then
            return false
        end

        local save = mark()
        pos = pos + 1
        local from = plainLen + 1
        local merged = merge(inherited, attrs)
        if parse("]", merged) and pos == rb then
            pos = attrEnd + 1
            record(save, from, merged)
            return true
        end
        rewind(save)
        return false
    end

    parse = function(close, inherited)
        while pos <= len do
            local atClose = close ~= nil and text:sub(pos, pos + #close - 1) == close
            -- `**' inside an italic span opens a nested one; it is not the
            -- single `*' that would close it.
            if atClose and close == "*" and text:sub(pos, pos + 1) == "**" then
                atClose = false
            end
            if atClose then
                return true
            end

            local ch = text:sub(pos, pos)
            if ch == "\\" and pos < len then
                emit(text:sub(pos + 1, pos + 1))
                pos = pos + 2
            elseif text:sub(pos, pos + 1) == "**" then
                if not delimited("**", "**", { bold = true }, inherited) then
                    emit("**")
                    pos = pos + 2
                end
            elseif text:sub(pos, pos + 1) == "~~" then
                if not delimited("~~", "~~", { strike = true }, inherited) then
                    emit("~~")
                    pos = pos + 2
                end
            elseif ch == "*" then
                if not delimited("*", "*", { italic = true }, inherited) then
                    emit("*")
                    pos = pos + 1
                end
            elseif ch == "[" then
                if not attrSpan(inherited) then
                    emit("[")
                    pos = pos + 1
                end
            else
                emit(ch)
                pos = pos + 1
            end
        end
        return close == nil
    end

    parse(nil, nil)
    return table.concat(chunks), runs
end

--- ** Geometry
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
local function wrapText(text, maxChars)
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
            local lines, spans = wrapText(alertDisplayText(alert), maxChars)
            local full = math.max(kMinBandHeight,
                                  #lines * kLineHeight + 2 * kPaddingY)
            local grant = math.max(0, math.min(full - oneLine, remaining))
            remaining = remaining - grant
            local height = oneLine + grant
            local fits = math.floor((height - 2 * kPaddingY) / kLineHeight)
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
local function bandAlignment(band)
    -- Centring is for a short message that fits on its line. A truncated one
    -- reads as a fragment of something longer, so it lines up with the
    -- multi-line bands instead.
    return (#band.lines == 1 and not band.truncated) and "center" or "left"
end

local function runAttributes(attrs)
    local face = kFont
    if attrs.bold and attrs.italic then
        face = kFont .. "-BoldItalic"
    elseif attrs.bold then
        face = kFont .. "-Bold"
    elseif attrs.italic then
        face = kFont .. "-Italic"
    end
    local out = { font = { name = face, size = kTextSize } }
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
        font = { name = kFont, size = kTextSize },
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
        local textHeight = #band.lines * kLineHeight
        table.insert(elements, {
            type = "text",
            text = bandText(band),
            textColor = { white = 1 },
            textFont = kFont,
            textSize = kTextSize,
            textAlignment = bandAlignment(band),
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
                local lines, spans = wrapText(alertDisplayText(band.alert), maxChars)
                if #lines ~= #band.lines then
                    render()
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
---   markup       "plain" (default) or "md"; see ** Markup above
---   color        band colour, default dark slate. A table, or one of the
---                names default/warn/amber/crit/agent/free/notice
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

    -- Parse before anything else touches the text: everything downstream --
    -- the id heartbeat below included -- works on the marker-free string, so
    -- markup never leaks into layout or into a comparison.
    local runs = nil
    if opts.markup == "md" then
        text, runs = parseMarkup(text)
    end

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
    alert.runs = runs
    alert.color = resolveBandColor(opts.color)
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
