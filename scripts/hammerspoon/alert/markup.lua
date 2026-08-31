--- * Alert markup
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
function AlertEngine.parseMarkup(text)
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
--- @end
