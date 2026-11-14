-- Layouts for multi-emoji sticker exports, shared by Hyper+A (core/choosers.lua)
-- and Hyper+Shift+A (core/telegram-emoji.lua). See docs/omni-prompter.md.
--
-- A layout turns N emoji into *cells* {i, x, y, s, rot}: emoji number i,
-- centred at (x, y) with side s and rot degrees clockwise, in arbitrary units
-- with y growing downwards. A layout may make several *pages* (Each, Pages),
-- one sticker each. The same cells drive the 64px previews drawn here and the
-- final export by bin/emoji-compose.py, which trims every glyph, fits it into
-- its cell, and scales the result so its long side is 512px.
EmojiLayout = EmojiLayout or {}
local L = EmojiLayout
L.gap = 0.92            -- Share of a cell a glyph fills when there are several.
L.sequenceDelay = L.sequenceDelay or 0.6  -- Seconds between separate pastes.
L.renderTimeout = L.renderTimeout or 10   -- Per Telegram emoji, in seconds.
L.jumbleSeed = L.jumbleSeed or 1
L.python = "/opt/homebrew/bin/python3"
L.script = os.getenv("HOME") .. "/scripts/hammerspoon/bin/emoji-compose.py"
L.tasks = L.tasks or {}  -- Running tasks, so they are not collected.
local kLastKey = "emojiLayout.last.v1"
local SQ3 = math.sqrt(3) / 2

-- Rows listed top to bottom, each centred. Rows whose counts differ by an odd
-- number sit half a cell apart, so they interlock at 0.866 of a cell.
function L.rowCells(counts, first)
    local cells, y, i = {}, 0, (first or 1) - 1
    for r, count in ipairs(counts) do
        if r > 1 then y = y + ((counts[r-1] - count) % 2 ~= 0 and SQ3 or 1) end
        for c = 1, count do
            i = i + 1
            cells[#cells+1] = {i=i, x=c - (count + 1) / 2, y=y, s=1}
        end
    end
    return cells
end

-- A near-square grid with its short row centred on top: 3 is [1, 2], 5 is
-- [2, 3], 7 is [1, 3, 3]; 2 stays side by side.
function L.autoCounts(n)
    local c = math.ceil(math.sqrt(n))
    local r = math.ceil(n / c)
    local counts = {n - c * (r - 1)}
    for _ = 2, r do counts[#counts+1] = c end
    return counts
end

-- Returns left, top, width, height; gap shrinks each glyph inside its cell.
function L.bounds(cells, gap)
    local minx, miny, maxx, maxy = math.huge, math.huge, -math.huge, -math.huge
    for _, c in ipairs(cells) do
        local rad = math.rad(c.rot or 0)
        local e = c.s * (gap or 1) / 2 * (math.abs(math.cos(rad)) + math.abs(math.sin(rad)))
        minx, maxx = math.min(minx, c.x - e), math.max(maxx, c.x + e)
        miny, maxy = math.min(miny, c.y - e), math.max(maxy, c.y + e)
    end
    return minx, miny, maxx - minx, maxy - miny
end

local function gapFor(cells) return #cells > 1 and L.gap or 1 end

-- Equal arc spacing along a closed curve fn(t), t from 0 to 2*pi.
local function pathCells(n, fn, fill)
    local samples, points, length = 400, {}, {0}
    for k = 0, samples do
        local x, y = fn(2 * math.pi * k / samples)
        points[k+1] = {x, y}
        if k > 0 then
            local p = points[k]
            length[k+1] = length[k] + math.sqrt((x - p[1])^2 + (y - p[2])^2)
        end
    end
    local step, cells, j = length[#length] / n, {}, 1
    for i = 1, n do
        local target = (i - 1) * step
        while length[j+1] < target do j = j + 1 end
        local f = (target - length[j]) / math.max(1e-9, length[j+1] - length[j])
        local a, b = points[j], points[j+1]
        cells[i] = {i=i, x=a[1] + f * (b[1] - a[1]), y=a[2] + f * (b[2] - a[2]), s=fill * step}
    end
    return cells
end

-- Small seeded generator, so a Jumble preview and its export match.
function L.random(seed)
    local state = (seed * 2654435761 + 12345) & 0x7fffffff
    return function(a, b)
        state = (state * 1103515245 + 12345) & 0x7fffffff
        local u = state / 0x80000000
        return a and (a + (b - a) * u) or u
    end
end

local function shuffle(list, rand)
    for k = #list, 2, -1 do
        local j = math.floor(rand() * k) + 1
        list[k], list[j] = list[j], list[k]
    end
    return list
end

local function heart(n)
    if n <= 20 then
        -- The classic parametric heart, starting at its bottom tip.
        return {pathCells(n, function(t)
            t = t + math.pi
            return 16 * math.sin(t)^3,
                -(13 * math.cos(t) - 5 * math.cos(2*t) - 2 * math.cos(3*t) - math.cos(4*t))
        end, 0.9)}
    end
    -- Filled: a hexagonal grid inside (x^2+y^2-1)^3 - x^2 y^3 <= 0, coarse
    -- enough to hold n; the shallowest surplus points are dropped.
    local function points(d)
        local found, row = {}, 0
        for y = 1.3, -1.1, -d * SQ3 do
            local offset = row % 2 == 1 and d / 2 or 0
            for x = -1.2 + offset, 1.2, d do
                local a = x*x + y*y - 1
                local depth = a*a*a - x*x*y*y*y
                if depth <= 0 then found[#found+1] = {x=x, y=-y, depth=depth} end
            end
            row = row + 1
        end
        return found
    end
    local d, found = 0.6, nil
    repeat found = points(d); d = d * 0.98 until #found >= n
    d = d / 0.98
    local order = {}
    for k = 1, #found do order[k] = k end
    table.sort(order, function(a, b) return found[a].depth < found[b].depth end)
    local keep = {}
    for k = 1, n do keep[order[k]] = true end
    local cells = {}
    for k, p in ipairs(found) do
        if keep[k] then cells[#cells+1] = {i=#cells+1, x=p.x, y=p.y, s=d} end
    end
    return {cells}
end

local function orbit(n)
    local cells = {}
    if n <= 12 then
        local s = math.min(2 * math.sin(math.pi / n) * 0.95, 1.2)
        for k = 1, n do
            local a = 2 * math.pi * (k - 1) / n
            cells[k] = {i=k, x=math.sin(a), y=-math.cos(a), s=s}
        end
        return {cells}
    end
    local m = 1
    while 1 + 3 * m * (m + 1) < n do m = m + 1 end
    cells[1] = {i=1, x=0, y=0, s=0.95}
    local left = n - 1
    for k = 1, m do
        local count = math.min(6 * k, left)
        local offset = k % 2 == 0 and math.pi / (6 * k) or 0
        for j = 1, count do
            local a = offset + 2 * math.pi * (j - 1) / count
            cells[#cells+1] = {i=#cells+1, x=k * math.sin(a), y=-k * math.cos(a), s=0.95}
        end
        left = left - count
    end
    return {cells}
end

local function sunflower(n)
    local cells = {}
    for k = 0, n - 1 do
        local a, r = math.rad(k * 137.508), math.sqrt(k + 0.5)
        cells[k+1] = {i=k+1, x=r * math.sin(a), y=-r * math.cos(a), s=1.5}
    end
    return {cells}
end

-- The first emoji at the outer, largest end; each turn grows by G.
local function nautilus(n)
    -- More steps per turn keep the innermost emoji from shrinking away.
    local m = math.max(6, math.min(14, math.ceil(0.8 * n)))
    local k = 1.94 * math.sin(math.pi / m)
    local g = ((1 + k / 2) / (1 - k / 2)) ^ (1 / m)
    local cells = {}
    for j = 0, n - 1 do
        local a, r = 2 * math.pi * j / m, g ^ j
        cells[#cells+1] = {i=n - j, x=r * math.sin(a), y=-r * math.cos(a), s=k * r}
    end
    return {cells}
end

local function best(candidates)
    local winner, score
    for _, cells in ipairs(candidates) do
        local _, _, w, h = L.bounds(cells)
        local size = math.huge
        for _, c in ipairs(cells) do size = math.min(size, c.s) end
        if not score or size / math.max(w, h) > score + 1e-9 then
            winner, score = cells, size / math.max(w, h)
        end
    end
    return winner
end

local function beehive(n)
    local candidates = {}
    for c = 2, n do
        local counts, left, k = {}, n, 0
        while left > 0 do
            local take = math.min(k % 2 == 0 and c or c - 1, left)
            table.insert(counts, 1, take)
            left, k = left - take, k + 1
        end
        candidates[#candidates+1] = L.rowCells(counts)
    end
    return {best(candidates)}
end

local function daisy(n)
    local p = n - 1
    local s = math.min(2 * math.sin(math.pi / p) * 0.95, 0.8)
    local cells = {{i=1, x=0, y=0, s=0.9 * (2 - s)}}
    for k = 1, p do
        local a = 2 * math.pi * (k - 1) / p
        -- Upright: petals turned outwards put faces upside down.
        cells[#cells+1] = {i=k+1, x=math.sin(a), y=-math.cos(a), s=s}
    end
    return {cells}
end

-- The first emoji as a hero over the rest, in the row count that keeps the
-- small ones largest. The hero is at least twice their size.
local function marquee(n)
    local rest, candidates = n - 1, {}
    for r = 1, math.min(4, rest) do
        local c = math.ceil(rest / r)
        if c * (r - 1) < rest then
            local hero = math.max(2, math.min(c, c - r))
            local counts = {rest - c * (r - 1)}
            for _ = 2, r do counts[#counts+1] = c end
            local cells = L.rowCells(counts, 2)
            for _, cell in ipairs(cells) do cell.y = cell.y + hero / 2 + 0.5 end
            table.insert(cells, 1, {i=1, x=0, y=0, s=hero})
            candidates[#candidates+1] = cells
        end
    end
    return {best(candidates)}
end

local function pyramid(n)
    local k = math.ceil((math.sqrt(8 * n + 1) - 1) / 2)
    local counts, left = {}, n
    while left > 0 do
        local take = math.min(k, left)
        table.insert(counts, 1, take)
        left, k = left - take, k - 1
    end
    return {L.rowCells(counts)}
end

-- Random cells of a sparse grid that leaves no row or column empty, each
-- nudged, turned and resized a little. Tab picks another seed.
local function jumble(n, seed)
    local rand = L.random(seed)
    local g = math.ceil(math.sqrt(1.35 * n))
    local chosen
    for _ = 1, 40 do
        local slots = {}
        for k = 1, g * g do slots[k] = k - 1 end
        shuffle(slots, rand)
        chosen = {table.unpack(slots, 1, n)}
        local rows, cols, rowCount, colCount = {}, {}, 0, 0
        for _, slot in ipairs(chosen) do
            local r, c = slot // g, slot % g
            if not rows[r] then rows[r] = true; rowCount = rowCount + 1 end
            if not cols[c] then cols[c] = true; colCount = colCount + 1 end
        end
        if rowCount == math.min(g, n) and colCount == math.min(g, n) then break end
    end
    local order = {}
    for k = 1, n do order[k] = k end
    shuffle(order, rand)
    local cells = {}
    for k, slot in ipairs(chosen) do
        cells[k] = {i=order[k], x=slot % g + rand(-0.12, 0.12), y=slot // g + rand(-0.12, 0.12),
                    s=rand(0.85, 1.1), rot=rand(-15, 15)}
    end
    return {cells}
end

local function pages(n)
    local result = {}
    for first = 1, n, 16 do
        result[#result+1] = L.rowCells(L.autoCounts(math.min(16, n - first + 1)), first)
    end
    return result
end

-- min/max bound N; repeatTo cycles the selection to at least that many cells.
L.modes = {
    {key="auto", name="Auto", note="grid, short row centred on top",
     build=function(n) return {L.rowCells(L.autoCounts(n))} end},
    {key="row", name="Row", note="one line", min=2,
     build=function(n) return {L.rowCells({n})} end},
    {key="column", name="Column", note="one stack", min=2,
     build=function(n)
         local cells = {}
         for i = 1, n do cells[i] = {i=i, x=0, y=i, s=1} end
         return {cells}
     end},
    {key="each", name="Each", note="separate stickers", min=2, separate=true,
     build=function(n)
         local result = {}
         for i = 1, n do result[i] = {{i=i, x=0, y=0, s=1}} end
         return result
     end},
    {key="pages", name="Pages", note="Auto grids of up to 16", min=17, separate=true, build=pages},
    {key="beehive", name="Beehive", note="interlocked rows", min=5, build=beehive},
    {key="daisy", name="Daisy", note="first in the middle, petals around", min=5, build=daisy},
    {key="marquee", name="Marquee", note="first as a hero over the rest", min=3, max=25, build=marquee},
    {key="heart", name="Heart", note="outline, filled past 20", repeatTo=6, build=heart},
    {key="orbit", name="Orbit", note="ring, or rings past 12", repeatTo=3, build=orbit},
    {key="sunflower", name="Sunflower", note="golden-angle spiral", repeatTo=8, build=sunflower},
    {key="pyramid", name="Pyramid", note="rows shrinking upwards", min=3, max=28, build=pyramid},
    {key="nautilus", name="Nautilus", note="growing spiral, first largest", min=5, max=30, build=nautilus},
    {key="jumble", name="Jumble", note="random; Tab re-rolls", min=2, build=jumble},
}

function L.mode(key)
    for _, mode in ipairs(L.modes) do if mode.key == key then return mode end end
    error("Unknown emoji layout: " .. tostring(key))
end

function L.available(n)
    local result = {}
    for _, mode in ipairs(L.modes) do
        if n >= (mode.min or 1) and n <= (mode.max or 128) then result[#result+1] = mode end
    end
    return result
end

-- Pages of cells for n emoji, and how many cells repeating filled.
function L.build(key, n, seed)
    assert(n >= 1 and n <= 128, "Choose between 1 and 128 emoji")
    local mode = L.mode(key)
    local slots = math.max(n, mode.repeatTo or 0)
    local result = mode.build(slots, seed or L.jumbleSeed)
    for _, cells in ipairs(result) do
        for _, cell in ipairs(cells) do cell.i = (cell.i - 1) % n + 1 end
    end
    return result, slots
end

-- Approximate, because the real glyph boxes are known only after trimming.
function L.describe(key, n, built, slots)
    local mode = L.mode(key)
    local parts = {mode.note}
    if mode.separate then
        parts[#parts+1] = #built .. " stickers"
    else
        local cells = built[1]
        local gap = gapFor(cells)
        local _, _, w, h = L.bounds(cells, gap)
        local scale, size = 512 / math.max(w, h), math.huge
        for _, c in ipairs(cells) do size = math.min(size, c.s) end
        local pw, ph = math.floor(w * scale + 0.5), math.floor(h * scale + 0.5)
        parts[#parts+1] = string.format("≈%d×%d, %dpx cells", pw, ph, math.floor(size * gap * scale + 0.5))
        if math.min(pw, ph) < 128 then parts[#parts+1] = "small in Telegram" end
    end
    if slots > n then parts[#parts+1] = "repeated to " .. slots end
    return table.concat(parts, " · ")
end

-- A size x size preview from 128px images; Each shows up to four tiles.
function L.thumbnail(images, key, built, size)
    size = size or 64
    local mode = L.mode(key)
    local cells, tiles = built[1], false
    if mode.separate then
        local count = math.min(#built, 4)
        cells, tiles = L.rowCells(L.autoCounts(count)), true
        for k, cell in ipairs(cells) do cell.i = built[k][1].i end
        if key == "pages" then cells = built[1]; tiles = false end
    end
    local gap = tiles and 0.84 or gapFor(cells)
    local left, top, w, h = L.bounds(cells, 1)
    local scale = (size - 2) / math.max(w, h)
    local ox, oy = (size - w * scale) / 2 - left * scale, (size - h * scale) / 2 - top * scale
    local canvas = hs.canvas.new({x=0, y=0, w=size, h=size})
    local ok, result = pcall(function()
        for _, cell in ipairs(cells) do
            local cx, cy, side = ox + cell.x * scale, oy + cell.y * scale, cell.s * scale
            if tiles then
                canvas:insertElement({type="rectangle", action="fill", fillColor={white=1, alpha=0.18},
                    roundedRectRadii={xRadius=side/8, yRadius=side/8},
                    frame={x=cx - side/2 + 0.5, y=cy - side/2 + 0.5, w=side - 1, h=side - 1}})
            end
            local image = images[cell.i]
            if image then
                local glyph = side * gap
                local element = {type="image", image=image, imageScaling="scaleProportionally",
                    frame={x=cx - glyph/2, y=cy - glyph/2, w=glyph, h=glyph}}
                if cell.rot and cell.rot ~= 0 then
                    element.transformation = hs.canvas.matrix.translate(cx, cy):rotate(cell.rot):translate(-cx, -cy)
                end
                canvas:insertElement(element)
            end
        end
        return canvas:imageFromCanvas()
    end)
    canvas:delete()
    if not ok then error(result) end
    return result
end

-- Chooser rows for the Ctrl+Enter menu, last used first.
function L.choices(images, n)
    local rows, last = {}, hs.settings.get(kLastKey)
    for _, mode in ipairs(L.available(n)) do
        local built, slots = L.build(mode.key, n, L.jumbleSeed)
        local name = mode.key == "jumble" and (mode.name .. " #" .. L.jumbleSeed) or mode.name
        local row = {text=name, subText=L.describe(mode.key, n, built, slots), layout=mode.key,
                     searchText=mode.name .. " " .. mode.note}
        local ok, image = pcall(L.thumbnail, images, mode.key, built)
        if ok then row.image = image end
        if mode.key == last then table.insert(rows, 1, row) else rows[#rows+1] = row end
    end
    return rows
end

function L.remember(key) hs.settings.set(kLastKey, key) end

local function readBytes(path)
    local file = io.open(path, "rb")
    if not file then return nil end
    local data = file:read("*a"); file:close()
    return data
end

-- Runs bin/emoji-compose.py on items (see its docstring), then encodes each
-- page as a WebP pasteboard item, or a PNG one. callback(err, datas, report),
-- with one pasteboard table per page. Returns a function that cancels it.
function L.compose(items, built, format, callback)
    local base = os.tmpname()
    local spec = {items=items, gap=L.gap, trim=true, timeout=L.renderTimeout, outputs={}}
    for k, cells in ipairs(built) do
        local output = {path=base .. "-" .. k .. ".png", cells=cells, maxLong=4096}
        if format == "webp" then output.long = 512 else output.cell = "source" end
        spec.outputs[k] = output
    end
    local specPath = base .. ".json"
    local file = assert(io.open(specPath, "w"))
    file:write(hs.json.encode(spec)); file:close()
    local cancelled, task, progress = false, nil, nil
    local function clean()
        if progress then progress:stop(); progress = nil; alert_gateway_dismiss("emoji-layout-progress") end
        os.remove(specPath); os.remove(base)
        for _, output in ipairs(spec.outputs) do os.remove(output.path) end
    end
    task = hs.task.new(L.python, function(code, stdout, stderr)
        L.tasks[task] = nil
        if cancelled then clean(); return end
        local ok, err = pcall(function()
            if code ~= 0 then error(stderr ~= "" and stderr or "emoji-compose.py failed", 0) end
            local report = hs.json.decode(stdout)
            local pngs = {}
            for k, output in ipairs(spec.outputs) do
                pngs[k] = assert(readBytes(output.path), "Missing composed emoji page")
            end
            clean()
            if format ~= "webp" then
                local datas = {}
                for k, png in ipairs(pngs) do datas[k] = {["public.png"]=png} end
                callback(nil, datas, report)
                return
            end
            local datas = {}
            local function encode(k)
                if cancelled then return end
                if k > #pngs then callback(nil, datas, report); return end
                emojiChooserWebpPng(pngs[k], function(failure, data)
                    if failure then callback(failure); return end
                    datas[k] = data
                    encode(k + 1)
                end)
            end
            encode(1)
        end)
        if not ok then clean(); callback(tostring(err)) end
    end, {"-I", L.script, specPath})
    assert(task and task:start(), "Cannot start emoji-compose.py")
    L.tasks[task] = true
    -- Demand renders can take seconds; say so rather than look stuck.
    progress = hs.timer.doAfter(0.6, function()
        progress = nil
        alert_gateway("Rendering " .. #items .. " emoji…", {id="emoji-layout-progress", seconds=30, screens="typing"})
    end)
    return function()
        cancelled = true
        if task and task:isRunning() then task:terminate() end
        clean()
    end
end

-- The first page is pasted by the caller; the rest follow one at a time.
-- Purple reads one WebP per paste, and adds each to its open send box as a
-- separate sticker.
function L.pasteRest(datas)
    if L.sequenceTimer then L.sequenceTimer:stop() end
    local k = 1
    local function step()
        L.sequenceTimer = nil
        k = k + 1
        if k > #datas then return end
        if hs.pasteboard.writeAllData(datas[k]) then doPaste() end
        if k < #datas then L.sequenceTimer = hs.timer.doAfter(L.sequenceDelay, step) end
    end
    if #datas > 1 then L.sequenceTimer = hs.timer.doAfter(L.sequenceDelay, step) end
end

-- The Ctrl+Enter menu inside an open chooser. o.images(items) gives preview
-- images, o.export(key, format, items) exports, o.back() restores the list.
function L.menu(o)
    local m = {active=false}
    local enter
    local function show(keepRow)
        local row = keepRow and o.chooser:selectedRow() or 1
        ChooserSearch.setChoices(o.chooser, L.choices(o.images(m.items), #m.items))
        o.chooser:selectedRow(row)
        o.chooser:placeholderText("Layout for " .. #m.items .. " emoji · Enter: WebP · " ..
            "Cmd+Shift+Enter: PNG · Tab: re-roll Jumble · Ctrl+Enter: back")
    end
    function m.open(items)
        if #items == 0 then return end
        if not m.active then m.query = o.chooser:query() end
        m.items, m.active = items, true
        o.chooser:query("")
        show(false)
        enter = enter or hs.hotkey.bind("", "return", function() m.pick("webp") end)
    end
    function m.refresh(items)
        if not m.active then return end
        if items and #items > 0 then m.items = items end
        show(true)
    end
    function m.close()
        if not m.active then return end
        m.cleanup()
        o.chooser:query(m.query or "")
        o.back()
    end
    function m.reroll()
        L.jumbleSeed = L.jumbleSeed + 1
        show(true)
    end
    function m.pick(format)
        local row = o.chooser:selectedRowContents()
        if not (row and row.layout) then return end
        L.remember(row.layout)
        o.export(row.layout, format, m.items)
    end
    function m.cleanup()
        m.active = false
        if enter then enter:delete(); enter = nil end
    end
    return m
end
