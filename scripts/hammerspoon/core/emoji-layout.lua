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

-- Rows of a bitmap joined by "/", X lit: one s=1 cell per lit pixel on a
-- square lattice, in reading order, so glyph boxes never intersect.
local function bitmapCells(bitmap)
    local cells, y = {}, 0
    for row in bitmap:gmatch("[^/]+") do
        for x = 1, #row do
            if row:sub(x, x) == "X" then cells[#cells+1] = {i=#cells+1, x=x - 1, y=y, s=1} end
        end
        y = y + 1
    end
    return cells
end

-- Pixel hearts: an outline of 10, then filled ones. The smallest that holds
-- the selection; the surplus cycles. A curve or a hex mask let neighbours
-- overlap in the cleft and lost the notch once filled.
local hearts = {
    ".X.X./X.X.X/X...X/.X.X./..X..",
    ".X.X./XXXXX/XXXXX/.XXX./..X..",
    ".XX.XX./XXXXXXX/XXXXXXX/.XXXXX./..XXX../...X...",
    ".XX...XX./XXXX.XXXX/XXXXXXXXX/XXXXXXXXX/.XXXXXXX./..XXXXX../...XXX.../....X....",
    ".XXX...XXX./XXXXX.XXXXX/XXXXXXXXXXX/XXXXXXXXXXX/XXXXXXXXXXX/.XXXXXXXXX./..XXXXXXX../...XXXXX.../....XXX..../.....X.....",
    "..XXX...XXX../.XXXXX.XXXXX./XXXXXXXXXXXXX/XXXXXXXXXXXXX/XXXXXXXXXXXXX/XXXXXXXXXXXXX/.XXXXXXXXXXX./..XXXXXXXXX../...XXXXXXX.../....XXXXX..../.....XXX...../......X......",
    "..XXXX...XXXX../.XXXXXX.XXXXXX./XXXXXXXXXXXXXXX/XXXXXXXXXXXXXXX/XXXXXXXXXXXXXXX/XXXXXXXXXXXXXXX/.XXXXXXXXXXXXX./..XXXXXXXXXXX../...XXXXXXXXX.../....XXXXXXX..../.....XXXXX...../......XXX....../.......X.......",
}
local function heart(slots)
    local cells
    for _, bitmap in ipairs(hearts) do
        cells = bitmapCells(bitmap)
        if #cells >= slots then break end
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

-- A disc of seeds on the golden-angle spiral, all the first emoji, inside a
-- ring of larger petals cycling the rest. The petal count keeps petals about
-- 1.45 times the seeds.
local function sunflower(slots)
    local sig, best = 1.25, nil
    for petals = 8, math.max(8, slots - 4) do
        local seeds = math.max(1, slots - petals)
        local disc = math.sqrt(seeds - 0.5) + sig / 2
        local k = 0.95 * math.sin(math.pi / petals)
        local radius = (disc + 0.06) / (1 - k)
        local size = 2 * radius * k
        local score = math.abs(math.log(size / (1.45 * sig)))
        if not best or score < best.score then
            best = {score=score, petals=petals, seeds=seeds, radius=radius, size=size}
        end
    end
    local cells = {}
    for k = 0, best.seeds - 1 do
        local a, r = math.rad(k * 137.508), math.sqrt(k + 0.5)
        cells[#cells+1] = {i=1, x=r * math.sin(a), y=-r * math.cos(a), s=sig}
    end
    for j = 0, best.petals - 1 do
        local a = 2 * math.pi * j / best.petals
        cells[#cells+1] = {i=j + 2, x=best.radius * math.sin(a), y=-best.radius * math.cos(a), s=best.size}
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
-- nudged, turned and resized a little unless flat. Tab picks another seed;
-- flat keeps the same cells for a seed, since the jitter is drawn last.
local function jumble(n, seed, _, flat)
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
        if flat then
            cells[k] = {i=order[k], x=slot % g, y=slot // g, s=1}
        else
            cells[k] = {i=order[k], x=slot % g + rand(-0.12, 0.12), y=slot // g + rand(-0.12, 0.12),
                        s=rand(0.85, 1.1), rot=rand(-15, 15)}
        end
    end
    return {cells}
end

-- Alternating squares with the bottom-right one empty, for the timestamp.
-- Two emoji fill the whole board, one per colour. Assigns its own emoji.
local function chessboard(slots, _, n)
    local r, c = 4, 4
    while (r * c) // 2 < slots do
        if c <= r then c = c + 1 else r = r + 1 end
    end
    local cells, k = {}, 0
    for y = 0, r - 1 do
        for x = 0, c - 1 do
            local dark = (x + y) % 2 == (r + c - 1) % 2
            if n == 2 then
                cells[#cells+1] = {i=dark and 1 or 2, x=x, y=y, s=1}
            elseif dark then
                k = k + 1
                cells[#cells+1] = {i=(k - 1) % n + 1, x=x, y=y, s=1}
            end
        end
    end
    return {cells}
end

-- Six arms of L cells round the first emoji, with barb pairs at the arm
-- positions in B. Cells come in groups of six, one emoji per group while
-- the selection allows, so the flake stays symmetric. Assigns its own emoji.
local snowflakes = {
    {L=2, B={2}}, {L=3, B={3}}, {L=4, B={4}}, {L=3, B={2, 3}}, {L=4, B={2, 4}}, {L=4, B={2, 3, 4}},
}
local function snowflake(slots, _, n)
    local shape = snowflakes[#snowflakes]
    for _, candidate in ipairs(snowflakes) do
        if 1 + 6 * candidate.L + 12 * #candidate.B >= slots then shape = candidate; break end
    end
    local function u(k)
        local a = math.rad(60 * (k % 6))
        return math.sin(a), -math.cos(a)
    end
    local groups = {}
    for j = 1, shape.L do
        local group = {}
        for k = 0, 5 do
            local ux, uy = u(k)
            group[#group+1] = {x=j * ux, y=j * uy, s=1}
        end
        groups[#groups+1] = group
    end
    for _, j in ipairs(shape.B) do
        for _, side in ipairs({1, -1}) do
            local group = {}
            for k = 0, 5 do
                local ux, uy = u(k)
                local vx, vy = u(k + side)
                group[#group+1] = {x=j * ux + 0.85 * vx, y=j * uy + 0.85 * vy, s=0.7}
            end
            groups[#groups+1] = group
        end
    end
    -- Splitting each group of six into f parts (every 6/f-th cell) keeps
    -- 6/f-fold symmetry with f times as many distinct emoji.
    local rest, f = n - 1, 6
    for _, parts in ipairs({1, 2, 3}) do
        if rest <= parts * #groups then f = parts; break end
    end
    local cells = {{i=1, x=0, y=0, s=1}}
    for g, group in ipairs(groups) do
        for k, cell in ipairs(group) do
            local part = (g - 1) * f + (k - 1) % f
            cell.i = rest < 1 and 1 or part % rest + 2
            cells[#cells+1] = cell
        end
    end
    return {cells}
end

-- Foliage tiers of consecutive row counts, each starting one below where the
-- last ended, so {3, 3} is rows 1 2 3 2 3 4; the first emoji is the star on
-- top, then a trunk. The smallest tree that holds the selection.
local pines = {{4}, {3, 3}, {3, 4}, {4, 4}, {4, 5}, {5, 5}, {4, 4, 4}}
local function pineCounts(tiers)
    local counts, start = {}, 1
    for _, length in ipairs(tiers) do
        for row = start, start + length - 1 do counts[#counts+1] = row end
        start = counts[#counts] - 1
    end
    local base = counts[#counts]
    local trunk = base <= 4 and {1} or base <= 6 and {1, 1} or {2, 2}
    for _, count in ipairs(trunk) do counts[#counts+1] = count end
    return counts
end
local function pine(slots)
    local counts
    for _, tiers in ipairs(pines) do
        counts = pineCounts(tiers)
        local total = 0
        for _, count in ipairs(counts) do total = total + count end
        if total >= slots then break end
    end
    local cells = L.rowCells(counts)
    cells[1].s, cells[1].y = 1.25, cells[1].y - 0.2
    return {cells}
end

-- Hero layouts: the first emoji large, p = slots - 1 small ones.

-- Hero in the bottom-right, small cells along the top and left edges.
local function lead(slots)
    local p = slots - 1
    local k = p // 2 + 1
    local cells = {{i=1, x=k / 2, y=k / 2, s=k - 1}}
    for x = (p % 2 == 1 and 0 or 1), k - 1 do cells[#cells+1] = {i=#cells+1, x=x, y=0, s=1} end
    for y = 1, k - 1 do cells[#cells+1] = {i=#cells+1, x=0, y=y, s=1} end
    return {cells}
end

-- Hero filling the sticker, small ones tucked over its corners: a badge,
-- ears, or three with the timestamp corner free.
local function quad(slots)
    local cells = {{i=1, x=0, y=0, s=1.6}}
    local corners = {{1, -1}, {-1, -1}, {-1, 1}, {1, 1}}
    for k = 1, math.min(4, slots - 1) do
        cells[#cells+1] = {i=k+1, x=0.746 * corners[k][1], y=0.746 * corners[k][2], s=0.509}
    end
    return {cells}
end

-- Small cells on an arc over the hero.
local function crown(slots)
    local p = slots - 1
    local delta = p > 1 and math.min(math.rad(210) / (p - 1), math.rad(43.2)) or math.rad(43.2)
    local s = math.min(1.9 * math.sin(delta / 2), 0.7)
    local cells = {{i=1, x=0, y=0, s=0.9 * (2 - s)}}
    for k = 0, p - 1 do
        local a = (k - (p - 1) / 2) * delta
        cells[#cells+1] = {i=k+2, x=math.sin(a), y=-math.cos(a), s=s}
    end
    return {cells}
end

-- A tilted ring passing behind and in front of the hero. Only the visible
-- stretches of the ring get cells; later cells are drawn on top, so the list
-- runs back cells, hero, front cells.
-- Wide, like Saturn's: a tight ring ran across the planet's face and showed
-- only slivers behind it.
L.saturnRing = L.saturnRing or {a=1.15, b=0.32}
local function saturn(slots)
    local p = slots - 1
    local tilt, samples = math.rad(-18), 720
    local points, length = {}, 0
    for k = 0, samples - 1 do
        local t = 2 * math.pi * k / samples
        local lx, ly = L.saturnRing.a * math.cos(t), L.saturnRing.b * math.sin(t)
        local x = lx * math.cos(tilt) - ly * math.sin(tilt)
        local y = lx * math.sin(tilt) + ly * math.cos(tilt)
        local front = ly > 0
        if front or math.sqrt(x * x + y * y) >= 0.6 then
            local last = points[#points]
            if last and last.k == k - 1 then length = length + math.sqrt((x - last.x)^2 + (y - last.y)^2) end
            points[#points+1] = {k=k, x=x, y=y, front=front, at=length}
        end
    end
    local step = length / p
    local s = math.min(0.9 * step, 0.34)
    local back, front, j = {}, {}, 1
    for k = 1, p do
        local target = (k - 0.5) * step
        while points[j+1] and points[j+1].at <= target do j = j + 1 end
        local point = points[j]
        local cell = {i=k+1, x=point.x, y=point.y, s=s}
        if point.front then front[#front+1] = cell else back[#back+1] = cell end
    end
    local cells = back
    cells[#cells+1] = {i=1, x=0, y=0, s=1}
    for _, cell in ipairs(front) do cells[#cells+1] = cell end
    return {cells}
end

-- The hero inside a square border of small cells, clockwise from top-left.
local function frame(slots)
    local p, k = slots - 1, 5
    while 4 * (k - 1) < p do k = k + 1 end
    local cells = {{i=1, x=(k - 1) / 2, y=(k - 1) / 2, s=k - 2}}
    local function add(x, y) cells[#cells+1] = {i=#cells+1, x=x, y=y, s=1} end
    for x = 0, k - 2 do add(x, 0) end
    for y = 0, k - 2 do add(k - 1, y) end
    for x = k - 1, 1, -1 do add(x, k - 1) end
    for y = k - 1, 1, -1 do add(0, y) end
    return {cells}
end

local function pages(n)
    local result = {}
    for first = 1, n, 16 do
        result[#result+1] = L.rowCells(L.autoCounts(math.min(16, n - first + 1)), first)
    end
    return result
end

-- Every shape takes any selection: repeatTo cycles a short one to at least
-- that many cells, and hero layouts keep the first emoji for their first cell
-- and cycle the rest. Only Each and Pages, which split stickers, need min;
-- max is where cells get too small to read. A shape of fixed size may return
-- more cells than asked, which cycle too. build(slots, seed, n) gets the real
-- selection size as n; assigns=true layouts number their cells 1..n
-- themselves.
L.modes = {
    {key="auto", name="Auto", note="grid, short row centred on top",
     build=function(n) return {L.rowCells(L.autoCounts(n))} end},
    {key="row", name="Row", note="one line", repeatTo=3,
     build=function(n) return {L.rowCells({n})} end},
    {key="column", name="Column", note="one stack", repeatTo=3,
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
    {key="beehive", name="Beehive", note="interlocked rows", repeatTo=7, build=beehive},
    {key="daisy", name="Daisy", note="first in the middle, petals around", repeatTo=7, hero=true, build=daisy},
    {key="marquee", name="Marquee", note="first as a hero over the rest", repeatTo=4, hero=true, build=marquee},
    {key="heart", name="Heart", note="outline to 10, filled past", repeatTo=10, build=heart},
    {key="orbit", name="Orbit", note="ring, or rings past 12", repeatTo=3, build=orbit},
    {key="sunflower", name="Sunflower", note="first as the seeds, petals around", repeatTo=21, max=80, hero=true, build=sunflower},
    {key="pyramid", name="Pyramid", note="rows shrinking upwards", repeatTo=6, build=pyramid},
    {key="nautilus", name="Nautilus", note="growing spiral, first largest", repeatTo=8, build=nautilus},
    {key="jumble", name="Jumble", note="random; Tab re-rolls", repeatTo=5, seeded=true, build=jumble},
    {key="jumble-flat", name="Jumble-No-Jitter", note="random cells, upright; Tab re-rolls", repeatTo=5,
     seeded=true, build=function(n, seed) return jumble(n, seed, nil, true) end},
    {key="chessboard", name="Chessboard", note="alternating squares; two fill both colours",
     repeatTo=8, max=50, assigns=true, build=chessboard},
    {key="snowflake", name="Snowflake", note="first in the middle, six barbed arms",
     repeatTo=31, max=61, hero=true, assigns=true, build=snowflake},
    {key="pine", name="Pine", note="first as the star, tiers and a trunk", repeatTo=11, max=58, hero=true, build=pine},
    {key="lead", name="Lead", note="first large, the rest along the top and left", repeatTo=5, max=20, hero=true, build=lead},
    {key="quad", name="Quad", note="first filling it, up to four on its corners", repeatTo=2, max=5, hero=true, build=quad},
    {key="crown", name="Crown", note="first under an arc of the rest", repeatTo=3, max=16, hero=true, build=crown},
    {key="saturn", name="Saturn", note="first as a planet, the rest its ring", repeatTo=11, max=19, hero=true, build=saturn},
    {key="frame", name="Frame", note="first inside a border of the rest", repeatTo=17, max=33, hero=true, build=frame},
}

-- ** Text
-- "text:WORD" spells WORD with one emoji per pixel of a font, so the word
-- travels inside the layout key: history, last used and export need nothing
-- extra. Pure ASCII/symbol text uses the hand-drawn fonts below; Arabic/Persian
-- (or mixed) text is a CoreText Tahoma raster from native/text-bitmap, reduced
-- to L.textRows.persian rows. Words are
-- typed in the layout menu, or preset in ~/witching/emoji/words.txt (one per
-- line, # comments) or the local setting emojiLayout.words.v1, so personal
-- names stay out of this repository.
L.wordsFile = os.getenv("HOME") .. "/witching/emoji/words.txt"
-- How many emoji a word takes, as the rows of one line of text: fewer rows
-- mean fewer and larger emoji and cruder letters. english picks one of the
-- hand-drawn fonts below (3 to 5 rows). persian is the full line height of the
-- Tahoma raster, from the madd of آ to the dots of چ (6 to 10 is the useful
-- range); a word without tall or deep letters takes fewer rows, and text that
-- mixes scripts follows it too. Change at runtime with, e.g.,
-- hs -c 'EmojiLayout.textRows.persian = 9'.
L.textRows = {english=4, persian=7}
-- Size of a dot, and of a stroke tip too short for a whole cell, relative to a
-- body emoji. Dots are never dropped: each takes one emoji of its own.
L.textDotSize = 0.7
L.textTipSize = 0.7
-- Clear space, in cells, kept between a dot group and the strokes around it.
L.textDotGap = 0.3
-- Extra space, in cells, put between two separate strokes that stand side by
-- side (the alef and the yeh of ای), so that they do not read as one thick stroke.
L.textStrokeGap = 0.4
-- The font Persian (and mixed) text is drawn in before it is reduced. Its
-- proportions carry over: Tahoma's alef barely clears its other letters.
-- Tab on a Persian text row of the layout menu moves to the next installed
-- font of L.textFonts; the choice is remembered across reloads, and a layout
-- key names its font (text[Vazirmatn]:word; none means Tahoma). Tahoma keeps
-- teeth clearest; the others have a taller alef and a yeh that dips below the
-- line. IranNastaliq is left out as unreadable at these sizes: adding it
-- here brings it back, its tuning below is kept.
L.textFonts = {"Tahoma", "Vazirmatn", "Parastoo", "Geeza Pro"}
-- What a font needs beyond the defaults. rows: a line that runs downhill
-- needs more rows for the same detail, so L.textRows.persian is multiplied.
-- size: the raster size is multiplied, for a font that is small for its point
-- size. dot: the width of one dot over the raster size, for a font that draws
-- a dot group as one fused piece. ligatures: the font joins letters into
-- shared glyphs, so a dot cannot be traced to its letter and dots are counted
-- by shape alone.
L.textFontTuning = {
    IranNastaliq={rows=1.8, size=2.6, dot=0.155, ligatures=true},
    ["Geeza Pro"]={dot=0.125}, ["B Nazanin"]={dot=0.105}, Damascus={dot=0.11}, Lalezar={dot=0.15},
}
-- Dots by letter. \219\140 is left out: it has two inside a word and none at its end.
local kLetterDots = {}
for count, letters in ipairs({"\216\168\216\172\216\174\216\176\216\178\216\182\216\184\216\186\217\129\217\134", "\216\170\217\130\216\169\217\138", "\217\190\216\171\218\134\218\152\216\180"}) do
    for _, code in utf8.codes(letters) do kLetterDots[code] = count end
end
local kTextFontKey = "emojiLayout.textFont"
L.textFont = (hs.settings and hs.settings.get(kTextFontKey)) or "Tahoma"
local function fontInstalled(name)
    local valid = hs.styledtext and hs.styledtext.validFont
    if not valid then return true end
    local ok, found = pcall(valid, name)
    return ok and found and true or false
end
-- A font that is not installed (another Mac) falls back to Tahoma.
local fontOverride
local function textFont()
    local font = fontOverride
    if not font then
        -- The remembered choice counts only while its font is still offered.
        for _, name in ipairs(L.textFonts) do if name == L.textFont then font = name end end
    end
    if type(font) == "string" and font ~= "" and (font == "Tahoma" or fontInstalled(font)) then return font end
    return "Tahoma"
end
local function withFont(font, fn, ...)
    local before = fontOverride
    fontOverride = font or before
    local result = table.pack(pcall(fn, ...))
    fontOverride = before
    if not result[1] then error(result[2], 0) end
    return table.unpack(result, 2, result.n)
end
function L.installedTextFonts()
    local fonts = {}
    for _, name in ipairs(L.textFonts) do
        if name == "Tahoma" or fontInstalled(name) then fonts[#fonts+1] = name end
    end
    return fonts
end
local function fontAfter(font)
    local fonts, at = L.installedTextFonts(), 0
    for k, name in ipairs(fonts) do if name == font then at = k end end
    return fonts[at % #fonts + 1] or "Tahoma"
end
function L.setTextFont(font)
    L.textFont = font
    if hs.settings then hs.settings.set(kTextFontKey, L.textFont) end
    return L.textFont
end
function L.cycleTextFont() return L.setTextFont(fontAfter(textFont())) end
-- Hand-drawn fonts by row count, mostly 3 wide; M and W are 5 and N and Q 4 in
-- the 4- and 5-row fonts, since at 3 wide round dots blur M, N and H together.
-- The 3-row font is close to a cipher. Rows are joined by "/", X is lit.
L.fonts = {}
for rows, glyphs in pairs({
    [5]={
        A=".X./X.X/XXX/X.X/X.X", B="XX./X.X/XX./X.X/XX.", C=".XX/X../X../X../.XX",
        D="XX./X.X/X.X/X.X/XX.", E="XXX/X../XX./X../XXX", F="XXX/X../XX./X../X..",
        G=".XX/X../X.X/X.X/.XX", H="X.X/X.X/XXX/X.X/X.X", I="X/X/X/X/X",
        J="..X/..X/..X/X.X/.X.", K="X.X/X.X/XX./X.X/X.X", L="X../X../X../X../XXX",
        M="X...X/XX.XX/X.X.X/X...X/X...X", N="X..X/XX.X/X.XX/X..X/X..X",
        O=".X./X.X/X.X/X.X/.X.", P="XX./X.X/XX./X../X..", Q=".XX./X..X/X..X/X.X./.X.X",
        R="XX./X.X/XX./X.X/X.X", S=".XX/X../.X./..X/XX.", T="XXX/.X./.X./.X./.X.",
        U="X.X/X.X/X.X/X.X/XXX", V="X.X/X.X/X.X/.X./.X.",
        W="X...X/X...X/X.X.X/XX.XX/X...X", X="X.X/X.X/.X./X.X/X.X",
        Y="X.X/X.X/.X./.X./.X.", Z="XXX/..X/.X./X../XXX",
        ["0"]="XXX/X.X/X.X/X.X/XXX", ["1"]=".X./XX./.X./.X./XXX", ["2"]="XXX/..X/XXX/X../XXX",
        ["3"]="XXX/..X/XXX/..X/XXX", ["4"]="X.X/X.X/XXX/..X/..X", ["5"]="XXX/X../XXX/..X/XXX",
        ["6"]="XXX/X../XXX/X.X/XXX", ["7"]="XXX/..X/..X/..X/..X", ["8"]="XXX/X.X/XXX/X.X/XXX",
        ["9"]="XXX/X.X/XXX/..X/XXX", ["♥"]=".X.X./XXXXX/XXXXX/.XXX./..X..",
        ["!"]="X/X/X/./X", ["?"]="XX./..X/.X./.../.X.",
    },
    [4]={
        A=".X./X.X/XXX/X.X", B="XX./XX./X.X/XX.", C=".XX/X../X../.XX",
        D="XX./X.X/X.X/XX.", E="XXX/XX./X../XXX", F="XXX/X../XX./X..",
        G=".XX/X../X.X/.XX", H="X.X/XXX/X.X/X.X", I="X/X/X/X",
        J="..X/..X/X.X/.X.", K="X.X/XX./XX./X.X", L="X../X../X../XXX",
        M="X...X/XX.XX/X.X.X/X...X", N="X..X/XX.X/X.XX/X..X",
        O=".X./X.X/X.X/.X.", P="XX./X.X/XX./X..", Q=".XX./X..X/X.X./.X.X",
        R="XX./X.X/XX./X.X", S=".XX/XX./..X/XX.", T="XXX/.X./.X./.X.",
        U="X.X/X.X/X.X/XXX", V="X.X/X.X/X.X/.X.",
        W="X...X/X.X.X/XX.XX/X...X", X="X.X/.X./.X./X.X",
        Y="X.X/X.X/.X./.X.", Z="XXX/.X./X../XXX",
        ["0"]="XXX/X.X/X.X/XXX", ["1"]=".X./XX./.X./XXX", ["2"]="XX./..X/.X./XXX",
        ["3"]="XXX/.XX/..X/XXX", ["4"]="X.X/XXX/..X/..X", ["5"]="XXX/XX./..X/XX.",
        ["6"]="X../XXX/X.X/XXX", ["7"]="XXX/..X/.X./.X.", ["8"]="XXX/XXX/X.X/XXX",
        ["9"]="XXX/X.X/XXX/..X", ["♥"]="XX.XX/XXXXX/.XXX./..X..",
        ["!"]="X/X/./X", ["?"]="XX./.XX/.../.X.",
    },
    [3]={
        A=".X./XXX/X.X", B="XX./XXX/XXX", C="XXX/X../XXX",
        D="XX./X.X/XX.", E="XXX/XX./XXX", F="XXX/XX./X..",
        G="XX./X.X/XXX", H="X.X/XXX/X.X", I="X/X/X",
        J="..X/X.X/XXX", K="X.X/XX./X.X", L="X../X../XXX",
        M="XXX/XXX/X.X", N="XX./X.X/X.X",
        O="XXX/X.X/XXX", P="XXX/XXX/X..", Q="XXX/XXX/..X",
        R="XXX/XX./X.X", S=".XX/.X./XX.", T="XXX/.X./.X.",
        U="X.X/X.X/XXX", V="X.X/X.X/.X.",
        W="X.X/XXX/XXX", X="X.X/.X./X.X",
        Y="X.X/.X./.X.", Z="XX./.X./.XX",
        ["0"]="XXX/X.X/XXX", ["1"]="XX./.X./XXX", ["2"]="XX./.X./.XX",
        ["3"]="XXX/.XX/XXX", ["4"]="X.X/XXX/..X", ["5"]=".XX/.X./XX.",
        ["6"]="X../XXX/XXX", ["7"]="XXX/..X/..X", ["8"]="XXX/XXX/XXX",
        ["9"]="XXX/XXX/..X", ["♥"]="X.X/XXX/.X.",
        ["!"]="X/./X", ["?"]="XXX/.XX/.X.",
    },
}) do
    local font = {}
    for c, text in pairs(glyphs) do
        local list = {}
        for row in text:gmatch("[^/]+") do list[#list+1] = row end
        font[c] = list
    end
    L.fonts[rows] = font
end
-- The character set of the hand-drawn fonts; all three cover the same one.
L.font = L.fonts[5]
-- The hand-drawn font L.textRows.english selects, and its row count.
local function latinFont()
    local rows = math.floor((tonumber(L.textRows.english) or 4) + 0.5)
    rows = math.max(3, math.min(5, rows))
    return L.fonts[rows], rows
end
-- Rows of a Persian line, kept to what the raster can resolve.
local function persianRows()
    return math.max(4, math.min(16, tonumber(L.textRows.persian) or 7))
end
L.textMax = 24

local function chars(text)
    local list = {}
    for _, code in utf8.codes(text) do list[#list+1] = utf8.char(code) end
    return list
end

local function isFontWord(word)
    for _, c in ipairs(chars(word)) do
        if c ~= " " and not L.font[c] then return false end
    end
    return true
end

local function isBitmapCodepoint(cp)
    return cp == 0x200C or cp == 0x200D
        or (cp >= 0x0600 and cp <= 0x06FF)
        or (cp >= 0x0750 and cp <= 0x077F)
        or (cp >= 0x0870 and cp <= 0x08FF)
        or (cp >= 0xFB50 and cp <= 0xFDFF)
        or (cp >= 0xFE70 and cp <= 0xFEFF)
end

local textBitmapDir
do
    local src = debug and debug.getinfo and debug.getinfo(1, "S").source
    if type(src) == "string" and src:sub(1, 1) == "@" then
        local base = src:sub(2):match("^(.-)/?core/emoji%-layout%.lua$")
        if base then textBitmapDir = (base == "" and "." or base) .. "/native/text-bitmap" end
    end
    if not textBitmapDir and type(nightdir) == "string" then
        textBitmapDir = nightdir .. "/hammerspoon/native/text-bitmap"
    end
end

local textBitmapLoadFailed = false
local function readFileBytes(path)
    if type(io) ~= "table" or type(io.open) ~= "function" then return nil end
    local ok, file = pcall(io.open, path, "rb")
    if not ok or not file then return nil end
    local data = file:read("*a")
    file:close()
    return data
end

local function safeGetenv(name)
    if type(os) == "table" and type(os.getenv) == "function" then
        return os.getenv(name)
    end
    return nil
end

local function textBitmapKey(data)
    if type(hs) == "table" and hs.hash and type(hs.hash.SHA256) == "function" then
        return hs.hash.SHA256(data)
    end
    local a, b = 2166136261, 5381
    for i = 1, #data do
        local byte = data:byte(i)
        a = ((a + byte) * 65521) % 4294967296
        b = (b * 33 + byte) % 4294967296
    end
    return string.format("%08x%08x-%d", a, b, #data)
end

local function findLuaIncludeDir()
    if type(hs) == "table" and hs.processInfo and hs.processInfo.bundlePath then
        local dir = hs.processInfo.bundlePath .. "/Contents/Frameworks/LuaSkin.framework/Headers"
        if hs.fs and type(hs.fs.attributes) == "function" and hs.fs.attributes(dir .. "/lua.h", "mode") == "file" then
            return dir
        end
        if readFileBytes(dir .. "/lua.h") then return dir end
        return dir
    end
    for _, dir in ipairs({
        "/opt/homebrew/include/lua",
        "/usr/local/include/lua",
        "/opt/homebrew/include/lua5.4",
        "/usr/local/include/lua5.4",
    }) do
        if readFileBytes(dir .. "/lua.h") then return dir end
    end
    return nil
end

-- Compiles and loads the CoreText text rasterizer on demand, caching the .so
-- by source + header path so neither Hammerspoon nor tests rebuild each run.
function L.loadTextBitmap(force)
    if force then
        L.textBitmap = nil
        textBitmapLoadFailed = false
    end
    if L.textBitmap then return L.textBitmap end
    if textBitmapLoadFailed then return nil end
    if not textBitmapDir or type(package) ~= "table" or type(package.loadlib) ~= "function" then
        textBitmapLoadFailed = true
        return nil
    end
    local modSrc = readFileBytes(textBitmapDir .. "/module.m")
    local buildScript = readFileBytes(textBitmapDir .. "/build.sh")
    local includeDir = findLuaIncludeDir()
    if not modSrc or not buildScript or not includeDir then
        textBitmapLoadFailed = true
        return nil
    end
    local key = textBitmapKey(modSrc .. "\0" .. buildScript .. "\0" .. includeDir)
    local inHammerspoon = type(hs) == "table" and hs.processInfo and hs.processInfo.bundlePath
        and type(hs.execute) == "function"
    local cacheDir = inHammerspoon
        and ((safeGetenv("HOME") or "/tmp") .. "/Library/Caches/org.hammerspoon.Hammerspoon/text-bitmap")
        or ("/tmp/night-emoji-text-bitmap-" .. (safeGetenv("USER") or "user"))
    local binary = cacheDir .. "/" .. key .. ".so"
    local cached = inHammerspoon and hs.fs and type(hs.fs.attributes) == "function"
        and hs.fs.attributes(binary, "mode") == "file"
    if not cached and not inHammerspoon then
        cached = readFileBytes(binary) ~= nil
    end
    if not cached then
        local buildCmd = string.format("/bin/sh %q %q %q", textBitmapDir .. "/build.sh", includeDir, binary)
        local ok
        if inHammerspoon then
            local _, status = hs.execute(buildCmd)
            ok = status == true
        elseif type(os) == "table" and type(os.execute) == "function" then
            local res = os.execute(buildCmd .. " >/dev/null 2>&1")
            ok = res == true or res == 0
        end
        if not ok then
            textBitmapLoadFailed = true
            return nil
        end
    end
    local init = package.loadlib(binary, "luaopen_text_bitmap")
    if not init then
        textBitmapLoadFailed = true
        return nil
    end
    L.textBitmap = init()
    return L.textBitmap
end

-- *** Reducing a raster to few cells
-- Turns a large raster (pixels {x, y, letter} on a w by h grid) into cells
-- {x, y, s, letter} on a grid `cell` pixels coarse, in three parts:
-- - A dot is a connected piece no larger than dotMax whose letter has a larger
--   piece too. It is set aside and never thresholded: each dot becomes one
--   emoji of size L.textDotSize, in its group's own arrangement (a pair, a
--   triangle), at the clear spot nearest its true position.
-- - The rest is thinned to a one-pixel skeleton, and a cell is lit by the
--   skeleton pixels inside it. A stroke is then one cell wide whatever its
--   thickness or position, and stays connected. Cells the skeleton only clips
--   are dropped when that breaks nothing.
-- - A stroke tip too short for a whole cell (a low tooth, the end of a tail)
--   becomes a smaller emoji of size L.textTipSize against its stroke.
-- Returns the cells, with a full cell at the left and top edges centred on 0,
-- and the width and height they cover in cells.
local kRing = {{0,-1},{1,-1},{1,0},{1,1},{0,1},{-1,1},{-1,0},{-1,-1}}

local function rasterComponents(mask, w, h)
    local seen, comps = {}, {}
    for start in pairs(mask) do
        if not seen[start] then
            local comp = {minX=math.huge, maxX=-math.huge, minY=math.huge, maxY=-math.huge}
            local stack = {start}
            seen[start] = true
            while #stack > 0 do
                local k = table.remove(stack)
                comp[#comp+1] = k
                local x, y = k % w, k // w
                comp.minX, comp.maxX = math.min(comp.minX, x), math.max(comp.maxX, x)
                comp.minY, comp.maxY = math.min(comp.minY, y), math.max(comp.maxY, y)
                for _, d in ipairs(kRing) do
                    local nx, ny = x + d[1], y + d[2]
                    if nx >= 0 and nx < w and ny >= 0 and ny < h then
                        local nk = ny * w + nx
                        if mask[nk] and not seen[nk] then seen[nk] = true; stack[#stack+1] = nk end
                    end
                end
            end
            comps[#comps+1] = comp
        end
    end
    table.sort(comps, function(a, b)
        if a.minY ~= b.minY then return a.minY < b.minY end
        return a.minX < b.minX
    end)
    return comps
end

-- Zhang-Suen thinning, in place.
local function rasterThin(mask, w, h)
    local function at(x, y) return (x >= 0 and x < w and y >= 0 and y < h and mask[y * w + x]) and 1 or 0 end
    local changed = true
    while changed do
        changed = false
        for pass = 1, 2 do
            local del = {}
            for k in pairs(mask) do
                local x, y = k % w, k // w
                local p2, p3, p4, p5 = at(x, y-1), at(x+1, y-1), at(x+1, y), at(x+1, y+1)
                local p6, p7, p8, p9 = at(x, y+1), at(x-1, y+1), at(x-1, y), at(x-1, y-1)
                local b = p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9
                if b >= 2 and b <= 6 then
                    local seq, a = {p2, p3, p4, p5, p6, p7, p8, p9, p2}, 0
                    for i = 1, 8 do if seq[i] == 0 and seq[i+1] == 1 then a = a + 1 end end
                    local ok
                    if pass == 1 then ok = p2 * p4 * p6 == 0 and p4 * p6 * p8 == 0
                    else ok = p2 * p4 * p8 == 0 and p2 * p6 * p8 == 0 end
                    if a == 1 and ok then del[#del+1] = k end
                end
            end
            for _, k in ipairs(del) do mask[k] = nil; changed = true end
        end
    end
end

-- dotUnit, for a font that draws a dot group as one fused piece (Nastaliq),
-- is the width of one dot: a compact piece up to about two dots across that
-- belongs to a letter with a body is split into its dots. expect, letter to
-- dot count, is what the alphabet says (three for \217\190): the dot pieces of such a
-- letter share that many dots by area, however the font draws them.
-- byArea, for a font that squeezes several letters' dots into one blob,
-- counts the dots of a piece by its area and takes pieces up to four across.
function L.reduceRaster(pixels, w, h, cell, dotMax, dotUnit, expect, byArea)
    local dotSize = math.max(0.3, math.min(1, tonumber(L.textDotSize) or 0.7))
    local tipSize = math.max(0.3, math.min(1, tonumber(L.textTipSize) or 0.7))
    local dotGap = math.max(0, math.min(1, tonumber(L.textDotGap) or 0.3))
    local mask, owner = {}, {}
    for _, p in ipairs(pixels) do
        mask[p.y * w + p.x] = true
        owner[p.y * w + p.x] = p.letter
    end

    -- Dots apart from the body.
    local comps, large = rasterComponents(mask, w, h), {}
    -- A speck of a pixel or two is the rasterizer's, not the letter's.
    for k = #comps, 1, -1 do
        if #comps[k] < 0.02 * dotMax * dotMax and #comps > 1 then
            for _, q in ipairs(comps[k]) do mask[q] = nil end
            table.remove(comps, k)
        end
    end
    for _, comp in ipairs(comps) do
        local cw, ch = comp.maxX - comp.minX + 1, comp.maxY - comp.minY + 1
        comp.small = cw - 1 < dotMax and ch - 1 < dotMax
        comp.letter = owner[comp[1]]
        local reach = (byArea and 4 or 2.3) * (dotUnit or 0)
        comp.fused = not comp.small and dotUnit and cw <= reach and ch <= reach
            and #comp >= 0.4 * cw * ch and math.max(1, math.floor(byArea
                and #comp / (0.72 * dotUnit * dotUnit) + 0.5 or math.max(cw, ch) / dotUnit + 0.5))
        if not comp.small and not comp.fused then
            for _, k in ipairs(comp) do large[owner[k]] = true end
        end
    end
    local byLetter = {}
    for _, comp in ipairs(comps) do
        if comp.fused and not large[comp.letter] then comp.fused = nil end
        if comp.small then comp.fused = 1 end
        if comp.fused and large[comp.letter] then
            byLetter[comp.letter] = byLetter[comp.letter] or {area=0}
            local pieces = byLetter[comp.letter]
            pieces[#pieces+1] = comp
            pieces.area = pieces.area + #comp
        end
    end
    for letter, pieces in pairs(byLetter) do
        local want = expect and expect[letter]
        if want and #pieces <= want then
            -- Largest remainders, at least a dot a piece.
            local left = want
            for _, comp in ipairs(pieces) do
                comp.share = want * #comp / pieces.area
                comp.fused = math.max(1, math.floor(comp.share))
                left = left - comp.fused
            end
            table.sort(pieces, function(a, b)
                local ra, rb = a.share - a.fused, b.share - b.fused
                if ra ~= rb then return ra > rb end
                return a[1] < b[1]
            end)
            for _, comp in ipairs(pieces) do
                if left <= 0 then break end
                comp.fused, left = comp.fused + 1, left - 1
            end
        end
    end
    local dots, body, pieceOf = {}, {}, {}
    for id, comp in ipairs(comps) do
        if comp.fused and comp.fused > 1 then
            -- Bands across the longer side, a dot at the middle of each.
            local wide = comp.maxX - comp.minX >= comp.maxY - comp.minY
            local lo = wide and comp.minX or comp.minY
            local span = (wide and comp.maxX or comp.maxY) - lo + 1
            local sums = {}
            for _, k in ipairs(comp) do
                local at = wide and k % w or k // w
                local band = math.min(comp.fused, math.floor((at - lo) * comp.fused / span) + 1)
                local sum = sums[band] or {x=0, y=0, n=0}
                sums[band] = sum
                sum.x, sum.y, sum.n = sum.x + k % w, sum.y + k // w, sum.n + 1
            end
            for band = 1, comp.fused do
                local sum = sums[band]
                if sum then
                    dots[#dots+1] = {px=sum.x / sum.n + 0.5, py=sum.y / sum.n + 0.5,
                        letter=comp.letter, s=dotSize}
                end
            end
        elseif comp.fused or comp.small then
            local sx, sy = 0, 0
            for _, k in ipairs(comp) do sx, sy = sx + k % w, sy + k // w end
            -- A small piece that is all of its letter (a digit ۰) is drawn full size.
            dots[#dots+1] = {px=sx / #comp + 0.5, py=sy / #comp + 0.5, letter=comp.letter,
                s=large[comp.letter] and dotSize or 1}
        else
            for _, k in ipairs(comp) do body[k] = true; pieceOf[k] = id end
        end
    end
    rasterThin(body, w, h)

    -- The busiest skeleton row is the baseline. A cell edge goes just above
    -- it, so that teeth standing on the baseline fill the row above.
    local hist, baseY, baseN = {}, 0, 0
    for k in pairs(body) do hist[k // w] = (hist[k // w] or 0) + 1 end
    for y = 0, h - 1 do
        if (hist[y] or 0) > baseN then baseY, baseN = y, hist[y] end
    end
    local oy = (baseY - 1.5) % cell

    local grid = {}
    local function gk(cx, cy) return cx .. "," .. cy end
    local function lit(cx, cy) return grid[gk(cx, cy)] ~= nil end
    for k in pairs(body) do
        local cx, cy = math.floor((k % w + 0.5) / cell), math.floor((k // w + 0.5 - oy) / cell)
        local g = grid[gk(cx, cy)]
        if not g then
            g = {cx=cx, cy=cy, n=0, letters={}, pieces={}}
            grid[gk(cx, cy)] = g
        end
        g.n = g.n + 1
        g.letters[owner[k]] = (g.letters[owner[k]] or 0) + 1
        g.pieces[pieceOf[k]] = (g.pieces[pieceOf[k]] or 0) + 1
    end

    -- Neighbours of a cell: how many, how many of them edge to edge, whether
    -- they stay joined to each other without it, and the way to the stroke it
    -- sticks out of (nil unless every neighbour lies on one side).
    local function around(cx, cy)
        local v, count, edges, toward = {}, 0, 0, nil
        for i, d in ipairs(kRing) do
            v[i] = lit(cx + d[1], cy + d[2])
            if v[i] then
                count = count + 1
                if d[1] == 0 or d[2] == 0 then edges = edges + 1; toward = d end
            end
        end
        local seen, groups = {}, 0
        for i = 1, 8 do
            if v[i] and not seen[i] then
                groups = groups + 1
                local stack = {i}
                seen[i] = true
                while #stack > 0 do
                    local a = kRing[table.remove(stack)]
                    for j = 1, 8 do
                        local b = kRing[j]
                        if v[j] and not seen[j] and math.abs(a[1] - b[1]) <= 1 and math.abs(a[2] - b[2]) <= 1 then
                            seen[j] = true
                            stack[#stack+1] = j
                        end
                    end
                end
            end
        end
        if edges == 1 then
            for i, d in ipairs(kRing) do
                if v[i] and d[1] * toward[1] + d[2] * toward[2] ~= 1 then toward = nil; break end
            end
        elseif count == 1 then
            for i, d in ipairs(kRing) do if v[i] then toward = d end end
        else
            toward = nil
        end
        return count, edges, groups == 1, toward
    end

    local order = {}
    for _, g in pairs(grid) do order[#order+1] = g end
    table.sort(order, function(a, b)
        if a.n ~= b.n then return a.n < b.n end
        if a.cy ~= b.cy then return a.cy < b.cy end
        return a.cx < b.cx
    end)
    for _, g in ipairs(order) do
        local count, edges, joined, toward = around(g.cx, g.cy)
        local drop = false
        if count == 0 then
            drop = false                                    -- All that is left of a piece.
        elseif g.n < 0.2 * cell then
            drop = joined                                   -- Barely clipped.
        elseif g.n < 0.34 * cell then
            drop = joined and not toward                    -- Clipped in passing; a tip is kept, small.
        elseif g.n < 0.6 * cell and joined and count == 2 and edges == 2 then
            -- A staircase corner: its two neighbours still touch diagonally.
            drop = lit(g.cx + 1, g.cy) ~= lit(g.cx - 1, g.cy)
        end
        if drop then grid[gk(g.cx, g.cy)] = nil end
    end
    -- A stroke is one cell wide: a full 2 by 2 block loses a cell that nothing
    -- hangs on, the one with the fewest neighbours outside the block (a bump
    -- on a stroke, not a piece of it), then the weakest. Only a block that one
    -- connected piece fills is a thick stroke: two pieces side by side (the
    -- alef next to the yeh in \216\167\219\140) are separate strokes, and thinning them
    -- would eat one from its end.
    local function onePiece(x, y)
        for id in pairs(grid[gk(x, y)].pieces) do
            if grid[gk(x + 1, y)].pieces[id] and grid[gk(x, y + 1)].pieces[id]
                and grid[gk(x + 1, y + 1)].pieces[id] then return true end
        end
        return false
    end
    local thinned = true
    while thinned do
        thinned = false
        for _, g in ipairs(order) do
            local x, y = g.cx, g.cy
            if lit(x, y) and lit(x + 1, y) and lit(x, y + 1) and lit(x + 1, y + 1) and onePiece(x, y) then
                local pick, pickOutside
                for _, d in ipairs({{0,0},{1,0},{0,1},{1,1}}) do
                    local c = grid[gk(x + d[1], y + d[2])]
                    local count, _, joined = around(c.cx, c.cy)
                    local outside = count - 3
                    -- A cell a stroke runs straight through stays, or the stroke would show a gap.
                    local through = (lit(c.cx - 1, c.cy) and lit(c.cx + 1, c.cy))
                        or (lit(c.cx, c.cy - 1) and lit(c.cx, c.cy + 1))
                    if joined and not through and (not pick or outside < pickOutside
                        or (outside == pickOutside and c.n < pick.n)) then
                        pick, pickOutside = c, outside
                    end
                end
                if pick then
                    grid[gk(pick.cx, pick.cy)] = nil
                    thinned = true
                end
            end
        end
    end

    -- Two pieces standing side by side, a cell apart, would read as one thick
    -- stroke. Where no piece crosses the line between their columns, the text
    -- is cut there and everything to the right moves over by L.textStrokeGap.
    local strokeGap = math.max(0, math.min(1, tonumber(L.textStrokeGap) or 0.4))
    -- A cell belongs to the piece with most of its pixels: the hook of a
    -- neighbouring letter may reach a little into it.
    local function piece(g)
        if not g.piece then
            local most = -1
            for id, n in pairs(g.pieces) do
                if n > most or (n == most and id < g.piece) then g.piece, most = id, n end
            end
        end
        return g.piece
    end
    local function shared(a, b) return piece(a) == piece(b) end
    local touching, crossed, cuts = {}, {}, {}
    for _, g in pairs(grid) do
        for dy = -1, 1 do
            local r = grid[gk(g.cx + 1, g.cy + dy)]
            if r and shared(g, r) then crossed[g.cx] = true
            elseif r and dy == 0 then touching[g.cx] = true end
        end
    end
    local cutAt = {}
    for b in pairs(touching) do
        if not crossed[b] and strokeGap > 0 then cuts[#cuts+1] = b + 0.5; cutAt[b] = true end
    end
    -- Where no cut can part them (one piece above another, as the bar of a gaf
    -- over its arm), the smaller piece moves away whole, by the same gap.
    local pieceSize, push = {}, {}
    for _, g in pairs(grid) do pieceSize[piece(g)] = (pieceSize[piece(g)] or 0) + 1 end
    for _, g in pairs(grid) do
        for _, d in ipairs({{1, 0}, {0, 1}}) do
            local r = grid[gk(g.cx + d[1], g.cy + d[2])]
            if r and piece(r) ~= piece(g) and not (d[1] == 1 and cutAt[g.cx]) and strokeGap > 0 then
                local small, way = piece(g), -1
                if pieceSize[piece(r)] < pieceSize[small] then small, way = piece(r), 1 end
                push[small] = push[small] or {0, 0}
                push[small][1], push[small][2] = push[small][1] + d[1] * way, push[small][2] + d[2] * way
            end
        end
    end
    local function pushed(g)
        local p = push[piece(g)]
        if not p then return 0, 0 end
        if math.abs(p[2]) >= math.abs(p[1]) then return 0, (p[2] < 0 and -1 or 1) * strokeGap end
        return (p[1] < 0 and -1 or 1) * strokeGap, 0
    end
    local function cutShift(x)
        local n = 0
        for _, b in ipairs(cuts) do if b < x then n = n + 1 end end
        return n * strokeGap
    end

    local out, solid = {}, {}
    for _, g in pairs(grid) do
        local letter, most = nil, -1
        for l, n in pairs(g.letters) do
            if n > most or (n == most and l < letter) then letter, most = l, n end
        end
        local pushX, pushY = pushed(g)
        local c = {x=g.cx + cutShift(g.cx) + pushX, y=g.cy + pushY, s=1, letter=letter}
        local count, _, joined, toward = around(g.cx, g.cy)
        if count > 0 and joined and toward and g.n < 0.55 * cell and tipSize < 1 then
            c.s = tipSize
            c.x, c.y = c.x + toward[1] * (1 - tipSize) / 2, c.y + toward[2] * (1 - tipSize) / 2
        end
        out[#out+1] = c
        solid[#solid+1] = c
    end

    -- Dots, by letter and nearness into groups.
    local groups = {}
    table.sort(dots, function(a, b)
        if a.py ~= b.py then return a.py < b.py end
        return a.px < b.px
    end)
    for _, d in ipairs(dots) do
        local home
        for _, g in ipairs(groups) do
            if g.letter == d.letter and g.s == d.s then
                for _, e in ipairs(g) do
                    if math.abs(e.px - d.px) <= 2.5 * dotMax and math.abs(e.py - d.py) <= 2.5 * dotMax then home = g end
                end
            end
        end
        if not home then
            home = {letter=d.letter, s=d.s}
            groups[#groups+1] = home
        end
        home[#home+1] = d
    end
    -- Offsets to try, nearest first.
    local shifts = {}
    for dy = -30, 30 do for dx = -30, 30 do shifts[#shifts+1] = {dx / 10, dy / 10} end end
    table.sort(shifts, function(a, b)
        local da, db = a[1]^2 + a[2]^2, b[1]^2 + b[2]^2
        if da ~= db then return da < db end
        if a[2] ~= b[2] then return a[2] < b[2] end
        return a[1] < b[1]
    end)
    for _, g in ipairs(groups) do
        local rows, cx, cy = {}, 0, 0
        for _, d in ipairs(g) do
            local row = rows[#rows]
            if not row or d.py - row.py > 0.3 * dotMax then
                row = {py=d.py}
                rows[#rows+1] = row
            end
            row[#row+1] = d
            cx, cy = cx + (d.px / cell - 0.5) / #g, cy + ((d.py - oy) / cell - 0.5) / #g
        end
        -- The group's arrangement, one dot apart: rows of unequal parity interlock.
        local shape, yOff, mean = {}, 0, 0
        for r, row in ipairs(rows) do
            table.sort(row, function(a, b) return a.px < b.px end)
            if r > 1 then yOff = yOff + ((#row + #rows[r-1]) % 2 == 1 and 0.87 or 1) * g.s end
            for j = 1, #row do shape[#shape+1] = {dx=(j - (#row + 1) / 2) * g.s, dy=yOff} end
        end
        for _, p in ipairs(shape) do mean = mean + p.dy / #shape end
        local function clear(ax, ay)
            for _, p in ipairs(shape) do
                local x, y = ax + p.dx, ay + p.dy - mean
                for _, c in ipairs(solid) do
                    local reach = (c.s + g.s) / 2 + dotGap
                    if math.abs(c.x - x) < reach and math.abs(c.y - y) < reach then return false end
                end
            end
            return true
        end
        cx = cx + cutShift(cx)
        local ax, ay = cx, cy
        for _, shift in ipairs(shifts) do
            if clear(cx + shift[1], cy + shift[2]) then ax, ay = cx + shift[1], cy + shift[2]; break end
        end
        for _, p in ipairs(shape) do
            local c = {x=ax + p.dx, y=ay + p.dy - mean, s=g.s, letter=g.letter}
            out[#out+1] = c
            solid[#solid+1] = c
        end
    end

    if #out == 0 then return out, 0, 0 end
    local minX, minY, maxX, maxY = math.huge, math.huge, -math.huge, -math.huge
    for _, c in ipairs(out) do
        minX, maxX = math.min(minX, c.x - c.s / 2), math.max(maxX, c.x + c.s / 2)
        minY, maxY = math.min(minY, c.y - c.s / 2), math.max(maxY, c.y + c.s / 2)
    end
    for _, c in ipairs(out) do c.x, c.y = c.x - minX - 0.5, c.y - minY - 0.5 end
    table.sort(out, function(a, b)
        if a.y ~= b.y then return a.y < b.y end
        return a.x < b.x
    end)
    return out, maxX - minX, maxY - minY
end

-- Tahoma is rasterized large, so that dots and strokes are well apart, then
-- reduced to L.textRows.persian rows.
L.textRasterSize = 48
local bitmapLineCache, bitmapLineCount, bitmapCacheMod, bitmapRefHeight = {}, 0, nil, nil
local function rasterBitmapLine(tb, text)
    if bitmapCacheMod ~= tb then
        bitmapLineCache, bitmapLineCount, bitmapCacheMod, bitmapRefHeight = {}, 0, tb, nil
    end
    local font = textFont()
    local tuning = L.textFontTuning[font] or {}
    local size = L.textRasterSize * (tonumber(tuning.size) or 1)
    local rows = persianRows() * (tonumber(tuning.rows) or 1)
    local id = table.concat({text, size, rows, tostring(L.textDotSize), tostring(L.textTipSize),
        tostring(L.textStrokeGap) .. "\0" .. tostring(L.textDotGap), font}, "\0")
    local cached = bitmapLineCache[id]
    if cached then return cached end
    if not bitmapRefHeight or bitmapRefHeight.size ~= size or bitmapRefHeight.font ~= font then
        -- A full line: from the madd of آ to the dots of چ.
        local _, _, height = tb.rasterize("آگچ", font, size)
        bitmapRefHeight = {size=size, font=font, height=math.max(1, height or 0, size)}
    end
    local pixels, width, height, letters = tb.rasterize(text, font, size)
    -- The rasterizer numbers the visible letters in reading order; when that
    -- matches the text's own letters, each one's dot count is known.
    local expect, codes = {}, {}
    for _, code in utf8.codes(text) do
        if code > 0x20 and code ~= 0x200C and code ~= 0x200D and code ~= 0x640
            and not (code >= 0x64B and code <= 0x65F) then codes[#codes+1] = code end
    end
    if #codes == letters and not tuning.ligatures then
        for k, code in ipairs(codes) do expect[k] = kLetterDots[code] end
    end
    local cells, cellsWide, cellsHigh = L.reduceRaster(pixels, width, height,
        bitmapRefHeight.height / rows, size * (tuning.dot and 0.2 or 0.17), tuning.dot and size * tuning.dot, expect, tuning.ligatures)
    local entry = {text=text, cells=cells, width=cellsWide, height=cellsHigh, letters=letters}
    if bitmapLineCount >= 256 then
        bitmapLineCache, bitmapLineCount = {}, 0
    end
    bitmapLineCache[id] = entry
    bitmapLineCount = bitmapLineCount + 1
    return entry
end

-- The canonical word for text, or nil when neither the hand-drawn fonts nor CoreText can draw it.
function L.textWord(text)
    text = tostring(text or "")
    if not utf8.len(text) then return nil end
    local word = text:gsub("[a-z]", string.upper):gsub("%s+", " "):match("^%s*(.-)%s*$")
    if word == "" or utf8.len(word) > L.textMax then return nil end
    if isFontWord(word) then return word end
    for _, code in utf8.codes(word) do
        local c = utf8.char(code)
        if c ~= " " and not L.font[c] and not isBitmapCodepoint(code) then return nil end
    end
    local tb = L.loadTextBitmap()
    if not tb then return nil end
    local raster = rasterBitmapLine(tb, word)
    if not raster or #raster.cells == 0 then return nil end
    return word
end

-- Columns of a line: glyphs one column apart, a space two columns.
local function textWidth(list)
    local w, font = 0, latinFont()
    for _, c in ipairs(list) do w = w + (c == " " and 2 or #font[c][1]) + 1 end
    return math.max(0, w - 1)
end

-- Splits into 1 to 3 lines, scoring each split by the pixel size it allows
-- in a square sticker, 1 / max(width, height). Only at spaces when there are
-- any. A line is added only for a 25% gain; ties keep fewer lines, then the
-- narrower split, then the longer first line.
local function textLines(word)
    local list, h = chars(word), select(2, latinFont())
    local spaces = word:find(" ", 1, true) ~= nil
    local function trim(part)
        while part[1] == " " do table.remove(part, 1) end
        while part[#part] == " " do table.remove(part) end
        return part
    end
    local function slice(a, b) return trim({table.unpack(list, a, b)}) end
    local function candidates(count, from)
        if count == 1 then return {{slice(from, #list)}} end
        local result = {}
        for cut = from, #list - 1 do
            if not spaces or list[cut + 1] == " " then
                local head = slice(from, cut)
                if #head > 0 then
                    for _, rest in ipairs(candidates(count - 1, cut + 1)) do
                        if #rest[1] > 0 then result[#result+1] = {head, table.unpack(rest)} end
                    end
                end
            end
        end
        return result
    end
    local best, bestScore
    for count = 1, 3 do
        local winner, score, winnerWidth
        for _, lines in ipairs(candidates(count, 1)) do
            local w = 0
            for _, line in ipairs(lines) do w = math.max(w, textWidth(line)) end
            local value = 1 / math.max(w, (h + 1) * count - 1)
            local tie = score and math.abs(value - score) < 1e-12
            if not score or value > score + 1e-12
                or (tie and (w < winnerWidth or (w == winnerWidth and #lines[1] > #winner[1]))) then
                winner, score, winnerWidth = lines, value, w
            end
        end
        if winner and (not bestScore or score >= 1.25 * bestScore) then best, bestScore = winner, score end
    end
    return best or {list}
end

local function fontCells(word)
    local font, h = latinFont()
    local cells, top, letter = {}, 0, 0
    for _, line in ipairs(textLines(word)) do
        local glyphs, x = {}, 0
        for _, c in ipairs(line) do
            if c ~= " " then
                letter = letter + 1
                glyphs[#glyphs+1] = {rows=font[c], x=x, letter=letter}
            end
            x = x + (c == " " and 2 or #font[c][1]) + 1
        end
        local width = x - 1
        for y = 1, h do
            for _, glyph in ipairs(glyphs) do
                local row = glyph.rows[y]
                for dx = 1, #row do
                    if row:sub(dx, dx) == "X" then
                        cells[#cells+1] = {i=glyph.letter, x=glyph.x + dx - 1 - (width - 1) / 2, y=top + y - 1, s=1}
                    end
                end
            end
        end
        top = top + h + 1
    end
    return cells, letter, math.max(0, top - 1)
end

-- Splits non-ASCII text strictly at spaces into 1..3 lines one blank row
-- apart, applying the same 25% square-fit gain rule without breaking joined words.
local function bitmapLines(tb, word)
    local tokens, gap = {}, 1
    for tok in word:gmatch("[^ ]+") do tokens[#tokens+1] = tok end
    if #tokens == 0 then return {} end
    local function sliceRaster(a, b)
        return rasterBitmapLine(tb, table.concat(tokens, " ", a, b))
    end
    local function candidates(count, from)
        if count == 1 then return {{sliceRaster(from, #tokens)}} end
        local result = {}
        for cut = from, #tokens - count + 1 do
            local head = sliceRaster(from, cut)
            for _, rest in ipairs(candidates(count - 1, cut + 1)) do
                result[#result+1] = {head, table.unpack(rest)}
            end
        end
        return result
    end
    local best, bestScore
    for count = 1, math.min(3, #tokens) do
        local winner, score, winnerWidth, winnerFirstLen
        for _, lines in ipairs(candidates(count, 1)) do
            local w, totalH = 0, gap * (count - 1)
            for _, line in ipairs(lines) do
                w = math.max(w, line.width)
                totalH = totalH + line.height
            end
            local value = 1 / math.max(1, w, totalH)
            local firstLen = utf8.len(lines[1].text) or #lines[1].text
            local tie = score and math.abs(value - score) < 1e-12
            if not score or value > score + 1e-12
                or (tie and (w < winnerWidth or (w == winnerWidth and firstLen > winnerFirstLen))) then
                winner, score, winnerWidth, winnerFirstLen = lines, value, w, firstLen
            end
        end
        if winner and (not bestScore or score >= 1.25 * bestScore) then best, bestScore = winner, score end
    end
    return best or {rasterBitmapLine(tb, word)}
end

local function bitmapCells(word)
    local tb = L.loadTextBitmap()
    if not tb then return {}, 0, 0 end
    local cells, top, letter, gap = {}, 0, 0, 1
    for _, line in ipairs(bitmapLines(tb, word)) do
        for _, px in ipairs(line.cells) do
            cells[#cells+1] = {
                i = letter + px.letter,
                x = px.x - (line.width - 1) / 2,
                y = top + px.y,
                s = px.s,
            }
        end
        letter = letter + line.letters
        top = top + line.height + gap
    end
    return cells, letter, math.max(0, top - gap)
end

-- One cell per lit pixel, lines centred. Each letter is one emoji, cycling
-- through the selection. With more emoji than letters, each pixel row is one
-- emoji (stripes), and with more than rows, each pixel.
function L.textCells(word, n, font)
    local cells, letter, totalHeight
    if isFontWord(word) then
        cells, letter, totalHeight = fontCells(word)
    else
        cells, letter, totalHeight = withFont(font, bitmapCells, word)
    end
    n = n or letter
    if n > 0 then
        for k, cell in ipairs(cells) do
            if n <= letter then cell.i = (cell.i - 1) % n + 1
            elseif n <= totalHeight then cell.i = math.floor(cell.y + 0.5) % n + 1
            else cell.i = (k - 1) % n + 1 end
        end
    end
    return cells
end

-- Cached per word and row setting, since repeatTo is the cell count.
local textModes = {}
local function textMode(key, word, font)
    font = font and withFont(font, textFont)
    local id = table.concat({key, select(2, latinFont()), persianRows(), tostring(L.textDotSize),
        tostring(L.textTipSize), tostring(L.textStrokeGap), tostring(L.textDotGap), font or ""}, "\0")
    if not textModes[id] then
        local cells = L.textCells(word, nil, font)
        local name = "Text " .. word
        if font then name = name .. " · " .. font end
        textModes[id] = {key=key, name=name, note="the word in emoji, a letter per emoji",
            word=word, font=font, repeatTo=#cells, assigns=true,
            build=function(_, _, n) return {L.textCells(word, n, font)} end}
    end
    return textModes[id]
end

-- A text layout's key. Persian (or mixed) text names its font, text[Font]:word;
-- Tahoma, the font of every key written before fonts could be chosen, is left out.
function L.textKey(word, font)
    if isFontWord(word) then return "text:" .. word end
    font = font or textFont()
    return font == "Tahoma" and ("text:" .. word) or ("text[" .. font .. "]:" .. word)
end
local function parseTextKey(key)
    if type(key) ~= "string" then return nil end
    local font, raw = key:match("^text%[([^%]]+)%]:(.+)$")
    if not raw then font, raw = nil, key:match("^text:(.+)$") end
    local word = raw and L.textWord(raw)
    if not word then return nil end
    if isFontWord(word) then return word, nil end
    return word, font or "Tahoma"
end

-- Whether a layout is drawn in a font of L.textFonts (Persian or mixed text).
function L.usesTextFont(key)
    local word, font = parseTextKey(key)
    return word and font and true or false
end

function L.mode(key)
    for _, mode in ipairs(L.modes) do if mode.key == key then return mode end end
    local word, font = parseTextKey(key)
    if word then return textMode(L.textKey(word, font), word, font) end
    error("Unknown emoji layout: " .. tostring(key))
end

-- What Tab steps through for a layout: the installed fonts for Persian text,
-- the next seeds for a seeded layout. Nil when there is nothing to choose.
function L.variants(key, count)
    local word, font = parseTextKey(key)
    local list = {}
    if word and font then
        for _, name in ipairs(L.installedTextFonts()) do list[#list+1] = {key=L.textKey(word, name), font=name} end
    elseif L.seeded(key) then
        for seed = L.jumbleSeed, L.jumbleSeed + (count or 8) - 1 do list[#list+1] = {key=key, seed=seed} end
    end
    return #list > 1 and list or nil
end
-- The next of them after the given one: a key and a seed.
function L.nextVariant(key, seed)
    local word, font = parseTextKey(key)
    if word and font then return L.textKey(word, fontAfter(font)), seed end
    if L.seeded(key) then return key, (seed or L.jumbleSeed) + 1 end
    return key, seed
end

-- Whether a layout's cells depend on the seed, which Tab re-rolls.
function L.seeded(key)
    if parseTextKey(key) then return false end
    local ok, mode = pcall(L.mode, key)
    return ok and mode.seeded or false
end

function L.words()
    local result, seen, texts = {}, {}, {}
    local file = io.open(L.wordsFile, "r")
    if file then
        local data = file:read("a") or ""
        file:close()
        for line in data:gmatch("[^\n]+") do
            local text = line:gsub("#.*$", "")
            if text:find("%S") then texts[#texts+1] = text end
        end
    end
    local saved = hs.settings.get("emojiLayout.words.v1")
    for _, text in ipairs(type(saved) == "table" and saved or {}) do texts[#texts+1] = text end
    for _, text in ipairs(texts) do
        local word = L.textWord(text)
        if word and not seen[word] then seen[word] = true; result[#result+1] = word end
    end
    return result
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
    local result = mode.build(slots, seed or L.jumbleSeed, n)
    if not mode.separate then slots = math.max(slots, #result[1]) end
    for _, cells in ipairs(result) do
        for _, cell in ipairs(cells) do
            if mode.assigns then
                assert(cell.i >= 1 and cell.i <= n, "Layout assigned an emoji out of range")
            elseif mode.hero and n > 1 then
                cell.i = cell.i == 1 and 1 or (cell.i - 2) % (n - 1) + 2
            else
                cell.i = (cell.i - 1) % n + 1
            end
        end
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
    if mode.word then parts[#parts+1] = #built[1] .. " cells"
    elseif slots > n then parts[#parts+1] = "repeated to " .. slots end
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
function L.row(images, n, mode, seed)
    local built, slots = L.build(mode.key, n, seed or L.jumbleSeed)
    local name = mode.seeded and (mode.name .. " #" .. (seed or L.jumbleSeed)) or mode.name
    local row = {text=name, subText=L.describe(mode.key, n, built, slots), layout=mode.key,
                 seed=mode.seeded and seed or nil, searchText=mode.name .. " " .. mode.note}
    local ok, image = pcall(L.thumbnail, images, mode.key, built)
    if ok then row.image = image end
    return row
end

-- The menu's row for whatever is typed, when the font can spell it.
function L.textRow(images, n, query)
    local word = L.textWord(query)
    if not word then return nil end
    local row = L.row(images, n, L.mode(L.textKey(word)))
    row.searchText = query
    return row
end

function L.choices(images, n)
    local rows, last = {}, hs.settings.get(kLastKey)
    local modes = L.available(n)
    for _, word in ipairs(L.words()) do modes[#modes+1] = L.mode(L.textKey(word)) end
    local lastKey = last
    if type(last) == "string" and last:match("^text[:%[]") then
        local ok, lastMode = pcall(L.mode, last)
        if ok then
            -- The remembered word, in the font chosen now.
            lastMode = L.mode(L.textKey(lastMode.word))
            lastKey = lastMode.key
            local seen = false
            for _, mode in ipairs(modes) do if mode.key == lastKey then seen = true; break end end
            if not seen then modes[#modes+1] = lastMode end
        end
    end
    for _, mode in ipairs(modes) do
        local row = L.row(images, n, mode)
        if mode.key == lastKey then table.insert(rows, 1, row) else rows[#rows+1] = row end
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

-- A large picture of the highlighted row, at the right of the screen: a row's
-- own image is too small to judge a shape by. resolve(row) gives the layout
-- key, the emoji count, the seed and a function returning the images, or nil
-- for a row that has no layout.
L.previewSize = L.previewSize or 440
function L.previewPane(chooser, resolve)
    local pane, canvas, timer, shown = {}, nil, nil, nil
    local function draw()
        local row = chooser:selectedRowContents()
        local key, n, seed, images
        if row and next(row) then key, n, seed, images = resolve(row) end
        local id = key and table.concat({key, tostring(n), tostring(seed), tostring(row.text), tostring(row.history)}, "\0")
        if id == shown then return end
        shown = id
        local image
        if id then
            local frame = hs.screen.mainScreen():frame()
            local size = math.floor(math.min(L.previewSize, frame.w * 0.27))
            local ok, result = pcall(function()
                return L.thumbnail(images(), key, (L.build(key, n, seed)), size - 28)
            end)
            if ok and result then
                image = result
                if not canvas then
                    canvas = hs.canvas.new({x=frame.x + frame.w - size - 20, y=frame.y + frame.h * 0.14, w=size, h=size})
                    canvas:level(hs.canvas.windowLevels.screenSaver)
                    canvas:behavior({"canJoinAllSpaces", "stationary"})
                end
                -- Dark in the system's dark mode, near white otherwise.
                local dark = hs.host and hs.host.interfaceStyle and hs.host.interfaceStyle() == "Dark"
                canvas:replaceElements(
                    {type="rectangle", fillColor={white=dark and 0.13 or 0.97, alpha=0.97},
                     strokeColor={white=dark and 1 or 0, alpha=0.25}, strokeWidth=1, action="strokeAndFill",
                     roundedRectRadii={xRadius=20, yRadius=20}},
                    {type="image", image=image, imageScaling="scaleProportionally",
                     frame={x=14, y=14, w=size - 28, h=size - 28}})
                canvas:show()
            end
        end
        if not image and canvas then canvas:hide() end
    end
    function pane.start()
        if timer or not (hs.timer and hs.screen and hs.canvas and hs.canvas.windowLevels) then return end
        shown = nil
        timer = hs.timer.doEvery(0.12, function() pcall(draw) end)
        pcall(draw)
    end
    function pane.stop()
        if timer then timer:stop(); timer = nil end
        if canvas then canvas:delete(); canvas = nil end
        shown = nil
    end
    return pane
end

-- The Ctrl+Enter menu inside an open chooser. o.images(items) gives preview
-- images, o.export(key, format, items) exports, o.back() restores the list.
-- o.ownEnter means the caller already binds Enter and calls m.pick itself;
-- o.ownPane that it shows its own L.previewPane.
-- Ctrl+Enter goes one level deeper each time (m.advance): the layouts, then,
-- on a row that has them, every choice Tab would step through (the fonts of
-- a Persian word, the next Jumble seeds), then back to the caller's list.
function L.menu(o)
    local m = {active=false}
    local enter
    local pane = not o.ownPane and L.previewPane(o.chooser, function(row) return m.preview(row) end)
    function m.preview(row)
        if not (m.active and row.layout) then return nil end
        return row.layout, #m.items, row.seed or L.jumbleSeed, function() return o.images(m.items) end
    end
    local function show(keepRow)
        local row = keepRow and o.chooser:selectedRow() or 1
        local images = o.images(m.items)
        if m.variantsOf then
            local choices = {}
            for _, variant in ipairs(L.variants(m.variantsOf, 8) or {}) do
                local choice = L.row(images, #m.items, L.mode(variant.key), variant.seed)
                if not keepRow and variant.key == m.variantsOf and not variant.seed then row = #choices + 1 end
                choices[#choices+1] = choice
            end
            ChooserSearch.setDynamic(o.chooser, nil)
            ChooserSearch.setChoices(o.chooser, choices)
            o.chooser:selectedRow(row)
            o.chooser:placeholderText((L.seeded(m.variantsOf) and "Seeds" or "Fonts") .. " for " ..
                L.mode(m.variantsOf).name .. " · Enter: WebP · Cmd+Shift+Enter: PNG" ..
                (L.seeded(m.variantsOf) and " · Tab: more" or "") .. " · Ctrl+Enter: back")
            return
        end
        local choices = L.choices(images, #m.items)
        local byLayout = {}
        for _, choice in ipairs(choices) do
            if choice.layout then byLayout[choice.layout] = choice end
        end
        -- Typing a word offers it as a Text layout above the matching rows.
        -- Reuse any preset row of the same key and deduplicate by layout so
        -- typing a hardcoded or remembered word promotes it without duplicating it.
        ChooserSearch.setDynamic(o.chooser, function(query)
            local word = L.textWord(query)
            if not word then return nil end
            local textRow = byLayout[L.textKey(word)] or L.textRow(images, #m.items, query)
            return textRow and {textRow} or nil
        end, "layout")
        ChooserSearch.setChoices(o.chooser, choices)
        o.chooser:selectedRow(row)
        o.chooser:placeholderText("Layout for " .. #m.items .. " emoji · type a word to spell it · " ..
            "Enter: WebP · Cmd+Shift+Enter: PNG · Tab: next seed or font · Ctrl+Enter: all of them, or back")
    end
    function m.open(items)
        if #items == 0 then return end
        if not m.active then m.query = o.chooser:query() end
        m.items, m.active, m.variantsOf = items, true, nil
        o.chooser:query("")
        show(false)
        if pane then pane.start() end
        if not o.ownEnter then
            enter = enter or hs.hotkey.bind("", "return", function() m.pick("webp") end)
        end
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
    function m.advance()
        if not m.active then return end
        local row = not m.variantsOf and o.chooser:selectedRowContents()
        if row and row.layout and L.variants(row.layout) then
            m.variantsOf = row.layout
            o.chooser:query("")
            show(false)
            return
        end
        m.close()
    end
    -- Tab: the next font on a Persian text row, a new Jumble anywhere else.
    function m.reroll()
        if m.variantsOf then
            if L.seeded(m.variantsOf) then L.jumbleSeed = L.jumbleSeed + 8; show(false) end
            return
        end
        local row = o.chooser:selectedRowContents()
        if row and L.usesTextFont(row.layout) then L.cycleTextFont() else L.jumbleSeed = L.jumbleSeed + 1 end
        show(true)
    end
    function m.pick(format)
        local row = o.chooser:selectedRowContents()
        if not (row and row.layout) then return end
        if row.seed then L.jumbleSeed = row.seed end
        if L.usesTextFont(row.layout) then L.setTextFont(L.mode(row.layout).font) end
        L.remember(row.layout)
        o.export(row.layout, format, m.items)
    end
    function m.cleanup()
        if m.active then ChooserSearch.setDynamic(o.chooser, nil) end
        m.active, m.variantsOf = false, nil
        if pane then pane.stop() end
        if enter then enter:delete(); enter = nil end
    end
    return m
end

-- Recent sticker sets for Hyper+Ctrl+A: every image export of several emoji,
-- or of one in a layout other than Auto. Items are {emoji=...} for Unicode
-- and {id=..., emoji=...} for Telegram premium emoji; seeded layouts keep their seed.
local kHistoryKey = "emojiLayout.history.v1"
L.historyLimit = L.historyLimit or 60

local function historyKey(items, layout, seed)
    local parts = {layout, L.seeded(layout) and tostring(seed) or ""}
    for _, item in ipairs(items) do parts[#parts+1] = item.id or item.emoji end
    return table.concat(parts, "\31")
end

function L.history()
    local saved = hs.settings.get(kHistoryKey)
    local result = {}
    for _, entry in ipairs(type(saved) == "table" and saved or {}) do
        if type(entry) == "table" and type(entry.items) == "table" and #entry.items > 0
            and pcall(L.mode, entry.layout) then
            result[#result+1] = entry
        end
    end
    return result
end

function L.record(entries, layout, seed)
    layout = layout or "auto"
    if #entries < 2 and layout == "auto" then return end
    local items = {}
    for i, entry in ipairs(entries) do
        items[i] = {emoji=entry.emoji, id=not entry.unicode and entry.id or nil}
    end
    seed = L.seeded(layout) and (seed or L.jumbleSeed) or nil
    local key = historyKey(items, layout, seed)
    local history = {{items=items, layout=layout, seed=seed, time=os.time()}}
    for _, entry in ipairs(L.history()) do
        if #history >= L.historyLimit then break end
        if historyKey(entry.items, entry.layout, entry.seed) ~= key then history[#history+1] = entry end
    end
    hs.settings.set(kHistoryKey, history)
end

-- Tab on a history row: the same set in its next font or seed, in place.
function L.revise(index)
    local history = L.history()
    local entry = history[index]
    if not entry then return false end
    local layout, seed = L.nextVariant(entry.layout, entry.seed)
    if layout == entry.layout and seed == entry.seed then return false end
    entry.layout, entry.seed = layout, seed
    hs.settings.set(kHistoryKey, history)
    return true
end

function L.forget(index)
    local history = L.history()
    table.remove(history, index)
    hs.settings.set(kHistoryKey, history)
end

-- Shift+Up/Down in the emoji lists: toggle the highlighted row in or out of
-- the selection, then move one row. toggle(row) does the bookkeeping.
function L.toggleMove(chooser, delta, toggle)
    local row = chooser:selectedRow()
    local contents = chooser:selectedRowContents()
    if contents and next(contents) then toggle(contents) end
    local target = row + delta
    local nextRow = target >= 1 and chooser:selectedRowContents(target)
    if nextRow and next(nextRow) then chooser:selectedRow(target) end
end
