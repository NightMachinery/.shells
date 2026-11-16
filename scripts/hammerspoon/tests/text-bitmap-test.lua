local function runTests()
    local scriptDir = debug.getinfo(1, "S").source:match("^@(.+)/[^/]+$") or "tests"
    local root = scriptDir == "tests" and "." or scriptDir:gsub("/tests$", "")

    local settings = {}
    local env = setmetatable({
        hs = {
            settings = {
                get = function(k) return settings[k] end,
                set = function(k, v) settings[k] = v end,
            },
            json = {
                encode = function() return "{}" end,
                decode = function() return {} end,
            },
            fs = {
                attributes = function() return nil end,
            },
        },
    }, {__index = _G})

    assert(loadfile(root .. "/core/emoji-layout.lua", "t", env))()
    local L = env.EmojiLayout
    L.wordsFile = "/nonexistent/words.txt"

    local tb = assert(L.loadTextBitmap(), "native text-bitmap module failed to compile/load")
    assert(type(tb.rasterize) == "function", "rasterize function missing")

    -- Verify CoreGraphics top-to-bottom orientation using 'T' (wide top bar at y = 0).
    local tCells, tw, th, tLetters = tb.rasterize("T", "Tahoma", 12)
    assert(tw > 0 and th > 0 and tLetters == 1 and #tCells > 0)
    local topCount, bottomCount = 0, 0
    for _, p in ipairs(tCells) do
        if p.y == 0 then topCount = topCount + 1 end
        if p.y == th - 1 then bottomCount = bottomCount + 1 end
    end
    assert(topCount > bottomCount, "y = 0 must be top row so glyphs are upright")

    -- Pure ASCII uses the hand-drawn font L.textRows.english selects.
    assert(L.textWord("hi") == "HI")
    assert(L.textRows.english == 4 and L.textRows.persian == 7, "defaults changed; update docs too")
    assert(#L.textCells("HI") == 13, "HI in the default 4-row font")
    for rows, count in pairs({[3]=10, [4]=13, [5]=16, [1]=10, [9]=16}) do
        L.textRows.english = rows
        assert(#L.textCells("HI") == count, "HI at english = " .. rows)
        local maxY = 0
        for _, c in ipairs(L.textCells("HI")) do maxY = math.max(maxY, c.y) end
        assert(maxY + 1 == math.max(3, math.min(5, rows)), "one line is as many rows as the font")
    end
    L.textRows.english = 4
    for rows, font in pairs(L.fonts) do
        for c, glyph in pairs(L.fonts[5]) do
            assert(font[c] and #font[c] == rows, rows .. "-row font lacks or mis-sizes " .. c)
            for _, row in ipairs(font[c]) do assert(#row == #font[c][1], "ragged glyph " .. c) end
        end
        for c in pairs(font) do assert(L.fonts[5][c], "extra glyph " .. c) end
    end
    assert(L.textWord("love") == "LOVE")

    -- Rejections: invalid UTF-8, comments/punctuation, zero-width only, empty, overlong.
    assert(L.textWord("") == nil)
    assert(L.textWord("   ") == nil)
    assert(L.textWord("\xff") == nil)
    assert(L.textWord("a#b") == nil)
    assert(L.textWord("کتاب#1") == nil)
    assert(L.textWord("\u{200C}") == nil, "Zero-width non-joiner alone renders no pixels")
    assert(not pcall(L.mode, "text:#"), "Disallowed text layout key must fail")
    local tooLong = string.rep("ا", L.textMax + 1)
    assert(L.textWord(tooLong) == nil, "Strings over textMax codepoints must be rejected")

    -- Primary target: Farsi word "کتاب" in RTL reading order with 4 distinct clusters.
    assert(L.textWord("کتاب") == "کتاب")
    assert(L.textWord("  کتاب  ") == "کتاب")
    local word4 = L.textCells("کتاب")
    assert(#word4 >= 15 and #word4 < 46, "کتاب at 7 rows takes fewer cells than the 46 of the old 12pt raster")

    local seenXY, sumX, countByLetter = {}, {}, {}
    local minX, maxX, minY, maxY = math.huge, -math.huge, math.huge, -math.huge
    for _, cell in ipairs(word4) do
        assert(cell.s > 0 and cell.s <= 1)
        assert(cell.i >= 1 and cell.i <= 4)
        local key = string.format("%.4f,%.4f", cell.x, cell.y)
        assert(not seenXY[key], "Duplicate cell at " .. key)
        seenXY[key] = true
        sumX[cell.i] = (sumX[cell.i] or 0) + cell.x
        countByLetter[cell.i] = (countByLetter[cell.i] or 0) + 1
        if cell.x < minX then minX = cell.x end
        if cell.x > maxX then maxX = cell.x end
        if cell.y < minY then minY = cell.y end
        if cell.y > maxY then maxY = cell.y end
    end
    assert(minY == 0 and maxY >= 4 and maxY <= 7, "about L.textRows.persian rows")
    assert(math.abs(minX + maxX) < 1e-6, "Cells should be centered horizontally around x = 0")

    -- Logical letter IDs 1..4 must follow RTL positions right-to-left (ک -> ت -> ا -> ب).
    local meanX = {}
    for i = 1, 4 do
        assert((countByLetter[i] or 0) > 0, "Missing pixels for letter " .. i)
        meanX[i] = sumX[i] / countByLetter[i]
    end
    assert(meanX[1] > meanX[2] and meanX[2] > meanX[3] and meanX[3] > meanX[4],
        string.format("Expected RTL x order 1..4, got %.2f, %.2f, %.2f, %.2f",
            meanX[1], meanX[2], meanX[3], meanX[4]))

    -- Emoji color cycling modes: <= letters, letter..height (stripes), > height (per-pixel).
    local z1 = L.textCells("کتاب", 1)
    for _, c in ipairs(z1) do assert(c.i == 1) end
    local z2 = L.textCells("کتاب", 2)
    for _, c in ipairs(z2) do assert(c.i == 1 or c.i == 2) end
    local height = math.floor(maxY + 0.5) + 1
    local zStripe = L.textCells("کتاب", height - 1)
    for _, c in ipairs(zStripe) do assert(c.i == math.floor(c.y + 0.5) % (height - 1) + 1) end
    local zPixel = L.textCells("کتاب", height + 5)
    for k, c in ipairs(zPixel) do assert(c.i == (k - 1) % (height + 5) + 1) end

    -- Other Persian words, ligatures, ZWNJ, Persian digits, mixed ASCII/heart.
    for _, sample in ipairs({
        {"پژواک", "پژواک"},
        {"سلام", "سلام"},
        {"گلاب", "گلاب"},
        {"چشم", "چشم"},
        {"لا", "لا"},
        {"می\u{200C}خواهم", "می\u{200C}خواهم"},
        {"۱۲۳", "۱۲۳"},
        {"salam کتاب", "SALAM کتاب"},
        {"کتاب ♥", "کتاب ♥"},
        {"کتاب؟!", "کتاب؟!"},
    }) do
        local input, expected = sample[1], sample[2]
        assert(L.textWord(input) == expected, "textWord failed for " .. input)
        local mode = assert(L.mode("text:" .. input), "L.mode failed for " .. input)
        assert(mode.key == "text:" .. expected and mode.repeatTo >= 5)
        local built = L.build(mode.key, 3)
        assert(#built == 1 and #built[1] == mode.repeatTo)
    end

    -- Multi-word Persian wrapping: breaks at the space, so two lines are taller than either word.
    local function extent(cells)
        local top, bottom = math.huge, -math.huge
        for _, c in ipairs(cells) do
            top, bottom = math.min(top, c.y - c.s / 2), math.max(bottom, c.y + c.s / 2)
        end
        return bottom - top
    end
    local phrase = L.textWord("دوستت دارم")
    assert(phrase == "دوستت دارم")
    local wrapped = L.textCells(phrase)
    local one, two = extent(L.textCells("دوستت")), extent(L.textCells("دارم"))
    assert(math.abs(extent(wrapped) - (one + two + 1)) < 1e-6,
        "Multi-word phrase should wrap at the space, one blank row between the lines")

    -- Single long Persian word without spaces must stay on one line (never split cursive joining).
    local joined = L.textCells("خوشحال")
    local jRows, jMax = {}, 0
    for _, c in ipairs(joined) do
        local y = math.floor(c.y + 0.5)
        jRows[y] = true
        if y > jMax then jMax = y end
    end
    local jEmptyBands = 0
    for y = 1, jMax - 1 do
        if not jRows[y] and not jRows[y + 1] then jEmptyBands = jEmptyBands + 1 end
    end
    assert(jEmptyBands == 0, "Single Persian word without spaces must never wrap across lines")

    -- Fewer rows, fewer emoji; and a dot is never lost, whatever the rows: پیشی has
    -- 3 + 2 + 3 dots (its final ی has none), each a smaller emoji of its own.
    local function overlaps(cells)
        for a = 1, #cells do
            for b = a + 1, #cells do
                local reach = (cells[a].s + cells[b].s) / 2 - 1e-6
                -- As circles: the dots of a triangle interlock like the rows of a beehive.
                if (cells[a].x - cells[b].x)^2 + (cells[a].y - cells[b].y)^2 < reach^2 then return true end
            end
        end
        return false
    end
    -- With tips at full size, only dots are smaller than a body emoji.
    local tipSize = L.textTipSize
    L.textTipSize = 1
    local counts = {}
    for _, rows in ipairs({5, 6, 7, 8, 9, 10, 12}) do
        L.textRows.persian = rows
        local cells = L.textCells("پیشی")
        local dots, perLetter = 0, {}
        for _, c in ipairs(cells) do
            if c.s == L.textDotSize then dots = dots + 1 end
            perLetter[c.i] = true
        end
        assert(dots == 8, string.format("پیشی lost a dot at %d rows: %d left", rows, dots))
        for i = 1, 4 do assert(perLetter[i], "letter " .. i .. " of پیشی vanished at " .. rows .. " rows") end
        assert(not overlaps(cells), "overlapping cells at " .. rows .. " rows")
        assert(L.mode("text:پیشی").repeatTo == #cells, "the layout must follow L.textRows")
        counts[rows] = #cells
    end
    assert(counts[6] < counts[8] and counts[8] < counts[12], "fewer rows must mean fewer emoji")
    L.textRows.persian = 7
    for _, word in ipairs({"کتاب", "خوشحال", "گلاب", "چشم", "آب", "ژ", "ث", "۰", "؟!"}) do
        local cells = L.textCells(word)
        assert(#cells > 0 and not overlaps(cells), "bad cells for " .. word)
    end
    -- Three dots keep their triangle: two rows.
    local ys = {}
    for _, c in ipairs(L.textCells("ث")) do if c.s == L.textDotSize then ys[string.format("%.2f", c.y)] = true end end
    local dotRows = 0
    for _ in pairs(ys) do dotRows = dotRows + 1 end
    assert(dotRows == 2, "the three dots of ث should sit in two rows")
    L.textTipSize = tipSize

    -- Preset + remembered Farsi word normalization in L.words() / L.describe().
    settings["emojiLayout.words.v1"] = {"  کتاب ", "می\u{200C}خواهم", "bad#word"}
    local words = L.words()
    assert(#words == 2 and words[1] == "کتاب" and words[2] == "می\u{200C}خواهم")
    local built, slots = L.build("text:کتاب", 4)
    local desc = L.describe("text:کتاب", 4, built, slots)
    assert(desc:find("cells", 1, true))

    print("TEXT_BITMAP_TESTS_PASSED")
end

runTests()
