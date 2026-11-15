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

    -- Pure ASCII continues to use the 5-row font identically.
    assert(L.textWord("hi") == "HI")
    assert(#L.textCells("HI") == 16, "ASCII 5-row cell count must stay unchanged")
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
    assert(#word4 >= 25, "کتاب should render with visible bitmap cells")

    local seenXY, sumX, countByLetter = {}, {}, {}
    local minX, maxX, minY, maxY = math.huge, -math.huge, math.huge, -math.huge
    for _, cell in ipairs(word4) do
        assert(cell.s == 1)
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
    assert(minY == 0 and maxY >= 6)
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
    local height = maxY + 1
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

    -- Multi-word Persian wrapping: must break strictly at spaces with a 2px blank gap.
    local phrase = L.textWord("دوستت دارم")
    assert(phrase == "دوستت دارم")
    local wrapped = L.textCells(phrase)
    local rows = {}
    local maxRow = 0
    for _, c in ipairs(wrapped) do
        local y = math.floor(c.y + 0.5)
        rows[y] = true
        if y > maxRow then maxRow = y end
    end
    local emptyBands = 0
    for y = 1, maxRow - 1 do
        if not rows[y] and not rows[y + 1] then
            emptyBands = emptyBands + 1
        end
    end
    assert(emptyBands >= 1, "Multi-word phrase should wrap at the space with a 2px gap")

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
