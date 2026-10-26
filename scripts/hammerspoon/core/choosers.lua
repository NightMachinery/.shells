-- Chooser integrations version 1.1.0: shared accepted-query history.
function chis()
    -- https://www.hammerspoon.org/docs/hs.chooser.html
    -- @todo it'd probably be better if we put the URLs as `subtext`, and their titles as `text`. hs.chooser has this better than fzf.
    local tab = nil
    c = ChooserSearch.remote(function(x)
            if tab then tab:delete() end
            if not x then return end
            brishz_eval_hs(("chis_clean %q | inargsf open"):format(x.text))
    end)
    c:placeholderText("Search history ...")
    c:width(95)
    c:rows(15)
    tab = hs.hotkey.bind('', 'tab', function()
                             local x = c:selectedRowContents()
                             if not x then
                                 return
                             end
                             brishz_eval_hs(("bell-lm-mhm ; chis_clean %q | inargsf open ; "):format(x.text))
    end)
    chis_first = true
    local timer
    c:choices(function()
            local q = c:query()
            local cmd = ("chis_find %q"):format(q)
            if chis_first then
                chis_first = false
                cmd = "deus " .. cmd
            end
            local res = brishz_eval(cmd)
            local out = {}
            for l in res:gmatch("([^\r\n]+)\r?\n?") do
                table.insert(out, {["text"] = l})
                -- table.insert(out, {["text"] = hs.styledtext.ansi(l)}) -- so slow it's completely broken
            end
            return out
    end)
    ChooserSearch.onQueryChanged(c,function(query)
            if timer and timer:running() then
                timer:stop()
            end
            timer = hs.timer.doAfter(0.2, function() c:refreshChoicesCallback() end)
    end)
    c:show()
end
-- hyper_bind_v1("o", chis)
--
function ntagFinder()
    -- allows you to add (use enter or tab) or remove (shift+tab) tags from the selected files in Finder.
    --
    -- Every garden call here is asynchronous and takes an argument list. The
    -- choices used to come from a synchronous brishz_eval on every keystroke,
    -- which froze Hammerspoon for the round trip, and the query and the tag
    -- went into zsh command strings through Lua's %q, which is not shell
    -- quoting: a `$(...)' in either would have run in the garden.
    ---
    local timer
    local tab = nil
    local antitab = nil
    local label = "ntagFinder"
    -- Which refresh is current, so that a late answer for an older query is
    -- dropped.
    local generation = 0
    c = ChooserSearch.remote(function(x)
            if tab then tab:delete() end
            if antitab then antitab:delete() end
            if not x then return end
            brishz_eval_q_hs({"ntag-finder-sel-add", x.text}, label)
    end)
    c:placeholderText("ntag ...")
    -- c:width(95)
    c:rows(11)
    tab = hs.hotkey.bind('', 'tab', function()
                             local x = c:selectedRowContents()
                             if not x then
                                 return
                             end
                             brishz_eval_hs("bell-lm-mhm", label)
                             brishz_eval_q_hs({"ntag-finder-sel-add", x.text}, label)
    end)
    antitab = hs.hotkey.bind('shift', 'tab', function()
                                 local x = c:selectedRowContents()
                                 if not x then
                                     return
                                 end
                                 brishz_eval_hs("bell-pp-piece", label)
                                 brishz_eval_q_hs({"ntag-finder-sel-rm", x.text}, label)
    end)
    local chooser = c
    local function refresh()
        generation = generation + 1
        local mine = generation
        brishz_eval_q_out_hs({"ntag-select", chooser:query()}, function(res)
            if mine ~= generation then return end
            local out = {}
            for l in (res or ""):gmatch("([^\r\n]+)\r?\n?") do
                -- @upstreambug https://github.com/Hammerspoon/hammerspoon/issues/2574
                -- table.insert(out, {["text"] = hs.styledtext.ansi(l, {font={size=25}})})
                table.insert(out, {["text"] = l})
            end
            chooser:choices(out)
        end, label, { quiet = true })
    end
    ChooserSearch.onQueryChanged(c,function(query)
            if timer and timer:running() then
                timer:stop()
            end
            timer = hs.timer.doAfter(0.0, refresh)
    end)
    c:show()
    refresh()
end
hyper_bind_v2{mods={"cmd"}, key='n', pressedfn=ntagFinder}
--- * Emoji Chooser


local emojiData = {}
local function loadEmojiData()
    local filePath = os.getenv("HOME") .. "/code/misc/unicode-emoji-json/data-by-emoji.json"
    local file = io.open(filePath, "r")
    if not file then
        alert_gateway("Emoji data file not found", { color = "warn" })
        return
    end

    local data = file:read("*a")

    file:close()
    emojiData = json.decode(data)
end
loadEmojiData()

-- A fixed id rather than a handle: re-showing it rewrites the running tally of
-- picked emoji in place, so there is nothing to close before showing the next
-- one.
local kEmojiAlertId = "emoji-chooser"
local emojiPreviews = {}

function emojiChooserImage(emoji, resolution)
    resolution = resolution or 128
    local key = emoji .. ":" .. resolution
    if emojiPreviews[key] == nil then
        local codes = {}
        for _, code in utf8.codes(emoji) do
            if code ~= 0xfe0f then codes[#codes + 1] = string.format("%04x", code) end
        end
        local suffix = resolution == 128 and "" or "-" .. resolution
        local path = os.getenv("HOME") .. "/Library/Caches/night-emoji/noto-18.0" .. suffix .. "/emoji_u" ..
            table.concat(codes, "_") .. ".png"
        emojiPreviews[key] = hs.fs.attributes(path, "mode") == "file" and
            hs.image.imageFromPath(path) or false
    end
    -- A missing export image may become available after the asynchronous fetch.
    local image = emojiPreviews[key]
    if not image and resolution ~= 128 then emojiPreviews[key] = nil end
    return image or nil
end

local emojiAliasContent, emojiAliasData

function emojiChooserAliases()
    local file = io.open(os.getenv("HOME") .. "/code/misc/unicode-emoji-json/aliases.json", "r")
    if not file then return {} end
    local content = file:read("*a"); file:close()
    if content == emojiAliasContent then return emojiAliasData end
    local decoder = hs.json and hs.json.decode or json.decode
    local ok, data = pcall(decoder, content)
    if not ok or type(data) ~= "table" then
        alert_gateway("Cannot read emoji aliases.json", {color="warn"})
        return {}
    end
    local result = {}
    for emoji, words in pairs(data) do
        if type(words) == "table" then
            result[emoji] = {}
            for _, word in ipairs(words) do
                if type(word) == "string" then result[emoji][#result[emoji]+1] = word end
            end
        end
    end
    emojiAliasContent, emojiAliasData = content, result
    return result
end

local emojiAliasIndexes = setmetatable({}, {__mode="k"})

-- Keep the data on its canonical entry; fold presentation and gender variants
-- here so Unicode rows and Telegram's raw alt text share the same vocabulary.
function emojiChooserAliasesFor(aliases, emoji)
    if type(emoji) ~= "string" then return {} end
    local function presentation(text) return text:gsub(utf8.char(0xfe0f), "") end
    local index = emojiAliasIndexes[aliases]
    if not index then
        index = {}
        local keys = {}
        for key in pairs(aliases) do keys[#keys+1] = key end
        table.sort(keys)
        for _,key in ipairs(keys) do
            local canonical = presentation(key)
            index[canonical] = index[canonical] or {}
            index[canonical][#index[canonical]+1] = key
        end
        emojiAliasIndexes[aliases] = index
    end
    local result, seen = {}, {}
    local function add(words)
        for _,word in ipairs(words or {}) do
            local key = word:lower()
            if not seen[key] then seen[key] = true; result[#result+1] = word end
        end
    end
    local function inherit(candidate)
        add(aliases[candidate])
        for _,key in ipairs(index[presentation(candidate)] or {}) do add(aliases[key]) end
    end
    inherit(emoji)
    local codes = {}
    for _,code in utf8.codes(emoji) do
        if code ~= 0xfe0f and not (code >= 0x1f3fb and code <= 0x1f3ff) then
            codes[#codes+1] = utf8.char(code)
        end
    end
    local base = table.concat(codes)
    inherit(base)
    for _,gender in ipairs({"♀", "♂"}) do
        local suffix = "‍" .. gender
        if base:sub(-#suffix) == suffix then base = base:sub(1, -#suffix-1) end
    end
    inherit(base)
    for _,person in ipairs({"👨", "👩"}) do
        local prefix = person .. "‍"
        if base:sub(1, #prefix) == prefix then inherit("🧑‍" .. base:sub(#prefix+1)) end
    end
    return result
end

function emojiChooserName(emoji)
    local info = emojiData[emoji] or emojiData[emoji .. "\239\184\143"]
    return info and info.name or emoji
end

-- Offscreen canvas: no windows, screenshots or font substitution are involved.
function emojiChooserPng(emojis, side, resolution)
    assert(#emojis > 0 and #emojis <= 128, "Choose between 1 and 128 emoji for a PNG")
    side = side or 128
    local canvas = hs.canvas.new({x=0, y=0, w=side*#emojis, h=side})
    local ok, result = pcall(function()
        for i, emoji in ipairs(emojis) do
            local image = assert(emojiChooserImage(emoji, resolution), "Missing cached emoji image; run emoji-images.py")
            canvas:insertElement({type="image", image=image, imageScaling="scaleProportionally",
                                  frame={x=(i-1)*side, y=0, w=side, h=side}})
        end
        local image = assert(canvas:imageFromCanvas())
        local data = assert(image:encodeAsURLString(true, "PNG"):match("^data:image/png;base64,(.*)$"))
        return assert(hs.base64.decode(data))
    end)
    canvas:delete()
    if not ok then error(result) end
    return result
end

local emojiExportTasks = {}

function emojiChooserPrepareImages(emojis, callback)
    local ready = true
    for _,emoji in ipairs(emojis) do if not emojiChooserImage(emoji, 512) then ready = false; break end end
    if ready then callback(); return end
    local args = {os.getenv("HOME") .. "/scripts/hammerspoon/bin/emoji-images.py", "--size", "512"}
    for _,emoji in ipairs(emojis) do args[#args+1] = "--emoji"; args[#args+1] = emoji end
    local task
    task = hs.task.new("/opt/homebrew/bin/python3", function(code, _, stderr)
        emojiExportTasks[task] = nil
        callback(code ~= 0 and ("High-resolution emoji download failed: " .. tostring(stderr)) or nil)
    end, args)
    assert(task and task:start(), "Cannot start high-resolution emoji download")
    emojiExportTasks[task] = true
end

function emojiChooserTelegramDocument()
    local clipboard = hs.pasteboard.changeCount()
    local function attempt(remaining)
        if hs.pasteboard.changeCount() ~= clipboard then return end
        local app = hs.application.frontmostApplication()
        local id = app and (app:bundleID() or ""):lower() or ""
        if not (id:find("telegram", 1, true) or id:find("tdesktop", 1, true)) then return end
        local root = hs.axuielement.applicationElement(app):attributeValue("AXFocusedWindow")
        local found, count = nil, 0
        local function walk(e, depth)
            if not e or found or depth > 18 or count >= 400 then return end
            count = count + 1
            e:setTimeout(.01)
            if e:attributeValue("AXRole") == "AXCheckBox" then
                local label = e:attributeValue("AXTitle")
                if not label or label == "" then label = e:attributeValue("AXDescription") end
                if label == "Send as a document" or label == "Send as documents" then found = e; return end
            end
            if e:attributeValue("AXRole") == "AXMenuBar" then return end
            for _,child in ipairs(e:attributeValue("AXChildren") or {}) do walk(child, depth+1) end
        end
        walk(root, 0)
        if found then
            local value = found:attributeValue("AXValue")
            if value == 0 or value == false then found:performAction("AXPress") end
        elseif remaining > 0 then
            hs.timer.doAfter(.25, function() attempt(remaining-1) end)
        else
            alert_gateway("Telegram: choose Send as a document to keep the WebP sticker transparent", {color="warn"})
        end
    end
    hs.timer.doAfter(.25, function() attempt(3) end)
end

-- Telegram consumes file URLs before image data. A WebP file keeps its sticker
-- identity; pasting an NSImage instead takes Telegram's opaque photo path.
local function encodeEmojiWebp(emojis, callback)
    local png = emojiChooserPng(emojis, 512 / #emojis, 512)
    local encoder = "/opt/homebrew/bin/cwebp"
    assert(hs.fs.attributes(encoder, "mode") == "file", "Install Homebrew webp for cwebp")
    local folder = os.getenv("HOME") .. "/Library/Caches/night-emoji/exports"
    hs.fs.mkdir(folder)
    assert(hs.fs.attributes(folder, "mode") == "directory", "Cannot create emoji export cache")
    local output = folder .. "/emoji-" .. hs.hash.SHA256("webp-native512-lossless-v2:" .. png) .. ".webp"
    local function complete()
        local file = assert(io.open(output, "rb")); local webp = file:read("*a"); file:close()
        assert(webp:sub(1, 4) == "RIFF" and webp:sub(9, 12) == "WEBP", "Invalid WebP export")
        local url = "file://" .. output:gsub("[^A-Za-z0-9%-%._~/]", function(c)
            return string.format("%%%02X", c:byte())
        end)
        callback(nil, {["public.file-url"]=url, ["org.webmproject.webp"]=webp, ["public.png"]=png,
            ["org.nightmachinery.emoji-export"]='{"version":1,"format":"webp","intent":"sticker","preserveAlpha":true}'})
    end
    if hs.fs.attributes(output, "mode") == "file" then complete(); return end
    local input = os.tmpname()
    local encoded = os.tmpname()
    local file = assert(io.open(input, "wb")); file:write(png); file:close()
    local task
    task = hs.task.new(encoder, function(code, _, stderr)
        emojiExportTasks[task] = nil
        os.remove(input)
        if code ~= 0 then
            os.remove(encoded)
            callback("WebP encoder failed: " .. tostring(stderr))
            return
        end
        local ok, err = pcall(function()
            assert(os.rename(encoded, output))
            complete()
        end)
        os.remove(encoded)
        if not ok then callback(tostring(err)) end
    end, {"-quiet", "-lossless", "-exact", input, "-o", encoded})
    if not task or not task:start() then
        os.remove(input)
        os.remove(encoded)
        error("Cannot start WebP encoder")
    end
    emojiExportTasks[task] = true
end

function emojiChooserWebp(emojis, callback)
    emojiChooserPrepareImages(emojis, function(err)
        if err then callback(err); return end
        local ok, failure = pcall(encodeEmojiWebp, emojis, callback)
        if not ok then callback(tostring(failure)) end
    end)
end

local emojiRowCache
local function emojiRows(aliases)
    if not emojiRowCache or emojiRowCache.data ~= emojiData or emojiRowCache.aliases ~= aliases then
        local rows = {}
        for emoji, info in pairs(emojiData) do
            local version = tonumber(info.unicode_version) or tonumber(info.emoji_version) or 0
            local image = emojiChooserImage(emoji)
            rows[#rows+1] = {
                text=image and info.name or emoji,
                subText=image and ("Unicode " .. tostring(version)) or info.name,
                emoji=emoji, name=info.name, image=image, unicodeVersion=version,
                searchText=emoji .. " " .. (info.slug or "") .. " " .. table.concat(emojiChooserAliasesFor(aliases,emoji)," "),
            }
        end
        emojiRowCache = {data=emojiData, aliases=aliases, rows=rows}
    end
    -- Sorting is per opening; cached row metadata never stores a stale MRU order.
    local rows = {}
    for i,row in ipairs(emojiRowCache.rows) do rows[i] = row end
    return rows
end

-- The caches above die with the Lua state, so warm them after each load: a cold
-- first Hyper+A took about 1 s, 0.7 s of it loading previews. Images load in
-- chunks of about 25 ms; decoding aliases and building rows take one tick
-- each. Set emojiChooserPrewarmDelay=false to disable.
-- The timer handle is global because a local one is collected and never fires.
emojiChooserPrewarmDelay = emojiChooserPrewarmDelay == nil and 5 or emojiChooserPrewarmDelay
emojiChooserPrewarmBatch = emojiChooserPrewarmBatch or 64
function emojiChooserPrewarm(done)
    if emojiChooserPrewarmTimer then emojiChooserPrewarmTimer:stop() end
    local pending, index = {}, 0
    for emoji in pairs(emojiData) do pending[#pending+1] = emoji end
    local function step()
        emojiChooserPrewarmTimer = nil
        for _ = 1, emojiChooserPrewarmBatch do
            index = index + 1
            if index > #pending then
                -- Separate ticks, so no single stall holds both large steps.
                emojiChooserAliases()
                emojiChooserPrewarmTimer = hs.timer.doAfter(0.05, function()
                    emojiChooserPrewarmTimer = nil
                    emojiRows(emojiChooserAliases())
                    if done then done() end
                end)
                return
            end
            emojiChooserImage(pending[index])
        end
        emojiChooserPrewarmTimer = hs.timer.doAfter(0.05, step)
    end
    emojiChooserPrewarmTimer = hs.timer.doAfter(emojiChooserPrewarmDelay or 0, step)
end

function emojiChooser()
    local recent=ChooserSearch.recent("emoji")
    local selectedEmojis = {} -- Table to store selected emojis
    local pendingImage, exporting, closed

    local function updateAlert()
        if #selectedEmojis >= 1 then
            alert_gateway(table.concat(selectedEmojis), {
                id = kEmojiAlertId,
                -- The chooser clears this itself; the engine's own 4h ceiling is
                -- only a backstop against a crash leaving it on screen.
                seconds = math.huge,
                -- Next to the chooser, which opens on the focused screen. It
                -- was mainScreen() before the alertV2 migration made it
                -- "primary" by accident.
                screens = "typing",
            })
        else
            alert_gateway_dismiss(kEmojiAlertId)
        end
    end

    local function clearAlert()
        alert_gateway_dismiss(kEmojiAlertId)
    end

    local chooser = ChooserSearch.new(function(choice)
            if not choice then
                -- canceled
                return
            end

            if #selectedEmojis == 0 then
                table.insert(selectedEmojis, choice.emoji)
            end

            local emojiString = table.concat(selectedEmojis)
            ---
            -- hs.eventtap.keyStrokes(emojiString)
            -- =keyStrokes= cannot insert some emojis.
            ---
            recent.record(selectedEmojis)
            if pendingImage then
                if hs.pasteboard.writeAllData(pendingImage) then
                    doPaste()
                    if pendingImage["public.file-url"] then emojiChooserTelegramDocument() end
                end
            else
                copyToClipboard(emojiString)
                doPaste()
            end
            ---

            selectedEmojis = {}
            clearAlert()
            return
    end)
    local sortMode = "recent"

    local aliases = emojiChooserAliases()
    local choices = emojiRows(aliases)

    local function alphabetical(a, b)
        if a.name ~= b.name then return a.name < b.name end
        return a.emoji < b.emoji
    end

    local function refreshSort()
        if sortMode == "recent" then
            recent.sort(choices, function(row) return row.emoji end, alphabetical)
        else
            table.sort(choices, function(a, b)
                if a.unicodeVersion ~= b.unicodeVersion then
                    return a.unicodeVersion > b.unicodeVersion
                end
                return alphabetical(a, b)
            end)
        end
        chooser:placeholderText("Choose an emoji... [" ..
            (sortMode == "recent" and "recent usage" or "newest Unicode") .. "; Tab: sort]")
        ChooserSearch.setChoices(chooser, choices)
        chooser:selectedRow(1)
    end

    refreshSort()

    local function cycleSort()
        sortMode = sortMode == "recent" and "unicode" or "recent"
        refreshSort()
    end

    local function addCurrent()
        local choice = chooser:selectedRowContents()

        if choice then
            table.insert(selectedEmojis, choice.emoji)
            updateAlert()
        end
    end

    local function removeLast()
        if #selectedEmojis > 0 then
            table.remove(selectedEmojis)
            updateAlert()
        end
    end

    local function pasteImage(format)
        if exporting or closed then return end
        local emojis = selectedEmojis
        if #emojis == 0 then
            local choice = chooser:selectedRowContents()
            if not choice then return end
            emojis = {choice.emoji}
        end
        exporting = true
        local function finish(err, data)
            exporting = false
            if closed then return end
            if err then
                alert_gateway("Emoji " .. format .. ": " .. tostring(err), {color="warn"})
                return
            end
            pendingImage = data
            chooser:select()
        end
        local ok, err = pcall(function()
            if format == "WebP" then
                emojiChooserWebp(emojis, finish)
            else
                emojiChooserPrepareImages(emojis, function(failure)
                    if failure then finish(failure); return end
                    local success, png = pcall(emojiChooserPng, emojis, 512, 512)
                    if success then finish(nil, {["public.png"]=png}) else finish(png) end
                end)
            end
        end)
        if not ok then
            finish(err)
        end
    end

    local hotkeys = {
        shiftEnter = hs.hotkey.bind('shift', 'return', addCurrent),
        webp = hs.hotkey.bind('cmd', 'return', function() pasteImage("WebP") end),
        png = hs.hotkey.bind({'cmd', 'shift'}, 'return', function() pasteImage("PNG") end),
        tab = hs.hotkey.bind('', 'tab', cycleSort),
        backslash = hs.hotkey.bind('', '\\', removeLast),
        shiftTab = hs.hotkey.bind('shift', 'tab', removeLast)
    }

    -- Carbon hotkeys can latch Shift+Delete until the modifier is released.
    -- Read each key-down directly so repeated presses with Shift held all work.
    local deleteTap = hs.eventtap.new({ hs.eventtap.event.types.keyDown }, function(event)
        local flags = event:getFlags()
        if event:getKeyCode() == 51 and flags.shift and
            not (flags.cmd or flags.alt or flags.ctrl) then
            removeLast()
            return true
        end
        return false
    end):start()

    local function cleanup()
        closed = true
        deleteTap:stop()
        -- hs.alert("emojiChooser: cleanup")

        for _, hk in pairs(hotkeys) do hk:delete() end

        inputLangPop()

        clearAlert()
    end

    chooser:hideCallback(cleanup)

    local function main()
        inputLangPush()
        langSetEn()

        chooser:show()
    end

    -- Wrap the main execution in pcall to catch any errors
    local success, err = pcall(main)

    -- If an error occurred, clean up and rethrow the error
    if not success then
        alert_gateway("emojiChooser error:" .. err, { color = "crit", seconds = 10 })

        cleanup()
        error(err)
    end

    -- Use the defer function to ensure cleanup happens on garbage collection
    -- This will act as a "finally" block
    -- local function defer(func)
    --     return setmetatable({}, { __gc = func })
    -- end
    -- local _ = defer(cleanup)
end
hyper_bind_v2{mods={}, key="a", pressedfn=emojiChooser}
if emojiChooserPrewarmDelay then emojiChooserPrewarm() end
--- * Wi-Fi Chooser
-- One id for the whole connect flow, so "Connecting" is replaced by its own
-- outcome rather than leaving two bands stacked.
local kWifiAlertId = "wifi-chooser"

local wifiChooserScan = nil
local wifiChooserNetworkCache = nil
local wifiChooserNetworkCacheAt = nil

local function wifiChooserCacheAgeText()
    if wifiChooserNetworkCacheAt then
        return "cached " .. tostring(math.floor(hs.timer.secondsSinceEpoch() - wifiChooserNetworkCacheAt)) .. "s ago"
    end

    return "cached"
end

local function wifiChooserInterface()
    local ok, details = pcall(wifi.interfaceDetails)
    if ok and details and details.interface then
        return details.interface
    end

    local interfaces = wifi.interfaces()
    return interfaces and interfaces[1] or nil
end

function wifiChooser()
    local interface = wifiChooserInterface()
    local currentNetwork = wifi.currentNetwork(interface)
    local allChoices = {}
    local active = true
    local retryTimer = nil

    local chooser = ChooserSearch.new(function(choice)
            if not choice or not choice.ssid or choice.ssid == "" then
                return
            end

            if choice.connected then
                wifi.disassociate(interface)
                alert_gateway("Disconnected from Wi-Fi: " .. choice.ssid, { id = kWifiAlertId })
                return
            end

            if not interface then
                alert_gateway("No Wi-Fi interface found", { id = kWifiAlertId, color = "warn" })
                return
            end

            alert_gateway("Connecting Wi-Fi: " .. choice.ssid, { id = kWifiAlertId })
            hs.task.new("/usr/sbin/networksetup", function(exitCode, stdOut, stdErr)
                    if exitCode == 0 then
                        alert_gateway("Connected Wi-Fi: " .. choice.ssid, { id = kWifiAlertId })
                    else
                        local err = stdErr or stdOut or ""
                        alert_gateway("Wi-Fi connect failed: " .. choice.ssid .. "\n" .. err, { id = kWifiAlertId, color = "crit" })
                    end
            end, {"-setairportnetwork", interface, choice.ssid}):start()
    end)

    chooser:placeholderText("Choose Wi-Fi network...")
    ChooserSearch.setChoices(chooser, {{text="Scanning Wi-Fi networks...", subText=currentNetwork and ("Current: " .. currentNetwork) or "Not connected"}})
    chooser:rows(12)


    local function updateChoices(networks)
        currentNetwork = wifi.currentNetwork(interface)

        local bySsid = {}
        for _, network in ipairs(networks or {}) do
            local ssid = network.ssid
            if ssid and ssid ~= "" then
                local existing = bySsid[ssid]
                if not existing or (network.rssi or -999) > (existing.rssi or -999) then
                    bySsid[ssid] = network
                end
            end
        end

        if currentNetwork and currentNetwork ~= "" and not bySsid[currentNetwork] then
            bySsid[currentNetwork] = {ssid=currentNetwork}
        end

        allChoices = {}
        for ssid, network in pairs(bySsid) do
            local connected = ssid == currentNetwork
            local prefix = connected and "* " or "  "
            local parts = {}

            if connected then
                table.insert(parts, "connected")
            end
            if network.rssi then
                table.insert(parts, "RSSI " .. tostring(network.rssi))
            end
            if network.wlanChannel and network.wlanChannel.number then
                table.insert(parts, "channel " .. tostring(network.wlanChannel.number))
            end

            table.insert(allChoices, {
                             text=prefix .. ssid,
                             subText=table.concat(parts, " | "),
                             ssid=ssid,
                             connected=connected,
                             rssi=network.rssi or -999,
            })
        end

        table.sort(allChoices, function(a, b)
                if a.connected ~= b.connected then
                    return a.connected
                end
                if a.rssi ~= b.rssi then
                    return a.rssi > b.rssi
                end
                return a.ssid < b.ssid
        end)

        if #allChoices == 0 then
            allChoices = {{text="No Wi-Fi networks found", subText="", ssid=""}}
        end

        ChooserSearch.setChoices(chooser, allChoices)
    end

    chooser:hideCallback(function()
            active = false
            if retryTimer and retryTimer:running() then
                retryTimer:stop()
            end
    end)

    if wifiChooserNetworkCache then
        updateChoices(wifiChooserNetworkCache)
        chooser:placeholderText("Choose Wi-Fi network... " .. wifiChooserCacheAgeText())
    end

    local function startScan()
        if wifiChooserScan and not wifiChooserScan:isDone() then
            return
        end

        wifiChooserScan = wifi.backgroundScan(function(networks)
            wifiChooserScan = nil
            if not active then
                return
            end

            if type(networks) == "string" then
                -- With Internet Sharing serving over Wi-Fi, the interface is
                -- an access point and CoreWLAN refuses every scan
                -- (kCWNotSupportedErr, -3903), so retrying cannot help.
                local okd, details = pcall(wifi.interfaceDetails, interface)
                if okd and details and details.interfaceMode == "Host AP" then
                    ChooserSearch.setChoices(chooser, {{text="Wi-Fi is hosting Internet Sharing",
                                      subText="It cannot scan as an access point; turn sharing off to choose a network", ssid=""}})
                    return
                end
                if wifiChooserNetworkCache then
                    updateChoices(wifiChooserNetworkCache)
                    chooser:placeholderText("Choose Wi-Fi network... " .. wifiChooserCacheAgeText() .. "; scan retrying")
                else
                    ChooserSearch.setChoices(chooser, {{text="Scanning Wi-Fi networks...", subText="Retrying after scan error: " .. networks, ssid=""}})
                end

                retryTimer = hs.timer.doAfter(3, startScan)
                return
            else
                wifiChooserNetworkCache = networks
                wifiChooserNetworkCacheAt = hs.timer.secondsSinceEpoch()
                chooser:placeholderText("Choose Wi-Fi network...")
            end
            updateChoices(networks)
        end, interface)
    end

    chooser:show()
    startScan()
end
_G["wifi-chooser"] = wifiChooser
hyper_bind_v2{mods={}, key="w", pressedfn=wifiChooser}
---
-- Search suggestions for anycomplete: Google's, or DuckDuckGo's when Google
-- fails, straight over hs.http rather than through BrishGarden, so hyper+g
-- works while the garden is down. The old path also formatted the query into
-- a zsh command with Lua's %q, which is not shell quoting: a `$(...)' in the
-- clipboard ran in the garden. cb(list of strings) is called once.
-- @duplicateCode/75d37f39797b269628a76eff06708a21: autosuggestions-gateway,
-- autosuggestions-goo and autosuggestions-ddg in
-- zshlang/auto-load/others/web.zsh: the endpoints, the user agent, and an
-- empty query meaning the clipboard.
local kSuggestHeaders = {
    ["User-Agent"] = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/107.0.0.0 Safari/537.36",
}

local function suggestTrim(s)
    return (tostring(s or ""):gsub("^%s+", ""):gsub("%s+$", ""))
end

function anycompleteSuggest(query, cb)
    local q = suggestTrim(query)
    if q == "" then q = suggestTrim(hs.pasteboard.getContents()) end
    if q == "" then return cb({}) end
    local enc = hs.http.encodeForQuery(q)

    hs.http.asyncGet("http://suggestqueries.google.com/complete/search?client=firefox&ie=utf-8&oe=utf-8&q=" .. enc,
                     kSuggestHeaders, function(status, body)
        local ok, j = pcall(hs.json.decode, body or "")
        if status == 200 and ok and type(j) == "table" and type(j[2]) == "table" then
            return cb(j[2])
        end
        hs.http.asyncGet("https://duckduckgo.com/ac/?q=" .. enc, kSuggestHeaders, function(status2, body2)
            local ok2, j2 = pcall(hs.json.decode, body2 or "")
            local out = {}
            if status2 == 200 and ok2 and type(j2) == "table" then
                for _, e in ipairs(j2) do
                    if type(e) == "table" and e.phrase then out[#out + 1] = e.phrase end
                end
            end
            cb(out)
        end)
    end)
end

function anycomplete()
    local timer
    -- Which refresh is current: an answer for an older query arrives late and
    -- is dropped, since hs.http requests cannot be cancelled.
    local generation = 0
    local refreshChoices
    local tab = nil
    local antitab = nil

    if hs.keycodes.currentSourceID() == inputEnglish then
        eventtap.keyStroke({"shift", "alt"}, hs.keycodes.map['left'])
    else
        eventtap.keyStroke({"shift", "alt"}, hs.keycodes.map['right'])
    end

    doCopy()

    c = ChooserSearch.remote(function(x)
            if tab then tab:delete() end
            if antitab then antitab:delete() end
            if not x then return end

            hs.eventtap.keyStrokes(x.text)
    end)
    c:placeholderText("anycomplete ...")
    c:width(70)
    c:rows(11)
    tab = hs.hotkey.bind('', 'tab', function()
                             local x = c:selectedRowContents()
                             if not x then
                                 return
                             end
                             c:query(x.text)
                             refreshChoices()
    end)
    antitab = hs.hotkey.bind('shift', 'tab', function()
                                 local x = c:selectedRowContents()
                                 if not x then
                                     return
                                 end
                                 hs.pasteboard.setContents(x.text)
                                 c:hide()
    end)
    -- c:choices(function()
    --     local q = c:query()
    --     local cmd = ("autosuggestions-gateway %q"):format(q)
    --     print("cmd: " .. cmd)
    --     local res = brishz_eval(cmd)
    --     local out = {}
    --     for l in res:gmatch("([^\r\n]+)\r?\n?") do
    --       table.insert(out, {["text"] = l})
    --     end
    --     return out
    -- end)
    refreshChoices = function()
        generation = generation + 1
        local mine = generation
        local chooser = c
        anycompleteSuggest(chooser:query(), function(list)
            if mine ~= generation then return end
            local out = {}
            for _, l in ipairs(list) do
                out[#out + 1] = { ["text"] = tostring(l) }
            end
            chooser:choices(out)
        end)
    end
    ChooserSearch.onQueryChanged(c,function(query)
            if timer and timer:running() then
                timer:stop()
            end
            -- timer = hs.timer.doAfter(0.3, function() c:refreshChoicesCallback() end)
            timer = hs.timer.doAfter(0.0, refreshChoices)
    end)
    c:query(hs.pasteboard.getContents())
    c:show()
    if hs.keycodes.currentSourceID() == inputEnglish then
        eventtap.keyStroke({}, hs.keycodes.map['right'])
    else
        eventtap.keyStroke({}, hs.keycodes.map['left'])
    end
end
hyper_bind_v1("g", anycomplete)
