function chis()
    -- https://www.hammerspoon.org/docs/hs.chooser.html
    -- @todo it'd probably be better if we put the URLs as `subtext`, and their titles as `text`. hs.chooser has this better than fzf.
    local tab = nil
    c = hs.chooser.new(function(x)
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
    c:queryChangedCallback(function(query)
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
    c = hs.chooser.new(function(x)
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
    c:queryChangedCallback(function(query)
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
-- Reusable function to filter choices based on space-separated regexp patterns
local function filterChoicesByPatterns(params)
    local query = params.query
    local choices = params.choices
    local filterKey = params.on or "text" -- Default to "text" if no key is provided

    -- Determine if the query contains any uppercase characters
    local case_sensitive_p = query:match("%u")

    local patterns = {}
    for pattern in query:gmatch("%S+") do -- Split query into space-separated patterns

        -- If case_sensitive_p is true, use the pattern as is; otherwise, convert to lowercase
        if not case_sensitive_p then
            pattern = pattern:lower()
        end

        table.insert(patterns, pattern)
    end

    local filteredChoices = {}
    for _, choice in ipairs(choices) do
        local match = true
        for _, pattern in ipairs(patterns) do
            local choiceText = choice[filterKey]

            -- If case_sensitive_p is true, use the choiceText as is; otherwise, convert to lowercase
            if not case_sensitive_p then
                choiceText = choiceText:lower()
            end

            if not string.match(choiceText, pattern) then
                match = false
                break
            end
        end
        if match then
            table.insert(filteredChoices, choice)
        end
    end
    return filteredChoices
end


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

function emojiChooser()
    local selectedEmojis = {} -- Table to store selected emojis

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

    local chooser = hs.chooser.new(function(choice)
            if not choice then
                -- canceled
                return
            end

            if #selectedEmojis == 0 then
                table.insert(selectedEmojis, choice.text)
            end

            local emojiString = table.concat(selectedEmojis)
            ---
            -- hs.eventtap.keyStrokes(emojiString)
            -- =keyStrokes= cannot insert some emojis.
            ---
            copyToClipboard(emojiString)
            doPaste()
            ---

            selectedEmojis = {}
            clearAlert()
            return
    end)
    chooser:placeholderText("Choose an emoji...")

    local choices = {}
    for emoji, info in pairs(emojiData) do
        table.insert(choices, {
                         text = emoji,
                         subText = info.name
                         ---
                         -- subText = emoji,
                         -- text = info.name
                         ---
                         -- image property can be added here if you have images for emojis
        })
    end

    table.sort(choices, function(a, b) return a.subText < b.subText end)

    chooser:choices(choices)

    -- Update the chooser choices based on the query
    chooser:queryChangedCallback(function(query)
            local filteredChoices = filterChoicesByPatterns{query=query, choices=choices, on="subText"}
            chooser:choices(filteredChoices)
    end)

    local function addCurrent()
        local choice = chooser:selectedRowContents()

        if choice then
            table.insert(selectedEmojis, choice.text)
            updateAlert()
        end
    end

    local function removeLast()
        if #selectedEmojis > 0 then
            table.remove(selectedEmojis)
            updateAlert()
        end
    end

    local hotkeys = {
        shiftEnter = hs.hotkey.bind('shift', 'return', addCurrent),
        tab = hs.hotkey.bind('', 'tab', addCurrent),
        -- Distinct keys: cleanup deletes what this table holds, and a second
        -- `backspace' used to overwrite the first, which left shift+delete
        -- bound, globally, after the first emoji chooser.
        backspace = hs.hotkey.bind('shift', 'delete', removeLast),
        backslash = hs.hotkey.bind('', '\\', removeLast),
        shiftTab = hs.hotkey.bind('shift', 'tab', removeLast)
    }

    local function cleanup()
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

    local chooser = hs.chooser.new(function(choice)
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
    chooser:choices({{text="Scanning Wi-Fi networks...", subText=currentNetwork and ("Current: " .. currentNetwork) or "Not connected"}})
    chooser:rows(12)

    chooser:queryChangedCallback(function(query)
            local filteredChoices = filterChoicesByPatterns{query=query, choices=allChoices, on="ssid"}
            chooser:choices(filteredChoices)
    end)

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

        chooser:choices(filterChoicesByPatterns{query=chooser:query(), choices=allChoices, on="ssid"})
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
                if wifiChooserNetworkCache then
                    updateChoices(wifiChooserNetworkCache)
                    chooser:placeholderText("Choose Wi-Fi network... " .. wifiChooserCacheAgeText() .. "; scan retrying")
                else
                    chooser:choices({{text="Scanning Wi-Fi networks...", subText="Retrying after scan error: " .. networks, ssid=""}})
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

    c = hs.chooser.new(function(x)
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
    c:queryChangedCallback(function(query)
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
