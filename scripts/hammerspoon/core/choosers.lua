function chis()
    -- https://www.hammerspoon.org/docs/hs.chooser.html
    -- @todo it'd probably be better if we put the URLs as `subtext`, and their titles as `text`. hs.chooser has this better than fzf.
    local tab = nil
    c = hs.chooser.new(function(x)
            if tab then tab:delete() end
            if not x then return end
            brishzeval2bg(("chis_clean %q | inargsf open"):format(x.text))
    end)
    c:placeholderText("Search history ...")
    c:width(95)
    c:rows(15)
    tab = hs.hotkey.bind('', 'tab', function()
                             local x = c:selectedRowContents()
                             if not x then
                                 return
                             end
                             brishzeval2bg(("bell-lm-mhm ; chis_clean %q | inargsf open ; "):format(x.text))
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
            local res = brishzeval2(cmd)
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
    ---
    local timer
    local tab = nil
    local antitab = nil
    c = hs.chooser.new(function(x)
            if tab then tab:delete() end
            if antitab then antitab:delete() end
            if not x then return end
            brishzeval2_out(("ntag-finder-sel-add %q"):format(x.text))
    end)
    c:placeholderText("ntag ...")
    -- c:width(95)
    c:rows(11)
    tab = hs.hotkey.bind('', 'tab', function()
                             local x = c:selectedRowContents()
                             if not x then
                                 return
                             end
                             -- brishzeval2bg(("ntag-finder-sel-add %q ; bell-lm-mhm"):format(x.text))
                             brishzeval2bg(("bell-lm-mhm ; ntag-finder-sel-add %q ; "):format(x.text))
    end)
    antitab = hs.hotkey.bind('shift', 'tab', function()
                                 local x = c:selectedRowContents()
                                 if not x then
                                     return
                                 end
                                 -- brishzeval2bg(("ntag-finder-sel-rm %q ; bell-pp-piece"):format(x.text))
                                 brishzeval2bg(("bell-pp-piece ; ntag-finder-sel-rm %q ; "):format(x.text))
    end)
    c:choices(function()
            local q = c:query()
            local cmd = ("ntag-select %q"):format(q)
            local res = brishzeval2(cmd)
            local out = {}
            for l in res:gmatch("([^\r\n]+)\r?\n?") do
                -- @upstreambug https://github.com/Hammerspoon/hammerspoon/issues/2574
                -- table.insert(out, {["text"] = hs.styledtext.ansi(l, {font={size=25}})})
                table.insert(out, {["text"] = l})
            end
            return out
    end)
    c:queryChangedCallback(function(query)
            if timer and timer:running() then
                timer:stop()
            end
            timer = hs.timer.doAfter(0.0, function() c:refreshChoicesCallback() end)
    end)
    c:show()
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
        hs.alert.show("Emoji data file not found")
        return
    end

    local data = file:read("*a")

    file:close()
    emojiData = json.decode(data)
end
loadEmojiData()

local emojiAlert -- Reference to the alert

local emojiStyle = {
    -- Define your custom style here, similar to hyperStyle
    atScreenEdge = 1,
    fadeInDuration = 0.001,
    fadeOutDuration = 0.001,
    fillColor = { white = 1, alpha = 2 / 3 },

    radius = 24,
    strokeColor = { red = 19 / 255, green = 182 / 255, blue = 133 / 255, alpha = 1},
    strokeWidth = 16,
    textColor = { white = 0.125 },
    textSize = 48,
}

function emojiChooser()
    local selectedEmojis = {} -- Table to store selected emojis

    local function updateAlert()
        if emojiAlert then
            hs.alert.closeSpecific(emojiAlert)
        end

        if #selectedEmojis >= 1 then
            local message = table.concat(selectedEmojis)
            emojiAlert = hs.alert.show(message, emojiStyle, hs.screen.mainScreen(), 'infinite')
        end
    end

    local function clearAlert()
        if emojiAlert then
            hs.alert.closeSpecific(emojiAlert)
            emojiAlert = nil
        end
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
        backspace = hs.hotkey.bind('shift', 'delete', removeLast),
        backspace = hs.hotkey.bind('', '\\', removeLast),
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
        hs.alert("emojiChooser error:" .. err)

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
                hs.alert("Disconnected from Wi-Fi: " .. choice.ssid)
                return
            end

            if not interface then
                hs.alert("No Wi-Fi interface found")
                return
            end

            hs.alert("Connecting Wi-Fi: " .. choice.ssid)
            hs.task.new("/usr/sbin/networksetup", function(exitCode, stdOut, stdErr)
                    if exitCode == 0 then
                        hs.alert("Connected Wi-Fi: " .. choice.ssid)
                    else
                        local err = stdErr or stdOut or ""
                        hs.alert("Wi-Fi connect failed: " .. choice.ssid .. "\n" .. err)
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
function anycomplete()
    local timer
    local myTask = nil
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
    --     local res = brishzeval2(cmd)
    --     local out = {}
    --     for l in res:gmatch("([^\r\n]+)\r?\n?") do
    --       table.insert(out, {["text"] = l})
    --     end
    --     return out
    -- end)
    function refreshChoices()
        if myTask then
            myTask:terminate()
            myTask = nil
        end
        local q = c:query()
        local cmd = ("autosuggestions-gateway %q"):format(q)
        -- print("cmd: " .. cmd)
        myTask = hs.task.new("/usr/local/bin/brishz2.dash",
                             function(exitCode, stdOut, stdErr)
                                 local res = stdOut
                                 local out = {}
                                 for l in res:gmatch("([^\r\n]+)\r?\n?") do
                                     table.insert(out, {["text"] = l})
                                 end
                                 c:choices(out)
                             end, {cmd})
        if myTask  then
            myTask:start()
        end
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
