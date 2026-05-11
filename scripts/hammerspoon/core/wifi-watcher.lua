--- * Wi-Fi Watcher
wifiWatcher = nil
previousSSID = hs.wifi.currentNetwork()

-- Define the callback function
function ssidChangedCallback()
    local newSSID = hs.wifi.currentNetwork()

    local alert_dur = 3

    if newSSID ~= previousSSID then
        if not newSSID then
            hs.alert("Disconnected from Wi-Fi network: " .. (previousSSID or "None"), alert_dur)

            brishzeval2bg("wifi-disconnect-hook")

        else
            hs.alert("Connected to Wi-Fi network: " .. (newSSID or "None"), alert_dur)

            if newSSID == "Tealy" then
                brishzeval2bg("tealy-connect-hook")

            else
                brishzeval2bg("wifi-unknown-connect-hook")
            end
        end

        previousSSID = newSSID
    end
end

-- Create and start the Wi-Fi watcher
wifiWatcher = hs.wifi.watcher.new(ssidChangedCallback)
wifiWatcher:start()
