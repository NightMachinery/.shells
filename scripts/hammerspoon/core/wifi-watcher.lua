--- * Wi-Fi Watcher
-- Connect and disconnect share an id: the network you are on is one fact, so
-- a later change replaces the earlier band instead of stacking on it.
local kWifiWatcherAlertId = "wifi-watcher"

wifiWatcher = nil
previousSSID = hs.wifi.currentNetwork()

-- Define the callback function
function ssidChangedCallback()
    local newSSID = hs.wifi.currentNetwork()

    local alert_dur = 3

    if newSSID ~= previousSSID then
        if not newSSID then
            alertV2("Disconnected from Wi-Fi network: " .. (previousSSID or "None"),
                    { id = kWifiWatcherAlertId, seconds = alert_dur, color = "warn" })

            brishzeval2bg("wifi-disconnect-hook")

        else
            alertV2("Connected to Wi-Fi network: " .. (newSSID or "None"),
                    { id = kWifiWatcherAlertId, seconds = alert_dur })

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
