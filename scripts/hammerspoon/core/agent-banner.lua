--- * Agent focus banner
--- A strip across the top of every screen, shown while a coding agent is
--- driving this machine's UI and needs the focus left alone. It exists so an
--- agent can click through an app without having to guess whether a human is
--- about to reach for the same keyboard.
---
--- This is a thin wrapper over the v2 alert engine (core/alert-engine.lua):
--- the banner is an alert with a crimson colour, a countdown, and a fixed id.
--- The rendering, stacking, wrapping, screen watching and click-through all
--- live there, so an agent banner and an ordinary alert can be on screen at
--- once without either hiding the other.
---
--- Shell interface, for the agent:
---   hs -c 'agentBannerOn("clicking through Telegram", 1800)'
---   hs -c 'agentBannerOff()'
---   hs -c 'return agentBannerActive()'
---
--- Both arguments are optional. Calling agentBannerOn again while it is up
--- refreshes the countdown, which is how a long task keeps it alive.
---
--- It always expires on its own. An agent that crashes, is killed, or simply
--- forgets cannot leave the screen permanently branded, and that safety net is
--- worth more than the countdown being exactly right.

agentBannerState = agentBannerState or {
    message = nil,
}

--- The banner covers the whole screen for this long before collapsing to the
--- strip, so it cannot be missed by someone looking at the middle of a monitor.
--- Set to 0 to go straight to the strip.
---
--- The engine fades the wash in and out inside this window rather than around
--- it, so the number is still the whole life of the flash - but it now has to
--- pay for two ramps as well as the plateau, which is why it is not as short as
--- it was when the flash was a hard cut.
agentBannerFlashSeconds = agentBannerFlashSeconds or 0.35

--- The same, for the "screen is yours" flash when the banner comes down. The
--- end is the moment a human actually wants to notice, so it gets longer than
--- the raise.
agentBannerReleaseFlashSeconds = agentBannerReleaseFlashSeconds or 0.5

local kBannerId = "agent-banner"
local kReleaseId = "agent-banner-release"
local kDefaultSeconds = 30 * 60
local kMaxSeconds = 4 * 60 * 60

function agentBannerActive()
    return alert_gateway_exists(kBannerId)
end

function agentBannerOn(message, seconds, flashSeconds)
    if message ~= nil and message ~= "" then
        agentBannerState.message = tostring(message)
    end
    if flashSeconds ~= nil then
        agentBannerFlashSeconds = tonumber(flashSeconds) or agentBannerFlashSeconds
    end

    local duration = tonumber(seconds) or kDefaultSeconds
    duration = math.max(10, math.min(duration, kMaxSeconds))

    local text = agentBannerState.message
        and ("Agent: " .. agentBannerState.message)
        or "Agent is using the screen - please leave the focus alone"

    -- Raising the banner inside the release flash would otherwise leave both
    -- bands stacked, saying opposite things.
    alert_gateway_dismiss(kReleaseId)

    -- The engine's same-id, same-text rule is what makes a heartbeat quiet: it
    -- pushes the deadline out without flashing again. A changed message does
    -- flash, because it is new information.
    alert_gateway(text, {
        id = kBannerId,
        color = "agent",
        seconds = duration,
        countdown = true,
        -- Somebody alerting sixty lines of command output must not be able to
        -- push "a machine is driving this screen" off the top of it.
        pinned = true,
        flashSeconds = agentBannerFlashSeconds,
        position = "top",
    })
    return true
end

--- silent skips the release flash, for callers that just want it gone.
function agentBannerOff(silent)
    local wasActive = agentBannerActive()

    agentBannerState.message = nil
    alert_gateway_dismiss(kBannerId)

    if wasActive and not silent and agentBannerReleaseFlashSeconds > 0 then
        -- Lives exactly as long as its own flash, so the blue drains away and
        -- takes the words with it.
        alert_gateway("Screen is yours", {
            id = kReleaseId,
            color = "free",
            seconds = agentBannerReleaseFlashSeconds,
            flashSeconds = agentBannerReleaseFlashSeconds,
            position = "top",
        })
    end

    return true
end
--- @end
