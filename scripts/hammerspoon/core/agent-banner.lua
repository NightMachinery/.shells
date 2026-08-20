--- * Agent focus banner
--- A strip across the top of every screen, shown while a coding agent is
--- driving this machine's UI and needs the focus left alone. It exists so an
--- agent can click through an app without having to guess whether a human is
--- about to reach for the same keyboard.
---
--- The banner never takes focus and never swallows clicks: no canvas mouse
--- events are registered, so the strip is inert to the pointer.
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
    canvases = {},
    message = nil,
    expiry = nil,
    ticker = nil,
    pending = nil,
    flashing = false,
    hooked = false,
}

--- The banner covers the whole screen for this long before collapsing to the
--- strip, so it cannot be missed by someone looking at the middle of a monitor.
--- Set to 0 to go straight to the strip.
agentBannerFlashSeconds = agentBannerFlashSeconds or 0.2

--- The same, for the "screen is yours" flash when the banner comes down. The
--- end is the moment a human actually wants to notice.
agentBannerReleaseFlashSeconds = agentBannerReleaseFlashSeconds or 0.35

local kBannerHeight = 30
local kDefaultSeconds = 30 * 60
local kMaxSeconds = 4 * 60 * 60
local kStripTextSize = 15
local kBusyColor = { red = 0.80, green = 0.36, blue = 0.02, alpha = 1.0 }
local kFreeColor = { red = 0.09, green = 0.055, blue = 0.42, alpha = 1.0 }

local function bannerRemaining()
    if not agentBannerState.expiry then
        return nil
    end
    return math.max(0, agentBannerState.expiry - os.time())
end

local function bannerMessage()
    return agentBannerState.message
        or "Agent is using the screen - please leave the focus alone"
end

local function bannerStripText()
    local text = bannerMessage()
    if agentBannerState.message then
        text = "Agent: " .. text
    end
    local left = bannerRemaining()
    if left then
        text = string.format("%s   -   clears in %d:%02d",
                             text,
                             math.floor(left / 60),
                             left % 60)
    end
    return text
end

local function bannerCancelPending()
    if agentBannerState.pending then
        agentBannerState.pending:stop()
        agentBannerState.pending = nil
    end
    agentBannerState.flashing = false
end

local function bannerDestroy()
    for _, canvas in ipairs(agentBannerState.canvases) do
        canvas:delete()
    end
    agentBannerState.canvases = {}
end

--- full=true covers the whole screen, menu bar included; otherwise just the
--- strip below the menu bar, so the strip never fights it for the same pixels.
---
--- The text is drawn in the strip's band either way, at the same size. Only the
--- coloured area differs, so when the flash collapses the words do not move,
--- resize or reflow - the orange just drains away from under a line that was
--- already where it was going to stay. A flash that centred its text would
--- yank it out from under whoever started reading it.
local function bannerShow(opts)
    bannerDestroy()

    for _, scr in ipairs(ModalMode.targetScreens("all")) do
        local full = scr:fullFrame()
        local work = scr:frame()
        local rect = opts.full
            and { x = full.x, y = full.y, w = full.w, h = full.h }
            or { x = work.x, y = work.y, w = work.w, h = kBannerHeight }
        local bandTop = opts.full and (work.y - full.y) or 0

        local canvas = hs.canvas.new(rect)
        canvas:level(hs.canvas.windowLevels.overlay)
        canvas:behavior(hs.canvas.windowBehaviors.canJoinAllSpaces
                            + hs.canvas.windowBehaviors.stationary
                            + hs.canvas.windowBehaviors.fullScreenAuxiliary)

        -- Shrink rather than wrap, for the same reason: a long message that
        -- reflowed between the two would move the words after all. 0.55em is a
        -- rough average glyph width, and overestimating is the safe direction.
        local textSize = kStripTextSize
        local needed = #opts.text * textSize * 0.55
        local budget = rect.w * 0.92
        if needed > budget then
            textSize = math.max(9, math.floor(textSize * budget / needed))
        end

        -- hs.canvas has no vertical alignment for text elements, so the line is
        -- centred in the band by placing its own box. 1.4 leaves room for
        -- descenders without clipping.
        local lineHeight = textSize * 1.4
        canvas:appendElements({
            type = "rectangle",
            action = "fill",
            fillColor = opts.color,
        }, {
            type = "text",
            text = opts.text,
            textColor = { white = 1 },
            textSize = textSize,
            textAlignment = "center",
            frame = {
                x = 0,
                y = bandTop + (kBannerHeight - lineHeight) / 2,
                w = rect.w,
                h = lineHeight,
            },
        })
        canvas:show()
        table.insert(agentBannerState.canvases, canvas)
    end
end

local function bannerShowStrip()
    bannerShow({
        full = false,
        color = kBusyColor,
        text = bannerStripText(),
    })
end

local function bannerFlashThenStrip()
    if agentBannerFlashSeconds <= 0 then
        bannerShowStrip()
        return
    end

    agentBannerState.flashing = true
    bannerShow({
        full = true,
        color = kBusyColor,
        text = bannerStripText(),
    })
    agentBannerState.pending = hs.timer.doAfter(agentBannerFlashSeconds, function()
        agentBannerState.pending = nil
        agentBannerState.flashing = false
        -- Off may have been called inside the flash; do not resurrect it.
        if agentBannerActive() then
            bannerShowStrip()
        else
            bannerDestroy()
        end
    end)
end

local function bannerTick()
    if bannerRemaining() == 0 then
        agentBannerOff()
        return
    end
    -- Safe during the flash too: it carries the same string, and only the
    -- digits of the countdown change, so the fitted size still holds.
    local text = bannerStripText()
    for _, canvas in ipairs(agentBannerState.canvases) do
        canvas[2].text = text
    end
end

function agentBannerActive()
    return agentBannerState.expiry ~= nil
end

function agentBannerOn(message, seconds, flashSeconds)
    local wasActive = agentBannerActive()
    local previous = agentBannerState.message

    if message ~= nil and message ~= "" then
        agentBannerState.message = tostring(message)
    end

    local duration = tonumber(seconds) or kDefaultSeconds
    duration = math.max(10, math.min(duration, kMaxSeconds))
    agentBannerState.expiry = os.time() + duration

    if flashSeconds ~= nil then
        agentBannerFlashSeconds = tonumber(flashSeconds) or agentBannerFlashSeconds
    end

    bannerCancelPending()

    -- Flash for a new banner or a changed message, but not for a heartbeat that
    -- only pushes the deadline out. A long task would otherwise strobe.
    if wasActive and agentBannerState.message == previous then
        bannerShowStrip()
    else
        bannerFlashThenStrip()
    end

    if not agentBannerState.ticker then
        agentBannerState.ticker = hs.timer.doEvery(1, bannerTick)
    end
    if not agentBannerState.hooked then
        -- A monitor arriving or leaving would otherwise leave the strip on the
        -- old geometry, or missing from the new screen entirely. ModalMode
        -- already runs one screen watcher for every overlay in this config.
        ModalMode.onScreenChange(function()
            if agentBannerActive() and not agentBannerState.flashing then
                bannerShowStrip()
            end
        end)
        agentBannerState.hooked = true
    end

    return true
end

--- silent skips the release flash, for callers that just want it gone.
function agentBannerOff(silent)
    local wasActive = agentBannerActive()

    agentBannerState.expiry = nil
    agentBannerState.message = nil
    bannerCancelPending()

    if agentBannerState.ticker then
        agentBannerState.ticker:stop()
        agentBannerState.ticker = nil
    end

    if wasActive and not silent and agentBannerReleaseFlashSeconds > 0 then
        bannerShow({
            full = true,
            color = kFreeColor,
            text = "Screen is yours",
        })
        agentBannerState.pending = hs.timer.doAfter(
            agentBannerReleaseFlashSeconds,
            function()
                agentBannerState.pending = nil
                -- On may have been called inside the release flash.
                if not agentBannerActive() then
                    bannerDestroy()
                end
            end)
    else
        bannerDestroy()
    end

    return true
end
--- @end
