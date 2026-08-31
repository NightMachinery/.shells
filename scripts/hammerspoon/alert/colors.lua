--- * Alert colours
--- Every colour an alert can wear, in one place, so the palette can be read at
--- a glance rather than reconstructed from the engine around it.
---
--- Two palettes, deliberately separate. Band colours are what a whole band is
--- painted in: dark and translucent, because they sit *under* the text and over
--- whatever window they land on. Markup colours are for [text]{red} spans, which
--- sit *on* a band: light and saturated. A name in one is not a name in the
--- other.


--- Dark slate. Quiet enough to live on screen for a few seconds without being

alertV2DefaultColor = alertV2DefaultColor
    or { red = 0.16, green = 0.19, blue = 0.24, alpha = alertV2BandAlpha }

--- Amber, the colour the agent banner shipped with: something wants attention
--- but nothing is on fire.
alertV2WarnColor = alertV2WarnColor
    or { red = 0.80, green = 0.36, blue = 0.02, alpha = alertV2BandAlpha }

--- Crimson: "a machine is driving this screen, do not touch the keyboard".
alertV2AgentColor = alertV2AgentColor
    or { red = 0.62, green = 0.06, blue = 0.10, alpha = alertV2BandAlpha }

--- Blue: the screen is yours again.
alertV2FreeColor = alertV2FreeColor
    or { red = 0.09, green = 0.055, blue = 0.42, alpha = alertV2BandAlpha }

--- Dimmed grey for the "earlier alerts hidden" notice, so it reads as chrome
--- rather than as another alert.
alertV2NoticeColor = alertV2NoticeColor
    or { red = 0.25, green = 0.25, blue = 0.28, alpha = alertV2BandAlpha }

--- Band colours by name, so a caller reachable only through a shell -- where a
--- table literal would have to survive `hammerspoon -c' quoting -- can say
--- `color = "warn"'. Resolved per call rather than cached in a table, so
--- reassigning one of the colours above takes effect immediately.
function AlertEngine.resolveBandColor(color)
    if type(color) == "table" then
        return color
    end
    if type(color) ~= "string" then
        return alertV2DefaultColor
    end
    return ({
        default = alertV2DefaultColor,
        warn    = alertV2WarnColor,
        amber   = alertV2WarnColor,
        crit    = alertV2AgentColor,
        agent   = alertV2AgentColor,
        free    = alertV2FreeColor,
        notice  = alertV2NoticeColor,
    })[color] or alertV2DefaultColor
end

--- Text colours for markup runs. Deliberately separate from the band colours
--- above: these sit *on* a band, so they are light and saturated rather than
--- dark and translucent.
---
--- `grey' and `dim' are translucent white rather than a fixed grey, because
--- dimness is a relation to the background and not a value. A fixed grey has to
--- pick a background to be dim against, and the one it picked was the dark slate
--- default -- so `[x]{dim}' on the amber band was mid-grey on orange at about
--- 1.6:1, i.e. unreadable, and would have failed the same way on crimson or
--- blue. Blending toward whatever band is underneath dims against all of them:
--- soft on slate, and readable with a warm tint on amber.
alertV2MarkupColors = alertV2MarkupColors or {
    red    = { red = 1.00, green = 0.45, blue = 0.40 },
    amber  = { red = 1.00, green = 0.76, blue = 0.33 },
    green  = { red = 0.56, green = 0.87, blue = 0.52 },
    blue   = { red = 0.58, green = 0.76, blue = 1.00 },
    grey   = { white = 1.00, alpha = 0.85 },
    dim    = { white = 1.00, alpha = 0.75 },
    white  = { white = 1.00 },
}
--- @end
