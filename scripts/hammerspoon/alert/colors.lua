--- * Alert colours
--- Every colour an alert can wear, in one place, so the palette can be read at
--- a glance rather than reconstructed from the engine around it.
---
--- Two palettes, deliberately separate. Band colours are what a whole band is
--- painted in: dark and translucent, because they sit *under* the text and over
--- whatever window they land on. Markup colours are for [text]{red} spans, which
--- sit *on* a band: light and saturated. A name in one is not a name in the
--- other.
---
--- A caller names a colour as a string rather than passing a table, because the
--- shell is a first-class caller here and a table literal would have to survive
--- `hammerspoon -c' quoting. AlertEngine.resolveBandColor below is what turns a
--- name into something hs.canvas will paint.

--- ** The five originals
--- These stay separate globals rather than moving into the palette table below,
--- because they are the ones a user is most likely to have overridden in a
--- console or a local file, and resolveBandColor reads them live so such an
--- override still takes effect immediately.

--- Dark slate. Quiet enough to live on screen for a few seconds without being
--- the loudest thing on the monitor.
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

--- ** The named palette
--- Applies alertV2BandAlpha so a palette author cannot forget it. The alpha is
--- baked in here at load time, exactly as it is in the five globals above.
local function band(r, g, b)
    return { red = r, green = g, blue = b, alpha = alertV2BandAlpha }
end

--- Dark enough, every one of them, that white text sits on them comfortably --
--- which is not decoration, it is the rule AlertEngine.textColorFor applies:
--- anything brighter would flip the text to black, and a band bright enough to
--- need black text is a band bright enough to be the loudest thing on screen.
--- Keep new entries under roughly 0.45 relative luminance.
AlertEngine.bandColors = AlertEngine.bandColors or {
    success  = band(0.05, 0.38, 0.13),
    forest   = band(0.05, 0.25, 0.10),
    ocean    = band(0.02, 0.26, 0.40),
    teal     = band(0.02, 0.35, 0.35),
    sky      = band(0.10, 0.32, 0.48),
    violet   = band(0.28, 0.10, 0.45),
    plum     = band(0.35, 0.12, 0.32),
    rose     = band(0.55, 0.10, 0.30),
    blood    = band(0.30, 0.02, 0.05),
    rust     = band(0.45, 0.18, 0.05),
    gold     = band(0.55, 0.42, 0.03),
    olive    = band(0.28, 0.30, 0.05),
    slate    = band(0.22, 0.26, 0.33),
    graphite = band(0.14, 0.14, 0.16),
    midnight = band(0.04, 0.05, 0.16),
    ink      = band(0.02, 0.02, 0.04),
}

--- x11 has its own `green' (a blast of pure #00FF00) and its own `gold', and
--- the fallback below would find them. Ours win: a name that reads as a mood
--- should render as a band, not as a highlighter.
AlertEngine.bandColors.green = AlertEngine.bandColors.success
AlertEngine.bandColors.info = AlertEngine.bandColors.ocean
AlertEngine.bandColors.error = alertV2AgentColor

--- ** Animated colours
--- A band colour may be a *descriptor* instead of a colour table:
---
---   { animated = true,
---     period    = <seconds>,
---     textColor = <colour table>,   -- fixed for the whole cycle
---     at        = function(now) return <colour table> end }
---
--- `at' must be a pure function of the wall clock, because that is the only
--- kind of animation this engine can keep. Canvases are torn down and rebuilt
--- constantly -- on every new alert, on every dismissal, once a second while a
--- countdown is up -- so animation state stored anywhere would be wiped. Phase
--- from `now % period' survives all of it, needs no per-alert bookkeeping, and
--- makes two bands wearing the same colour animate in lockstep.
---
--- `textColor' is fixed rather than recomputed per frame: text that flipped
--- between black and white mid-cycle would strobe. So an animation has to stay
--- inside one contrast regime for its whole period, which is why the brightness
--- of each is capped rather than swinging over the full range.
---
--- Always include an alpha in what `at' returns. Assigning an HSB table without
--- one resets the element's alpha to fully opaque.

--- 0 -> 1 -> 0 over one period, cosine-smooth, so there is no visible corner at
--- the turn. The same shape as the flood fade's smoothstep, for the same reason.
local function wave(now, period)
    return (1 - math.cos(2 * math.pi * (now % period) / period)) / 2
end

local function lerp(a, b, t)
    return a + (b - a) * t
end

--- The `-1' suffix is a version, not a count: a variant that spins faster or
--- picks different hues becomes rainbow-2 rather than replacing this one, so a
--- caller that liked the old one keeps it.
AlertEngine.animatedColors = AlertEngine.animatedColors or {
    --- The whole hue circle in ten seconds. Saturation and brightness are held
    --- flat so only the hue moves; brightness 0.52 keeps every hue dark enough
    --- for white text, which a full-brightness rainbow would not be -- pure
    --- yellow and cyan are far too bright to sit under white.
    ["rainbow-1"] = {
        animated = true, period = 10, textColor = { white = 1 },
        at = function(now)
            return { hue = (now % 10) / 10, saturation = 0.70,
                     brightness = 0.52, alpha = alertV2BandAlpha }
        end,
    },

    --- A pale metal band breathing slowly. The one light colour here, so it is
    --- the one that takes black text, and it carries a higher alpha than the
    --- rest: black on a translucent light band over a dark desktop is the worst
    --- contrast case in the whole palette, and opacity is what rescues it.
    ["silver-pulse-1"] = {
        animated = true, period = 4, textColor = { white = 0 },
        at = function(now)
            return { hue = 0.60, saturation = 0.06,
                     brightness = lerp(0.60, 0.78, wave(now, 4)),
                     alpha = 0.92 }
        end,
    },

    --- Something watching from the dark: a near-black blue-grey that swells
    --- toward amber-gold and sinks back. The blend tops out at 30%, so it
    --- glows rather than lights up, and the band stays dark throughout.
    ["wolf-eye-1"] = {
        animated = true, period = 6, textColor = { white = 1 },
        at = function(now)
            local w = 0.30 * wave(now, 6)
            return { red   = lerp(0.06, 0.85, w),
                     green = lerp(0.08, 0.62, w),
                     blue  = lerp(0.12, 0.10, w),
                     alpha = alertV2BandAlpha }
        end,
    },
}

--- ** Resolving a name
local kLegacyNames = {
    default = "alertV2DefaultColor",
    warn    = "alertV2WarnColor",
    amber   = "alertV2WarnColor",
    crit    = "alertV2AgentColor",
    agent   = "alertV2AgentColor",
    free    = "alertV2FreeColor",
    notice  = "alertV2NoticeColor",
}

--- Bands must stay dark enough to read white text on, and x11 is full of
--- colours that are not: `white', `yellow', `ivory'. Capping brightness pulls
--- the worst offenders down without flattening the rest -- it is a ceiling, not
--- a rescale, so anything already dark passes through untouched.
local kX11MaxBrightness = 0.85

--- A name becomes a colour in this order: our own names first, so the palette
--- is predictable and cannot be shifted out from under a caller by whatever
--- x11 happens to contain; then x11 as a long tail; then the default. An
--- unknown name is not an error -- it renders as an ordinary alert, which is a
--- better failure than no alert at all.
function AlertEngine.resolveBandColor(color)
    if type(color) == "table" then
        return color
    end
    if type(color) ~= "string" then
        return alertV2DefaultColor
    end

    -- Read live, so reassigning one of the globals still takes effect at once.
    local legacy = kLegacyNames[color]
    if legacy then
        return _G[legacy] or alertV2DefaultColor
    end
    if AlertEngine.bandColors[color] then
        return AlertEngine.bandColors[color]
    end
    if AlertEngine.animatedColors[color] then
        return AlertEngine.animatedColors[color]
    end

    -- hs.drawing.color.x11 keys are all lowercase, so this needs no index.
    local x11 = hs.drawing.color.x11[color:lower()]
    if x11 then
        local hsb = hs.drawing.color.asHSB(x11)
        if hsb then
            return { hue = hsb.hue, saturation = hsb.saturation,
                     brightness = math.min(hsb.brightness, kX11MaxBrightness),
                     alpha = alertV2BandAlpha }
        end
    end

    return alertV2DefaultColor
end

--- ** Using a colour
--- A malformed descriptor would otherwise blow up thirty times a second inside
--- the animation timer, so `at' being callable is part of being animated.
function AlertEngine.isAnimated(color)
    return type(color) == "table"
        and color.animated == true
        and type(color.at) == "function"
end

--- What to actually paint. Every place that hands a band colour to hs.canvas
--- goes through here, so a descriptor never reaches the canvas itself.
function AlertEngine.colorAt(color, now)
    if AlertEngine.isAnimated(color) then
        return color.at(now or hs.timer.secondsSinceEpoch())
    end
    return color
end

--- Black or white text, whichever the band can carry. Relative luminance with
--- the usual coefficients: the eye is far more sensitive to green than to blue,
--- so a plain average would call amber dark and navy light, which is backwards.
---
--- The threshold sits above the midpoint deliberately, biasing toward white.
--- Bands are translucent, so whatever is behind them bleeds through and drags
--- the effective brightness *down*; a band that measures as borderline light
--- will usually look darker than it measures, and white is the safer bet.
local kLightBandThreshold = 0.55

function AlertEngine.textColorFor(color)
    if AlertEngine.isAnimated(color) then
        return color.textColor or { white = 1 }
    end
    -- asRGB normalises the { white = }, HSB and RGB forms alike. A caller can
    -- pass any of them, so guard rather than assume.
    local ok, rgb = pcall(hs.drawing.color.asRGB, color)
    if not ok or type(rgb) ~= "table" then
        return { white = 1 }
    end
    local luminance = 0.2126 * (rgb.red or 0)
        + 0.7152 * (rgb.green or 0)
        + 0.0722 * (rgb.blue or 0)
    return luminance > kLightBandThreshold and { white = 0 } or { white = 1 }
end

--- ** Markup colours
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
---
--- These assume a dark band, which every curated name is. On a light band -- an
--- x11 name, or silver-pulse-1 -- they wash out, and `grey'/`dim' disappear
--- into it entirely. That is left alone rather than second-guessed: a caller
--- who asked for a light band and coloured spans on it gets what they asked
--- for, and silently dropping the colour they named would be the worse answer.
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
