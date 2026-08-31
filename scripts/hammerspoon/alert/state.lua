--- * Alert engine (v2) - shared state, knobs and geometry
--- Coloured bands across the screen, stacking, one per live alert. This is the
--- generalisation of what used to be the single-purpose agent focus banner: the
--- banner is now just an alert with a crimson colour and a countdown.
---
--- Why bands instead of hs.alert's centred box:
---   - several alerts can be on screen at once, each with its own colour, so a
---     new one does not hide the one you were reading;
---   - long text wraps and the band grows, instead of being cut off or shrunk;
---   - an optional fullscreen flash makes one impossible to miss.
---
--- Nothing here takes focus and nothing swallows clicks: no canvas mouse events
--- are registered, so every canvas is inert to the pointer. Everything expires
--- on its own, so a caller that crashes cannot brand the screen permanently.
---
--- ** The files
---   state.lua   this file: the namespace, the live state, every tunable
---   colors.lua  the palette, static and animated, and how a name resolves
---   markup.lua  the **bold** / [text]{red} subset and its parser
---   layout.lua  measuring, wrapping, and where each band goes on each screen
---   render.lua  canvases, the fullscreen flood, the countdown ticker, animation
---   api.lua     alertV2* and the alert_gateway* the rest of the config calls
---
--- boot.lua loads them in that order with dofile, and dofile gives each file its
--- own chunk: a file-local is invisible to the next file. So anything shared
--- between them hangs off the AlertEngine table below, the same way modal-mode
--- uses ModalMode. Only the public alertV2*/alert_gateway* functions and the
--- user-tunable alertV2* knobs are globals in their own right.
---
--- Shell interface:
---   hs -c 'alertV2("hello", { seconds = 5 })'
---   hs -c 'alertV2("**low** [12%]{red}", { markup = "md" })'
---   hs -c 'alertV2FromBase64("aGVsbG8=", { position = "bottom" })'
---   hs -c 'alertV2DismissAll()'
---
--- The base64 entry point exists because escaping quotes, newlines and unicode
--- through `hammerspoon -c` is a losing game; see hs-alert-v2 in
--- zshlang/auto-load/others/hammerspoon.zsh.

AlertEngine = AlertEngine or {}

alertEngineState = alertEngineState or {
    alerts = {},         -- ordered oldest first; the render order too
    canvases = {},       -- { canvas, screen, position, stack }
    floodCanvases = {},
    flood = nil,         -- { color, startedAt, duration, fadeIn, fadeOut }
    floodTimer = nil,
    floodFadeTimer = nil,
    ticker = nil,
    hooked = false,
    counter = 0,
}
--- the loudest thing on the monitor.
--- How opaque a band is. Slightly see-through, so a band lying across a window
--- does not read as a hole punched in it - you can still tell what it is
--- covering. One knob rather than an alpha buried in each colour below; a
--- colour passed in by a caller keeps whatever alpha it carries.
alertV2BandAlpha = alertV2BandAlpha or 0.8

--- How opaque the fullscreen flash is. The alert's own colour, but much more
--- see-through than a band: the flash has to be impossible to miss without
--- blacking out the screen it covers. The bands drawn on top of it keep their
--- own opacity.
alertV2FloodAlpha = alertV2FloodAlpha or 0.33

--- The flash ramps its opacity up and back down instead of snapping on and off,
--- which reads as a glitch rather than as something arriving. Both ramps live
--- *inside* flashSeconds, so adding them does not lengthen the flood: a caller
--- that asked for half a second still gets exactly half a second. The alternative
--- - flashSeconds meaning "time at full opacity", with the ramps added around it
--- - would silently stretch every existing caller, and would break the release
--- banner, which is built so that its alert and its flash end together.
---
--- In is quicker than out: arriving is an alarm and wants to be abrupt, leaving
--- is a release and wants to drain.
alertV2FloodFadeInSeconds = alertV2FloodFadeInSeconds or 0.10
alertV2FloodFadeOutSeconds = alertV2FloodFadeOutSeconds or 0.20

--- How often the ramp is redrawn. Each step assigns one colour per screen, so
--- this is cheap; it is nowhere near a full re-render.
alertV2FloodFadeFps = alertV2FloodFadeFps or 30

--- ** Geometry
AlertEngine.kFont = "Menlo"
AlertEngine.kTextSize = 15
AlertEngine.kLineHeight = AlertEngine.kTextSize * 1.4
AlertEngine.kPaddingX = 10
AlertEngine.kPaddingY = 6
AlertEngine.kMinBandHeight = 30

--- No position may eat more than this fraction of a screen's usable height.
--- This is what makes hs-reval-alert (up to 30 lines of command output)
--- survivable: past the cap the text is truncated, and the band says so.
alertV2MaxStackFraction = alertV2MaxStackFraction or 0.45

AlertEngine.kPositions = { "top", "center", "bottom" }
--- @end
