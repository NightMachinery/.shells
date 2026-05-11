ModalMode = ModalMode or {}

function ModalMode.positionedFrame(screenFrame, frame, position, margin)
    position = position or "center-top"
    margin = margin or 0

    local horizontal, vertical = position:match("^([^%-]+)%-([^%-]+)$")
    if position == "top" then
        horizontal = "center"
        vertical = "top"
    elseif position == "center" then
        horizontal = "center"
        vertical = "center"
    end

    if position == "left-top" then
        horizontal = "left"
        vertical = "top"
    end

    horizontal = horizontal or "center"
    vertical = vertical or "top"

    if horizontal == "left" then
        frame.x = screenFrame.x + margin
    elseif horizontal == "right" then
        frame.x = screenFrame.x + screenFrame.w - frame.w - margin
    else
        frame.x = screenFrame.x + (screenFrame.w - frame.w) / 2
    end

    if vertical == "top" then
        frame.y = screenFrame.y + margin
    elseif vertical == "bottom" then
        frame.y = screenFrame.y + screenFrame.h - frame.h - margin
    else
        frame.y = screenFrame.y + (screenFrame.h - frame.h) / 2
    end

    return frame
end

function ModalMode.createIndicator(style)
    local strokeWidth = style.strokeWidth / 1.5
    local text = style.text or style.name or ""

    local indicator = hs.canvas.new{x=0, y=0, w=0, h=0}:insertElement{
        id = "background",
        type = "rectangle",
        action = "strokeAndFill",
        fillColor = style.fillColor,
        roundedRectRadii = { xRadius = style.radius, yRadius = style.radius },
        strokeColor = style.strokeColor,
        strokeWidth = strokeWidth,
        padding = strokeWidth / 2,
    }:insertElement{
        id = "textBox",
        type = "text",
        text = text,
        textAlignment = "center",
        textColor = style.textColor,
        textSize = style.textSize,
    }

    local textBoxSize = indicator:minimumTextSize(2, text)
    local screenFrame = hs.screen.primaryScreen():fullFrame()
    local frame = {
        w = textBoxSize.w + style.strokeWidth * 2 + style.textSize,
        h = textBoxSize.h + style.strokeWidth * 2 + style.textSize,
    }
    ModalMode.positionedFrame(screenFrame, frame, style.overlayPosition or style.position, style.overlayMargin)

    indicator.textBox.frame = {
        x = (frame.w - textBoxSize.w) / 2,
        y = ((frame.h - textBoxSize.h) / 2) + (style.textYOffset or 5),
        w = textBoxSize.w,
        h = textBoxSize.h,
    }
    indicator:frame(frame)
    indicator:behavior{"canJoinAllSpaces", "transient", "fullScreenAuxiliary"}

    return indicator
end

function ModalMode.defaultStyle(o)
    o = o or {}

    return {
        fadeInDuration = 0.001,
        fadeOutDuration = 0.001,
        fillColor = o.fillColor or { white = 1, alpha = 2 / 3 },
        radius = o.radius or 24,
        strokeColor = o.strokeColor or { red = 19 / 255, green = 182 / 255, blue = 133 / 255, alpha = 1 },
        strokeWidth = o.strokeWidth or 16,
        textColor = o.textColor or { white = 0.125 },
        textSize = o.textSize or 48,
        text = o.text,
        textYOffset = o.textYOffset,
        overlayPosition = o.overlayPosition or o.position,
        overlayMargin = o.overlayMargin,
    }
end

function ModalMode.create(o)
    o = o or {}

    local mode = {
        name = o.name,
        modality = o.modality or hs.hotkey.modal.new(),
        auto_trigger_p = o.auto_trigger_p,
    }

    mode.modality.exit_on_release_p = false
    mode.modality.down_p = false
    mode.modality.entered_p = false

    function mode.enter()
        mode.modality:enter()
    end

    function mode.exit()
        mode.modality:exit()
    end

    function mode.toggle()
        if mode.modality.entered_p then
            mode.exit()
        else
            mode.enter()
        end
    end

    function mode.down()
        mode.modality.down_p = true
        mode.toggle()
    end

    function mode.up()
        mode.modality.down_p = false

        if mode.modality.exit_on_release_p then
            mode.exit()
        end
    end

    function mode.triggered()
        mode.modality.exit_on_release_p = true
        if not mode.modality.down_p then
            mode.exit()
        end
    end

    function mode.bindV1(key, pressedfn)
        return mode.bindV2{key=key, pressedfn=pressedfn}
    end

    function mode.bindV2(bindOptions)
        local hotkeyHolder = { my_hotkey = nil }
        local autoTrigger = bindOptions.auto_trigger_p
        if autoTrigger == nil then
            autoTrigger = mode.auto_trigger_p
        end
        if autoTrigger == nil then
            autoTrigger = true
        end

        local pressedfn
        local releasedfn
        local repeatfn

        if autoTrigger then
            if bindOptions.pressedfn then
                pressedfn = function()
                    mode.triggered()
                    bindOptions.pressedfn()
                end
            end

            if bindOptions.releasedfn then
                releasedfn = function()
                    mode.triggered()
                    bindOptions.releasedfn()
                end
            end

            if bindOptions.repeatfn then
                repeatfn = function()
                    if mode.modality.down_p == false then
                        mode.enter()
                        mode.exit()
                        return
                    end

                    mode.triggered()
                    bindOptions.repeatfn()
                end
            end
        else
            pressedfn = bindOptions.pressedfn
            releasedfn = bindOptions.releasedfn
            repeatfn = bindOptions.repeatfn
        end

        hotkeyHolder.my_hotkey = mode.modality:bind(bindOptions.mods or {}, bindOptions.key, pressedfn, releasedfn, repeatfn)
        return hotkeyHolder.my_hotkey
    end

    return mode
end

function ModalMode.installGlobals(mode, prefix)
    _G[prefix .. "_mode"] = mode
    _G[prefix .. "_modality"] = mode.modality
    _G[prefix .. "_enter"] = mode.enter
    _G[prefix .. "_exit"] = mode.exit
    _G[prefix .. "_toggle"] = mode.toggle
    _G[prefix .. "_down"] = mode.down
    _G[prefix .. "_up"] = mode.up
    _G[prefix .. "_triggered"] = mode.triggered
    _G[prefix .. "_bind_v1"] = mode.bindV1
    _G[prefix .. "_bind_v2"] = mode.bindV2
end

function ModalMode.appMatches(app, o)
    if not app then
        return false
    end

    if o.bundleID and app:bundleID() == o.bundleID then
        return true
    end

    if o.appName and app:name() == o.appName then
        return true
    end

    return false
end

function ModalMode.createAppFocusMode(o)
    local mode = ModalMode.create{
        name = o.name,
        auto_trigger_p = o.auto_trigger_p,
    }

    local indicator
    if o.overlay ~= false then
        local style = ModalMode.defaultStyle(o.overlay or {})
        style.text = style.text or o.label or o.appName or o.name
        indicator = ModalMode.createIndicator(style)
    end

    function mode.modality:entered()
        mode.modality.entered_p = true
        if indicator then
            indicator:show()
        end
    end

    function mode.modality:exited()
        mode.modality.entered_p = false
        mode.modality.exit_on_release_p = false
        if indicator then
            indicator:hide()
        end
    end

    local function syncWithFrontmost(app)
        app = app or hs.application.frontmostApplication()

        if ModalMode.appMatches(app, o) then
            if not mode.modality.entered_p then
                mode.enter()
            end
        elseif mode.modality.entered_p then
            mode.exit()
        end
    end

    mode.watcher = hs.application.watcher.new(function(appName, event, app)
        if event == hs.application.watcher.activated then
            syncWithFrontmost(app)
        elseif ModalMode.appMatches(app, o) then
            if event == hs.application.watcher.deactivated or event == hs.application.watcher.hidden or event == hs.application.watcher.terminated then
                syncWithFrontmost()
            end
        end
    end)
    mode.watcher:start()
    syncWithFrontmost()

    return mode
end
