-- Restore a temporarily unfocused control only while its native window still
-- owns focus. Call cancel before requesting an intentional app/window switch.
return function(options)
    options=options or {}
    local F={version="1.0.1"}
    local function bounded(element)
        if element and element:setTimeout(.15) then return element end
    end
    local function nativeWindow(window)
        window=bounded(window)
        if window and window:attributeValue("AXRole")=="AXGroup" then
            window=bounded(window:attributeValue("AXWindow"))
        end
        if window and window:attributeValue("AXRole")=="AXWindow" then return window end
    end
    local function appElement(app)
        return bounded(hs.axuielement.applicationElement(app))
    end
    function F.cancel()
        F.element,F.window,F.pid,F.app=nil,nil,nil,nil
    end
    function F.capture(app)
        F.cancel()
        F.lastUnfocusError,F.lastRestoreError=nil,nil
        if not app then return end
        local ax=appElement(app)
        if not ax then return end
        local element=bounded(ax:attributeValue("AXFocusedUIElement"))
        if not element or not element:isValid() then return end
        local window=nativeWindow(ax:attributeValue("AXFocusedWindow"))
        if not window or not window:isValid() or element:pid()~=app:pid() then return end
        local elementWindow=element:attributeValue("AXWindow")
        if elementWindow and nativeWindow(elementWindow)~=window then return end
        F.element,F.pid,F.window,F.app=element,app:pid(),window,app
        -- A timed-out write may still apply. Keep the validated token so release
        -- can make one guarded compensating request; never retry the blur.
        local accepted,err=element:setAttributeValue("AXFocused",false)
        if not accepted then F.lastUnfocusError=err or "AX unfocus failed" end
        return element
    end
    function F.restore(expected)
        local element,window,pid,owner=F.element,F.window,F.pid,F.app
        F.cancel()
        if not element or expected~=element then return false end
        local app=hs.application.frontmostApplication()
        if not app or not owner or owner:pid()~=pid then return false end
        -- Clipboard managers and other known overlays borrow focus temporarily.
        -- Their own window is irrelevant; verify the captured owner's window.
        if app:pid()~=pid and not (options.isTransient and options.isTransient(app)) then return false end
        element,window=bounded(element),bounded(window)
        if not element or not window or not element:isValid() or not window:isValid()
            or element:pid()~=pid then return false end
        local ax=appElement(owner)
        if not ax or nativeWindow(ax:attributeValue("AXFocusedWindow"))~=window then return false end
        local accepted,err=element:setAttributeValue("AXFocused",true)
        if not accepted then F.lastRestoreError=err or "AX restore failed" end
        return not not accepted
    end
    return F
end
