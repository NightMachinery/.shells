-- Restore a temporarily unfocused control only while its native window still
-- owns focus. Call cancel before requesting an intentional app/window switch.
return function()
    local F={version="1.0.0"}
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
        F.element,F.window,F.pid=nil,nil,nil
    end
    function F.capture(app)
        F.cancel()
        if not app then return end
        local ax=appElement(app)
        if not ax then return end
        local element=bounded(ax:attributeValue("AXFocusedUIElement"))
        if not element or not element:isValid() then return end
        local window=nativeWindow(element:attributeValue("AXWindow"))
            or nativeWindow(ax:attributeValue("AXFocusedWindow"))
        if not window or not window:isValid() or element:pid()~=app:pid() then return end
        F.element,F.pid,F.window=element,app:pid(),window
        if not element:setAttributeValue("AXFocused",false) then F.cancel();return end
        return element
    end
    function F.restore(expected)
        local element,window,pid=F.element,F.window,F.pid
        F.cancel()
        if not element or expected~=element then return false end
        local app=hs.application.frontmostApplication()
        if not app or app:pid()~=pid then return false end
        element,window=bounded(element),bounded(window)
        if not element or not window or not element:isValid() or not window:isValid()
            or element:pid()~=pid then return false end
        local ax=appElement(app)
        if not ax or nativeWindow(ax:attributeValue("AXFocusedWindow"))~=window then return false end
        return not not element:setAttributeValue("AXFocused",true)
    end
    return F
end
