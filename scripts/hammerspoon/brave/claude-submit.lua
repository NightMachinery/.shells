-- Submit Claude's URL-prefilled query by activating its native Send control.
return function(config)
    local C={generation=0,version="1.0.0"}
    local function normalized(value)
        return type(value)=="string" and value:gsub("\r\n","\n"):match("^%s*(.-)%s*$") or nil
    end
    local function newChatURL(url)
        if type(url)=="table" then url=url.url end
        local path=type(url)=="string" and url:match("^https://claude%.ai([^?#]*)")
        return path=="/new" or path=="/new/"
    end
    local function bounded(element)
        if element and element:setTimeout(.1) then return element end
    end
    function C.controls(root,query)
        local out={}
        local queue,cursor={{element=root,web=false}},1
        local deadline=hs.timer.secondsSinceEpoch()+.6
        while queue[cursor] and cursor<=700 and hs.timer.secondsSinceEpoch()<deadline do
            local item=queue[cursor];cursor=cursor+1
            local e=bounded(item.element)
            if e then
                local role=e:attributeValue("AXRole")
                local web,descend=item.web,true
                if role=="AXWebArea" then
                    web=newChatURL(e:attributeValue("AXURL"))
                    if web then out.webArea=e else descend=false end
                end
                if web and role=="AXTextArea" and normalized(e:attributeValue("AXValue"))==normalized(query) then
                    out.composer=e
                elseif web and role=="AXButton" then
                    local title=e:attributeValue("AXTitle") or ""
                    local description=e:attributeValue("AXDescription") or ""
                    if title=="Send message" or description=="Send message" then out.send=e end
                end
                if descend then
                    for _,child in ipairs(e:attributeValue("AXChildren") or {}) do
                        queue[#queue+1]={element=child,web=web}
                    end
                end
            end
        end
        return out
    end
    function C.stop()
        C.generation=C.generation+1
        if C.timer then C.timer:stop();C.timer=nil end
    end
    function C.submit(opened,query)
        C.stop()
        if type(opened)~="table" or type(opened.tabId)~="string"
            or not normalized(query) or normalized(query)=="" then return false end
        local generation=C.generation
        local started=hs.timer.secondsSinceEpoch()
        local deadline=started+45
        local focusRequest
        local function current() return generation==C.generation end
        local function finish(status,reason)
            if not current() then return end
            C.stop();C.lastResult={status=status,reason=reason,elapsed=hs.timer.secondsSinceEpoch()-started}
            if reason then config.unavailable(reason) end
        end
        local check
        local function again(delay)
            if hs.timer.secondsSinceEpoch()>=deadline then
                return finish("timeout","Claude did not become ready to send the exact query")
            end
            C.timer=hs.timer.doAfter(delay or .15,check)
        end
        check=function()
            if not current() then return end
            C.timer=nil
            local app=config.braveInFront()
            if not app then return finish("cancelled","Claude submission cancelled because browser focus changed") end
            local root=config.nativeWindow(app)
            local controls=root and C.controls(root,query) or {}
            -- All AX work so far is read-only. Verify native identity before
            -- focusing or activating a control in the freshly created tab.
            config.engine.activeState(function(state)
                if not current() then return end
                if not state.ok or not state.frontmost or state.tabId~=opened.tabId
                    or not newChatURL(state.url) or not config.braveInFront() then
                    return finish("cancelled","Claude submission cancelled because the active tab changed")
                end
                local composer,send=controls.composer,controls.send
                if not controls.webArea or not newChatURL(controls.webArea:attributeValue("AXURL"))
                    or not composer or not send or send:attributeValue("AXEnabled")~=true then return again() end
                if normalized(composer:attributeValue("AXValue"))~=normalized(query) then return again() end
                for _,action in ipairs(send:actionNames() or {}) do
                    if action=="AXPress" then
                        local accepted=send:performAction("AXPress")
                        return finish(accepted and "submitted" or "failed",
                            not accepted and "Claude Send action was not confirmed; it was not retried" or nil)
                    end
                end
                -- Chromium may expose only AXShowMenu on this web button.
                -- Activate the confirmed button with Space, never a blind Return
                -- aimed at an unverified composer or whichever control is focused.
                if send:attributeValue("AXFocused")==true then
                    if normalized(composer:attributeValue("AXValue"))~=normalized(query) then return again() end
                    hs.eventtap.keyStroke({},"space",0,app)
                    return finish("submitted")
                end
                if focusRequest and focusRequest.element==send then
                    if hs.timer.secondsSinceEpoch()>=focusRequest.deadline then
                        return finish("cancelled","Claude Send button did not take keyboard focus")
                    end
                    return again()
                end
                send:setAttributeValue("AXFocused",true)
                focusRequest={element=send,deadline=hs.timer.secondsSinceEpoch()+3}
                again(0)
            end)
        end
        C.lastResult={status="waiting"}
        check()
        return true
    end
    return C
end
