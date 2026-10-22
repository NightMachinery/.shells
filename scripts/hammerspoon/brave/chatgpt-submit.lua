-- Submit only the exact query in the newly created, still-active ChatGPT tab.
return function(config)
    local C = {generation=0,version="1.1.0"}
    local function normalized(text)
        return type(text)=="string" and text:gsub("\r\n", "\n"):match("^%s*(.-)%s*$") or nil
    end

    function C.findComposer(root, query)
        local queue, cursor = {{element=root,web=false}}, 1
        local composer, sendReady
        local deadline = hs.timer.secondsSinceEpoch() + 0.6
        while queue[cursor] and cursor<=700 and hs.timer.secondsSinceEpoch()<deadline do
            local item=queue[cursor]; cursor=cursor+1
            local element=item.element
            local role=element:attributeValue("AXRole")
            local web=item.web or role=="AXWebArea"
            if web and (role=="AXTextArea" or role=="AXTextField")
                and normalized(element:attributeValue("AXValue"))==normalized(query) then
                composer=element
            end
            if web and role=="AXButton" and element:attributeValue("AXEnabled")~=false then
                for _,attribute in ipairs({"AXTitle","AXDescription"}) do
                    local label=element:attributeValue(attribute)
                    if label=="Send" or label=="Send prompt" or label=="Send message" then sendReady=true end
                end
            end
            if composer and sendReady then return composer end
            for _,child in ipairs(element:attributeValue("AXChildren") or {}) do
                queue[#queue+1]={element=child,web=web}
            end
        end
    end

    function C.stop()
        C.generation=C.generation+1
        if C.timer then C.timer:stop(); C.timer=nil end
    end

    function C.submit(opened, query, done)
        C.stop()
        if type(opened)~="table" or type(opened.tabId)~="string"
            or not normalized(query) or normalized(query)=="" then
            C.lastResult={status="cancelled",reason="Invalid new ChatGPT tab or empty query"}
            return false
        end
        local generation=C.generation
        local deadline=hs.timer.secondsSinceEpoch()+45
        local function current() return generation==C.generation end
        local function finish(status, reason)
            if not current() then return end
            C.stop()
            C.lastResult={status=status,reason=reason}
            if reason then config.unavailable(reason) end
            if done then done(C.lastResult) end
        end
        local function valid(state)
            local path=type(state.url)=="string" and state.url:match("^https://chatgpt%.com([^?#]*)")
            return state.ok and state.frontmost and state.tabId==opened.tabId and path=="/"
        end
        local check
        local function again()
            if hs.timer.secondsSinceEpoch()>=deadline then
                return finish("timeout", "ChatGPT did not become ready to submit the query")
            end
            C.timer=hs.timer.doAfter(0.35,check)
        end
        check=function()
            if not current() then return end
            C.timer=nil
            local app=config.braveInFront()
            if not app then return finish("cancelled", "ChatGPT submission cancelled because browser focus changed") end
            config.engine.activeState(function(state)
                if not current() then return end
                if not valid(state) then return finish("cancelled", "ChatGPT submission cancelled because the active tab changed") end
                local root=config.nativeWindow(app)
                local composer=root and C.findComposer(root, query)
                if not composer then return again() end
                -- Recheck native tab identity after traversing the page's AX tree.
                config.engine.activeState(function(latest)
                    if not current() then return end
                    if not valid(latest) or not config.braveInFront() then
                        return finish("cancelled", "ChatGPT submission cancelled because the active tab changed")
                    end
                    composer:setAttributeValue("AXFocused",true)
                    if composer:attributeValue("AXFocused")~=true
                        or normalized(composer:attributeValue("AXValue"))~=normalized(query) then return again() end
                    -- One send only. Never retry a key whose delivery may have succeeded.
                    hs.eventtap.keyStroke({}, "return", 0, app)
                    finish("submitted")
                end)
            end)
        end
        C.lastResult={status="waiting"}
        check()
    end
    return C
end
