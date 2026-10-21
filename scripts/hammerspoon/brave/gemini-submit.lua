-- Fill Gemini's consumer app through its accessible controls,
-- waiting for focus and value changes before advancing. No page JavaScript.
return function(config)
    local C={generation=0}
    local function normalized(value)
        return type(value)=="string" and value:gsub("\r\n","\n"):match("^%s*(.-)%s*$") or nil
    end

    function C.controls(root)
        local out={menus={}}
        local queue,cursor={{element=root,web=false}},1
        local deadline=hs.timer.secondsSinceEpoch()+0.6
        while queue[cursor] and cursor<=1100 and hs.timer.secondsSinceEpoch()<deadline do
            local item=queue[cursor];cursor=cursor+1
            local e=item.element
            local role=e:attributeValue("AXRole")
            local web=item.web or role=="AXWebArea"
            if web then
                local label=e:attributeValue("AXTitle") or ""
                if role=="AXTextArea" and label=="Enter a prompt for Gemini" then out.composer=e end
                if role=="AXPopUpButton" and label:match("^Open mode picker, currently ") then out.picker=e end
                if role=="AXMenuItem" and e:attributeValue("AXEnabled")~=false then
                    out.menus[#out.menus+1]=e
                    if label:match("^Selected 4 Argon ") then out.argonSelected=true end
                    if label:match("^4 Argon ") then out.argon=e end
                    if label=="Thinking level" then out.thinking=e end
                    if label:match("^Selected High ") then out.highSelected=true end
                    if label:match("^High ") then out.high=e end
                end
            end
            for _,child in ipairs(e:attributeValue("AXChildren") or {}) do
                queue[#queue+1]={element=child,web=web}
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
        local deadline=hs.timer.secondsSinceEpoch()+45
        local stage,activation="picker",nil
        local choseArgon,choseHigh=false,false
        local function current() return generation==C.generation end
        local function finish(status,reason)
            if not current() then return end
            C.stop();C.lastResult={status=status,reason=reason}
            if reason then config.unavailable(reason) end
        end
        local function valid(state)
            local path=type(state.url)=="string" and state.url:match("^https://gemini%.google%.com([^?#]*)")
            return state.ok and state.frontmost and state.tabId==opened.tabId
                and (path=="/app" or path=="/app/")
        end
        local function activate(e,nextStage)
            if not e then return end
            e:setAttributeValue("AXFocused",true)
            activation={element=e,nextStage=nextStage,deadline=hs.timer.secondsSinceEpoch()+3}
        end
        local check
        local function again()
            if hs.timer.secondsSinceEpoch()>=deadline then
                return finish("timeout","Gemini did not become ready with Argon High")
            end
            C.timer=hs.timer.doAfter(0.2,check)
        end
        local function advance(controls)
            if activation then
                local e=activation.element
                if (activation.nextStage=="model" and controls.thinking)
                    or (activation.nextStage=="high" and (controls.high or controls.highSelected)) then
                    -- Web menus can open as a consequence of focusing them.
                    -- Pressing Return again would toggle that menu closed.
                    stage=activation.nextStage;activation=nil
                elseif e:isValid() and e:attributeValue("AXFocused")==true then
                    -- AXShowMenu opens Chromium's context menu, not this web
                    -- popup. Focus, wait for confirmation, then press Return.
                    hs.eventtap.keyStroke({},"return",0)
                    stage=activation.nextStage;activation=nil
                elseif hs.timer.secondsSinceEpoch()>=activation.deadline then
                    return finish("cancelled","Gemini control did not take keyboard focus")
                end
            elseif stage=="picker" then
                if #controls.menus==0 and controls.picker and controls.composer then activate(controls.picker,"model") end
            elseif stage=="model" then
                if controls.argonSelected and controls.thinking then
                    activate(controls.thinking,"high")
                elseif controls.argon and not choseArgon then
                    choseArgon=true;activate(controls.argon,"picker")
                elseif #controls.menus>0 then
                    return finish("cancelled","Gemini Argon is unavailable in this account")
                end
            elseif stage=="high" then
                if controls.highSelected then stage="close"
                elseif controls.high and not choseHigh then
                    choseHigh=true;activate(controls.high,"picker")
                elseif controls.high then
                    return finish("cancelled","Gemini High selection was not confirmed")
                end
            elseif stage=="close" then
                if #controls.menus>0 then hs.eventtap.keyStroke({},"escape",0)
                elseif controls.composer then stage="fill" end
            elseif stage=="fill" and controls.composer then
                local value=normalized(controls.composer:attributeValue("AXValue"))
                if value~="" and value~="Ask Gemini" then
                    return finish("cancelled","Gemini composer already contains text")
                end
                controls.composer:setAttributeValue("AXValue",query)
                stage="verify"
            elseif stage=="verify" and controls.composer then
                if normalized(controls.composer:attributeValue("AXValue"))==normalized(query) then
                    controls.composer:setAttributeValue("AXFocused",true);stage="send"
                end
            elseif stage=="send" and controls.composer then
                if normalized(controls.composer:attributeValue("AXValue"))~=normalized(query) then
                    return finish("cancelled","Gemini composer text changed before submission")
                end
                if controls.composer:attributeValue("AXFocused")==true then
                    hs.eventtap.keyStroke({},"return",0)
                    return finish("submitted")
                end
            end
            C.lastResult={status="waiting",stage=stage}
            again()
        end
        check=function()
            if not current() then return end
            C.timer=nil
            local app=config.braveInFront()
            if not app then return finish("cancelled","Gemini submission cancelled because browser focus changed") end
            config.engine.activeState(function(state)
                if not current() then return end
                if not valid(state) then return finish("cancelled","Gemini submission cancelled because the active tab changed") end
                local root=config.nativeWindow(app)
                if not root then return again() end
                local controls=C.controls(root)
                -- Native identity is checked again after walking the AX tree.
                config.engine.activeState(function(latest)
                    if not current() then return end
                    if not valid(latest) or not config.braveInFront() then
                        return finish("cancelled","Gemini submission cancelled because the active tab changed")
                    end
                    advance(controls)
                end)
            end)
        end
        C.lastResult={status="waiting",stage=stage}
        check()
        return true
    end
    return C
end
