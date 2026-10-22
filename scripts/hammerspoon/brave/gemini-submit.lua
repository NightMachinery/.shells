-- Gemini consumer-app submission through native Accessibility. Batch independent
-- editor writes; await dependent menus and verify readiness before one send.
return function(config)
    local C={generation=0,version="2.0.0"}
    local function normalized(value)
        return type(value)=="string" and value:gsub("\r\n","\n"):match("^%s*(.-)%s*$") or nil
    end
    local function newChatURL(url)
        -- LuaSkin represents an AX NSURL as {url=..., __luaSkinType=...}.
        if type(url)=="table" then url=url.url end
        local path=type(url)=="string" and url:match("^https://gemini%.google%.com([^?#]*)")
        return path=="/app" or path=="/app/"
    end
    local function bounded(element)
        if element and element:setTimeout(.1) then return element end
    end
    local function emptyComposer(value,element)
        -- AXValue may be absent while Chromium is initializing its editor.
        -- That is not evidence of an existing draft.
        if type(value)~="string" then return nil end
        local text=normalized(value):gsub("\194\160"," "):gsub("\226\128\139",""):gsub("\239\187\191","")
        text=normalized(text)
        local placeholder=normalized(element:attributeValue("AXPlaceholderValue"))
        return text=="" or text=="Ask Gemini" or (placeholder~=nil and placeholder~="" and text==placeholder)
    end

    function C.controls(root)
        local out={menus={}}
        local queue,cursor={{element=root,web=false}},1
        local deadline=hs.timer.secondsSinceEpoch()+0.6
        while queue[cursor] and cursor<=1100 and hs.timer.secondsSinceEpoch()<deadline do
            local item=queue[cursor];cursor=cursor+1
            local e=bounded(item.element)
            if e then
                local role=e:attributeValue("AXRole")
                local web=item.web
                local descend=true
                if role=="AXWebArea" then
                    web=newChatURL(e:attributeValue("AXURL"))
                    if web then out.webArea=e else descend=false end
                end
                if web and (role=="AXTextArea" or role=="AXPopUpButton" or role=="AXMenuItem" or role=="AXButton") then
                    local label=e:attributeValue("AXTitle") or ""
                    if label=="" then label=e:attributeValue("AXDescription") or "" end
                    if role=="AXTextArea" and label=="Enter a prompt for Gemini" then out.composer=e end
                    if role=="AXPopUpButton" and label:match("^Open mode picker, currently ") then
                        out.picker=e
                        local current=label:match("^Open mode picker, currently (.+)$")
                        out.argonHigh=current=="Argon High" or current=="4 Argon High"
                    end
                    if role=="AXButton" and (label=="Send message" or label=="Send prompt" or label=="Submit") then out.send=e end
                    if role=="AXMenuItem" and e:attributeValue("AXEnabled")~=false then
                        out.menus[#out.menus+1]=e
                        if label:match("^Selected 4 Argon ") then out.argonSelected=true end
                        if label:match("^4 Argon ") then out.argon=e end
                        if label=="Thinking level" then out.thinking=e end
                        if label:match("^Selected High ") then out.highSelected=true end
                        if label:match("^High ") then out.high=e end
                    end
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
        local stage,activation="picker",nil
        local choseArgon,choseHigh=false,false
        local checks,usedFastPath=0,false
        local function current() return generation==C.generation end
        local function result(status,reason)
            return {status=status,reason=reason,stage=stage,checks=checks,
                elapsed=hs.timer.secondsSinceEpoch()-started,fastPath=usedFastPath}
        end
        local function finish(status,reason)
            if not current() then return end
            C.stop();C.lastResult=result(status,reason)
            if reason then config.unavailable(reason) end
        end
        local function valid(state)
            return state.ok and state.frontmost and state.tabId==opened.tabId and newChatURL(state.url)
        end
        local check
        local function again(delay)
            if hs.timer.secondsSinceEpoch()>=deadline then
                return finish("timeout","Gemini did not become ready with Argon High")
            end
            C.lastResult=result("waiting")
            C.timer=hs.timer.doAfter(delay or .1,check)
        end
        local function activate(e,nextStage)
            if not e then return again() end
            e:setAttributeValue("AXFocused",true)
            activation={element=e,nextStage=nextStage,deadline=hs.timer.secondsSinceEpoch()+3}
            return again(0)
        end
        local function advance(controls,app)
            if not controls.webArea then return again() end
            if activation then
                local e=activation.element
                if (activation.nextStage=="model" and controls.thinking)
                    or (activation.nextStage=="high" and (controls.high or controls.highSelected)) then
                    -- Focus can open a menu itself; Return would close it again.
                    stage=activation.nextStage;activation=nil
                elseif e:isValid() and e:attributeValue("AXFocused")==true then
                    hs.eventtap.keyStroke({},"return",0,app)
                    stage=activation.nextStage;activation=nil
                    return again(0)
                elseif hs.timer.secondsSinceEpoch()>=activation.deadline then
                    return finish("cancelled","Gemini control did not take keyboard focus")
                else return again() end
            end
            -- Advance pure state transitions in the same callback. UI actions
            -- that reveal new controls yield to the event loop before rescan.
            for _=1,4 do
                if stage=="picker" then
                    if controls.argonHigh and #controls.menus==0 then
                        usedFastPath=true;stage="fill"
                    elseif #controls.menus>0 then stage="model"
                    elseif controls.picker and controls.composer then return activate(controls.picker,"model")
                    else return again() end
                elseif stage=="model" then
                    if controls.argonSelected and controls.thinking then return activate(controls.thinking,"high")
                    elseif controls.argon and not choseArgon then
                        choseArgon=true;return activate(controls.argon,"picker")
                    elseif #controls.menus>0 then return finish("cancelled","Gemini Argon is unavailable in this account")
                    else return again() end
                elseif stage=="high" then
                    if controls.highSelected then stage="close"
                    elseif controls.high and not choseHigh then
                        choseHigh=true;return activate(controls.high,"picker")
                    elseif controls.high then return finish("cancelled","Gemini High selection was not confirmed")
                    else return again() end
                elseif stage=="close" then
                    if #controls.menus>0 then
                        hs.eventtap.keyStroke({},"escape",0,app);return again(0)
                    end
                    stage="fill"
                elseif stage=="fill" and controls.composer then
                    local empty=emptyComposer(controls.composer:attributeValue("AXValue"),controls.composer)
                    if empty==nil then return again() end
                    if not empty then return finish("cancelled","Gemini composer already contains text") end
                    controls.composer:setAttributeValue("AXValue",query)
                    controls.composer:setAttributeValue("AXFocused",true)
                    stage="verify"
                    return again(0)
                elseif stage=="verify" and controls.composer then
                    local value=controls.composer:attributeValue("AXValue")
                    if normalized(value)~=normalized(query) then
                        if emptyComposer(value,controls.composer)==false then
                            return finish("cancelled","Gemini composer text changed before submission")
                        end
                        return again()
                    end
                    if controls.composer:attributeValue("AXFocused")~=true then
                        controls.composer:setAttributeValue("AXFocused",true);return again(0)
                    end
                    if not controls.send or controls.send:attributeValue("AXEnabled")~=true then return again() end
                    -- Recheck exact value at the send boundary. Never retry Return.
                    if normalized(controls.composer:attributeValue("AXValue"))~=normalized(query) then
                        return finish("cancelled","Gemini composer text changed before submission")
                    end
                    hs.eventtap.keyStroke({},"return",0,app)
                    return finish("submitted")
                else return again() end
            end
            again(0)
        end
        check=function()
            if not current() then return end
            C.timer=nil
            local app=config.braveInFront()
            if not app then return finish("cancelled","Gemini submission cancelled because browser focus changed") end
            local root=config.nativeWindow(app)
            local controls=root and C.controls(root) or {menus={}}
            -- Scan is read-only. One native identity check immediately before
            -- acting replaces the old before-and-after AppleScript launches.
            config.engine.activeState(function(latest)
                if not current() then return end
                checks=checks+1
                if not valid(latest) or not config.braveInFront() then
                    return finish("cancelled","Gemini submission cancelled because the active tab changed")
                end
                if not controls.webArea or not newChatURL(controls.webArea:attributeValue("AXURL")) then return again() end
                advance(controls,app)
            end)
        end
        C.lastResult=result("waiting")
        check()
        return true
    end
    return C
end
