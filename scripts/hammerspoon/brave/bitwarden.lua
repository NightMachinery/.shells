-- Native extension actions only. Never send the conflicting Cmd+B to Brave.
return function(config)
    local B = {}
    local extensionID = "nngceckbapebfimnlniiiahkandclblb"
    local buttonRoles = {AXButton=true, AXPopUpButton=true, AXMenuButton=true}

    local function labelMatches(element, predicate)
        if not buttonRoles[element:attributeValue("AXRole")] or
            element:attributeValue("AXEnabled") == false then return false end
        for _,key in ipairs({"AXTitle", "AXDescription"}) do
            local label = element:attributeValue(key)
            if type(label) == "string" and predicate(label:lower():match("^%s*(.-)%s*$")) then return true end
        end
        return false
    end

    local function bitwardenLabel(label)
        return label == "bitwarden" or label:match("^bitwarden[%s%(,:%-]") ~= nil
    end

    -- Do not enter page content, even if it contains an imitation extension
    -- button. Native toolbar controls and the native Extensions popup remain.
    local function find(root, predicate)
        if not root then return end
        local queue, index = {{element=root,depth=0}}, 1
        local started = hs.timer.absoluteTime()
        while index <= #queue and index <= 250 and hs.timer.absoluteTime()-started < 5e8 do
            local entry=queue[index]; index=index+1
            local e=entry.element
            local role=e:attributeValue("AXRole")
            if labelMatches(e,predicate) then return e end
            if entry.depth < 14 and role ~= "AXWebArea" and role ~= "AXRadioButton" and role ~= "AXTab" then
                for _,child in ipairs(e:attributeValue("AXChildren") or {}) do
                    queue[#queue+1]={element=child,depth=entry.depth+1}
                    if #queue >= 250 then break end
                end
            end
        end
    end

    local function alreadyOpen(root)
        if not root then return false end
        local queue,index={root},1
        local started=hs.timer.absoluteTime()
        while index <= #queue and index <= 250 and hs.timer.absoluteTime()-started < 5e8 do
            local e=queue[index]; index=index+1
            local role=e:attributeValue("AXRole")
            if role == "AXWebArea" then
                local url=e:attributeValue("AXURL")
                if type(url)=="table" then url=url.url end
                if type(url)=="string" and url:match("^chrome%-extension://"..extensionID.."/") then return true end
            elseif role ~= "AXRadioButton" and role ~= "AXTab" then
                for _,child in ipairs(e:attributeValue("AXChildren") or {}) do
                    queue[#queue+1]=child
                    if #queue >= 250 then break end
                end
            end
        end
        return false
    end

    local function press(button,source)
        -- No retry after AXPress: a timed-out action may still have happened.
        if button:performAction("AXPress") then
            B.lastResult=source
            config.clearError()
            return true
        end
        return config.unavailable("Bitwarden's native button did not respond")
    end

    function B.stop()
        if B.pending then B.pending:stop(); B.pending=nil end
    end

    function B.open()
        local app=config.braveInFront()
        if not app then return false end
        if B.pending then return true end
        local root=config.nativeWindow(app)
        if alreadyOpen(root) then B.lastResult="already open"; config.clearError(); return true end
        local button=find(root,bitwardenLabel)
        if button then return press(button,"toolbar") end
        local menu=find(root,function(label) return label=="extensions" end)
        if not menu then return config.unavailable("Bitwarden is not exposed in this window's native toolbar") end
        if not menu:performAction("AXPress") then return config.unavailable("Could not open the native Extensions menu") end
        local pid,attempts=app:pid(),0
        local deadline=hs.timer.absoluteTime()+1e9
        B.pending=hs.timer.doEvery(.1,function()
            local current=config.braveInFront()
            if not current or current:pid()~=pid then B.stop(); return end
            attempts=attempts+1
            local popup=config.nativeWindow(current)
            local target=find(popup,bitwardenLabel)
            if target then B.stop(); press(target,"extensions menu"); return end
            if attempts>=10 or hs.timer.absoluteTime()>=deadline then
                B.stop()
                -- Close only the native Extensions dialog we opened.
                local close=find(popup,function(label) return label=="close" end)
                if popup and popup:attributeValue("AXSubrole")=="AXUnknown" and close then close:performAction("AXPress") end
                config.unavailable("Bitwarden is unavailable in this window; check its profile and pin it to the toolbar")
            end
        end)
        return true
    end
    return B
end
