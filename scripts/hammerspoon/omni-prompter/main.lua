-- Clipboard transformations share the garden stdin/pipeline API and paste helper.
return function(config)
    local P={version="1.2.0",generation=0,runGeneration=0,allowed={},chain={}}
    local clipboard=config.clipboard or hs.pasteboard
    local notify=config.notify or function(message,success)
        alert_gateway("OmniPrompter: " .. message,{id="omni-prompter",color=success and "success" or "warn",seconds=3})
    end
    local discover=config.discover or brishz_eval_q_out_hs
    local execute=config.execute or brishz_eval_pipeline_out_hs
    local recent=config.recent or ChooserSearch.recent("omni-prompter")
    local defer=config.defer or function(fn)return hs.timer.doAfter(0,fn)end
    local paste=config.paste or doPaste
    local function bounded(element)
        if element and element:setTimeout(.1) then return element end
    end
    local function window(element)
        element=bounded(element)
        if not element then return end
        if element:attributeValue("AXRole")~="AXWindow" then element=bounded(element:attributeValue("AXWindow")) end
        if element and element:attributeValue("AXRole")=="AXWindow" then return element end
    end
    local capture=config.captureTarget or function()
        local app=hs.application.frontmostApplication()
        local ax=app and bounded(hs.axuielement.applicationElement(app))
        if not ax then return end
        local field=bounded(ax:attributeValue("AXFocusedUIElement"))
        local win=window(ax:attributeValue("AXFocusedWindow"))
        if not field or not win then return end
        return {pid=app:pid(),window=win,field=field,range=field:attributeValue("AXSelectedTextRange")}
    end
    local targetMatches=config.targetMatches or function(target)
        if not target then return false end
        local current=capture()
        if not current or current.pid~=target.pid or current.window~=target.window or current.field~=target.field then return false end
        local a,b=current.range,target.range
        return not b or (a and a.location==b.location and a.length==b.length) or false
    end
    local roles={fragment="Fragment · fixed text",prompt="Prompt · clipboard on stdin",
        metaprompt="Metaprompt · clipboard on stdin",snippet="Snippet · clipboard on stdin"}
    function P.parse(text)
        local rows,seen={},{}
        for name in (text .. "\n"):gmatch("([^\n]*)\n") do
            local family=name:match("^([a-z]+)%-")
            if roles[family] and name:match("^[%w_.:%-]+$") and not seen[name] then
                seen[name]=true;rows[#rows+1]={text=name,subText=roles[family],name=name}
            end
        end
        return rows,seen
    end
    local function stopWatcher()
        if P.interaction then P.interaction:stop();P.interaction=nil end
    end
    local function render()
        local rows={}
        for _,row in ipairs(P.catalog or {}) do
            local positions={}
            for i,name in ipairs(P.chain) do if name==row.name then positions[#positions+1]=tostring(i) end end
            rows[#rows+1]={name=row.name,text=row.text,subText=row.subText .. (#positions>0 and " · Chain: " .. table.concat(positions,", ") or "")}
        end
        recent.sort(rows,function(row)return row.name end,function(a,b)
            if #a.name~=#b.name then return #a.name<#b.name end
            if a.name:lower()~=b.name:lower() then return a.name:lower()<b.name:lower() end
            return a.name<b.name
        end)
        P.chooser:placeholderText(#P.chain>0 and ("Chain: " .. table.concat(P.chain," → ") .. " · Enter: run · Shift+Backspace: undo") or
            "OmniPrompter · Shift+Enter: chain · Cmd+R: refresh · Enter: run and paste")
        ChooserSearch.setChoices(P.chooser,rows)
    end
    function P.refresh()
        if P.stopped then return end
        P.generation=P.generation+1
        local generation=P.generation
        discover({"omni-prompter-functions"},function(out)
            if P.stopped or generation~=P.generation then return end
            if out==nil then notify("Could not list prompt functions",false);return end
            P.catalog,P.allowed=P.parse(out);P.functionCount=#P.catalog
            if P.chooser:isVisible() then render() end
            if #P.catalog==0 then notify("No prompt-family functions are loaded in BrishGarden",false) end
        end,"OmniPrompter functions",{quiet=true,nolog=true,timeout=10})
    end
    function P.add()
        local choice=P.chooser:selectedRowContents()
        if choice and P.allowed[choice.name] then
            ChooserSearch.recordQuery(P.chooser)
            P.chain[#P.chain+1]=choice.name;P.chooser:query("");render();return true
        end
        return false
    end
    function P.remove()
        if #P.chain>0 then table.remove(P.chain);render() end
    end
    function P.run(name)
        if P.stopped or P.busy or P.snapshot==nil then return false end
        local names={table.unpack(P.chain)}
        if #names==0 then
            local choice=P.chooser:selectedRowContents()
            names[1]=name or (choice and choice.name)
        end
        if #names==0 then return false end
        local stages={}
        for _,selected in ipairs(names) do
            if not P.allowed[selected] then notify("A selected function is no longer available; refresh the chain",false);return false end
            stages[#stages+1]={selected}
        end
        ChooserSearch.recordQuery(P.chooser)
        local input,target=P.snapshot,P.target
        P.snapshot=nil;P.target=nil;P.pendingOutput=nil;P.busy=true;P.chooser:hide()
        P.runGeneration=P.runGeneration+1
        local run=P.runGeneration
        P.lastResult={status="running",stages=#stages}
        P.pending=defer(function()
            P.pending=nil
            if P.stopped or run~=P.runGeneration then return end
            P.targetChanged=false
            if not config.execute then
                P.interaction=hs.eventtap.new({hs.eventtap.event.types.keyDown,hs.eventtap.event.types.leftMouseDown,
                    hs.eventtap.event.types.rightMouseDown},function()P.targetChanged=true;return false end):start()
            end
            execute(stages,function(out)
                if P.stopped or run~=P.runGeneration then return end
                stopWatcher();P.busy=false
                if out==nil then
                    P.lastResult={status="failed",stages=#stages};notify("Pipeline failed; clipboard was not replaced",false)
                elseif clipboard.setContents(out) then
                    recent.record(names)
                    local shouldPaste=not P.targetChanged and targetMatches(target)
                    P.lastResult={status=shouldPaste and "pasted" or "copied",stages=#stages,bytes=#out}
                    if shouldPaste then paste() else notify("Copied output; original input is no longer focused",true) end
                else
                    P.lastResult={status="copy-failed",stages=#stages};P.pendingOutput=out
                    notify("Could not copy output; it is retained in OmniPrompter.pendingOutput",false)
                end
            end,"OmniPrompter",{stdin=input,trim=false,nolog=true,quiet=true,timeout=120})
        end)
        return true
    end
    P.chooser=ChooserSearch.new(function(choice)
        if choice then P.run(choice.name) else P.snapshot=nil;P.target=nil end
    end,nil,{limit=200,historyId="omni-prompter"})
    P.chooser:rows(12)
    function P.key(code,flags)
        if not P.chooser:isVisible() or P.stopped then return false end
        if flags.alt or flags.ctrl or flags.fn then return false end
        if flags.cmd then
            if code==15 and not flags.shift then P.refresh();return true end
            return false
        end
        if flags.shift then
            if code==36 or code==76 then P.add();return true end
            if code==51 then P.remove();return true end
        elseif code==36 or code==76 then P.run();return true end
        return false
    end
    if not config.noEventtap then
        P.keys=hs.eventtap.new({hs.eventtap.event.types.keyDown},function(event)return P.key(event:getKeyCode(),event:getFlags())end)
        P.chooser:showCallback(function()P.keys:start()end)
        P.chooser:hideCallback(function()if P.keys then P.keys:stop()end end)
    end
    function P.show()
        if P.stopped then return false end
        if P.busy then notify("A prompt pipeline is still running",false);return false end
        -- Clipboard first; no shell or UI action can overwrite the input snapshot.
        P.snapshot=clipboard.getContents() or "";P.target=capture();P.chain={}
        P.chooser:query("");render();ChooserSearch.fit(P.chooser):show()
        if not P.catalog then P.refresh() end
        return true
    end
    function P.stop()
        P.stopped=true;P.snapshot=nil;P.target=nil;P.pendingOutput=nil
        P.generation=P.generation+1;P.runGeneration=P.runGeneration+1
        if P.pending then P.pending:stop();P.pending=nil end
        stopWatcher()
        if P.keys then P.keys:stop();P.keys=nil end
        P.chooser:hide();P.chooser:delete()
        if P.binding then P.binding:delete();P.binding=nil end
    end
    return P
end
