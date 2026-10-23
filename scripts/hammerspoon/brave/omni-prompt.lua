return function(config)
    local O={version="1.1.0",generation=0,querySerial=0,baseChoices={},historyRows={}}
    local sources=config.sources or {tabs=true,bookmarks=true,history=true}
    local recentBookmarks=sources.bookmarks and ChooserSearch.recent("bookmarks")
    local function complete(choice)
        O.generation=O.generation+1;O.querySerial=O.querySerial+1;O.pendingEnter=nil
        if choice then ChooserSearch.recordQuery(O.chooser) end
        if not choice or O.queryActions.accept(choice) then return end
        if choice.kind=="tab" then config.engine.selectTab(choice.tabId)
        else config.engine.openBookmark(choice.url,{replace=O.queryActions.replace,tabId=O.queryActions.targetTabId},function(result)
            if result.ok and choice.kind=="bookmark" then recentBookmarks.record({choice.url}) end
        end) end
    end
    O.chooser=ChooserSearch.new(complete,nil,{limit=200,historyId="browser-" .. (config.id or config.name or "omni")})
    O.queryActions=dofile(nightdir .. "/hammerspoon/brave/query-actions.lua")({
        chooser=O.chooser,engine=config.engine,magic=config.magic~=false,enabled=config.queryActions~=false,
        showHelp=function()O.showHelp()end,
        modeChanged=function()O.updatePlaceholder()end,
        waitForResults=function()
            if not O.historyPending then return false end
            O.pendingEnter=O.chooser:query();return true
        end,
    })
    local hint=config.queryActions==false and "" or " · Shift+Enter: Google · Cmd+Enter: ChatGPT · Alt+Enter: Gemini · Ctrl+Enter: Claude · Ctrl+Shift+Enter: Google Web"
    function O.updatePlaceholder()
        local mode=O.queryActions.modeLabel()
        O.chooser:placeholderText(mode .. " · " .. (config.name or "Omni") .. (config.magic~=false and " · .help" or "") .. " · Tab: toggle" .. hint)
        ChooserSearch.inputColor(O.chooser,O.queryActions.replace and {red=1,green=.65,blue=.2,alpha=1} or nil)
    end
    O.updatePlaceholder()
    O.chooser:rows(12)
    O.helpChooser=ChooserSearch.new(function(choice)
        if not choice or O.stopped then return end
        O.pendingHelp=hs.timer.doAfter(0,function()
            O.pendingHelp=nil
            if not O.stopped then O.show(choice.prefix .. " ",true) end
        end)
    end)
    O.helpChooser:placeholderText("Magic shortcuts · filter by name · Enter: use prefix · Esc: close")
    O.helpChooser:rows(12)
    function O.showHelp()
        if O.stopped or not config.braveInFront() then return false end
        O.helpChooser:query("")
        ChooserSearch.setChoices(O.helpChooser,O.queryActions.helpChoices())
        ChooserSearch.fit(O.helpChooser):show()
        return true
    end
    local function placeCaretAfterPrefix(query)
        if not query or query=="" or not O.chooser:isVisible() or O.chooser:query()~=query then return end
        local app=hs.axuielement.applicationElement(hs.processInfo.processID)
        if not app or not app:setTimeout(.15) then return end
        local field=app:attributeValue("AXFocusedUIElement")
        if not field or not field:setTimeout(.15) then return end
        if field:attributeValue("AXRole")=="AXTextField" and field:attributeValue("AXValue")==query then
            field:setAttributeValue("AXSelectedTextRange",{location=#query,length=0})
        end
    end
    local function identity(row)
        return row and (row.kind or "") .. "\0" .. (row.tabId or row.url or row.text or "")
    end
    local function render(preserve)
        local previous=preserve and O.chooser:selectedRowContents()
        local choices={}
        O.sourceCounts={tabs=0,bookmarks=0,history=0}
        local kinds={tab="tabs",bookmark="bookmarks",history="history"}
        for _,row in ipairs(O.baseChoices) do choices[#choices+1]=row;O.sourceCounts[kinds[row.kind]]=O.sourceCounts[kinds[row.kind]]+1 end
        for _,row in ipairs(O.historyRows) do
            choices[#choices+1]={text=row.title~="" and row.title or row.url,
                subText="History · " .. row.url,kind="history",url=row.url}
            O.sourceCounts.history=O.sourceCounts.history+1
        end
        O.choiceCount=#choices
        ChooserSearch.setChoices(O.chooser,config.searchAliases.apply(choices,O.aliases))
        -- Appending remote matches must not reset a user's highlighted local row.
        if previous and O.chooser.selectedRow then
            for i,row in ipairs(ChooserSearch.filter(choices,O.chooser:query())) do
                if i>200 then break end
                if identity(row)==identity(previous) then O.chooser:selectedRow(i);break end
            end
        end
    end
    function O.searchHistory(query,immediate)
        O.querySerial=O.querySerial+1
        local serial,generation=O.querySerial,O.generation
        if O.debounce then O.debounce:stop();O.debounce=nil end
        O.pendingEnter=nil
        if not sources.history or not config.history or O.queryActions.magic(query) or (config.magic~=false and query==".help") then
            O.historyPending=false;O.historyRows={};render();return
        end
        O.historyPending=true
        render()
        local function search()
            O.debounce=nil
            local exclude={}
            for _,row in ipairs(O.baseChoices) do exclude[#exclude+1]=row.url end
            config.history.search(query,{aliases=O.aliases,exclude=exclude},function(result)
                if O.stopped or generation~=O.generation or serial~=O.querySerial or
                    not O.chooser:isVisible() or O.chooser:query()~=query then return end
                O.historyPending=false
                O.historyRows=result.ok and result.history or {}
                O.historyTotal=result.total or 0
                if not result.ok then config.unavailable(result.error) end
                render(true)
                if O.pendingEnter==query then
                    O.pendingEnter=nil
                    if not O.queryActions.key(36,{}) then
                        local choice=O.chooser:selectedRowContents()
                        O.chooser:cancel()
                        O.pendingSelection=hs.timer.doAfter(0,function()
                            O.pendingSelection=nil
                            if not O.stopped and config.braveInFront() then complete(choice) end
                        end)
                    end
                end
            end)
        end
        if immediate then search() else O.debounce=hs.timer.doAfter(.04,search) end
    end
    ChooserSearch.onQueryChanged(O.chooser,function(query)O.searchHistory(query)end)
    function O.show(query,continuing)
        if O.stopped or not config.braveInFront() then return false end
        if not continuing then O.queryActions.replace=false;O.queryActions.targetTabId=nil;O.updatePlaceholder() end
        O.generation=O.generation+1
        local generation=O.generation
        local bookmarks,bookmarkError
        if sources.bookmarks then
            bookmarks,bookmarkError=config.bookmarks.read()
            -- Copy before sorting: parsed bookmark rows belong to the shared cache.
            local sorted={}
            for i,row in ipairs(bookmarks or {}) do sorted[i]={text=row.text,url=row.url,order=i} end
            recentBookmarks.sort(sorted,function(row)return row.url end,function(a,b)return a.order<b.order end)
            bookmarks=sorted
        end
        local function ready(result)
            if O.stopped or generation~=O.generation or not config.braveInFront() then return end
            O.queryActions.targetTabId=result.activeTabId
            O.baseChoices={};O.historyRows={}
            O.aliases=config.searchAliases.read and config.searchAliases.read() or {}
            for _,tab in ipairs(result.ok and result.tabs or {}) do
                O.baseChoices[#O.baseChoices+1]={text=tab.title~="" and tab.title or tab.url,
                    subText="Tab " .. tab.index .. " · Window " .. tab.windowNumber .. " · " .. tab.url,
                    kind="tab",tabId=tab.tabId,url=tab.url}
            end
            for _,bookmark in ipairs(bookmarks or {}) do
                O.baseChoices[#O.baseChoices+1]={text=bookmark.text,subText="Bookmark · " .. bookmark.url,kind="bookmark",url=bookmark.url}
            end
            if not result.ok then config.unavailable(result.error) end
            if bookmarkError then config.unavailable(bookmarkError) end
            O.chooser:query(query or "")
            render();O.updatePlaceholder();ChooserSearch.fit(O.chooser):show();placeCaretAfterPrefix(query)
            O.searchHistory(query or "",true)
        end
        if sources.tabs then config.engine.listTabs(ready)
        elseif config.engine.activeState then config.engine.activeState(function(state)ready({ok=true,tabs={},activeTabId=state.ok and state.tabId or nil})end)
        else ready({ok=true,tabs={}}) end
        return true
    end
    function O.cancel()
        O.generation=O.generation+1;O.querySerial=O.querySerial+1;O.pendingEnter=nil;O.historyPending=false
        for _,key in ipairs({"pendingHelp","pendingSelection","debounce"}) do if O[key] then O[key]:stop();O[key]=nil end end
        if O.queryActions.pending then O.queryActions.pending:stop();O.queryActions.pending=nil end
        O.chooser:hide();O.helpChooser:hide()
    end
    function O.stop()
        O.cancel();O.queryActions.stop();O.stopped=true
        O.chooser:delete();O.helpChooser:delete()
    end
    return O
end
