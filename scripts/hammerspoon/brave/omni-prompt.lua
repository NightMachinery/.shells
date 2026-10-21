return function(config)
    local O = {generation=0}
    O.chooser = ChooserSearch.new(function(choice)
        O.generation = O.generation + 1
        if not choice or O.queryActions.accept(choice) then return end
        if choice.kind == "tab" then
            config.engine.selectTab(choice.tabId)
        else
            config.engine.openBookmark(choice.url)
        end
    end)
    O.queryActions = dofile(nightdir .. "/hammerspoon/brave/query-actions.lua")({
        chooser=O.chooser, engine=config.engine, magic=true,
    })
    O.chooser:placeholderText("Omni Prompt · Shift+Enter: Google · Cmd+Enter: ChatGPT")
    O.chooser:width(65):rows(12)

    function O.show()
        if O.stopped or not config.braveInFront() then return false end
        O.generation = O.generation + 1
        local generation = O.generation
        local bookmarks, bookmarkError = config.bookmarks.read()
        config.engine.listTabs(function(result)
            -- Never reopen after cancellation, reload, or a change of app.
            if O.stopped or generation ~= O.generation or not config.braveInFront() then return end
            local choices = {}
            for _, tab in ipairs(result.ok and result.tabs or {}) do
                choices[#choices+1] = {
                    text=tab.title ~= "" and tab.title or tab.url,
                    subText="Tab · Window " .. tab.windowNumber .. " · " .. tab.url,
                    kind="tab",tabId=tab.tabId,url=tab.url,
                }
            end
            for _, bookmark in ipairs(bookmarks or {}) do
                choices[#choices+1] = {text=bookmark.text,subText="Bookmark · " .. bookmark.url,
                    kind="bookmark",url=bookmark.url}
            end
            if not result.ok then config.unavailable(result.error) end
            if bookmarkError then config.unavailable(bookmarkError) end
            O.choiceCount = #choices
            O.chooser:query("")
            ChooserSearch.setChoices(O.chooser, config.searchAliases.apply(choices)):show()
        end)
        return true
    end

    function O.stop()
        O.queryActions.stop()
        O.stopped = true
        O.generation = O.generation + 1
        O.chooser:hide(); O.chooser:delete()
    end
    return O
end
