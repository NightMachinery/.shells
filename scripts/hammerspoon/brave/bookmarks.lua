return function(config)
    local B = {}
    B.path = (os.getenv("nightNotesPrivate") or (os.getenv("HOME") .. "/notes/private")) .. "/browser/bookmarks.org"

    function B.parse(text)
        local choices, seen, commentBlock = {}, {}, false
        for line in (text .. "\n"):gmatch("([^\n]*)\n") do
            if line:upper():match("^%s*#%+BEGIN_COMMENT%s*$") then commentBlock=true end
            if line:upper():match("^%s*#%+END_COMMENT%s*$") then commentBlock=false end
            if not commentBlock and not line:match("^%s*#") then
                for url, title in line:gmatch("%[%[([^%]]+)%]%[([^%]]+)%]%]") do
                    title = title:match("^%s*(.-)%s*$")
                    if url:match("^https?://%S+$") and title ~= "" and not seen[url .. "\0" .. title] then
                        seen[url .. "\0" .. title] = true
                        choices[#choices+1] = {text=title,subText=url,url=url}
                    end
                end
            end
        end
        return choices
    end

    B.chooser = ChooserSearch.new(function(choice)
        if choice and not B.queryActions.accept(choice) then config.engine.openBookmark(choice.url) end
    end)
    B.queryActions = dofile(nightdir .. "/hammerspoon/brave/query-actions.lua")({
        chooser=B.chooser, engine=config.engine,
    })
    B.chooser:searchSubText(true)
    B.chooser:placeholderText("Bookmarks · Shift+Enter: Google · Cmd+Enter: ChatGPT")

    function B.read()
        local file = io.open(B.path,"r")
        if not file then return nil, "Bookmark file is missing: " .. B.path end
        local content = file:read("*a"); file:close()
        local choices = B.parse(content)
        return config.searchAliases.apply(choices)
    end

    function B.show()
        local choices, err = B.read()
        if not choices then return config.unavailable(err) end
        B.chooser:query("")
        ChooserSearch.setChoices(B.chooser, choices):show()
        return true
    end

    function B.stop() B.queryActions.stop(); B.chooser:hide(); B.chooser:delete() end
    return B
end
