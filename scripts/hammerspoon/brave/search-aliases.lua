-- Exact-host keywords live beside private bookmarks, not in the public config.
return function(config)
    local A = {}
    local cache = dofile(nightdir .. "/hammerspoon/brave/file-cache.lua")()
    A.path = (os.getenv("nightNotesPrivate") or (os.getenv("HOME") .. "/notes/private"))
        .. "/browser/search-aliases.json"

    function A.read()
        local aliases,err=cache.read(A.path,function(content)
            local ok,value=pcall(hs.json.decode,content)
            if not ok or type(value)~="table" then return nil,"Browser search aliases must be a JSON object" end
            return value
        end)
        if err and err~="File is unavailable" then config.unavailable(err) end
        return aliases or {}
    end

    function A.apply(choices, aliases)
        aliases = aliases or A.read()
        for _, choice in ipairs(choices) do
            local host = (choice.url or ""):match("^https?://([^/?#]+)")
            host = host and host:lower():gsub(":%d+$", "")
            local keywords = host and aliases[host]
            -- Only string keywords participate; malformed entries stay harmless.
            local words = {}
            for _, word in ipairs(type(keywords)=="table" and keywords or {}) do
                if type(word)=="string" then words[#words+1]=word end
            end
            choice.searchText = table.concat(words, " ")
        end
        return choices
    end
    return A
end
