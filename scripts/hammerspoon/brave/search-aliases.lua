-- Exact-host keywords live beside private bookmarks, not in the public config.
return function(config)
    local A = {}
    A.path = (os.getenv("nightNotesPrivate") or (os.getenv("HOME") .. "/notes/private"))
        .. "/browser/search-aliases.json"

    function A.read()
        local file = io.open(A.path, "r")
        if not file then return {} end
        local content = file:read("*a"); file:close()
        local ok, aliases = pcall(hs.json.decode, content)
        if not ok or type(aliases) ~= "table" then
            config.unavailable("Browser search aliases must be a JSON object")
            return {}
        end
        return aliases
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
