return function(config)
    local B = {}
    local cache = dofile(nightdir .. "/hammerspoon/brave/file-cache.lua")()
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

    function B.read()
        local choices,err=cache.read(B.path,B.parse)
        if not choices then return nil,"Bookmark file could not be read: " .. (err or "Unknown error") end
        return choices
    end

    return B
end
