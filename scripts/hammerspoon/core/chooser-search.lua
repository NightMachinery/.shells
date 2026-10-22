-- Shared local chooser policy. Remote query providers keep their own callbacks.
ChooserSearch = {version = "1.3.1"}
local S = ChooserSearch
local states = setmetatable({}, {__mode="k"})

local function plain(value)
    if value == nil then return "" end
    -- hs.styledtext exposes its unformatted contents through getString().
    if type(value) == "userdata" and value.getString then return value:getString() end
    return tostring(value)
end

local function searchable(choice,fields)
    local parts={}
    for _,field in ipairs(fields or {"text","subText","url","searchText"}) do
        parts[#parts+1]=plain(choice[field]):lower()
    end
    return table.concat(parts,"\n")
end

function S.filter(choices, query, fields, index, limit)
    local terms, result = {}, {}
    for term in (query or ""):gmatch("%S+") do terms[#terms+1] = term:lower() end
    for i, choice in ipairs(choices or {}) do
        local text, matches = index and index[i] or searchable(choice,fields), true
        for _, term in ipairs(terms) do
            if not text:find(term, 1, true) then matches=false; break end
        end
        if matches then
            result[#result+1] = choice
            if limit and #result>=limit then break end
        end
    end
    return result
end

-- Use this constructor for local lists; update through setChoices(), which
-- keeps the complete source list separate from the displayed filtered rows.
function S.new(completion, fields, options)
    local chooser = hs.chooser.new(completion)
    states[chooser] = {choices={}, fields=fields,limit=options and options.limit}
    chooser:queryChangedCallback(function(query)
        local state = states[chooser]
        chooser:choices(S.filter(state.choices, query, state.fields,state.index,state.limit))
    end)
    return chooser
end

function S.setChoices(chooser, choices)
    local state = assert(states[chooser], "ChooserSearch.new must create the chooser")
    state.choices = choices or {}
    state.index={}
    for i,choice in ipairs(state.choices) do state.index[i]=searchable(choice,state.fields) end
    chooser:choices(S.filter(state.choices, chooser:query(), state.fields,state.index,state.limit))
    return chooser
end

-- Width is a percentage of the screen where hs.chooser opens. Recompute on show
-- because the typing screen may have changed since the chooser was created.
function S.fit(chooser, screen)
    screen=screen or hs.screen.mainScreen()
    local width=screen:frame().w
    return chooser:width(math.min(75,180000/width))
end

-- One preferences read per namespace per reload; filtering never touches disk.
local recentStores={}
function S.recent(namespace)
    if recentStores[namespace] then return recentStores[namespace] end
    local key="chooser.recent." .. namespace .. ".v1"
    local saved=hs.settings.get(key)
    local data=type(saved)=="table" and saved or {}
    data.sequence=tonumber(data.sequence) or 0
    data.items=type(data.items)=="table" and data.items or {}
    local R={}
    function R.record(names)
        for _,name in ipairs(names) do
            data.sequence=data.sequence+1
            local old=data.items[name]
            data.items[name]={last=data.sequence,count=type(old)=="table" and (tonumber(old.count) or 0)+1 or 1}
        end
        hs.settings.set(key,data)
    end
    function R.sort(rows, identity, fallback)
        table.sort(rows,function(a,b)
            local left,right=data.items[identity(a)],data.items[identity(b)]
            local la=type(left)=="table" and tonumber(left.last) or 0
            local lb=type(right)=="table" and tonumber(right.last) or 0
            la,lb=la or 0,lb or 0
            if la~=lb then return la>lb end
            return fallback(a,b)
        end)
        return rows
    end
    recentStores[namespace]=R
    return R
end

-- hs.chooser:fgColor also changes result titles and shortcut hints. This small
-- native extension touches only the input, including its active field editor.
local inputColorModule, inputColorError
function S.inputColor(chooser, color)
    if not inputColorModule and not inputColorError then
        local root=nightdir .. "/hammerspoon/native/chooser-input-color/"
        local function read(path)
            local file=assert(io.open(path,"rb"))
            local contents=file:read("*a");file:close();return contents
        end
        local ok,result=pcall(function()
            local bundle=hs.processInfo.bundlePath
            local key=hs.hash.SHA256(read(root .. "module.m") .. read(root .. "build.sh") ..
                bundle .. tostring(hs.processInfo.version) .. tostring(hs.processInfo.arch))
            local binary=os.getenv("HOME") .. "/Library/Caches/org.hammerspoon.Hammerspoon/chooser-input-color/" .. key .. ".so"
            if not hs.fs.attributes(binary) then
                local function quote(value)return "'" .. value:gsub("'", "'\\''") .. "'" end
                local output,success=hs.execute("/bin/sh " .. quote(root .. "build.sh") .. " " .. quote(bundle) .. " " .. quote(binary) .. " 2>&1",false)
                assert(success,"Cannot build chooser input color helper: " .. output)
            end
            return assert(package.loadlib(binary,"luaopen_chooser_input_color"))()
        end)
        if ok then inputColorModule=result else inputColorError=result;print(result) end
    end
    if inputColorModule then
        local ok,result=pcall(inputColorModule.color,chooser,color)
        if ok then return result end
        if not inputColorError then inputColorError=result;print(result) end
    end
    return chooser -- The textual replacement indicator still works if unavailable.
end
