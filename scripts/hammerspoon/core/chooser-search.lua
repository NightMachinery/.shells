-- Shared local chooser policy. Remote query providers keep their own callbacks.
ChooserSearch = {version = "1.2.0"}
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
