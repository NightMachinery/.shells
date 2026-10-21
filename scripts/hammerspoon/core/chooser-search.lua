-- Shared local chooser policy. Remote query providers keep their own callbacks.
ChooserSearch = {version = "1.1.0"}
local S = ChooserSearch
local states = setmetatable({}, {__mode="k"})

local function plain(value)
    if value == nil then return "" end
    -- hs.styledtext exposes its unformatted contents through getString().
    if type(value) == "userdata" and value.getString then return value:getString() end
    return tostring(value)
end

function S.filter(choices, query, fields)
    local terms, result = {}, {}
    for term in (query or ""):gmatch("%S+") do terms[#terms+1] = term:lower() end
    for _, choice in ipairs(choices or {}) do
        local parts = {}
        for _, field in ipairs(fields or {"text", "subText", "url", "searchText"}) do
            parts[#parts+1] = plain(choice[field]):lower()
        end
        local text, matches = table.concat(parts, "\n"), true
        for _, term in ipairs(terms) do
            if not text:find(term, 1, true) then matches=false; break end
        end
        if matches then result[#result+1] = choice end
    end
    return result
end

-- Use this constructor for local lists; update through setChoices(), which
-- keeps the complete source list separate from the displayed filtered rows.
function S.new(completion, fields)
    local chooser = hs.chooser.new(completion)
    states[chooser] = {choices={}, fields=fields}
    chooser:queryChangedCallback(function(query)
        local state = states[chooser]
        chooser:choices(S.filter(state.choices, query, state.fields))
    end)
    return chooser
end

function S.setChoices(chooser, choices)
    local state = assert(states[chooser], "ChooserSearch.new must create the chooser")
    state.choices = choices or {}
    chooser:choices(S.filter(state.choices, chooser:query(), state.fields))
    return chooser
end
