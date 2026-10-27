-- Shared local chooser policy. Remote query providers keep their own callbacks.
if ChooserSearch then
    if ChooserSearch.historyTap then ChooserSearch.historyTap:stop() end
    if hs and hs.chooser and hs.chooser.globalCallback==ChooserSearch.historyGlobalCallback then
        hs.chooser.globalCallback=ChooserSearch.previousGlobalCallback
    end
end
ChooserSearch = {version = "1.7.2"}
local S = ChooserSearch
local states = setmetatable({}, {__mode="k"})
S.activeChoosers={}
local function stateFor(chooser)
    if states[chooser] then return states[chooser],chooser end
    -- Native callbacks may wrap the same chooser in a distinct Lua userdata.
    -- Table keys use raw identity, even though hs.chooser implements __eq.
    for original,state in pairs(states) do if original==chooser then return state,original end end
end

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
local histories={}
local function history(state)
    if state.historyId==false then return end
    local id=state.historyId
    if not histories[id] then
        local saved=hs.settings.get("chooser.queries." .. id .. ".v1")
        local rows={}
        for _,query in ipairs(type(saved)=="table" and saved or {}) do
            if type(query)=="string" and query:find("%S") and #query<=65536 then
                rows[#rows+1]=query
                if #rows>=100 then break end
            end
        end
        histories[id]=rows
    end
    return histories[id]
end
local function recordQuery(state,query)
    local rows=history(state)
    state.cursor=nil;state.expected=nil;state.draft=nil
    if not rows or type(query)~="string" or not query:find("%S") or #query>65536 or rows[1]==query then return end
    table.insert(rows,1,query)
    while #rows>100 do table.remove(rows) end
    hs.settings.set("chooser.queries." .. state.historyId .. ".v1",rows)
end
function S.recordQuery(chooser)
    local state,original=stateFor(chooser)
    if state then recordQuery(state,original:query()) end
end
-- hs.chooser strips its metatable on delete() and in its finalizer, and
-- indexing it then throws. The weak key outlives both: until the caller drops
-- its reference, or for one more GC cycle. Prune such keys on sight; clearing
-- the current key is safe inside pairs(). Test doubles are plain tables.
local function visible(chooser)
    if type(chooser)~="table" and not getmetatable(chooser) then states[chooser]=nil;return false end
    return chooser.isVisible and chooser:isVisible()
end
function S.historyKey(code,flags)
    if not flags.alt or flags.cmd or flags.ctrl or flags.shift or flags.fn or (code~=35 and code~=45) then return false end
    for chooser,state in pairs(states) do
        if visible(chooser) and state.historyId~=false then
            local rows=history(state)
            local query=chooser:query()
            if query~=state.expected or not state.cursor then state.cursor=0;state.draft=query end
            local cursor=math.max(0,math.min(#rows,state.cursor+(code==35 and 1 or -1)))
            state.cursor=cursor
            state.expected=cursor==0 and state.draft or rows[cursor]
            chooser:query(state.expected)
            -- query() updates the field but does not run Lua filtering callbacks.
            if state.changed then state.changed(state.expected) end
            if hs.axuielement and hs.processInfo then
                local app=hs.axuielement.applicationElement(hs.processInfo.processID)
                if app and app:setTimeout(.1) then
                    local field=app:attributeValue("AXFocusedUIElement")
                    if field and field:setTimeout(.1) and field:attributeValue("AXRole")=="AXTextField" and
                        field:attributeValue("AXValue")==state.expected then
                        local length=0
                        for _,codepoint in utf8.codes(state.expected) do length=length+(codepoint>0xFFFF and 2 or 1) end
                        field:setAttributeValue("AXSelectedTextRange",{location=length,length=0})
                    end
                end
            end
            return true
        end
    end
    return false
end
-- Alt+Up/Down page through visible rows. Physical arrow keys carry the fn
-- flag, so it is not a modifier here. An enabled Hammerspoon or system hotkey,
-- pageKeys=false or reservedPageKeys={up=true} keeps the key for its owner.
function S.pageKey(code,flags)
    if not flags.alt or flags.cmd or flags.ctrl or flags.shift or (code~=125 and code~=126) then return false end
    local direction=code==126 and "up" or "down"
    for chooser,state in pairs(states) do
        if visible(chooser) then
            if state.pageKeys==false or (state.reservedPageKeys and state.reservedPageKeys[direction]) then return false end
            local index="⌥" .. direction:upper()
            local getHotkeys=hs.hotkey and hs.hotkey.getHotkeys
            for _,key in ipairs(getHotkeys and getHotkeys() or {}) do
                if key.idx==index then return false end
            end
            local assigned=hs.hotkey and hs.hotkey.systemAssigned and hs.hotkey.systemAssigned({"alt"},direction)
            if assigned and assigned.enabled then return false end
            hs.eventtap.keyStroke({},direction=="up" and "pageup" or "pagedown",0)
            return true
        end
    end
    return false
end
local function ensureHistoryTap()
    if S.historyTap or not hs.eventtap then return end
    -- This tap sees every keystroke in every app, and a callback that throws
    -- drops the key, so an error here passes it through instead.
    S.historyTap=hs.eventtap.new({hs.eventtap.event.types.keyDown},function(event)
        local ok,consumed=pcall(function()
            return S.pageKey(event:getKeyCode(),event:getFlags()) or S.historyKey(event:getKeyCode(),event:getFlags())
        end)
        if not ok then print("ChooserSearch key tap: " .. tostring(consumed)) end
        return ok and consumed
    end):start()
end
local function create(completion,fields,options,remote)
    options=options or {}
    local caller=debug.getinfo(3,"Sl") or {}
    local historyId=options.historyId
    if historyId==nil then historyId=(caller.short_src or "chooser") .. ":" .. tostring(caller.currentline or 0) end
    local state={choices={},fields=fields,limit=options.limit,historyId=historyId,
        pageKeys=options.pageKeys,reservedPageKeys=options.reservedPageKeys}
    local reference=setmetatable({}, {__mode="v"})
    local chooser=hs.chooser.new(function(choice)
        local current=reference[1]
        local query=state.closedQuery or (current and current:query())
        if choice then recordQuery(state,query) end
        state.cursor=nil;state.expected=nil;state.draft=nil;state.closedQuery=nil
        completion(choice)
    end)
    reference[1]=chooser
    states[chooser]=state
    if not remote then
        S.onQueryChanged(chooser,function(query)
            local current=reference[1]
            if current then current:choices(S.filter(state.choices,query,state.fields,state.index,state.limit)) end
        end)
    end
    ensureHistoryTap()
    return chooser
end
-- hs.chooser runs a hide callback twice for one close (measured 8 ms apart,
-- both after the window was gone), so a cleanup that deletes hotkeys failed on
-- its second run. This runs the callback once per opening.
function S.onHide(chooser,callback)
    local state=assert(stateFor(chooser),"ChooserSearch must create the chooser")
    state.hidden=false
    chooser:hideCallback(function(...)
        if state.hidden then return end
        state.hidden=true
        return callback(...)
    end)
    return chooser
end
function S.onQueryChanged(chooser,callback)
    assert(stateFor(chooser),"ChooserSearch must create the chooser").changed=callback
    chooser:queryChangedCallback(callback)
    return chooser
end
function S.new(completion,fields,options)
    local chooser=create(completion,fields,options,false);return chooser
end
-- Remote providers retain their own filtering and get the same input history.
function S.remote(completion,options)
    local chooser=create(completion,nil,options,true);return chooser
end

-- The global lifecycle hook survives consumers replacing show/hide callbacks.
-- Chain Hammerspoon's focus-restoration hook and undo this wrapper on reload.
if hs and hs.chooser then
    S.previousGlobalCallback=hs.chooser.globalCallback
    S.historyGlobalCallback=function(chooser,event)
        local state,original=stateFor(chooser)
        if state then
            if event=="willOpen" then
                state.hidden=false
                S.activeChoosers[original]=true
                state.cursor=nil;state.expected=nil;state.draft=nil;state.closedQuery=nil
            elseif event=="didClose" then
                state.closedQuery=original:query()
                S.activeChoosers[original]=nil
            end
        end
        if S.previousGlobalCallback then S.previousGlobalCallback(chooser,event) end
    end
    hs.chooser.globalCallback=S.historyGlobalCallback
end

-- options.stable promises that each row's search fields never change once
-- built, so its search text is kept across openings (keyed weakly by row).
local stableIndex=setmetatable({}, {__mode="k"})
function S.setChoices(chooser, choices, options)
    local state = assert(stateFor(chooser), "ChooserSearch.new must create the chooser")
    state.choices = choices or {}
    state.index={}
    local stable=options and options.stable
    for i,choice in ipairs(state.choices) do
        local cached=stable and stableIndex[choice]
        if not (cached and cached.fields==state.fields) then
            cached={fields=state.fields,text=searchable(choice,state.fields)}
            if stable then stableIndex[choice]=cached end
        end
        state.index[i]=cached.text
    end
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
