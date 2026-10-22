-- User overrides live outside the public repository. Reload HS after editing.
local V = {version="1.0.0"}
V.path=(os.getenv("nightNotesPrivate") or (os.getenv("HOME") .. "/notes/private")) .. "/browser/omni-variants.json"
V.defaults={
    {id="omni",name="Omni",mods={"cmd"},key=".",sources={tabs=true,bookmarks=true,history=true},magic=true,queryActions=true},
    {id="bookmarks",name="Bookmarks",mods={"cmd"},key=",",sources={tabs=false,bookmarks=true,history=false},magic=false,queryActions=true},
}
local modifiers={cmd=true,alt=true,ctrl=true,shift=true,fn=true}
local fields={id=true,name=true,mods=true,key=true,sources=true,magic=true,queryActions=true}
local sourceNames={tabs=true,bookmarks=true,history=true}
local function array(value)
    if type(value)~="table" then return false end
    for k in pairs(value) do if type(k)~="number" or k<1 or k%1~=0 or k>#value then return false end end
    return true
end
function V.validate(items, keycodes)
    if not array(items) or #items==0 then return nil,"Omni variants must be a nonempty array" end
    local ids,chords={},{}
    for _, item in ipairs(items) do
        if type(item)~="table" then return nil,"Each Omni variant must be an object" end
        for k in pairs(item) do if not fields[k] then return nil,"Unknown Omni variant setting" end end
        if type(item.id)~="string" or not item.id:match("^[a-z][a-z0-9_-]*$") or ids[item.id] then return nil,"Invalid or duplicate Omni variant id" end
        if type(item.name)~="string" or not item.name:find("%S") then return nil,"Each Omni variant needs a name" end
        if type(item.key)~="string" or #item.key~=1 or item.key:byte()<33 or item.key:byte()>126 then return nil,"Omni hotkeys require one printable ASCII key" end
        if keycodes and not keycodes[item.key:lower()] then return nil,"Omni key is not available on the current keyboard layout" end
        if not array(item.mods) or #item.mods==0 then return nil,"Omni hotkeys require modifiers" end
        local flags,sorted={},{}
        for _, mod in ipairs(item.mods) do
            if not modifiers[mod] or flags[mod] then return nil,"Invalid or duplicate Omni hotkey modifier" end
            flags[mod]=true; sorted[#sorted+1]=mod
        end
        table.sort(sorted)
        local chord=table.concat(sorted,"+") .. "+" .. item.key:lower()
        -- These native browser actions remain owned by brave-shortcuts.lua.
        if chords[chord] or ({["cmd+b"]=true,["cmd+n"]=true,["cmd+m"]=true,["cmd+/"]=true,["cmd+shift+/"]=true})[chord] then
            return nil,"Duplicate or reserved Omni hotkey"
        end
        if type(item.sources)~="table" then return nil,"Omni sources must be an object" end
        for k,v in pairs(item.sources) do if not sourceNames[k] or type(v)~="boolean" then return nil,"Invalid Omni source toggle" end end
        for _, key in ipairs({"magic","queryActions"}) do if type(item[key])~="boolean" then return nil,"Omni feature toggles must be booleans" end end
        if item.magic and not item.queryActions then return nil,"Omni magics require queryActions" end
        ids[item.id]=true; chords[chord]=true
    end
    return items
end
function V.load(unavailable)
    local file=io.open(V.path,"r")
    if not file then return V.defaults end
    local raw=file:read("*a"); file:close()
    local ok,items=pcall(hs.json.decode,raw)
    local valid,err
    if ok then valid,err=V.validate(items,hs.keycodes.map) end
    if valid then return valid end
    unavailable(err or "Invalid Omni variant JSON; using defaults")
    return V.defaults
end
return V
