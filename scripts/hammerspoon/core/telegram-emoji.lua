-- Personal, opt-in library of rich Telegram clipboard entries. No session/API
-- credentials or messages are read. Capture only one complete custom emoji.
TelegramEmoji = {}
local T = TelegramEmoji
local tagsType = "com.trolltech.anymime.application--x-td-field-tags"
local textType = "com.trolltech.anymime.application--x-td-field-text"
local directory = os.getenv("HOME") .. "/.local/share/night-emoji"
local libraryPath = directory .. "/telegram.json"

local function quote(s) return "'" .. s:gsub("'", "'\\''") .. "'" end

function T.parse(data)
    local text = data[textType] or data["public.utf8-plain-text"]
    local tags = data[tagsType]
    assert(type(text)=="string" and #text<=256 and type(tags)=="string" and #tags<=4096,
           "Copy a single premium emoji from Telegram first")
    assert(#tags>=16, "Incomplete Telegram emoji tags")
    local count, offset, length, bytes, pos = string.unpack(">i4i4i4I4", tags)
    assert(count==1 and offset==0 and bytes>0 and bytes%2==0 and pos+bytes-1==#tags,
           "Copy only one complete premium emoji, without surrounding text")
    local units=0
    for _,code in utf8.codes(text) do units=units+(code>0xffff and 2 or 1) end
    assert(length==units, "Telegram tag does not cover the whole copied emoji")
    local characters={}
    for i=pos,#tags,2 do
        local code=string.unpack(">I2",tags,i)
        assert(code<128,"Unexpected Telegram custom emoji tag")
        characters[#characters+1]=string.char(code)
    end
    local tag=table.concat(characters)
    local id=assert(tag:match("^custom%-emoji://(%d+)%??%d*$"),"No custom emoji ID in clipboard")
    return {id=id, emoji=text, clipboard={
        [tagsType]=hs.base64.encode(tags), [textType]=hs.base64.encode(text),
        ["public.utf8-plain-text"]=hs.base64.encode(text),
    }}
end

function T.load()
    local f=io.open(libraryPath,"r")
    if not f then return {version=1,entries={}} end
    local content=f:read("*a");f:close()
    local data=hs.json.decode(content)
    assert(type(data)=="table" and data.version==1 and type(data.entries)=="table",
           "Invalid personal Telegram emoji library; refusing to overwrite it")
    return data
end

function T.remember(data, label)
    local entry=T.parse(data)
    local library=T.load()
    for _,old in ipairs(library.entries) do
        if old.id==entry.id then
            old.clipboard=entry.clipboard
            if label then old.name=label end
            entry=old
            break
        end
    end
    local found=false
    for _,old in ipairs(library.entries) do if old==entry then found=true end end
    if not found then
        entry.name=label or (entry.emoji .. " premium emoji")
        entry.aliases={}
        library.entries[#library.entries+1]=entry
    end
    local _,ok=hs.execute("/bin/mkdir -p "..quote(directory).." && /bin/chmod 700 "..quote(directory))
    assert(ok,"Cannot protect personal emoji directory")
    local tmp=libraryPath..".tmp"
    local file=assert(io.open(tmp,"w"));file:write(hs.json.encode(library,true));file:close()
    local _,protected=hs.execute("/bin/chmod 600 "..quote(tmp))
    assert(protected,"Cannot protect personal emoji library")
    assert(os.rename(tmp,libraryPath))
    return entry.id
end

function T.captureClipboard(label)
    return T.remember(hs.pasteboard.readAllData(),label)
end

-- The rich clipboard of one custom emoji, as tdesktop writes it.
function T.richPayload(id, alt)
    local chars={}
    for c in ("custom-emoji://"..id.."?1"):gmatch(".") do chars[#chars+1]=string.pack(">I2",c:byte()) end
    local wide=table.concat(chars)
    local units=0
    for _,code in utf8.codes(alt) do units=units+(code>0xffff and 2 or 1) end
    return {[tagsType]=string.pack(">i4i4i4I4",1,0,units,#wide)..wide,[textType]=alt,["public.utf8-plain-text"]=alt}
end

-- Saved entries carry their captured clipboard; catalog entries build theirs
-- here, at paste time, so opening the chooser never packs thousands of them.
-- Either way the payload is parsed back and must name the entry's own ID.
function T.payload(entry)
    local data
    if entry.clipboard then
        data={}
        for _,key in ipairs({tagsType,textType,"public.utf8-plain-text"}) do
            data[key]=assert(hs.base64.decode(assert(entry.clipboard[key])))
        end
    else
        data=T.richPayload(entry.id,entry.emoji)
    end
    assert(T.parse(data).id==entry.id,"Saved custom emoji ID does not match its rich payload")
    return data
end

local function utf16Units(text)
    local units=0
    for _,code in utf8.codes(text) do units=units+(code>0xffff and 2 or 1) end
    return units
end

-- One rich paste of several premium emoji. tdesktop's DeserializeTags reads
-- count, then per tag offset, length (UTF-16 units) and a QString id; offsets
-- must ascend without overlap. Each tag is checked against its saved payload.
function T.combine(entries)
    assert(#entries>0,"Choose at least one premium emoji")
    local texts,tags,offset={}, {},0
    for i,entry in ipairs(entries) do
        local text=T.payload(entry)[textType]
        local chars={}
        for c in ("custom-emoji://"..entry.id.."?"..i):gmatch(".") do chars[#chars+1]=string.pack(">I2",c:byte()) end
        local wide=table.concat(chars)
        local units=utf16Units(text)
        tags[#tags+1]=string.pack(">i4i4I4",offset,units,#wide)..wide
        texts[#texts+1]=text;offset=offset+units
    end
    local text=table.concat(texts)
    return {[tagsType]=string.pack(">i4",#entries)..table.concat(tags),[textType]=text,["public.utf8-plain-text"]=text}
end

function T.catalogEntry(raw, folder)
    assert(type(raw.id)=="string" and raw.id:match("^%d+$"),"Catalog IDs must be decimal strings")
    assert(type(raw.alt)=="string" and #raw.alt>0 and #raw.alt<=256 and utf8.len(raw.alt),
           "Catalog emoji needs fallback text")
    local entry={id=raw.id,emoji=raw.alt}
    entry.name=emojiChooserName(raw.alt)
    entry.setTitle=raw.setTitle or ""
    entry.setShortName=raw.setShortName or ""
    entry.recentRank=tonumber(raw.recentRank)
    if raw.installed==false then entry.installed=false end
    local png=raw.png or ("png/"..raw.id.."-128.png")
    -- Existence is checked by T.icon on each opening, so a PNG written after
    -- the catalog still shows up.
    if type(png)=="string" and png:match("^png/%d+%-%d+[%w%-]*%.png$") then entry.icon=folder.."/"..png end
    return entry
end

local function nonempty(value)
    return type(value)=="string" and value:match("^%s*(.-)%s*$")~=""
end

function T.packName(entry)
    if nonempty(entry.setTitle) then return entry.setTitle end
    if nonempty(entry.setShortName) then return entry.setShortName end
    return "Unknown pack"
end

function T.mergeMetadata(entry, other)
    for _,key in ipairs({"setTitle", "setShortName", "icon"}) do
        if not nonempty(entry[key]) and nonempty(other[key]) then entry[key]=other[key] end
    end
end

-- Export is maintained by an opt-in Purple setting. Never discover sockets,
-- inspect encrypted account caches, or wake the client to open this chooser.
-- Built catalog entries are kept while the catalog bytes are unchanged, and
-- the merged list while the saved library bytes are unchanged too. Both files
-- are still read on every opening, so edits take effect at once.
local catalogCache,libraryCache,entriesCache

local function readFile(path)
    local f=io.open(path,"r")
    if not f then return nil end
    local content=f:read("*a");f:close()
    return content
end

local function buildCatalog(content,folder)
    local ok,catalog=pcall(hs.json.decode,content)
    if not (ok and type(catalog)=="table" and catalog.version==1 and type(catalog.entries)=="table") then
        return {content=content,state="invalid",entries={}}
    end
    local entries,seen={},{}
    for index,raw in ipairs(catalog.entries) do
        local valid,entry=pcall(T.catalogEntry,raw,folder)
        if valid then
            if seen[entry.id] then T.mergeMetadata(seen[entry.id],entry)
            else entry.order=index;entries[#entries+1]=entry;seen[entry.id]=entry end
        end
    end
    return {content=content,state="ok",entries=entries}
end

local function sortEntries(entries)
    table.sort(entries,function(a,b)
        local ar,br=a.recentRank,b.recentRank
        if ar and ar<=0 then ar=nil end
        if br and br<=0 then br=nil end
        if ar~=br then
            if not ar then return false end
            if not br then return true end
            return ar<br
        end
        -- Packs the account has not installed follow installed ones; entries
        -- without the optional flag keep catalog order.
        local ai,bi=a.installed~=false,b.installed~=false
        if ai~=bi then return ai end
        return a.order<b.order
    end)
    for rank,entry in ipairs(entries) do entry.rank=rank end
end

-- The second result describes the catalog for the placeholder: state is
-- "ok", "missing" or "invalid", and modified is its mtime when present.
-- Unchanged inputs return the same table, which callers may use as a cache key.
function T.entries()
    local path=os.getenv("NIGHT_TELEGRAM_EMOJI_CATALOG") or
        hs.settings.get("telegramEmoji.catalogPath") or
        (os.getenv("HOME").."/Library/Caches/Purple/emoji-export/catalog.json")
    local status={state="missing",modified=hs.fs.attributes(path,"modification")}
    local content=readFile(path)
    if not (catalogCache and catalogCache.path==path and catalogCache.content==content) then
        catalogCache=content and buildCatalog(content,path:match("^(.*)/[^/]+$")) or
            {content=nil,state="missing",entries={}}
        catalogCache.path=path
    end
    local catalog=catalogCache
    status.state=catalog.state
    local library=readFile(libraryPath) or ""
    if not (libraryCache and libraryCache.content==library) then
        libraryCache={content=library,entries=T.load().entries}
    end
    if entriesCache and entriesCache.catalog==catalog and entriesCache.library==libraryCache then
        return entriesCache.entries,status
    end
    -- Copies, so merging the library never changes the cached catalog entries.
    local entries,seen={},{}
    for _,base in ipairs(catalog.entries) do
        local entry={}
        for k,v in pairs(base) do entry[k]=v end
        entries[#entries+1]=entry;seen[entry.id]=entry
    end
    for _,entry in ipairs(libraryCache.entries) do
        if not seen[entry.id] then
            entry.order=#entries+1;entries[#entries+1]=entry;seen[entry.id]=entry
        else
            seen[entry.id].aliases=entry.aliases
            T.mergeMetadata(seen[entry.id],entry)
        end
    end
    sortEntries(entries)
    entriesCache={catalog=catalog,library=libraryCache,entries=entries}
    return entries,status
end

-- Pack PNGs are reread only when their mtime changes.
local icons={}
function T.icon(path)
    local modified=hs.fs.attributes(path,"modification")
    local cached=icons[path]
    if not cached or cached.modified~=modified then
        cached={modified=modified,image=modified and hs.image.imageFromPath(path) or nil}
        icons[path]=cached
    end
    return cached.image
end

-- Rows keep their search text while entries and aliases are unchanged. Images
-- are refreshed on every opening, and each opening sorts a fresh array by
-- current recency, so the cache never freezes usage order.
local rowCache
function T.rows(recent)
    local aliases=emojiChooserAliases()
    local entries,status=T.entries()
    if not (rowCache and rowCache.entries==entries and rowCache.aliases==aliases) then
        local built={}
        for _,entry in ipairs(entries) do
            built[#built+1]={text=entry.name,subText=T.packName(entry),entry=entry,
                searchText=entry.emoji.." "..(entry.setTitle or "").." "..(entry.setShortName or "").." "..
                    table.concat(entry.aliases or {}," ").." "..table.concat(emojiChooserAliasesFor(aliases,entry.emoji)," ")}
        end
        rowCache={entries=entries,aliases=aliases,rows=built}
    end
    local rows={}
    for i,row in ipairs(rowCache.rows) do
        row.image=row.entry.icon and T.icon(row.entry.icon) or emojiChooserImage(row.entry.emoji)
        rows[i]=row
    end
    if recent then
        -- Your own picks first, then Telegram's recent order, then the catalog.
        recent.sort(rows,function(row)return row.entry.id end,function(a,b)return a.entry.rank<b.entry.rank end)
    end
    return rows,status
end

-- Warm the caches after each load, as Hyper+A does: entries in one step, pack
-- images in batches on idle timers, then rows. Set telegramEmojiPrewarmDelay=false
-- to disable. The timer handle is global because a local one is collected.
telegramEmojiPrewarmDelay = telegramEmojiPrewarmDelay == nil and 8 or telegramEmojiPrewarmDelay
function T.prewarm(done)
    if telegramEmojiPrewarmTimer then telegramEmojiPrewarmTimer:stop() end
    local entries,index
    local function step()
        telegramEmojiPrewarmTimer=nil
        if not entries then
            entries=T.entries();index=0
        else
            for _=1,64 do
                index=index+1
                local entry=entries[index]
                if not entry then
                    T.rows(nil)
                    if done then done() end
                    return
                end
                if entry.icon then T.icon(entry.icon) end
            end
        end
        telegramEmojiPrewarmTimer=hs.timer.doAfter(0.05,step)
    end
    telegramEmojiPrewarmTimer=hs.timer.doAfter(telegramEmojiPrewarmDelay or 0,step)
end

function T.placeholder(count,status,now)
    local parts={"Telegram premium emoji",count==1 and "1 emoji" or (count.." emoji")}
    if status.state=="missing" then parts[#parts+1]="no Purple catalog yet"
    elseif status.state=="invalid" then parts[#parts+1]="Purple catalog unreadable"
    elseif status.modified then
        local days=math.floor(((now or os.time())-status.modified)/86400)
        if days>=1 then parts[#parts+1]="catalog "..days.."d old" end
    end
    parts[#parts+1]="Cmd+S: save copied emoji"
    return table.concat(parts," · ")
end

local kAlertId="telegram-emoji-chooser"

function telegramEmojiChooser()
    local app=hs.application.frontmostApplication()
    local bundle=app and (app:bundleID() or ""):lower() or ""
    local paste=bundle:find("telegram",1,true) or bundle:find("tdesktop",1,true)
    local recent=ChooserSearch.recent("telegram-emoji")
    -- Shift+Enter collects several emoji for one rich paste, as in Hyper+A.
    local selected={}
    local function updateAlert()
        if #selected==0 then alert_gateway_dismiss(kAlertId);return end
        local text={}
        for i,entry in ipairs(selected) do text[i]=entry.emoji end
        alert_gateway(table.concat(text),{id=kAlertId,seconds=math.huge,screens="typing"})
    end
    local chooser=ChooserSearch.new(function(choice)
        -- hs.chooser may hide (and run cleanup) before delivering this choice.
        -- Only completion clears the tally, so cleanup cannot lose selections.
        local entries=selected
        selected={}
        if not choice then return end
        if #entries==0 then entries={choice.entry} end
        local ok,err=pcall(function()
            local data=#entries==1 and T.payload(entries[1]) or T.combine(entries)
            local previous=paste and hs.pasteboard.readAllData() or nil
            assert(hs.pasteboard.writeAllData(data),"Clipboard write failed")
            local written=hs.pasteboard.changeCount()
            local ids={}
            for i,entry in ipairs(entries) do ids[i]=entry.id end
            recent.record(ids)
            if paste then
                doPaste()
                hs.timer.doAfter(0.5,function()
                    if hs.pasteboard.changeCount()==written then hs.pasteboard.writeAllData(previous) end
                end)
            else alert_gateway("Premium emoji copied. Paste it into Telegram.") end
        end)
        if not ok then alert_gateway(tostring(err),{color="warn"}) end
    end)
    local function refresh()
        local rows,status=T.rows(recent)
        chooser:placeholderText(T.placeholder(#rows,status))
        ChooserSearch.setChoices(chooser,rows,{stable=true})
    end
    local function addCurrent()
        local choice=chooser:selectedRowContents()
        if choice and choice.entry then selected[#selected+1]=choice.entry;updateAlert() end
    end
    local function removeLast()
        if #selected>0 then table.remove(selected);updateAlert() end
    end
    local hotkeys={
        save=hs.hotkey.bind("cmd","s",function()
            local ok,err=pcall(T.captureClipboard)
            if ok then refresh() else alert_gateway(tostring(err),{color="warn"}) end
        end),
        add=hs.hotkey.bind("shift","return",addCurrent),
        shiftTab=hs.hotkey.bind("shift","tab",removeLast),
        backslash=hs.hotkey.bind("","\\",removeLast),
    }
    -- Carbon hotkeys can latch Shift+Delete; read each key-down, as Hyper+A does.
    local deleteTap=hs.eventtap.new({hs.eventtap.event.types.keyDown},function(event)
        local flags=event:getFlags()
        if event:getKeyCode()==51 and flags.shift and not (flags.cmd or flags.alt or flags.ctrl) then
            removeLast();return true
        end
        return false
    end):start()
    local function cleanup()
        deleteTap:stop()
        for _,hotkey in pairs(hotkeys) do hotkey:delete() end
        alert_gateway_dismiss(kAlertId)
        inputLangPop()
    end
    ChooserSearch.onHide(chooser,cleanup)
    local ok,err=pcall(refresh)
    if not ok then cleanup();alert_gateway(tostring(err),{color="warn"});return end
    -- Type the search in English, as Hyper+A does; closing restores the source.
    inputLangPush()
    langSetEn()
    chooser:show()
end
hyper_bind_v2{mods={"shift"},key="a",pressedfn=telegramEmojiChooser}
if telegramEmojiPrewarmDelay then T.prewarm() end
