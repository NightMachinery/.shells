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
    if entry.unicode then return {["public.utf8-plain-text"]=entry.emoji} end
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
-- Unicode entries (from the Tab view) add their text and no tag.
function T.combine(entries)
    assert(#entries>0,"Choose at least one premium emoji")
    local texts,tags,offset={}, {},0
    for i,entry in ipairs(entries) do
        local text=entry.unicode and entry.emoji or T.payload(entry)[textType]
        local units=utf16Units(text)
        if not entry.unicode then
            local chars={}
            for c in ("custom-emoji://"..entry.id.."?"..i):gmatch(".") do chars[#chars+1]=string.pack(">I2",c:byte()) end
            local wide=table.concat(chars)
            tags[#tags+1]=string.pack(">i4i4I4",offset,units,#wide)..wide
        end
        texts[#texts+1]=text;offset=offset+units
    end
    local text=table.concat(texts)
    if #tags==0 then return {["public.utf8-plain-text"]=text} end
    return {[tagsType]=string.pack(">i4",#tags)..table.concat(tags),[textType]=text,["public.utf8-plain-text"]=text}
end

-- Hyper+A's Unicode emoji as entries of this chooser, one table per emoji.
local unicodeEntries={}
function T.unicodeEntry(emoji,name)
    local entry=unicodeEntries[emoji]
    if not entry then
        entry={emoji=emoji,name=name or emojiChooserName(emoji),unicode=true}
        unicodeEntries[emoji]=entry
    end
    return entry
end

function T.entryOf(choice)
    if not choice then return nil end
    return choice.entry or (choice.emoji and T.unicodeEntry(choice.emoji,choice.name)) or nil
end

local function entryKey(entry) return entry.unicode and ("u:"..entry.emoji) or entry.id end

function T.catalogEntry(raw, folder)
    assert(type(raw.id)=="string" and raw.id:match("^%d+$"),"Catalog IDs must be decimal strings")
    assert(type(raw.alt)=="string" and #raw.alt>0 and #raw.alt<=256 and utf8.len(raw.alt),
           "Catalog emoji needs fallback text")
    local entry={id=raw.id,emoji=raw.alt}
    entry.name=emojiChooserName(raw.alt)
    entry.setTitle=raw.setTitle or ""
    entry.setShortName=raw.setShortName or ""
    entry.recentRank=tonumber(raw.recentRank)
    entry.sourcePx=math.tointeger(tonumber(raw.sourcePx))
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
-- Only an image export asks Purple to render, through the control socket the
-- catalog itself names (see T.exportItems).
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
    local socket=catalog.controlSocket
    if not (type(socket)=="string" and #socket<1024 and socket:match("^/.+%.sock$")) then socket=nil end
    local entries,seen={},{}
    for index,raw in ipairs(catalog.entries) do
        local valid,entry=pcall(T.catalogEntry,raw,folder)
        if valid then
            if seen[entry.id] then T.mergeMetadata(seen[entry.id],entry)
            else entry.order=index;entries[#entries+1]=entry;seen[entry.id]=entry end
        end
    end
    return {content=content,state="ok",entries=entries,socket=socket}
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
function T.catalogPath()
    local override=os.getenv("NIGHT_TELEGRAM_EMOJI_CATALOG") or hs.settings.get("telegramEmoji.catalogPath")
    if override then return override end
    local home=os.getenv("HOME")
    local path=home.."/Library/Application Support/Purple/emoji-export/catalog.json"
    local legacy=home.."/Library/Caches/Purple/emoji-export/catalog.json"
    if hs.fs.attributes(path,"mode")~="file" and hs.fs.attributes(legacy,"mode")=="file" then return legacy end
    return path
end

function T.entries()
    local path=T.catalogPath()
    local status={state="missing",modified=hs.fs.attributes(path,"modification")}
    local content=readFile(path)
    if not (catalogCache and catalogCache.path==path and catalogCache.content==content) then
        catalogCache=content and buildCatalog(content,path:match("^(.*)/[^/]+$")) or
            {content=nil,state="missing",entries={}}
        catalogCache.path=path
        catalogCache.folder=path:match("^(.*)/[^/]+$")
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
    parts[#parts+1]="Tab: Unicode"
    parts[#parts+1]="Ctrl+Enter: layouts"
    return table.concat(parts," · ")
end

-- Items for bin/emoji-compose.py: Purple's largest render of each emoji
-- (512px for vector stickers, native size for rasters), asked for on demand,
-- else the 128px preview, else the Unicode image. Uses the catalog that
-- T.entries last read.
function T.exportItems(entries)
    assert(#entries>0 and #entries<=128,"Choose between 1 and 128 emoji")
    local catalog=catalogCache or {}
    local items={}
    for i,entry in ipairs(entries) do
        local fallback={}
        if entry.icon then fallback[#fallback+1]=entry.icon end
        fallback[#fallback+1]=emojiChooserImagePath(entry.emoji,512)
        fallback[#fallback+1]=emojiChooserImagePath(entry.emoji)
        items[i]={fallback=fallback}
        if entry.id and catalog.folder and catalog.state=="ok" then
            items[i].telegram={id=entry.id,export=catalog.folder,socket=catalog.socket}
        end
    end
    return items
end

-- Unicode entries first fetch their 512px Noto images (a failed download
-- falls back to the 128px previews). Returns a function that cancels.
function T.export(entries,format,layout,callback,seed)
    local unicode,cancel,cancelled={},nil,false
    for _,entry in ipairs(entries) do if entry.unicode then unicode[#unicode+1]=entry.emoji end end
    local function run()
        if cancelled then return end
        local ok,failure=pcall(function()
            cancel=EmojiLayout.compose(T.exportItems(entries),
                EmojiLayout.build(layout or "auto",#entries,seed),format,callback)
        end)
        if not ok then callback(tostring(failure)) end
    end
    if #unicode>0 then emojiChooserPrepareImages(unicode,run) else run() end
    return function()cancelled=true;if cancel then cancel() end end
end

-- Preview images for layout thumbnails.
function T.images(entries)
    local images={}
    for i,entry in ipairs(entries) do
        images[i]=(entry.icon and T.icon(entry.icon)) or emojiChooserImage(entry.emoji)
    end
    return images
end

-- Picking an emoji with Shift+Enter leaves an empty requests/<id> file when
-- no large render exists yet. Purple scans that folder in small batches, so
-- the render is usually ready by the time the export asks for it.
function T.prefetch(entry)
    local catalog=catalogCache or {}
    if not (catalog.folder and catalog.state=="ok" and entry.id and entry.id:match("^%d+$")) then return end
    local large=catalog.folder.."/large/"..entry.id.."-"
    for _,px in ipairs({entry.sourcePx or 512,512}) do
        if hs.fs.attributes(large..px..".png","mode")=="file" then return end
    end
    if hs.fs.attributes(catalog.folder.."/requests","mode")~="directory" then return end
    -- Append mode creates the file empty and never truncates or writes to it.
    local file=io.open(catalog.folder.."/requests/"..entry.id,"a")
    if file then file:close() end
end

local kAlertId="telegram-emoji-chooser"

function telegramEmojiChooser()
    local app=hs.application.frontmostApplication()
    local bundle=app and (app:bundleID() or ""):lower() or ""
    local paste=bundle:find("telegram",1,true) or bundle:find("tdesktop",1,true)
    local recent=ChooserSearch.recent("telegram-emoji")
    local unicodeRecent=ChooserSearch.recent("emoji")
    -- Shift+Enter collects several emoji for one rich paste, as in Hyper+A.
    -- Tab switches the list to Unicode emoji and back, so picks can mix both.
    local selected,source={},"telegram"
    -- pendingImages holds one pasteboard table per sticker, for Each/Pages.
    local pendingImages,pendingEntries,pendingLayout,closed,generation,cancelExport=nil,nil,nil,false,0,nil
    local menu -- The Ctrl+Enter layout menu, made once the chooser exists.
    -- Premium picks go to this chooser's recency, Unicode ones to Hyper+A's.
    local function recordPicks(entries)
        local ids,emojis={}, {}
        for _,entry in ipairs(entries) do
            if entry.unicode then emojis[#emojis+1]=entry.emoji else ids[#ids+1]=entry.id end
        end
        if #ids>0 then recent.record(ids) end
        if #emojis>0 then unicodeRecent.record(emojis) end
    end
    local function updateAlert()
        if #selected==0 then alert_gateway_dismiss(kAlertId);return end
        local text={}
        for i,entry in ipairs(selected) do text[i]=entry.emoji end
        -- The premium artwork itself: the text is only each emoji's Unicode
        -- stand-in. Any missing preview falls back to the text.
        local images=T.images(selected)
        local complete=true
        for i=1,#selected do if not images[i] then complete=false end end
        alert_gateway(table.concat(text),{id=kAlertId,seconds=math.huge,screens="typing",
            images=complete and images or nil})
    end
    local chooser=ChooserSearch.new(function(choice)
        -- hs.chooser may hide (and run cleanup) before delivering this choice.
        -- Only completion clears the tally, so cleanup cannot lose selections.
        local entries=pendingEntries or selected
        selected={}
        if not choice then return end
        local images=pendingImages
        pendingImages,pendingEntries=nil,nil
        if choice.layout and not images then
            -- A click on a layout row: the chooser is already gone, so export
            -- and paste once ready.
            local picked,seed=menu.items,choice.seed or EmojiLayout.jumbleSeed
            local ok,err=pcall(T.export,picked,"webp",choice.layout,function(failure,datas)
                if failure then alert_gateway(tostring(failure),{color="warn"});return end
                recordPicks(picked)
                EmojiLayout.record(picked,choice.layout,seed)
                if hs.pasteboard.writeAllData(datas[1]) then doPaste();EmojiLayout.pasteRest(datas) end
            end,seed)
            if not ok then alert_gateway(tostring(err),{color="warn"}) end
            return
        end
        if #entries==0 then entries={T.entryOf(choice)} end
        local layout=pendingLayout
        pendingLayout=nil
        local ok,err=pcall(function()
            local data=images and images[1] or (#entries==1 and T.payload(entries[1]) or T.combine(entries))
            local restore=paste and not images
            local previous=restore and hs.pasteboard.readAllData() or nil
            assert(hs.pasteboard.writeAllData(data),"Clipboard write failed")
            local written=hs.pasteboard.changeCount()
            recordPicks(entries)
            if images then EmojiLayout.record(entries,layout.key,layout.seed) end
            if paste or images then
                doPaste()
                if images then EmojiLayout.pasteRest(images) end
                if restore then
                    hs.timer.doAfter(0.5,function()
                        if hs.pasteboard.changeCount()==written then hs.pasteboard.writeAllData(previous) end
                    end)
                end
            else alert_gateway("Premium emoji copied. Paste it into Telegram.") end
        end)
        if not ok then alert_gateway(tostring(err),{color="warn"}) end
    end)
    local function refresh()
        if source=="unicode" then
            local rows=emojiChooserRows()
            chooser:placeholderText("Unicode emoji · "..#rows.." emoji · Tab: Telegram · Ctrl+Enter: layouts")
            ChooserSearch.setChoices(chooser,rows)
            return
        end
        local rows,status=T.rows(recent)
        chooser:placeholderText(T.placeholder(#rows,status))
        ChooserSearch.setChoices(chooser,rows,{stable=true})
    end
    local function switchSource()
        source=source=="telegram" and "unicode" or "telegram"
        refresh()
        chooser:selectedRow(1)
    end
    local function pick(entry)
        selected[#selected+1]=entry;updateAlert()
        pcall(T.prefetch,entry)
    end
    local function addCurrent()
        if menu.active then return end
        generation=generation+1
        local entry=T.entryOf(chooser:selectedRowContents())
        if entry then pick(entry) end
    end
    -- Shift+Up/Down: add the highlighted emoji, or remove it if picked, then move.
    local function toggleMove(delta)
        if menu.active then
            local target=chooser:selectedRow()+delta
            local row=target>=1 and chooser:selectedRowContents(target)
            if row and next(row) then chooser:selectedRow(target) end
            return
        end
        generation=generation+1
        EmojiLayout.toggleMove(chooser,delta,function(row)
            local entry=T.entryOf(row)
            if not entry then return end
            for i=#selected,1,-1 do
                if entryKey(selected[i])==entryKey(entry) then table.remove(selected,i);updateAlert();return end
            end
            pick(entry)
        end)
    end
    local function removeLast()
        generation=generation+1
        if #selected>0 then
            table.remove(selected);updateAlert()
            if #selected>0 then menu.refresh({table.unpack(selected)}) end
        end
    end
    -- Shift+Tab: the picks in a random order.
    local function shuffle()
        if #selected>1 then
            generation=generation+1
            EmojiLayout.shuffle(selected);updateAlert()
            menu.refresh({table.unpack(selected)})
        end
    end
    -- The picks, or else the highlighted emoji.
    local function currentEntries()
        if #selected>0 then return {table.unpack(selected)} end
        local entry=T.entryOf(chooser:selectedRowContents())
        return entry and {entry} or {}
    end
    -- Export first, then select, so the paste lands in Telegram. Demand
    -- renders can take a few seconds; Escape cancels them.
    local function exportImage(format,layout,entries)
        entries=entries or currentEntries()
        if #entries==0 then return end
        generation=generation+1
        local current=generation
        if cancelExport then cancelExport() end
        local seed=EmojiLayout.jumbleSeed
        local ok,err=pcall(function()
            cancelExport=T.export(entries,format,layout,function(failure,datas)
                if closed or current~=generation then return end
                cancelExport=nil
                if failure then alert_gateway(tostring(failure),{color="warn"});return end
                pendingImages,pendingEntries=datas,entries
                pendingLayout={key=layout or "auto",seed=seed}
                chooser:select()
            end,seed)
        end)
        if not ok then alert_gateway(tostring(err),{color="warn"}) end
    end
    menu=EmojiLayout.menu{
        chooser=chooser,
        images=T.images,
        export=function(layout,format,entries)exportImage(format,layout,entries)end,
        back=refresh,
    }
    local hotkeys={
        webp=hs.hotkey.bind("cmd","return",function()
            if menu.active then menu.pick("webp") else exportImage("webp") end
        end),
        png=hs.hotkey.bind({"cmd","shift"},"return",function()
            if menu.active then menu.pick("png") else exportImage("png") end
        end),
        layouts=hs.hotkey.bind("ctrl","return",function()
            if menu.active then menu.advance() else menu.open(currentEntries()) end
        end),
        tab=hs.hotkey.bind("","tab",function()if menu.active then menu.reroll() else switchSource() end end),
        save=hs.hotkey.bind("cmd","s",function()
            local ok,err=pcall(T.captureClipboard)
            if ok then refresh() else alert_gateway(tostring(err),{color="warn"}) end
        end),
        add=hs.hotkey.bind("shift","return",addCurrent),
        shiftTab=hs.hotkey.bind("shift","tab",shuffle),
        recolor=hs.hotkey.bind("","`",function()menu.recolor()end),
        backslash=hs.hotkey.bind("","\\",removeLast),
        shiftUp=hs.hotkey.bind("shift","up",function()toggleMove(-1)end,nil,function()toggleMove(-1)end),
        shiftDown=hs.hotkey.bind("shift","down",function()toggleMove(1)end,nil,function()toggleMove(1)end),
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
        closed=true;generation=generation+1
        deleteTap:stop()
        menu.cleanup()
        if cancelExport then cancelExport();cancelExport=nil end
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

--- * Recent sticker sets (Hyper+Ctrl+A)
-- Every multi-emoji image export from Hyper+A, Hyper+Shift+A or here, with the
-- layout it used (EmojiLayout.record). Stored items are resolved against the
-- current catalog, so premium emoji keep their icons and high-res renders.
function T.historyEntries(items)
    local byId={}
    local ok,entries=pcall(T.entries)
    for _,entry in ipairs(ok and entries or {}) do byId[entry.id]=entry end
    local result={}
    for i,item in ipairs(items) do
        if item.id then
            result[i]=byId[item.id] or {id=item.id,emoji=item.emoji,name=emojiChooserName(item.emoji)}
        else
            result[i]=T.unicodeEntry(item.emoji)
        end
    end
    return result
end

local function ago(time,now)
    local seconds=math.max(0,(now or os.time())-(time or 0))
    if seconds<3600 then return math.max(1,seconds//60).."m ago" end
    if seconds<86400 then return seconds//3600 .."h ago" end
    return seconds//86400 .."d ago"
end

-- changes, by history index, holds what Tab and Shift+Tab did to a row while
-- the list is open: {layout=, seed=, entries=}. Nothing is saved until the
-- set is sent, which records it as a new set.
function T.historyRows(now,changes)
    local rows={}
    for index,saved in ipairs(EmojiLayout.history()) do
        local change=changes and changes[index] or {}
        local set={items=saved.items,time=saved.time,layout=change.layout or saved.layout,seed=saved.seed}
        if change.layout then set.seed=change.seed end
        local entries=change.entries or T.historyEntries(saved.items)
        local text,names={}, {}
        for i,entry in ipairs(entries) do text[i]=entry.emoji;names[i]=entry.name or "" end
        local mode=EmojiLayout.mode(set.layout)
        local built,slots=EmojiLayout.build(set.layout,#entries,set.seed)
        local row={text=table.concat(text),entries=entries,set=set,history=index,
            subText=mode.name..(set.seed and (" #"..set.seed) or "").." · "..#entries.." emoji · "..(next(change) and "changed, not saved" or ago(set.time,now))..
                (slots>#entries and (" · repeated to "..slots) or ""),
            searchText=mode.name.." "..table.concat(names," ")}
        local ok,image=pcall(EmojiLayout.thumbnail,T.images(entries),set.layout,built)
        if ok then row.image=image end
        rows[#rows+1]=row
    end
    return rows
end

function emojiStickerHistoryChooser()
    local pendingImages,pendingSet,closed,generation,cancelExport=nil,nil,false,0,nil
    local menu
    local function paste(images,set)
        EmojiLayout.record(set.entries,set.layout,set.seed)
        if hs.pasteboard.writeAllData(images[1]) then
            doPaste()
            EmojiLayout.pasteRest(images)
        end
    end
    local chooser=ChooserSearch.new(function(choice)
        if not choice then return end
        local images,set=pendingImages,pendingSet
        pendingImages,pendingSet=nil,nil
        if images then paste(images,set);return end
        -- A click: the chooser is already gone, so export and paste once ready.
        local entries=choice.entries or menu.items
        local layout=choice.layout or choice.set.layout
        local seed=choice.layout and (choice.seed or EmojiLayout.jumbleSeed) or choice.set.seed
        local ok,err=pcall(T.export,entries,"webp",layout,function(failure,datas)
            if failure then alert_gateway(tostring(failure),{color="warn"});return end
            paste(datas,{entries=entries,layout=layout,seed=seed})
        end,seed)
        if not ok then alert_gateway(tostring(err),{color="warn"}) end
    end)
    local changes={}
    local function refresh()
        local rows=T.historyRows(nil,changes)
        chooser:placeholderText("Recent sticker sets · "..#rows.." · Enter: WebP · Cmd+Shift+Enter: PNG · "..
            "Tab: next font or seed · `: text colouring · Shift+Tab: shuffle · Ctrl+Enter: other layouts · Cmd+Backspace: forget")
        ChooserSearch.setChoices(chooser,rows)
    end
    -- Export first, then select, so the paste lands where Hyper+Ctrl+A started.
    local function exportSet(entries,layout,seed,format)
        generation=generation+1
        local current=generation
        if cancelExport then cancelExport() end
        local ok,err=pcall(function()
            cancelExport=T.export(entries,format,layout,function(failure,datas)
                if closed or current~=generation then return end
                cancelExport=nil
                if failure then alert_gateway(tostring(failure),{color="warn"});return end
                pendingImages,pendingSet=datas,{entries=entries,layout=layout,seed=seed}
                chooser:select()
            end,seed)
        end)
        if not ok then alert_gateway(tostring(err),{color="warn"}) end
    end
    local function exportRow(format)
        if menu.active then menu.pick(format);return end
        local row=chooser:selectedRowContents()
        if row and row.set then exportSet(row.entries,row.set.layout,row.set.seed,format) end
    end
    menu=EmojiLayout.menu{
        chooser=chooser,ownEnter=true,ownPane=true,
        images=T.images,
        export=function(layout,format,entries)exportSet(entries,layout,EmojiLayout.jumbleSeed,format)end,
        back=refresh,
    }
    -- The large preview covers the history rows as well as the menu's.
    local pane=EmojiLayout.previewPane(chooser,function(row)
        if menu.active then return menu.preview(row) end
        if not row.set then return nil end
        return row.set.layout,#row.entries,row.set.seed,function()return T.images(row.entries)end
    end)
    local function change(row,fields)
        local entry=changes[row.history] or {}
        for key,value in pairs(fields) do entry[key]=value end
        changes[row.history]=entry
        local selected=chooser:selectedRow()
        refresh()
        chooser:selectedRow(selected)
    end
    -- Tab on a history row: the same set in its next font or seed, until the
    -- list closes; sending it records a new set.
    local function vary()
        local row=chooser:selectedRowContents()
        if not (row and row.history) then return end
        local layout,seed=EmojiLayout.nextVariant(row.set.layout,row.set.seed)
        if layout~=row.set.layout or seed~=row.set.seed then change(row,{layout=layout,seed=seed}) end
    end
    -- Shift+Tab: its emoji in a random order, likewise.
    local function shuffle()
        if menu.active then
            if #menu.items>1 then menu.refresh(EmojiLayout.shuffle({table.unpack(menu.items)})) end
            return
        end
        local row=chooser:selectedRowContents()
        if row and row.history and #row.entries>1 then
            change(row,{entries=EmojiLayout.shuffle({table.unpack(row.entries)})})
        end
    end
    local hotkeys={
        enter=hs.hotkey.bind("","return",function()exportRow("webp")end),
        webp=hs.hotkey.bind("cmd","return",function()exportRow("webp")end),
        png=hs.hotkey.bind({"cmd","shift"},"return",function()exportRow("png")end),
        layouts=hs.hotkey.bind("ctrl","return",function()
            if menu.active then menu.advance();return end
            local row=chooser:selectedRowContents()
            if row and row.entries then menu.open(row.entries) end
        end),
        tab=hs.hotkey.bind("","tab",function()if menu.active then menu.reroll() else vary() end end),
        shiftTab=hs.hotkey.bind("shift","tab",shuffle),
        recolor=hs.hotkey.bind("","`",function()
            if menu.active then menu.recolor();return end
            local row=chooser:selectedRowContents()
            if not (row and row.history) then return end
            local layout=EmojiLayout.nextColoring(row.set.layout)
            if layout~=row.set.layout then change(row,{layout=layout,seed=row.set.seed}) end
        end),
        forget=hs.hotkey.bind("cmd","delete",function()
            local row=chooser:selectedRowContents()
            if menu.active or not (row and row.history) then return end
            EmojiLayout.forget(row.history)
            changes={}
            local selected=chooser:selectedRow()
            refresh()
            chooser:selectedRow(math.max(1,selected))
        end),
    }
    local function cleanup()
        closed=true;generation=generation+1
        menu.cleanup()
        pane.stop()
        if cancelExport then cancelExport();cancelExport=nil end
        for _,hotkey in pairs(hotkeys) do hotkey:delete() end
        inputLangPop()
    end
    ChooserSearch.onHide(chooser,cleanup)
    local ok,err=pcall(refresh)
    if not ok then cleanup();alert_gateway(tostring(err),{color="warn"});return end
    inputLangPush()
    langSetEn()
    chooser:show()
    pane.start()
end
hyper_bind_v2{mods={"ctrl"},key="a",pressedfn=emojiStickerHistoryChooser}
if telegramEmojiPrewarmDelay then T.prewarm() end
