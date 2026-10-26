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

function T.payload(entry)
    local data={}
    for _,key in ipairs({tagsType,textType,"public.utf8-plain-text"}) do
        data[key]=assert(hs.base64.decode(assert(entry.clipboard[key])))
    end
    assert(T.parse(data).id==entry.id,"Saved custom emoji ID does not match its rich payload")
    return data
end

function T.catalogEntry(raw, folder)
    assert(type(raw.id)=="string" and raw.id:match("^%d+$"),"Catalog IDs must be decimal strings")
    assert(type(raw.alt)=="string" and #raw.alt>0,"Catalog emoji needs fallback text")
    local tag="custom-emoji://"..raw.id.."?1"
    local chars={}
    for c in tag:gmatch(".") do chars[#chars+1]=string.pack(">I2",c:byte()) end
    local wide=table.concat(chars)
    local units=0
    for _,code in utf8.codes(raw.alt) do units=units+(code>0xffff and 2 or 1) end
    local packed=string.pack(">i4i4i4I4",1,0,units,#wide)..wide
    local entry=T.parse({[tagsType]=packed,[textType]=raw.alt})
    entry.name=emojiChooserName(raw.alt)
    entry.setTitle=raw.setTitle or ""
    entry.setShortName=raw.setShortName or ""
    entry.recentRank=tonumber(raw.recentRank)
    local png=raw.png or ("png/"..raw.id.."-128.png")
    if type(png)=="string" and png:match("^png/%d+%-%d+[%w%-]*%.png$") then
        local path=folder.."/"..png
        if hs.fs.attributes(path,"mode")=="file" then entry.icon=path end
    end
    return entry
end

-- Export is maintained by an opt-in Purple setting. Never discover sockets,
-- inspect encrypted account caches, or wake the client to open this chooser.
function T.entries()
    local path=os.getenv("NIGHT_TELEGRAM_EMOJI_CATALOG") or
        hs.settings.get("telegramEmoji.catalogPath") or
        (os.getenv("HOME").."/Library/Caches/Purple/emoji-export/catalog.json")
    local entries,seen={},{}
    local f=io.open(path,"r")
    if f then
        local content=f:read("*a");f:close()
        local ok,catalog=pcall(hs.json.decode,content)
        if ok and type(catalog)=="table" and catalog.version==1 and type(catalog.entries)=="table" then
            local folder=path:match("^(.*)/[^/]+$")
            for index,raw in ipairs(catalog.entries) do
                local valid,entry=pcall(T.catalogEntry,raw,folder)
                if valid and not seen[entry.id] then
                    entry.order=index;entries[#entries+1]=entry;seen[entry.id]=entry
                end
            end
        end
    end
    for _,entry in ipairs(T.load().entries) do
        if not seen[entry.id] then
            entry.order=#entries+1;entries[#entries+1]=entry;seen[entry.id]=entry
        else
            seen[entry.id].aliases=entry.aliases
        end
    end
    table.sort(entries,function(a,b)
        local ar,br=a.recentRank,b.recentRank
        if ar and ar<=0 then ar=nil end
        if br and br<=0 then br=nil end
        if ar~=br then
            if not ar then return false end
            if not br then return true end
            return ar<br
        end
        return a.order<b.order
    end)
    return entries
end

function telegramEmojiChooser()
    local app=hs.application.frontmostApplication()
    local bundle=app and (app:bundleID() or ""):lower() or ""
    local paste=bundle:find("telegram",1,true) or bundle:find("tdesktop",1,true)
    local recent=ChooserSearch.recent("telegram-emoji")
    local chooser=ChooserSearch.new(function(choice)
        if not choice then return end
        local ok,err=pcall(function()
            local data=T.payload(choice.entry)
            local previous=paste and hs.pasteboard.readAllData() or nil
            assert(hs.pasteboard.writeAllData(data),"Clipboard write failed")
            local written=hs.pasteboard.changeCount()
            recent.record({choice.entry.id})
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
        local rows={}
        local aliases=emojiChooserAliases()
        for _,entry in ipairs(T.entries()) do
            local image=entry.icon and hs.image.imageFromPath(entry.icon) or emojiChooserImage(entry.emoji)
            local subtitle=entry.setTitle or ""
            if subtitle=="" then subtitle=entry.icon and "Telegram premium" or "Unicode preview; premium paste preserved" end
            rows[#rows+1]={text=entry.name,subText=subtitle,
                           image=image,entry=entry,searchText=entry.emoji.." "..(entry.setTitle or "").." "..(entry.setShortName or "").." "..table.concat(entry.aliases or {}," ").." "..table.concat(emojiChooserAliasesFor(aliases,entry.emoji)," ")}
        end
        ChooserSearch.setChoices(chooser,rows)
    end
    local save=hs.hotkey.bind("cmd","s",function()
        local ok,err=pcall(T.captureClipboard)
        if ok then refresh() else alert_gateway(tostring(err),{color="warn"}) end
    end)
    chooser:placeholderText("Telegram premium emoji... [cached; Cmd+S: save copied emoji]")
    chooser:hideCallback(function()save:delete()end)
    local ok,err=pcall(refresh)
    if not ok then save:delete();alert_gateway(tostring(err),{color="warn"});return end
    chooser:show()
end
hyper_bind_v2{mods={"shift"},key="a",pressedfn=telegramEmojiChooser}
