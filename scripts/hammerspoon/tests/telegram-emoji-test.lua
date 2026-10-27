-- Inert IDs and data; never reads a real clipboard, session or personal library.
local env=setmetatable({telegramEmojiPrewarmDelay=false,hs={base64={encode=function(x)return x end,decode=function(x)return x end},fs={attributes=function()return 'file' end}},emojiChooserName=function()return 'heart eyes' end,hyper_bind_v2=function()end},{__index=_G})
assert(loadfile('hammerspoon/core/telegram-emoji.lua','t',env))()
local T=env.TelegramEmoji
local tag='custom-emoji://1234567890123456789?6'
local chars={};for c in tag:gmatch('.') do chars[#chars+1]=string.pack('>I2',c:byte()) end
local bytes=table.concat(chars)
local tags=string.pack('>i4i4i4I4',1,0,2,#bytes)..bytes
local key='com.trolltech.anymime.application--x-td-field-tags'
local data={[key]=tags,['public.utf8-plain-text']='😍',['unrelated-private-format']='do-not-copy'}
local parsed=T.parse(data)
assert(parsed.id=='1234567890123456789' and parsed.emoji=='😍')
assert(not parsed.clipboard['unrelated-private-format'])
local roundtrip=T.payload(parsed)
assert(roundtrip[key]==tags and roundtrip['public.utf8-plain-text']=='😍')
assert(not pcall(T.parse,{['public.utf8-plain-text']='😍'}))
assert(not pcall(T.parse,{[key]=tags,['public.utf8-plain-text']='😍 text'}))
assert(not pcall(T.parse,{[key]=tags:sub(1,-2),['public.utf8-plain-text']='😍'}))
assert(not pcall(T.payload,{id='other',clipboard=parsed.clipboard}))
local exported=T.catalogEntry({id='1234567890123456789',alt='🙂‍↔️',setTitle='Inert test set',recentRank=1},'/private/tmp/inert-export')
assert(exported.id=='1234567890123456789' and exported.recentRank==1)
assert(exported.icon=='/private/tmp/inert-export/png/1234567890123456789-128.png')
local unicodePayload=T.payload(exported)
assert(T.parse(unicodePayload).emoji=='🙂‍↔️','Rich range covers UTF-16 units of a ZWJ emoji')
assert(not pcall(T.catalogEntry,{id=12345,alt='😍'},'/private/tmp/inert-export'))
assert(T.packName(exported)=='Inert test set')
assert(T.packName({setTitle='',setShortName='ExamplePack'})=='ExamplePack')
assert(T.packName({setTitle='  ',setShortName='ExamplePack'})=='ExamplePack')
assert(T.packName({})=='Unknown pack')
local recent={id='1',setTitle='',setShortName='',recentRank=1}
T.mergeMetadata(recent,{setTitle='Installed pack',setShortName='InstalledPack',icon='/inert.png'})
assert(T.packName(recent)=='Installed pack' and recent.icon=='/inert.png' and recent.recentRank==1)
T.mergeMetadata(recent,{setTitle='Different pack'})
assert(T.packName(recent)=='Installed pack','A duplicate must not overwrite known pack metadata')
assert(T.placeholder(1,{state='missing'})=='Telegram premium emoji · 1 emoji · no Purple catalog yet · Cmd+S: save copied emoji')
assert(T.placeholder(3,{state='invalid'}):find('3 emoji · Purple catalog unreadable',1,true))
assert(T.placeholder(2,{state='ok',modified=0},86400*3):find('catalog 3d old',1,true))
assert(not T.placeholder(2,{state='ok',modified=100},200):find('old',1,true))
local mtime,loads=1,0
env.hs.fs.attributes=function()return mtime end
env.hs.image={imageFromPath=function(path)loads=loads+1;return path..loads end}
assert(T.icon('/inert/a.png')=='/inert/a.png1' and T.icon('/inert/a.png')=='/inert/a.png1' and loads==1,'Unchanged icons are cached')
mtime=2;assert(T.icon('/inert/a.png')=='/inert/a.png2','A changed icon is reloaded')
mtime=nil;assert(T.icon('/inert/a.png')==nil,'A deleted icon falls back to the Unicode preview')
-- Inert catalog through stubbed io: the real personal library is never opened.
local catalog={version=1,entries={{id='22',alt='😍',recentRank=2},{id='11',alt='🙂',setTitle='Inert set'},{id='33',alt='😀',recentRank=1}}}
env.os={getenv=function(k)return k=='NIGHT_TELEGRAM_EMOJI_CATALOG' and '/inert/catalog.json' or nil end,time=os.time}
env.io={open=function(path)
    if path=='/inert/catalog.json' then return {read=function()return 'catalog' end,close=function()end} end
end}
env.hs.json={decode=function(text)assert(text=='catalog');return catalog end}
env.hs.fs.attributes=function()return nil end
local decodes=0
env.hs.json={decode=function(text)assert(text=='catalog');decodes=decodes+1;return catalog end}
local entries,status=T.entries()
assert(status.state=='ok' and #entries==3)
T.entries();assert(decodes==1,'An unchanged catalog is decoded once')
assert(entries[1].id=='33' and entries[2].id=='22' and entries[3].id=='11','Telegram recent order first, then catalog order')
assert(entries[1].rank==1 and entries[3].rank==3)
catalog={version=1,entries={{id='44',alt='😀',installed=false},{id='55',alt='🙂'}}}
env.io.open=function(path)
    if path=='/inert/catalog.json' then return {read=function()return 'catalog2' end,close=function()end} end
end
env.hs.json={decode=function()return catalog end}
local installed=T.entries()
assert(installed[1].id=='55' and installed[2].id=='44','Packs the account has not installed come last')
env.io.open=function()end
local none,missing=T.entries()
assert(#none==0 and missing.state=='missing')
-- Several emoji in one rich paste, decoded as tdesktop's DeserializeTags reads it.
local first=T.catalogEntry({id='111',alt='😍'},'/inert')
local second=T.catalogEntry({id='222',alt='🙂‍↔️'},'/inert')
local combined=T.combine({first,second,first})
local raw=combined[key]
local count,pos=string.unpack('>i4',raw)
assert(count==3)
local expectText='😍🙂‍↔️😍'
assert(combined['public.utf8-plain-text']==expectText and combined['com.trolltech.anymime.application--x-td-field-text']==expectText)
local decoded,previousEnd={},0
for _=1,count do
    local offset,length,bytes;offset,length,bytes,pos=string.unpack('>i4i4I4',raw,pos)
    local id={}
    for i=pos,pos+bytes-1,2 do id[#id+1]=string.char((string.unpack('>I2',raw,i))) end
    pos=pos+bytes
    assert(offset==previousEnd and length>0,'Tags ascend without gaps or overlap')
    previousEnd=offset+length
    decoded[#decoded+1]=table.concat(id)
end
assert(pos==#raw+1 and previousEnd==2+5+2,'Tags cover the UTF-16 length of the whole text')
assert(decoded[1]:match('^custom%-emoji://111%?%d+$') and decoded[2]:match('^custom%-emoji://222%?') and decoded[3]:match('^custom%-emoji://111%?'))
assert(not pcall(T.combine,{}))
assert(not pcall(T.combine,{{id='999',clipboard=first.clipboard}}),'Each tag is checked against its saved payload')
-- Chooser keys with stubs: Shift+Enter collects, Shift+Backspace removes, Enter pastes all.
catalog={version=1,entries={{id='111',alt='😍',setTitle='Inert set'},{id='222',alt='🙂',setTitle='Inert set'}}}
env.io.open=function(path)
    if path=='/inert/catalog.json' then return {read=function()return 'catalog3' end,close=function()end} end
end
env.hs.json={decode=function()return catalog end}
local keys,tap,chooser,alertText,pasted,clip={},nil,nil,nil,0,nil
local recorded
env.ChooserSearch={
    recent=function()return {sort=function(rows,_,fallback)table.sort(rows,fallback)end,record=function(ids)recorded=ids end}end,
    new=function(complete)
        chooser={complete=complete,rows={},selected=1}
        function chooser:placeholderText(t)self.placeholder=t end
        function chooser:selectedRowContents()return self.rows[self.selected] end
        function chooser:hideCallback(fn)self.onHide=fn end
        function chooser:show()self.visible=true end
        return chooser
    end,
    setChoices=function(c,rows)c.rows=rows end,
    onHide=function(c,fn)c:hideCallback(fn) end,
}
env.emojiChooserAliases=function()return {} end
env.emojiChooserAliasesFor=function()return {} end
env.emojiChooserImage=function()return nil end
env.alert_gateway=function(text)alertText=text end
env.alert_gateway_dismiss=function()alertText=nil end
env.doPaste=function()pasted=pasted+1 end
env.hs.application={frontmostApplication=function()return {bundleID=function()return 'org.telegram.desktop' end}end}
env.hs.pasteboard={readAllData=function()return {previous=true} end,writeAllData=function(d)clip=d;return true end,changeCount=function()return 1 end}
env.hs.timer={doAfter=function()end}
env.hs.hotkey={bind=function(mods,key,fn)
    local hk={delete=function(self)self.deleted=true end}
    keys[(type(mods)=='table' and table.concat(mods,' ') or mods)..':'..key]={fn=fn,hk=hk};return hk
end}
env.hs.eventtap={event={types={keyDown=10}},new=function(_,fn)
    tap={callback=fn,start=function(self)self.running=true;return self end,stop=function(self)self.running=false end};return tap
end}
local language,pushed='Persian',nil
env.inputLangPush=function()pushed=language end
env.langSetEn=function()language='English' end
env.inputLangPop=function()if pushed then language=pushed;pushed=nil end end
env.telegramEmojiChooser()
assert(chooser.visible and chooser.placeholder:find('2 emoji',1,true))
assert(language=='English','The search is typed in English, as in Hyper+A')
keys['shift:return'].fn();chooser.selected=2;keys['shift:return'].fn();keys['shift:return'].fn()
assert(alertText=='😍🙂🙂')
local shiftDelete={getKeyCode=function()return 51 end,getFlags=function()return {shift=true} end}
assert(tap.callback(shiftDelete) and alertText=='😍🙂','Shift+Backspace removes the last pick')
assert(not tap.callback({getKeyCode=function()return 51 end,getFlags=function()return {} end}))
chooser.onHide() -- Native chooser can hide before delivering the selection.
chooser.complete(chooser.rows[1])
assert(clip['public.utf8-plain-text']=='😍🙂' and pasted==1,'Hide-before-completion must preserve the whole selection, not just the highlight')
assert(select(1,string.unpack('>i4',clip[key]))==2 and recorded[1]=='111' and recorded[2]=='222')
chooser.onHide()
assert(not tap.running and alertText==nil,'Closing releases the tap and the tally')
assert(language=='Persian','Closing restores the previous input source')
for _,binding in pairs(keys) do assert(binding.hk.deleted) end
keys={};env.telegramEmojiChooser();chooser.selected=2
chooser.complete(chooser.rows[2])
assert(clip['public.utf8-plain-text']=='🙂' and select(1,string.unpack('>i4',clip[key]))==1,'Without a selection Enter pastes the highlight alone')
-- Cached rows retain their search index while mutable usage order stays live.
local aliasTable,aliasBuilds={},0
local catalogBytes,libraryBytes="cache-a",nil
local catalogRows={version=1,entries={{id="901",alt="😍",setTitle="One"},{id="902",alt="🙂",setTitle="Two"}}}
local saved={version=1,entries={}}
env.io.open=function(path)
    local bytes=path=='/inert/catalog.json' and catalogBytes or libraryBytes
    if bytes then return {read=function()return bytes end,close=function()end} end
end
env.hs.json.decode=function(bytes)return bytes==libraryBytes and saved or catalogRows end
env.emojiChooserAliases=function()return aliasTable end
env.emojiChooserAliasesFor=function()aliasBuilds=aliasBuilds+1;return {"cached"} end
local a=T.rows(nil);local b=T.rows(nil)
assert(a~=b and a[1]==b[1] and aliasBuilds==2,'Warm openings reuse immutable row/search data')
assert(not a[1].entry.clipboard and T.parse(T.payload(a[1].entry)).id=="901",'Catalog rich payloads are built only on paste')
aliasTable={};local c=T.rows(nil)
assert(c[1]~=a[1] and aliasBuilds==4,'Alias replacement invalidates search rows')
libraryBytes='library-a';saved={version=1,entries={{id="901",aliases={"personal"}}}}
local d=T.rows(nil)
assert(d[1]~=c[1] and d[1].searchText:find('personal',1,true),'Saved-library edits invalidate merged entries and search rows')
catalogBytes='cache-b';catalogRows.entries[1].setTitle='Changed pack'
local e=T.rows(nil)
assert(e[1].subText=='Changed pack' and e[1]~=d[1],'Catalog edits invalidate built entries and metadata')
print('TELEGRAM_EMOJI_PAYLOAD_TESTS_PASSED')
