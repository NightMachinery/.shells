-- Inert IDs and data; never reads a real clipboard, session or personal library.
local env=setmetatable({hs={base64={encode=function(x)return x end,decode=function(x)return x end},fs={attributes=function()return 'file' end}},emojiChooserName=function()return 'heart eyes' end,hyper_bind_v2=function()end},{__index=_G})
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
local entries,status=T.entries()
assert(status.state=='ok' and #entries==3)
assert(entries[1].id=='33' and entries[2].id=='22' and entries[3].id=='11','Telegram recent order first, then catalog order')
assert(entries[1].rank==1 and entries[3].rank==3)
env.io.open=function()end
local none,missing=T.entries()
assert(#none==0 and missing.state=='missing')
print('TELEGRAM_EMOJI_PAYLOAD_TESTS_PASSED')
