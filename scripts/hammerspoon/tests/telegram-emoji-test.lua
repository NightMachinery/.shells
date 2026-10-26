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
print('TELEGRAM_EMOJI_PAYLOAD_TESTS_PASSED')
