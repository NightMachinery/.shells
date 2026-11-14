-- Run from scripts: lua hammerspoon/tests/emoji-chooser-test.lua
local source = assert(io.open('hammerspoon/core/choosers.lua')):read('*a')
local block = assert(source:match('(local emojiData = {}.-)\n%-%-%- %* Wi%-Fi Chooser'))
local keys, tap, chooser, tally, clipboard, canvas, pasted = {}, nil, nil, nil, nil, nil, 0
local aliasText, aliasDecodes = 'aliases', 0
local imageLoads, prewarmTimers = 0, {}
local settings = {}
local data = {
    ['😀']={name='a recent', unicode_version='6.1'},
    ['🫪']={name='b newer', unicode_version='17.0'},
    ['🫫']={name='c newest', unicode_version='18.0'},
}
local env = setmetatable({
    io={open=function(path) return {read=function() return path:find('aliases.json',1,true) and aliasText or '{}' end,close=function() end} end},
    json={decode=function(text)
        if text:match('^aliases') then aliasDecodes=aliasDecodes+1;return {['😀']=text=='aliases' and {'unimpressed','eye'} or {'fresh keyword'}} end
        return data
    end},
    alert_gateway=function(text) tally=text end,
    alert_gateway_dismiss=function() tally=nil end,
    inputLangPush=function() end, inputLangPop=function() end, langSetEn=function() end,
    hyper_bind_v2=function() end, copyToClipboard=function(text) clipboard=text end, doPaste=function() pasted=pasted+1 end,
    ChooserSearch={
        recent=function() return {sort=function(rows,_,fallback)table.sort(rows,fallback) end,record=function()end} end,
        new=function(complete)
            chooser={rows={},complete=complete}
            function chooser:placeholderText(text) self.placeholder=text end
            function chooser:selectedRow(n) if n then self.selected=n end;return self.selected end
            function chooser:selectedRowContents(n) return self.rows[n or self.selected or 1] or {} end
            function chooser:hideCallback(fn) self.onHide=fn end
            function chooser:show() self.visible=true end
            function chooser:select() self.complete(self:selectedRowContents());self.onHide() end
            return chooser
        end,
        setChoices=function(c,rows)c.rows=rows end,
        onHide=function(c,fn)c:hideCallback(fn) end,
    },
    hs={
        fs={attributes=function()return 'file' end},
        settings={get=function(k)return settings[k] end,set=function(k,v)settings[k]=v end},
        image={imageFromPath=function(path)imageLoads=imageLoads+1;return path end},
        timer={doAfter=function(delay,fn)prewarmTimers[#prewarmTimers+1]={delay=delay,fn=fn};return {stop=function()end} end},
        base64={decode=function(value)assert(value=='PNG');return 'png-bytes' end},
        pasteboard={writeAllData=function(value)clipboard=value;return true end},
        canvas={new=function(frame)
            canvas={frame=frame,images={}}
            function canvas:insertElement(element)self.images[#self.images+1]=element end
            function canvas:imageFromCanvas()return {encodeAsURLString=function()return 'data:image/png;base64,PNG' end}end
            function canvas:delete()self.deleted=true end
            return canvas
        end,matrix={translate=function()
            local m={};function m:rotate()return self end;function m:translate()return self end;return m
        end}},
        hotkey={bind=function(mod,key,fn)
            if type(mod)=='table' then mod=table.concat(mod,' ') end
            local hk={delete=function(self)self.deleted=true end}; keys[mod..':'..key]={fn=fn,hk=hk};return hk
        end},
        eventtap={event={types={keyDown=10}},new=function(_,fn)
            tap={callback=fn,start=function(self)self.running=true;return self end,stop=function(self)self.running=false end};return tap
        end},
    },
    emojiChooserPrewarmBatch=2,
}, {__index=_G})
assert(loadfile('hammerspoon/core/emoji-layout.lua','t',env))()
assert(load(block,'emoji-chooser','t',env))()
-- Layout math: pure functions, no Hammerspoon state.
local L=env.EmojiLayout
local function same(a,b)return table.concat(a,',')==table.concat(b,',') end
assert(same(L.autoCounts(1),{1}) and same(L.autoCounts(2),{2}) and same(L.autoCounts(3),{1,2}))
assert(same(L.autoCounts(5),{2,3}) and same(L.autoCounts(7),{1,3,3}) and same(L.autoCounts(9),{3,3,3}))
local tri=L.build('auto',3)[1]
assert(tri[1].x==0 and tri[2].x==-0.5 and tri[3].x==0.5,'Three is one centred over two')
assert(math.abs(tri[2].y-tri[1].y-math.sqrt(3)/2)<1e-9,'Rows half a cell apart interlock')
local quad=L.build('auto',4)[1]
assert(quad[3].y-quad[1].y==1,'Aligned rows keep a full cell')
local each=L.build('each',3)
assert(#each==3 and each[2][1].i==2 and #each[2]==1,'Each is one sticker per emoji')
local paged=L.build('pages',20)
assert(#paged==2 and #paged[1]==16 and #paged[2]==4 and paged[2][1].i==17)
local hearts,slots=L.build('heart',2)
assert(slots==6 and #hearts[1]==6 and hearts[1][3].i==1,'Small selections repeat around the heart')
assert(#L.build('heart',30)[1]==30,'A filled heart holds exactly the selection')
assert(#L.build('orbit',19)[1]==19 and L.build('orbit',19)[1][1].x==0,'Past 12, rings around a centre')
local j1,j2,j3=L.build('jumble',6,4)[1],L.build('jumble',6,4)[1],L.build('jumble',6,5)[1]
assert(j1[1].x==j2[1].x and j1[1].rot==j2[1].rot and j1[1].x~=j3[1].x,'Jumble repeats per seed')
local seen={};for _,c in ipairs(j1) do seen[c.i]=true end;assert(#seen==6,'Jumble places every emoji once')
local function keys_(n)local t={};for _,m in ipairs(L.available(n)) do t[#t+1]=m.key end;return table.concat(t,',') end
local shapes='auto,row,column,beehive,daisy,marquee,heart,orbit,sunflower,pyramid,nautilus,jumble'
assert(keys_(1)==shapes,'Every shape is offered for one emoji, by repeating it')
assert(keys_(3)=='auto,row,column,each,beehive,daisy,marquee,heart,orbit,sunflower,pyramid,nautilus,jumble')
assert(keys_(17):find('pages',1,true) and keys_(60):find('marquee',1,true) and not keys_(16):find('pages',1,true))
local daisy2,daisySlots=L.build('daisy',2)
assert(daisySlots==7 and daisy2[1][1].i==1,'The hero keeps its place')
for k=2,#daisy2[1] do assert(daisy2[1][k].i==2,'Petals cycle the rest, not the hero') end
local marquee3=L.build('marquee',3)[1]
assert(marquee3[1].i==1 and marquee3[2].i==2 and marquee3[3].i==3)
assert(#L.build('row',1)[1]==3 and L.build('row',1)[1][3].i==1,'Row repeats one emoji to three')
assert(L.describe('row',7,L.build('row',7),7):find('small in Telegram',1,true))
assert(L.describe('auto',3,L.build('auto',3),3):find('≈512×476',1,true))
assert(#prewarmTimers==1 and prewarmTimers[1].delay==5 and imageLoads==0 and aliasDecodes==0,'Loading only schedules the prewarm')
local prewarmed=false
env.emojiChooserPrewarm(function()prewarmed=true end)
local steps=0
while not prewarmed do steps=steps+1;table.remove(prewarmTimers).fn() end
assert(steps==3 and imageLoads==3 and aliasDecodes==1,'Prewarm loads images in chunks, then aliases and rows once')
local variantAliases={['🤦']={'smh','ffs'},['🤦‍♀️']={'facepalm woman','SMH'},
    ['🧑‍💻']={'coder','developer'},['👩‍💻']={'hacker'},['❤️']={'love'},['👍']={'yes'},['🧑']={'person'}}
local function words(emoji)return table.concat(env.emojiChooserAliasesFor(variantAliases,emoji),'|') end
assert(words('🤦‍♀️')=='facepalm woman|SMH|ffs','Keep own aliases first and deduplicate inherited terms')
assert(words('🤦‍♂️')=='smh|ffs')
assert(words('🤦🏽‍♀️')=='facepalm woman|SMH|ffs')
assert(words('👩‍💻')=='hacker|coder|developer')
assert(words('👨🏻‍💻')=='coder|developer')
assert(words('🧑🏿‍💻')=='coder|developer')
assert(words('❤')=='love' and words('❤️')=='love')
assert(words('👍🏿')=='yes')
assert(words('👩')=='' and words('🦀')=='','Do not invent unrelated person aliases')
env.emojiChooser()
assert(imageLoads==3 and aliasDecodes==1,'The first opening after prewarm reuses every cache')
assert(chooser.rows[1].emoji=='😀')
assert(chooser.rows[1].text=='a recent' and chooser.rows[1].image,'Every row uses the large icon layout')
assert(chooser.rows[1].searchText:find('unimpressed',1,true))
keys[':tab'].fn()
assert(chooser.rows[1].emoji=='🫫' and chooser.rows[2].emoji=='🫪')
assert(chooser.rows[1].image and chooser.rows[1].text=='c newest')
assert(tally==nil, 'Tab must sort, not add a selection')
keys[':tab'].fn()
assert(chooser.rows[1].emoji=='😀')
for _=1,3 do keys['shift:return'].fn() end
assert(tally=='😀😀😀')
local event={getKeyCode=function()return 51 end,getFlags=function()return {shift=true} end}
assert(tap.callback(event));assert(tap.callback(event))
assert(tally=='😀','Two Backspaces with Shift held must delete two selections')
assert(not tap.callback({getKeyCode=function()return 51 end,getFlags=function()return {} end}))
chooser.onHide()
assert(not tap.running,'Closing chooser must stop its key listener')
for _,binding in pairs(keys) do assert(binding.hk.deleted) end
env.emojiChooser()
keys[':tab'].fn()
chooser.complete(chooser.rows[1])
assert(clipboard=='🫫','Image preview must copy Unicode, not the display name')
chooser.onHide()
env.emojiChooser()
keys['shift:return'].fn()
keys['shift:return'].fn()
local beforePaste=pasted
local composed
L.compose=function(items,built,format,callback)
    composed={items=items,built=built,format=format}
    callback(nil,{{['public.png']='png-bytes'}})
end
keys['cmd shift:return'].fn()
assert(clipboard['public.png']=='png-bytes' and pasted==beforePaste+1)
assert(composed.format=='png' and #composed.items==2 and composed.items[1].path:find('noto%-18%.0%-512/emoji_u1f600%.png$'))
assert(#composed.built==1 and #composed.built[1]==2 and composed.built[1][1].y==composed.built[1][2].y,'Two emoji sit side by side')
assert(not tap.running,'PNG export releases chooser listeners')
local webpCallback,webpLayout
env.emojiChooserWebp=function(emojis,callback,layout)
    assert(#emojis==1 and emojis[1]=='😀')
    webpCallback,webpLayout=callback,layout
end
local documentRequests=0
local documentMode=env.emojiChooserTelegramDocument
env.emojiChooserTelegramDocument=function()documentRequests=documentRequests+1 end
env.emojiChooser()
beforePaste=pasted
keys['cmd:return'].fn()
assert(pasted==beforePaste,'WebP must wait for encoding')
assert(webpLayout==nil,'Cmd+Enter uses the default Auto layout')
webpCallback(nil,{{['public.file-url']='file:///emoji.webp',['org.webmproject.webp']='webp-bytes'}})
assert(clipboard['public.file-url']=='file:///emoji.webp' and pasted==beforePaste+1)
assert(documentRequests==1,'WebP should request Telegram document mode')
env.emojiChooser()
keys['cmd:return'].fn()
beforePaste=pasted
chooser.onHide()
webpCallback(nil,{{['public.file-url']='file:///cancelled.webp'}})
assert(pasted==beforePaste,'Cancelled chooser must not paste a late WebP result')
-- Shift+Down toggles the highlighted emoji and moves; Shift+Up comes back.
env.emojiChooser()
chooser.selected=1
keys['shift:down'].fn()
assert(tally=='😀' and chooser.selected==2,'Shift+Down picks and moves')
keys['shift:down'].fn()
assert(tally=='😀'..chooser.rows[2].emoji and chooser.selected==3)
keys['shift:up'].fn();keys['shift:up'].fn()
assert(tally=='😀'..chooser.rows[3].emoji and chooser.selected==1,'Each press toggles the row it leaves: row 3 on, row 2 off')
chooser.selected=#chooser.rows;local last=chooser.rows[#chooser.rows].emoji
keys['shift:down'].fn()
assert(chooser.selected==#chooser.rows and tally=='😀','The picked last row toggles off without moving off the list')
chooser.onHide()
-- Ctrl+Enter: layout rows with previews for the picks; Enter exports the
-- highlighted layout; Ctrl+Enter again restores the emoji list and query.
local exported
env.emojiChooserWebp=function(emojis,callback,layout)exported={emojis=emojis,layout=layout,callback=callback} end
env.emojiChooser()
chooser.query=function(self,q)if q then self.q=q end;return self.q or '' end
chooser.selected=1
for _=1,3 do keys['shift:return'].fn() end
chooser:query('smile')
keys['ctrl:return'].fn()
assert(chooser.rows[1].layout=='auto' and chooser.rows[1].image,'Layouts are listed with live previews')
assert(chooser.rows[1].subText:find('512',1,true) and chooser:query()=='')
assert(chooser.placeholder:find('Layout for 3 emoji',1,true))
local rowCount=#chooser.rows
keys['shift:return'].fn()
assert(tally=='😀😀😀','Shift+Enter does not add a layout row as an emoji')
chooser.selected=rowCount
assert(chooser.rows[rowCount].layout=='jumble' and chooser.rows[rowCount].text=='Jumble #1')
keys[':tab'].fn()
assert(chooser.rows[rowCount].text=='Jumble #2' and chooser.selected==rowCount,'Tab re-rolls Jumble in place')
chooser.selected=4
assert(chooser.rows[4].layout=='each')
keys[':return'].fn()
assert(exported.layout=='each' and #exported.emojis==3)
beforePaste=pasted
local sequence={}
env.hs.timer={doAfter=function(_,fn)sequence[#sequence+1]=fn;return {stop=function()end} end}
exported.callback(nil,{{['org.webmproject.webp']='a'},{['org.webmproject.webp']='b'},{['org.webmproject.webp']='c'}})
assert(clipboard['org.webmproject.webp']=='a' and pasted==beforePaste+1,'The first sticker pastes at once')
table.remove(sequence,1)()
assert(clipboard['org.webmproject.webp']=='b' and pasted==beforePaste+2)
table.remove(sequence,1)()
assert(clipboard['org.webmproject.webp']=='c' and pasted==beforePaste+3 and #sequence==0,'Each sticker is its own paste')
assert(settings['emojiLayout.last.v1']=='each' and keys[':return'].hk.deleted)
local history=settings['emojiLayout.history.v1']
assert(#history==2 and history[1].layout=='each' and #history[1].items==3 and history[1].items[1].emoji=='😀','Image exports join the sticker history, newest first')
assert(history[2].layout=='auto' and #history[2].items==2,'The earlier two-emoji PNG is there too')
env.emojiChooser()
chooser.query=function(self,q)if q then self.q=q end;return self.q or '' end
chooser.selected=1
keys['ctrl:return'].fn()
assert(chooser.rows[1].layout=='auto' and #chooser.rows==12,'One emoji offers every shape, repeated')
keys['ctrl:return'].fn()
keys['shift:return'].fn();keys['shift:return'].fn();keys['shift:return'].fn()
keys['ctrl:return'].fn()
assert(chooser.placeholder:find('Layout for 3 emoji',1,true))
keys['shift:tab'].fn()
assert(tally=='😀😀' and chooser.placeholder:find('Layout for 2 emoji',1,true),'Removing a pick refreshes the previews')
keys['ctrl:return'].fn()
assert(chooser.rows[1].emoji and chooser:query()=='' and chooser.placeholder:find('Ctrl+Enter: layouts',1,true),'Ctrl+Enter returns to the emoji list')
chooser.onHide()
env.emojiChooser()
chooser.query=function(self,q)if q then self.q=q end;return self.q or '' end
for _=1,6 do keys['shift:return'].fn() end
keys['ctrl:return'].fn()
assert(chooser.rows[1].layout=='each','The last used layout is listed first')
chooser.onHide()
local presses,timers=0,{}
local checkbox={setTimeout=function()end,attributeValue=function(_,key)
    return ({AXRole='AXCheckBox',AXTitle='Send as a document',AXValue=0})[key]
end,performAction=function(_,action)assert(action=='AXPress');presses=presses+1 end}
local window={setTimeout=function()end,attributeValue=function(_,key)
    return ({AXRole='AXWindow',AXChildren={checkbox}})[key]
end}
env.hs.pasteboard.changeCount=function()return 7 end
env.hs.application={frontmostApplication=function()return {bundleID=function()return 'org.telegram.desktop' end}end}
env.hs.axuielement={applicationElement=function()return {attributeValue=function()return window end}end}
env.hs.timer={doAfter=function(_,fn)timers[#timers+1]=fn end}
documentMode();table.remove(timers,1)()
assert(presses==1,'Only enable document checkbox; never press Send')
documentMode();env.hs.pasteboard.changeCount=function()return 8 end;table.remove(timers,1)()
assert(presses==1,'A newer clipboard must cancel late Telegram automation')
-- Purple honours the WebP marker in either compression mode, so a window with
-- no checkbox (Purple's own dialog, or none yet) must not raise a warning.
env.hs.pasteboard.changeCount=function()return 9 end
window.attributeValue=function(_,key)return ({AXRole='AXWindow',AXChildren={}})[key] end
tally=nil;local before=#timers
documentMode()
while #timers>before do table.remove(timers,before+1)() end
assert(tally==nil,'A missing Send as a document checkbox must not warn')
assert(presses==1,'Nothing is pressed when no checkbox exists')
local cachedAliases=env.emojiChooserAliases();local parsed=aliasDecodes
assert(env.emojiChooserAliases()==cachedAliases and aliasDecodes==parsed,'Unchanged aliases should not be decoded again')
env.emojiChooser();local cachedRow=chooser.rows[1];chooser.onHide()
env.emojiChooser();assert(chooser.rows[1]==cachedRow,'Unchanged row metadata should be reused');chooser.onHide()
aliasText='aliases2'
env.emojiChooser();assert(chooser.rows[1]~=cachedRow and chooser.rows[1].searchText:find('fresh keyword',1,true),'Alias edits must invalidate rows on the next opening');chooser.onHide()
print('EMOJI_CHOOSER_TESTS_PASSED')
