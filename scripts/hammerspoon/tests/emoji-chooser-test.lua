-- Run from scripts: lua hammerspoon/tests/emoji-chooser-test.lua
local source = assert(io.open('hammerspoon/core/choosers.lua')):read('*a')
local block = assert(source:match('(local emojiData = {}.-)\n%-%-%- %* Wi%-Fi Chooser'))
local keys, tap, chooser, tally, clipboard, canvas, pasted = {}, nil, nil, nil, nil, nil, 0
local data = {
    ['😀']={name='a recent', unicode_version='6.1'},
    ['🫪']={name='b newer', unicode_version='17.0'},
    ['🫫']={name='c newest', unicode_version='18.0'},
}
local env = setmetatable({
    io={open=function(path) return {read=function() return path:find('aliases.json',1,true) and 'aliases' or '{}' end,close=function() end} end},
    json={decode=function(text) return text=='aliases' and {['😀']={'unimpressed','eye'}} or data end},
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
            function chooser:selectedRowContents() return self.rows[self.selected or 1] end
            function chooser:hideCallback(fn) self.onHide=fn end
            function chooser:show() self.visible=true end
            function chooser:select() self.complete(self:selectedRowContents());self.onHide() end
            return chooser
        end,
        setChoices=function(c,rows)c.rows=rows end,
    },
    hs={
        fs={attributes=function()return 'file' end},
        image={imageFromPath=function(path)return path end},
        base64={decode=function(value)assert(value=='PNG');return 'png-bytes' end},
        pasteboard={writeAllData=function(value)clipboard=value;return true end},
        canvas={new=function(frame)
            canvas={frame=frame,images={}}
            function canvas:insertElement(element)self.images[#self.images+1]=element end
            function canvas:imageFromCanvas()return {encodeAsURLString=function()return 'data:image/png;base64,PNG' end}end
            function canvas:delete()self.deleted=true end
            return canvas
        end},
        hotkey={bind=function(mod,key,fn)
            local hk={delete=function(self)self.deleted=true end}; keys[mod..':'..key]={fn=fn,hk=hk};return hk
        end},
        eventtap={event={types={keyDown=10}},new=function(_,fn)
            tap={callback=fn,start=function(self)self.running=true;return self end,stop=function(self)self.running=false end};return tap
        end},
    },
}, {__index=_G})
assert(load(block,'emoji-chooser','t',env))()
env.emojiChooser()
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
keys['cmd:return'].fn()
assert(clipboard['public.png']=='png-bytes' and pasted==beforePaste+1)
assert(canvas.frame.w==256 and canvas.frame.h==128)
assert(canvas.images[1].frame.x==0 and canvas.images[2].frame.x==128)
assert(canvas.deleted and not tap.running,'PNG export releases canvas and chooser listeners')
print('EMOJI_CHOOSER_TESTS_PASSED')
