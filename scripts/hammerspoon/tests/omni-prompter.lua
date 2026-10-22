local clipboard='  input €\n\n'
local discoveries,runs,notices,pastes,queue={}, {}, {},0,{}
local settings={}
hs={settings={get=function(k)return settings[k]end,set=function(k,v)settings[k]=v end},
 screen={mainScreen=function()return {frame=function()return {w=1920}end}end},
 chooser={new=function(completion)
 local c={text='',visible=false,items={},selected=1}
 function c:query(v)if v==nil then return self.text end;self.text=v;return self end
 function c:queryChangedCallback(fn)self.changed=fn;return self end
 function c:choices(rows)self.items=rows;self.selected=1;return self end
 function c:selectedRowContents()return self.items[self.selected]end
 function c:placeholderText(v)self.placeholder=v;return self end
 function c:width(v)self.w=v;return self end
 function c:rows()return self end
 function c:show()self.visible=true;return self end
 function c:hide()self.visible=false;return self end
 function c:isVisible()return self.visible end
 function c:delete()self.deleted=true end
 function c:pick(row)self:hide();completion(row)end
 return c
end}}
dofile('core/chooser-search.lua')
local unchanged=true
local function make()
 return dofile('omni-prompter/main.lua')({noEventtap=true,
  clipboard={getContents=function()return clipboard end,setContents=function(v)clipboard=v;return true end},
  captureTarget=function()return {id=1}end,targetMatches=function(t)return unchanged and t.id==1 end,
  paste=function()pastes=pastes+1 end,
  defer=function(fn)local t={stop=function(self)self.stopped=true end};queue[#queue+1]=function()if not t.stopped then fn()end end;return t end,
  notify=function(m,ok)notices[#notices+1]={m,ok}end,
  discover=function(argv,callback,_,opts)assert(argv[1]=='omni-prompter-functions' and opts.nolog);discoveries[#discoveries+1]=callback end,
  execute=function(stages,callback,_,opts)runs[#runs+1]={stages=stages,callback=callback,opts=opts}end,
 })
end
local function flush()local q=queue;queue={};for _,f in ipairs(q)do f()end end
local p=make();assert(p.show() and p.chooser.w==75)
local original=clipboard
discoveries[#discoveries]('prompt-long\nfragment-a\nsnippet-b\nmetaprompt-c\nprompt-z\nprompt-z\nprompt-$(touch unsafe)\n')
assert(p.functionCount==5 and p.chooser.items[1].name=='prompt-z')
p.chooser:query('meta prompt');p.chooser.changed('meta prompt');assert(#p.chooser.items==1)
assert(p.key(36,{shift=true}) and p.chain[1]=='metaprompt-c' and p.chooser:isVisible())
p.chooser.selected=1;p.add();assert(#p.chain==2);p.key(51,{shift=true});assert(#p.chain==1)
p.chooser.selected=1;p.add();assert(p.chain[2]=='prompt-z')
clipboard='changed while choosing'
assert(p.key(36,{}));assert(p.busy and not p.chooser:isVisible());flush()
local run=runs[#runs];assert(#run.stages==2 and run.stages[1][1]=='metaprompt-c' and run.stages[2][1]=='prompt-z')
assert(run.opts.stdin==original and run.opts.trim==false and run.opts.nolog)
assert(not p.show() and not p.run('prompt-unknown'))
run.callback('  output €\n\n');assert(pastes==1 and clipboard=='  output €\n\n' and p.lastResult.status=='pasted')
local discovered=#discoveries
assert(p.show() and #discoveries==discovered and p.chooser.items[1].name=='prompt-z' and p.chooser.items[2].name=='metaprompt-c')
-- Refresh is explicit; stale callbacks never replace a newer catalog.
p.key(15,{cmd=true});local old=discoveries[#discoveries];p.refresh();old('fragment-stale')
assert(p.functionCount==5);discoveries[#discoveries]('prompt-z\nfragment-a\n')
p.chooser:pick(p.chooser.items[1]);flush();local before=clipboard
runs[#runs].callback(nil);assert(clipboard==before and p.lastResult.status=='failed' and pastes==1)
-- A changed focus permits copying, not pasting.
p.show();unchanged=false;p.run();flush();runs[#runs].callback('copied only');assert(clipboard=='copied only' and pastes==1 and p.lastResult.status=='copied')
p.show();p.chooser:pick(nil);assert(p.snapshot==nil)
p.show();p.run();local count=#runs;p.stop();flush();assert(#runs==count and p.chooser.deleted)
-- Preferences survive a module reload; catalog does not.
dofile('core/chooser-search.lua')
local other=make();other.show();assert(#discoveries==discovered+3)
discoveries[#discoveries]('prompt-very-long-never-used\nfragment-a\nmetaprompt-c\nprompt-z\n')
assert(other.chooser.items[1].name=='prompt-z' and other.chooser.items[2].name=='metaprompt-c')
clipboard=nil;other.chooser:pick(nil);other.show();other.run('fragment-a');flush();assert(runs[#runs].opts.stdin=='')
other.stop();runs[#runs].callback('must not copy');assert(clipboard==nil)
print('OmniPrompter chain, undo, cache refresh, persistent ordering, exact output and paste ownership tests passed')
