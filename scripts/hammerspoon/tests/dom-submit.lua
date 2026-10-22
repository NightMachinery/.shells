nightdir='..'
local now,pending,requests=0,{},{}
hs={timer={secondsSinceEpoch=function()return now end,absoluteTime=function()return now*1e9 end,
 doAfter=function(_,fn)local t={fn=fn,stop=function(self)self.stopped=true end};pending[#pending+1]=t;return t end},
 host={uuid=function()return 'fixture-nonce' end},json={encode=function()return '{}'end}}
local function make()return dofile('brave/dom-submit.lua')({engine={request=function(r,cb)requests[#requests+1]={r=r,cb=cb}end}})end
local D=make();local result,calls
local function cb(r)result=r;calls=(calls or 0)+1 end
D.submit({tabId='1'},'chatgpt','inert',cb)
assert(#requests==1 and requests[1].r.action=='dom' and requests[1].r.tabId=='1')
requests[1].cb({ok=false,jsUnavailable=true});assert(result.status=='native-required' and D.disabledUntil==72*60*60)
D.submit({tabId='2'},'claude','inert',cb);assert(#requests==1 and calls==2)
now=72*60*60+1;D.submit({tabId='2'},'claude','inert',cb);assert(#requests==2)
requests[2].cb({ok=true,result={status='waiting'}});assert(D.supported and D.disabledUntil==nil)
pending[#pending].fn();assert(#requests==3)
requests[3].cb({ok=true,result={status='submitted'}});assert(result.status=='submitted' and calls==3)
-- An uncertain transport error never falls back or retries a possible click.
D.submit({tabId='3'},'gemini','inert',cb);requests[#requests].cb({ok=false,error='timed out'})
assert(result.status=='failed' and calls==4)
D.submit({tabId='4'},'claude','inert',cb);local late=requests[#requests].cb;D.stop();late({ok=true,result={status='submitted'}});assert(calls==4)
-- Reloading the module clears even an unexpired disabled cache.
D=make();D.submit({tabId='5'},'chatgpt','inert',cb);assert(#requests==6)
print('DOM support detection, 72-hour negative cache, reload reset and ambiguous-error tests passed')

D=make();local start=#requests;local done=0
D.submit({tabId='a'},'chatgpt','inert',function()done=done+1 end)
D.submit({tabId='b'},'claude','inert',function()done=done+1 end)
D.submit({tabId='c'},'gemini','inert',function()done=done+1 end)
assert(#requests==start+1)
requests[#requests].cb({ok=false,jsUnavailable=true})
assert(#requests==start+1 and done==3)
print('Concurrent disabled-support detection coalesces to one Apple Event request')

D=make();local first=#requests+1
D.submit({tabId='cold'},'chatgpt','inert',cb);assert(requests[first].r.initial)
requests[first].cb({ok=true,result={status='waiting',navigationPending=true}})
pending[#pending].fn();assert(requests[first+1].r.initial)
requests[first+1].cb({ok=true,result={status='waiting'}})
pending[#pending].fn();assert(requests[first+2].r.initial==false)
requests[first+2].cb({ok=true,result={status='submitted'}})
print('Cold navigation keeps waiting until the intended page is first observed')

D=make();first=#requests+1
D.submit({tabId='loading'},'chatgpt','inert',cb)
D.submit({tabId='also-loading'},'claude','inert',cb)
requests[first].cb({ok=true,jsExecuted=false,result={status='waiting',navigationPending=true}})
assert(D.supported==nil and #requests==first)
pending[#pending].fn();requests[first+1].cb({ok=false,jsUnavailable=true})
assert(D.supported==false and #requests==first+1)
print('Native loading waits never incorrectly mark JavaScript support as enabled')
