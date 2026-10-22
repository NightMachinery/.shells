nightdir='..'
local pending,tasks,http={}, {}, {}
local payload
hs={processInfo={processID=123},hash={SHA256=function()return 'abcdef0123456789' end},
 fs={mkdir=function()end,attributes=function()return 'file' end},
 timer={absoluteTime=function()return 1e9 end,doAfter=function(_,fn)
  local t={fn=fn,stop=function(self)self.stopped=true end};pending[#pending+1]=t;return t
 end},
 json={encode=function(value)payload=value;return '{}' end,decode=function(raw)
  if raw=='READY\n' then return {url='http://127.0.0.1:12345/search',token='test-token'} end
  if raw=='notready' then return {ok=true,ready=false} end
  if raw=='success' then return {ok=true,ready=true,total=5000,revision=1,history={{url='https://example.org/',title='Example'}}} end
  error('invalid JSON')
 end},
 http={asyncPost=function(url,body,headers,fn)
  assert(url=='http://127.0.0.1:12345/search' and headers.Authorization=='Bearer test-token')
  http[#http+1]=fn
 end},
 task={new=function(binary,fn,stream,args)
  local t={binary=binary,fn=fn,stream=stream,args=args,start=function()return true end,terminate=function(self)self.terminated=true end}
  tasks[#tasks+1]=t;return t
 end},
}
local H=dofile('brave/history.lua')({})
local calls,last=0
local function receive(value)calls=calls+1;last=value end
H.search('paper',{aliases={['example.org']={'Team'}},exclude={'https://example.org/open'}},receive)
assert(#tasks==1 and #http==0 and calls==0)
assert(tasks[1].stream(tasks[1],'REA'))
assert(not tasks[1].stream(tasks[1],'DY\n'))
assert(#http==1 and payload.query=='paper' and payload.exclude[1]=='https://example.org/open')
http[1](200,'notready');assert(calls==0)
pending[#pending].fn();assert(#http==2)
http[2](200,'success');assert(calls==1 and last.total==5000 and #last.history==1)
assert(H.lastResult.returned==1 and H.lastResult.total==5000)
http[2](200,'success');assert(calls==1)
H.search('timeout',{},receive);local late=http[#http]
pending[#pending].fn();assert(calls==2 and not last.ok)
late(200,'success');assert(calls==2)
H.search('cancel',{},receive);late=http[#http]
H.stop();assert(tasks[1].terminated)
late(200,'success');assert(calls==2)
print('Managed history service client readiness, timeout, late reply and stop tests passed')
