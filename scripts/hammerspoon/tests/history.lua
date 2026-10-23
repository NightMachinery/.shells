local pending,http={},{}
local payload,epoch,available= nil,1,true
local file=os.tmpname();local f=assert(io.open(file,'w'));f:write('endpoint');f:close()
hs={base64={encode=function(v)return 'encoded:'..v end},
 fs={symlinkAttributes=function(path)
  if not available then return end
  if path==file then return {mode='file',permissions='rw-------',size=8,ino=epoch,modification=1,change=1}end
  return {mode='directory',permissions='rwx------'}
 end},
 timer={absoluteTime=function()return 1e9 end,doAfter=function(_,fn)
  local t={fn=fn,stop=function(self)self.stopped=true end};pending[#pending+1]=t;return t
 end},
 json={encode=function(v)payload=v;return '{}'end,decode=function(raw)
  if raw=='endpoint' then return {url='http://127.0.0.1:12345/search',password='test-'..epoch}end
  if raw=='notready' then return {ok=true,ready=false}end
  if raw=='success' then return {ok=true,ready=true,total=5000,revision=1,history={{url='https://example.org/',title='Example'}}}end
  error('invalid JSON')
 end},
 http={asyncPost=function(url,body,headers,fn)
  assert(url=='http://127.0.0.1:12345/search' and headers.Authorization=='Basic encoded:omni:test-'..epoch)
  http[#http+1]=fn
 end}}
local H=dofile('brave/history.lua')({stateFile=file})
local calls,last=0
local function receive(v)calls=calls+1;last=v end
H.search('paper',{aliases={['example.org']={'Team'}},exclude={'https://example.org/open'}},receive)
assert(#http==1 and payload.query=='paper' and payload.exclude[1]=='https://example.org/open')
http[1](200,'notready');assert(calls==0)
pending[#pending].fn();assert(#http==2)
http[2](200,'success');assert(calls==1 and last.total==5000 and H.lastResult.returned==1)
http[2](200,'success');assert(calls==1)
epoch=2;H.search('rotated',{},receive);http[#http](200,'success');assert(calls==2)
H.search('timeout',{},receive);local late=http[#http]
pending[#pending].fn();assert(calls==3 and not last.ok)
late(200,'success');assert(calls==3)
available=false;H.search('missing',{},receive);assert(calls==4 and not last.ok)
available=true;H.search('cancel',{},receive);late=http[#http];H.stop()
assert(next(H.requests)==nil and H.endpoint==nil);late(200,'success');assert(calls==4)
local oldDecode=hs.json.decode
hs.json.decode=function(raw)
 if raw=='safari'then return {ok=true,ready=true,browser='safari',history={},total=0}end
 if raw=='wrong'then return {ok=true,ready=true,browser='brave',history={},total=0}end
 return oldDecode(raw)
end
local safari=dofile('brave/history.lua')({stateFile=file,browser='safari'})
safari.search('source',{},receive);assert(payload.browser=='safari');http[#http](200,'success');assert(not last.ok)
safari.search('source',{},receive);http[#http](200,'wrong');assert(not last.ok)
safari.search('source',{},receive);http[#http](200,'safari');assert(last.ok)
safari.stop()
os.remove(file)
print('Shared history client authentication, rotation, readiness, timeout and cancellation tests passed')
