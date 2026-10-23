nightdir='..'
hs={timer={secondsSinceEpoch=function()return 0 end},json={}}
local pending,started,received={},{},{}
local E=dofile('brave/tabs.lua')({bundleID='org.mozilla.firefox',transport=function(request,done)
 if request.sequence==4 then error("native exception")end
 started[#started+1]=request;pending[#pending+1]=done
end})
for i=1,3 do E.request({action='list',sequence=i},function(result)received[#received+1]=result.sequence end)end
assert(#started==1 and #E.queue==2 and E.transportBusy)
pending[1]({ok=true,sequence=1});assert(#started==2 and #E.queue==1)
pending[1]({ok=true,sequence=99});assert(#started==2 and #received==1) -- once only
pending[2]({ok=true,sequence=2});assert(#started==3)
pending[3]({ok=true,sequence=3});assert(#received==3 and not E.transportBusy)
assert(started[1].bundleID=='org.mozilla.firefox')
local failed
E.request({action='list',sequence=4},function(result)failed=result end)
assert(failed and not failed.ok and not E.transportBusy)
E.request({action='list',sequence=5},function()end);assert(#started==4 and E.transportBusy)
pending[4]({ok=true});assert(not E.transportBusy)
E.stop();print('Browser transport: asynchronous adapters stay serialized, target the right browser and complete once')
