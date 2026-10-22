nightdir='..'
local attr={modification=1,change=1,size=10,ino=2,dev=3}
local exists=true
hs={fs={attributes=function()if exists then return attr end end}}
local C=dofile('brave/file-cache.lua')()
local path=os.tmpname()
local function write(value)local f=assert(io.open(path,'w'));f:write(value);f:close() end
write('one')
local parses=0
local function parse(raw)parses=parses+1;return {raw=raw} end
local first=C.read(path,parse);assert(first.raw=='one')
assert(C.read(path,parse)==first and parses==1)
write('two');attr.modification=2;assert(C.read(path,parse).raw=='two' and parses==2)
-- Atomic replacement can preserve mtime/size; inode/change still invalidate.
write('new');attr.ino=4;assert(C.read(path,parse).raw=='new' and parses==3)
exists=false;os.remove(path);assert(C.read(path,parse)==nil)
exists=true;write('back');assert(C.read(path,parse).raw=='back' and parses==4)
-- Never retain a result read while file metadata changed underneath us.
attr.modification=3
local function changing(raw)parses=parses+1;attr.change=attr.change+1;return {raw=raw} end
C.read(path,changing);C.read(path,changing);assert(parses==6)
os.remove(path)
print('File cache invalidation tests passed')
