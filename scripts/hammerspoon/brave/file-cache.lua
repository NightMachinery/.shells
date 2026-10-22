-- Parsed local files, invalidated by metadata. Never cache across a changed read.
return function()
    local C={version="1.0.0",entries={}}
    local function signature(path)
        local attr=hs.fs.attributes(path)
        if not attr then return nil end
        return table.concat({tostring(attr.modification),tostring(attr.change),tostring(attr.size),tostring(attr.ino),tostring(attr.dev)},":")
    end
    function C.read(path, parse)
        local before=signature(path)
        local cached=C.entries[path]
        if before and cached and cached.signature==before then return cached.value,cached.error end
        C.entries[path]=nil
        local file=io.open(path,"r")
        if not file then return nil,"File is unavailable" end
        local raw=file:read("*a"); file:close()
        if not raw then return nil,"File could not be read" end
        local ok,value,err=pcall(parse,raw)
        if not ok then value=nil;err="File could not be parsed" end
        if before and signature(path)==before then C.entries[path]={signature=before,value=value,error=err} end
        return value,err
    end
    return C
end
