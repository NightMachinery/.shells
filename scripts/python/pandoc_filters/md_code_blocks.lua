-- @forkedFrom [[id:1ffc4d15-5baf-44bd-9cd4-98d50b3270b4][Add option to disable indented code blocks? · Issue #2120 · jgm/pandoc]]
---
local formats = {
    markdown = true,
    markdown_strict = true,
    markdown_mmd = true,
    markdown_phpextra = true,
    djot = true,
    markdown_github = true,
    gfm = true,
    commonmark = true,
    commonmark_x = true
}


CodeBlock = function(elem)
    if not formats[FORMAT] then
        -- error(FORMAT .. " not supported")
        ---
        -- @no-op
        return elem
    end

    local text, classes, attr = elem.text, elem.classes, elem.attr

    local wd = 2
    for t in text:gmatch("%`+") do
        if #t > wd then
            wd = #t
        end
    end
    local fence = ('`'):rep(wd + 1)
    local block = fence .. "\n" .. text .. "\n" .. fence
    return pandoc.RawBlock(FORMAT, block)
end
