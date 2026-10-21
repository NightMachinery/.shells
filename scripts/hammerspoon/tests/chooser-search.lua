-- Run from hammerspoon/: lua tests/chooser-search.lua
local callback, shown
hs = {chooser={new=function()
    return {
        query=function() return "" end,
        queryChangedCallback=function(_, fn) callback=fn end,
        choices=function(self, rows) shown=rows; return self end,
    }
end}}
dofile("core/chooser-search.lua")
local rows = {
    {text="Inbox [Work]+",subText="https://example.org/account%20one"},
    {text="Other Inbox",subText="https://other.org/"},
}
local function count(query, expected)
    assert(#ChooserSearch.filter(rows,query)==expected, query)
end
count("",2); count("  \t ",2); count("org inbox",2)
count("example INBOX",1); count("inbox missing",0)
count("[Work]+ %20",1); count("[broken",0); count(".*",0)
count("ix",0) -- no fuzzy subsequence matching
assert(ChooserSearch.filter(rows,"example")[1] == rows[1])
local chooser = ChooserSearch.new(function() end)
ChooserSearch.setChoices(chooser,rows)
callback("work example"); assert(#shown==1)
callback("other"); assert(#shown==1 and shown[1]==rows[2])
callback(""); assert(#shown==2) -- filtering never loses the source list
local hiddenURL = {{text="Generic title", subText="Tab", url="https://example.org/unique-route"}}
assert(#ChooserSearch.filter(hiddenURL,"generic unique-route")==1)
assert(#ChooserSearch.filter({{text="Channel", searchText="Chat server"}},"channel server")==1)
print("chooser search tests passed")
