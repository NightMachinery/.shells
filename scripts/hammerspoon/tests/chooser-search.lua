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

-- Bound native rows without limiting which source entries can be searched.
local big={}
for i=1,5000 do big[i]={text='Paper '..i,url='https://example.org/'..i} end
local bounded=ChooserSearch.new(function()end,nil,{limit=200})
ChooserSearch.setChoices(bounded,big);assert(#shown==200)
callback('paper 4999');assert(#shown==1 and shown[1]==big[4999])
big[4999].text='Changed'
ChooserSearch.setChoices(bounded,big);callback('changed 4999');assert(#shown==1)
print('Cached chooser index and bounded native result tests passed')
