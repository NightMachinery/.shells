// Run from hammerspoon/: node tests/brave-tabs.js
const fs = require('node:fs'), vm = require('node:vm'), assert = require('node:assert/strict');
const context = {ObjC:{import(){}}};
vm.createContext(context);
vm.runInContext(fs.readFileSync('brave/tabs.js','utf8'),context);
function window(id, entries) {
    const tabs = {id:()=>entries.map(t=>t.id),title:()=>entries.map(t=>t.title),url:()=>entries.map(t=>t.url)};
    let index=1;
    return {id:()=>id,tabs,get activeTabIndex(){return ()=>index},set activeTabIndex(value){index=value}};
}
const entry = id=>({id,title:'Duplicate',url:'https://example.org/'});
const a=window(1,[entry(10),entry(11)]), b=window(2,[entry(20)]);
let activation=0;
const browser={windows:()=>[a,b],activate(){activation++}};
let result=context.handle({action:'list'},browser);
assert.equal(result.tabs.length,3);
assert.equal(new Set(result.tabs.map(t=>t.tabId)).size,3);
assert.equal(result.tabs[2].windowNumber,2);
// Move the selected ID to another window after listing.
a.tabs.id=()=>[11]; b.tabs.id=()=>[20,10];
result=context.handle({action:'selectTab',tabId:'10'},browser);
assert.equal(result.windowId,'2'); assert.equal(result.index,2);
assert.equal(activation,1);
result=context.handle({action:'selectTab',tabId:'999'},browser);
assert.equal(result.ok,false); assert.equal(activation,1);
console.log('native tab list and selection tests passed');
