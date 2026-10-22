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
const browser={windows:()=>[a,b],frontmost:()=>true,activate(){activation++}};
let state=context.handle({action:'activeState'},browser);
assert.equal(state.tabId,'10'); assert.equal(state.url,'https://example.org/');
assert.equal(state.frontmost,true);
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

// Query actions must make fresh tabs even if an identical URL is already open.
const made=[];
let current=1;
const tabs={id:()=>made.map((t,i)=>String(i+100)),push:t=>made.push(t)};
const tabCollection=()=>made;
Object.assign(tabCollection,tabs);
const win={id:()=>3,tabs:tabCollection,get activeTabIndex(){return ()=>current},set activeTabIndex(v){current=v}};
const fresh={windows:()=>[win],Tab:({url})=>({url}),activate(){}};
for(let i=0;i<2;i++) {
  result=context.handle({action:'openURL',url:'https://www.google.com/search?q=fixture'},fresh);
  assert.equal(result.ok,true); assert.equal(result.reused,false);
}
assert.equal(made.length,2);
assert.equal(context.handle({action:'openURL',url:'javascript:alert(1)'},fresh).ok,false);
console.log('fresh query tab tests passed');

// Replacement targets the captured ID, even after active-tab changes.
let urls=['https://example.org/one','https://example.org/two'];
const replacementTabs=()=>urls.map((url,i)=>({id:()=>String(i+500),url}));
Object.assign(replacementTabs,{id:()=>['500','501'],title:()=>['One','Two'],url:()=>urls});
const target={id:()=>8,tabs:replacementTabs,activeTabIndex:()=>2};
const replacing={windows:()=>[target],activate(){throw Error('replacement must not steal focus')}};
result=context.handle({action:'openURL',url:'https://example.org/new',replace:true,tabId:'500'},replacing);
assert.equal(result.ok,true);assert.equal(result.tabId,'500');assert.equal(result.replaced,true);
assert.equal(context.handle({action:'openURL',url:'https://example.org/new',replace:true,tabId:'missing'},replacing).ok,false);
let executed=0;
const dt={id:()=> 'dom-owned',url:()=> 'https://chatgpt.com/',execute({javascript}){executed++;return '{"status":"waiting"}'}};
const domBrowser={windows:()=>[{tabs:()=>[dt]}],activate(){throw Error('DOM must not steal focus')}};
result=context.handle({action:'dom',tabId:'dom-owned',destination:'chatgpt',code:'inert'},domBrowser);
assert.equal(result.result.status,'waiting');assert.equal(executed,1);
assert.equal(context.handle({action:'dom',tabId:'missing',destination:'chatgpt',code:'inert'},domBrowser).ok,false);
assert.equal(context.handle({action:'dom',tabId:'dom-owned',destination:'claude',code:'inert'},domBrowser).ok,false);
assert.equal(executed,1);
console.log('Stable replacement and focus-independent DOM routing tests passed');

// A multi-open selects the last destination internally, without app activation.
fresh.activate=()=>{throw Error('query batch must not activate the browser')};
result=context.handle({action:'openMany',items:[{url:'https://example.org/a'},{url:'https://example.org/b'}]},fresh);
assert.equal(result.opened.length,2);assert.equal(current,made.length);assert.equal(result.anchor,String(100+made.length-1));
console.log('Combined opens select the last new tab without activating the application');
