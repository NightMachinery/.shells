const fs=require('node:fs'),vm=require('node:vm'),assert=require('node:assert/strict');
const context={ObjC:{import(){}}};vm.createContext(context);vm.runInContext(fs.readFileSync('brave/tabs.js','utf8'),context);
const entries=[{url:'https://example.org/one',name:'One'},{url:'https://example.org/two',name:'Two'}];
let current=entries[0],activation=0;
function tab(entry){return {get url(){return ()=>entry.url},set url(v){entry.url=v},index:()=>entries.indexOf(entry)+1,get raw(){return entry}}}
const tabs=()=>entries.map(tab);tabs.url=()=>entries.map(t=>t.url);Object.defineProperty(tabs,'name',{value:()=>entries.map(t=>t.name)});tabs.push=t=>entries.push({url:t.url,name:'New'});
const w={id:()=>42,tabs,get currentTab(){return ()=>tab(current)},set currentTab(t){current=t.raw},set index(i){}};
const b={windows:()=>[w],frontmost:()=>true,activate(){activation++},Tab:p=>p,doJavaScript(code,{in:t}){assert.equal(t.raw,entries.find(e=>e.url==='https://chatgpt.com/?q=fixture'));return '{"status":"submitted"}'}};
const run=request=>context.safariHandle(request,b);
let list=run({action:'list'});assert.equal(list.tabs.length,2);
let first=list.tabs[0].tabId;
entries.reverse();let selected=run({action:'selectTab',tabId:first});assert.equal(selected.index,2);assert.equal(current.url,'https://example.org/one');
assert.equal(activation,1);
run({action:'openURL',url:'https://example.org/replaced',replace:true,tabId:first});assert.equal(current.url,'https://example.org/replaced');
// A navigation invalidates the old identity, rather than selecting by stale index.
entries[1].url='https://example.org/navigated';assert.throws(()=>run({action:'selectTab',tabId:first}),/navigated/);
entries.push({url:'https://example.org/two',name:'Duplicate'});
assert.throws(()=>run({action:'selectTab',tabId:run({action:'list'}).tabs[0].tabId}),/Ambiguous/);
entries.pop();
let batch=run({action:'openMany',items:[{url:'https://chatgpt.com/?q=fixture'},{url:'https://example.org/new'}]});
assert.equal(batch.opened.length,2);assert.equal(current.url,'https://example.org/new');assert.equal(activation,1);
const chat=batch.opened[0];assert.equal(run({action:'dom',tabId:chat.tabId,destination:'chatgpt',code:'inert'}).result.status,'submitted');
assert.equal(run({action:'dom',tabId:chat.tabId,destination:'claude',code:'inert'}).ok,false);
b.doJavaScript=()=>{throw Error('Allow JavaScript from Apple Events is disabled')};assert.equal(run({action:'dom',tabId:chat.tabId,destination:'chatgpt',code:'inert'}).jsUnavailable,true);
b.frontmost=()=>false;assert.equal(run({action:'selectTab',tabId:chat.tabId}).ok,false);
assert.equal(run({action:'openURL',url:'javascript:alert(1)'}).ok,false);
console.log('Safari adapter: URL identity, reorder/ambiguity/navigation guards, background selection and DOM permissions passed');
