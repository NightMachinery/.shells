const fs=require('node:fs'),vm=require('node:vm'),assert=require('node:assert/strict');
const source=fs.readFileSync('brave/dom-submit.js','utf8');
function fixture(destination,text='',ready=true){
 let clicks=0,filled=0;
 const editor={tagName:'DIV',innerText:text,isConnected:true,getClientRects:()=>[{}],focus(){},getAttribute(){return null}};
 const send={isConnected:true,getClientRects:()=>[{}],getAttribute(){return null},click(){clicks++}};
 const picker={isConnected:true,getClientRects:()=>[{}],getAttribute:()=> 'Open mode picker, currently Argon High',click(){}};
 const c={location:{origin:{chatgpt:'https://chatgpt.com',claude:'https://claude.ai',gemini:'https://gemini.google.com'}[destination],pathname:{chatgpt:'/',claude:'/new',gemini:'/app'}[destination]},window:{getSelection:()=>({removeAllRanges(){},addRange(){}})},InputEvent:class{},
 document:{querySelectorAll(selector){if(selector.includes('data-message-author-role'))return [];if(selector.includes('Open mode picker'))return [picker];if(selector.includes('menuitem'))return [];if(selector.startsWith('button'))return ready?[send]:[];return [editor]},createRange:()=>({selectNodeContents(){}}),execCommand(_,__,value){editor.innerText=value;filled++;return true}}};
 vm.createContext(c);vm.runInContext(source,c);
 const run=()=>JSON.parse(c.omniDOMSubmit({nonce:'one',destination,query:'inert prompt'}));
 return {run,c,editor,picker,clicks:()=>clicks,filled:()=>filled};
}
for(const destination of ['chatgpt','claude','gemini']){
 const f=fixture(destination);assert.equal(f.run().status,'waiting');assert.equal(f.filled(),1);
 assert.equal(f.run().status,'submitted');assert.equal(f.clicks(),1);
 assert.equal(f.run().status,'submitted');assert.equal(f.clicks(),1);
 const draft=fixture(destination,'other draft');assert.equal(draft.run().status,'cancelled');assert.equal(draft.filled(),0);
 const prefill=fixture(destination,'inert prompt');assert.equal(prefill.run().status,'submitted');assert.equal(prefill.filled(),0);
 const blocked=fixture(destination,'inert prompt',false);assert.equal(blocked.run().status,'waiting');assert.equal(blocked.clicks(),0);
 const moved=fixture(destination);moved.c.location.pathname='/elsewhere';assert.equal(moved.run().status,'cancelled');
}
const g=fixture('gemini');
let model='Fast',menu=null;
function item(text,isSelected,click){return {isConnected:true,innerText:text,getClientRects:()=>[{}],getAttribute:()=>null,classList:{contains:()=>isSelected},click};}
g.picker.getAttribute=()=> 'Open mode picker, currently '+model;
g.picker.click=()=>{menu=menu?null:'model'};
const baseQuery=g.c.document.querySelectorAll;
g.c.document.querySelectorAll=selector=>{
 if(!selector.includes('menuitem'))return baseQuery(selector);
 if(!menu)return [];
 const argon=item('4 Argon\nComplex tasks',model.startsWith('Argon'),()=>{model='Argon Low';menu=null});
 const thinking=item('Thinking level',false,()=>{menu='high'});
 return menu==='high'?[argon,thinking,item('High\nExtra thorough',false,()=>{model='Argon High';menu=null})]:[argon,thinking];
};
for(let i=0;i<10 && !g.clicks();i++)g.run();
assert.equal(model,'Argon High');assert.equal(g.clicks(),1);assert.equal(g.filled(),1);
// A model click that does not settle must not turn into an endless click loop.
const stuck=fixture('gemini');let menuOpen=false,modelClicks=0;
stuck.picker.getAttribute=()=> 'Open mode picker, currently Flash-Lite';
stuck.picker.click=()=>{menuOpen=!menuOpen};
const query=stuck.c.document.querySelectorAll;
stuck.c.document.querySelectorAll=selector=>selector.includes('menuitem')?(menuOpen?[item('4 Argon',false,()=>{modelClicks++;menuOpen=false})]:[]):query(selector);
let status;
for(let i=0;i<10;i++){status=stuck.run().status;if(status==='native-required')break;}
assert.equal(status,'native-required');assert.equal(modelClicks,1);assert.equal(stuck.filled(),0);
console.log('DOM exact-draft, single-click, navigation and Gemini model/effort tests passed');

const redirected=fixture('chatgpt','inert prompt');redirected.c.location.pathname='/c/allocated-fixture';
assert.equal(redirected.run().status,'submitted');assert.equal(redirected.clicks(),1);
const conversationEmpty=fixture('chatgpt');conversationEmpty.c.location.pathname='/c/existing-fixture';
assert.equal(conversationEmpty.run().status,'waiting');assert.equal(conversationEmpty.filled(),0);
const automatic=fixture('chatgpt');automatic.c.location.pathname='/c/automatic-fixture';
const original=automatic.c.document.querySelectorAll;automatic.c.document.querySelectorAll=s=>s.includes('data-message-author-role')?[{innerText:'inert prompt'}]:original(s);
assert.equal(automatic.run().status,'submitted');assert.equal(automatic.clicks(),0);assert.equal(automatic.filled(),0);
console.log('ChatGPT conversation-prefill and already-submitted message tests passed');

const repeated=fixture('chatgpt','inert prompt');repeated.c.location.pathname='/c/repeated-fixture';
const oldQuery=repeated.c.document.querySelectorAll;repeated.c.document.querySelectorAll=s=>s.includes('data-message-author-role')?[{innerText:'inert prompt'}]:oldQuery(s);
assert.equal(repeated.run().status,'submitted');assert.equal(repeated.clicks(),1);
console.log('A historical matching message never suppresses a new exact draft');
