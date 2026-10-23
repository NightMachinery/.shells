const fs=require('node:fs'),vm=require('node:vm'),assert=require('node:assert/strict');
const context={ObjC:{import(){},unwrap(v){return v}},$:{NSWorkspace:{sharedWorkspace:{frontmostApplication:{bundleIdentifier:'fixture.browser'}}}}};
vm.createContext(context);vm.runInContext(fs.readFileSync('brave/tabs.js','utf8'),context);
for(const kind of ['arc','dia']){
 let current='a',selects=0,focuses=0;
 const entries=['a','b'];
 const tabs=()=>entries.map(id=>({id:()=>id,select(){selects++;current=id},focus(){focuses++;current=id}}));
 tabs.id=()=>entries;tabs.title=()=>entries;tabs.url=()=>entries.map(id=>'https://example.org/'+id);tabs.push=()=>{};
 const win={id:()=>1,tabs,activeTab:()=>({id:()=>current})};
 const b={windows:()=>[win],activate(){throw Error('adapter must not activate')},Tab:p=>p};
 const adapter=context.sidebarBrowser(b,kind,'fixture.browser');
 assert.equal(adapter.windows()[0].activeTabIndex(),1);adapter.windows()[0].activeTabIndex=2;
 assert.equal(current,'b');assert.equal(kind==='arc'?selects:focuses,1);
 context.$.NSWorkspace.sharedWorkspace.frontmostApplication.bundleIdentifier='other.app';
 assert.throws(()=>{adapter.windows()[0].activeTabIndex=1},/focus changed/);
 assert.equal(current,'b');context.$.NSWorkspace.sharedWorkspace.frontmostApplication.bundleIdentifier='fixture.browser';
}
console.log('Arc/Dia dictionary adapters: read-only activeTab, distinct selection commands and focus refusal passed');
