// Native browser scripting bridge version 2.0.0. JSON requests; DOM execution is explicit and tab-targeted.
ObjC.import('Foundation');

// Normalize origin spelling only; path, query and fragment remain significant.
function safeBrowserError(message){var error=new Error(message);error.safeReason=message;return error;}
function canonicalURL(value) {
    var m = String(value).match(/^(https?):\/\/([^/?#]+)([^?#]*)([?#].*)?$/i);
    if (!m) return String(value);
    var scheme = m[1].toLowerCase();
    var host = m[2].toLowerCase().replace(scheme === 'https' ? /:443$/ : /:80$/, '');
    return scheme + '://' + host + (m[3] || '/') + (m[4] || '');
}

// Google redirects email-based authuser links to session-number URLs. Match
// the account on Gmail's own tab title after that redirect, not the number.
function gmailAccount(url, title) {
    var canonical = canonicalURL(url);
    if (!/^https:\/\/mail\.google\.com\/mail(?:\/|[?#]|$)/.test(canonical)) return null;
    var auth = canonical.match(/[?&]authuser=([^&#]*)/);
    if (auth) {
        try {
            var email = decodeURIComponent(auth[1].replace(/\+/g, ' '));
            if (/^[^\s@]+@[^\s@]+$/.test(email)) return email.toLowerCase();
        } catch (_) { /* A malformed parameter is not an account identity. */ }
    }
    var match = String(title || '').match(/(?:^| - )([^\s@]+@[^\s@]+) - Gmail$/i);
    return match ? match[1].toLowerCase() : null;
}

function handle(request, b) {
    function windows() { return b.windows(); }
    function snapshot(w) {
        if (!w) return {ok:false, error:'No browser window'};
        var ids = w.tabs.id().map(String);
        var index = w.activeTabIndex();
        return {ok:true, windowId:String(w.id()), tabIds:ids, tabId:ids[index-1], index:index};
    }
    function select(w, index) {
        w.activeTabIndex = index;
        w.index = 1;
        b.activate();
        return snapshot(w);
    }
    if (request.action === 'snapshot' || request.action === 'active') return snapshot(windows()[0]);
    if (request.action === 'activeState') {
        var w = windows()[0], state = snapshot(w);
        if (state.ok) {
            state.url = w.tabs.url()[state.index-1];
            state.frontmost = b.frontmost();
        }
        return state;
    }
    if (request.action === 'list') {
        var list = windows(), tabs = [];
        for (var i=0;i<list.length;i++) {
            var ids = list[i].tabs.id(), titles = list[i].tabs.title(), urls = list[i].tabs.url();
            for (var j=0;j<ids.length;j++) {
                tabs.push({tabId:String(ids[j]), windowId:String(list[i].id()),
                    windowNumber:i+1, index:j+1, title:titles[j] || '', url:urls[j] || ''});
            }
        }
        return {ok:true,tabs:tabs,activeTabId:snapshot(list[0]).tabId};
    }
    if (request.action === 'selectTab') {
        if (request.requireActiveTabId && (!b.frontmost() || snapshot(windows()[0]).tabId!==request.requireActiveTabId))
            return {ok:false,error:'Browser focus changed before native fallback'};
        // Find the stable ID again: the tab may have moved since the chooser opened.
        var list = windows();
        for (var i=0;i<list.length;i++) {
            var ids = list[i].tabs.id().map(String);
            var index = ids.indexOf(String(request.tabId));
            if (index >= 0) return select(list[i], index+1);
        }
        return {ok:false,error:'That tab has closed; reopen Omni Prompt to refresh'};
    }
    if (request.action === 'step') {
        if (!b.frontmost()) return {ok:false, error:'Browser focus changed'};
        var w = windows()[0], state = snapshot(w);
        if (!state.ok) return state;
        var step = request.direction === -1 ? -1 : 1;
        return select(w, (state.index - 1 + step + state.tabIds.length) % state.tabIds.length + 1);
    }
    if (request.action === 'openMany') {
        var anchor=snapshot(windows()[0]).tabId, opened=[];
        for (var i=0;i<request.items.length;i++) {
            opened.push(handle({action:'openURL',url:request.items[i].url,background:true,
                replace:i===0 && request.replace,tabId:request.tabId},b));
        }
        // Select the last destination inside Brave without raising a window or
        // activating the application. Other applications keep keyboard focus.
        for (var k=opened.length-1;k>=0;k--) {
            if (!opened[k].ok) continue;
            var list=windows();
            for (var i=0;i<list.length;i++) {
                var ids=list[i].tabs.id().map(String), index=ids.indexOf(opened[k].tabId);
                if (index>=0) {list[i].activeTabIndex=index+1;anchor=opened[k].tabId;break;}
            }
            break;
        }
        return {ok:true,anchor:anchor,opened:opened};
    }
    if (request.action === 'dom') {
        var list=windows();
        for (var i=0;i<list.length;i++) {
            var tabs=list[i].tabs();
            for (var j=0;j<tabs.length;j++) {
                if (String(tabs[j].id())!==request.tabId) continue;
                var url=tabs[j].url();
                var origins={chatgpt:'https://chatgpt.com',claude:'https://claude.ai',gemini:'https://gemini.google.com'};
                if (!origins[request.destination]) return {ok:false,error:'Unknown chat destination'};
                if (typeof tabs[j].loading==='function' && tabs[j].loading())
                    return {ok:true,jsExecuted:false,result:{status:'waiting',navigationPending:true}};
                if (url.split('/').slice(0,3).join('/')!==origins[request.destination]) {
                    if (request.initial) return {ok:true,jsExecuted:false,result:{status:'waiting',navigationPending:true}};
                    return {ok:false,error:'Query tab navigated away'};
                }
                try { return {ok:true,jsExecuted:true,result:JSON.parse(tabs[j].execute({javascript:request.code}))}; }
                catch (error) {
                    return {ok:false,jsUnavailable:/JavaScript.*(?:turned off|disabled|not allowed)/i.test(String(error)),error:'Tab JavaScript execution failed'};
                }
            }
        }
        return {ok:false,error:'Query tab closed'};
    }
    if (request.action === 'bookmark' || request.action === 'openURL') {
        var url = request.url;
        if (typeof url !== 'string' || !/^https?:\/\/[^\s]+$/i.test(url)) return {ok:false,error:'URL must be an HTTP or HTTPS URL'};
        var list = windows(), wanted = canonicalURL(url), account = gmailAccount(url);
        for (var i=0;request.action === 'bookmark' && i<list.length;i++) {
            var urls = list[i].tabs.url();
            var titles = account ? list[i].tabs.title() : [];
            for (var j=0;j<urls.length;j++) {
                if (canonicalURL(urls[j]) === wanted || (account && gmailAccount(urls[j], titles[j]) === account)) {
                    var result = select(list[i], j+1);
                    result.reused = true;
                    return result;
                }
            }
        }
        var w = list[0], replacing;
        if (request.replace) {
            for (var i=0;i<list.length;i++) {
                var ids=list[i].tabs.id().map(String), index=ids.indexOf(String(request.tabId));
                if (index>=0) { w=list[i];replacing=w.tabs()[index];break; }
            }
            if (!replacing) return {ok:false,error:'Replacement tab closed; reopen Omni to refresh'};
            replacing.url=url;
            return {ok:true,windowId:String(w.id()),tabId:String(request.tabId),reused:false,replaced:true};
        }
        if (!w) {
            w = b.Window().make();
            w.activeTab().url = url;
        } else {
            var previousIndex=w.activeTabIndex();
            w.tabs.push(b.Tab({url:url}));
            // Chromium may activate a newly pushed tab even without an explicit
            // activeTabIndex write. Restore the prior tab for background opens.
            w.activeTabIndex=request.background?previousIndex:w.tabs().length;
        }
        if (!request.background) {w.index=1;b.activate();}
        var result=snapshot(w);
        if (request.background) result.tabId=String(w.tabs.id()[w.tabs.id().length-1]);
        result.reused = false;
        return result;
    }
    return {ok:false, error:'Unknown browser action'};
}

// Safari has no documented stable tab ID. Use a window+URL identity, resolve
// again on every operation, and refuse ambiguity or navigation instead of
// silently selecting/replacing an index that now belongs to another tab.
function safariHandle(request,b) {
    var cachedRows,cachedWindows;
    function rows() {
        if(cachedRows)return cachedRows;
        var result=[];
        cachedWindows=b.windows();
        cachedWindows.forEach(function(w,wi) {
            var tabs=w.tabs(), urls=w.tabs.url(), titles=w.tabs.name();
            for(var i=0;i<tabs.length;i++) result.push({tabId:JSON.stringify([String(w.id()),String(urls[i]||'')]),
                windowId:String(w.id()),windowNumber:wi+1,index:i+1,title:titles[i]||'',url:urls[i]||'',w:w,tab:tabs[i]});
        });
        cachedRows=result;return result;
    }
    function publicRow(row) {
        if(!row) return {ok:false,error:'No browser window'};
        return {ok:true,tabId:row.tabId,windowId:row.windowId,index:row.index,url:row.url};
    }
    function active() {
        rows();var ws=cachedWindows;if(!ws.length)return {ok:false,error:'No browser window'};
        var index=ws[0].currentTab().index();
        var row=rows().filter(function(r){return r.windowId===String(ws[0].id())&&r.index===index;})[0];
        var out=publicRow(row);out.frontmost=b.frontmost();
        if(out.ok)out.tabIds=rows().filter(function(r){return r.windowId===row.windowId;}).map(function(r){return r.tabId;});
        return out;
    }
    function resolve(id) {
        var found=rows().filter(function(r){return r.tabId===id;});
        if(found.length!==1)throw safeBrowserError(found.length?'Ambiguous Safari tabs with the same URL':'Safari tab closed or navigated; reopen Omni');
        return found[0];
    }
    function select(row,foreground) {
        row.w.currentTab=row.tab;
        if(foreground){row.w.index=1;b.activate();}
        return publicRow(row);
    }
    if(request.action==='list')return {ok:true,tabs:rows().map(function(r){return {tabId:r.tabId,windowId:r.windowId,
        windowNumber:r.windowNumber,index:r.index,title:r.title,url:r.url};}),activeTabId:active().tabId};
    if(request.action==='activeState'||request.action==='active'||request.action==='snapshot')return active();
    if(request.action==='selectTab'){
        if(!b.frontmost()||(request.requireActiveTabId&&active().tabId!==request.requireActiveTabId))
            return {ok:false,error:'Browser focus changed before tab selection'};
        return select(resolve(request.tabId),true);
    }
    if(request.action==='step'){
        if(!b.frontmost())return {ok:false,error:'Browser focus changed'};
        var state=active();if(!state.ok)return state;
        var tabs=rows().filter(function(r){return r.windowId===state.windowId;});
        return select(tabs[(state.index-1+(request.direction===-1?-1:1)+tabs.length)%tabs.length],true);
    }
    if(request.action==='dom'){
        var row;
        try {row=resolve(request.tabId);}catch(_){return {ok:false,error:'Safari query tab closed, navigated or became ambiguous'};}
        var origin={chatgpt:'https://chatgpt.com',claude:'https://claude.ai',gemini:'https://gemini.google.com'}[request.destination];
        if(!origin||row.url.split('/').slice(0,3).join('/')!==origin)return {ok:false,error:'Query tab navigated away'};
        try{return {ok:true,jsExecuted:true,result:JSON.parse(b.doJavaScript(request.code,{in:row.tab}))};}
        catch(error){return {ok:false,jsUnavailable:/JavaScript.*(?:turned off|disabled|not allowed)|Allow JavaScript from Apple Events|not authorized to send JavaScript/i.test(String(error)),error:'Tab JavaScript execution failed'};}
    }
    if(request.action==='openMany'){
        var opened=[];
        request.items.forEach(function(item,i){opened.push(safariHandle({action:'openURL',url:item.url,background:true,
            replace:i===0&&request.replace,tabId:request.tabId},b));});
        for(var i=opened.length-1;i>=0;i--)if(opened[i].ok){select(resolve(opened[i].tabId),false);break;}
        return {ok:true,opened:opened,anchor:active().tabId};
    }
    if(request.action==='bookmark'||request.action==='openURL'){
        var url=request.url;
        if(typeof url!=='string'||!/^https?:\/\/[^\s]+$/i.test(url))return {ok:false,error:'URL must be an HTTP or HTTPS URL'};
        if(request.action==='bookmark'){
            if(!b.frontmost())return {ok:false,error:'Browser focus changed'};
            var account=gmailAccount(url),same=rows().filter(function(r){return canonicalURL(r.url)===canonicalURL(url)||(account&&gmailAccount(r.url,r.title)===account);});
            if(same.length){var reused=select(same[0],true);reused.reused=true;return reused;}
        }
        if(request.replace){
            var target=resolve(request.tabId);target.tab.url=url;
            return {ok:true,windowId:target.windowId,tabId:JSON.stringify([target.windowId,url]),replaced:true,reused:false};
        }
        var windows=b.windows(),w=windows[0];
        if(!w){
            b.Document({url:url}).make();cachedRows=null;
            // Safari can publish the new window after make() returns. Only read
            // while waiting; never retry the creation request.
            for(var attempt=0;attempt<10&&!w;attempt++){
                cachedRows=null;var created=rows().filter(function(r){return r.url===url;});
                if(created.length===1)w=created[0].w;
                else $.NSThread.sleepForTimeInterval(.03);
            }
            if(!w)return {ok:false,error:'New Safari window not yet available; creation was not retried'};
        }else{
            var previous=w.currentTab();w.tabs.push(b.Tab({url:url}));cachedRows=null;
            if(request.background)w.currentTab=previous;
        }
        var found=rows().filter(function(r){return r.windowId===String(w.id())&&r.url===url;});
        var row=found[found.length-1];
        if(!row)return {ok:false,error:'New Safari tab not yet available; creation was not retried'};
        var result=request.background?publicRow(row):select(row,true);result.reused=false;return result;
    }
    return {ok:false,error:'Unsupported Safari action'};
}

// Arc selects a tab with select(), Dia with focus(). Both expose activeTab as
// read-only, unlike Chrome's writable activeTabIndex. Do not call these focus
// commands over another app: fail safely if focus moved during a queued open.
function sidebarBrowser(b,kind,bundleID) {
    function wrap(w) {
        var tabs=function(){return w.tabs();};
        tabs.id=function(){return w.tabs.id();};tabs.title=function(){return w.tabs.title();};tabs.url=function(){return w.tabs.url();};
        tabs.push=function(tab){w.tabs.push(tab);};
        return {id:function(){return w.id();},tabs:tabs,
            get activeTabIndex(){return function(){return w.tabs.id().map(String).indexOf(String(w.activeTab().id()))+1;};},
            set activeTabIndex(index){
                if(!frontmostBundle(bundleID))throw safeBrowserError('Browser focus changed; native tab focus refused');
                if(kind==='arc')w.tabs()[index-1].select();else w.tabs()[index-1].focus();
            },
            set index(index){if(kind==='arc')w.index=index;},
            activeTab:function(){return w.activeTab();}};
    }
    return {windows:function(){return b.windows().map(wrap);},frontmost:function(){return frontmostBundle(bundleID);},
        activate:function(){b.activate();},Tab:function(properties){return b.Tab(properties);},
        Window:function(){throw safeBrowserError('Open a browser window before using Omni');}};
}
function frontmostBundle(bundleID){
    return ObjC.unwrap($.NSWorkspace.sharedWorkspace.frontmostApplication.bundleIdentifier)===bundleID;
}
function run() {
    try {
        ObjC.import('AppKit');
        var input = $.NSFileHandle.fileHandleWithStandardInput.readDataToEndOfFile;
        var raw = $.NSString.alloc.initWithDataEncoding(input, $.NSUTF8StringEncoding).js;
        var request = JSON.parse(raw);
        var supported={'com.brave.Browser':'chromium','com.google.Chrome':'chromium','com.microsoft.edgemac':'chromium',
            'com.apple.Safari':'safari','company.thebrowser.Browser':'arc','company.thebrowser.dia':'dia'};
        var id=request.bundleID||'com.brave.Browser',kind=supported[id];
        if(!kind)return JSON.stringify({ok:false,error:'Unsupported browser'});
        var b = Application(id);
        if (!b.running()) return JSON.stringify({ok:false,error:'Browser is not running'});
        if(request.requireFrontmost&&!frontmostBundle(id))return JSON.stringify({ok:false,error:'Browser focus changed'});
        if((kind==='arc'||kind==='dia')&&!frontmostBundle(id)&&request.action!=='list'&&request.action!=='activeState'&&request.action!=='dom')
            return JSON.stringify({ok:false,error:'Native tab focus requires this browser to remain frontmost'});
        return JSON.stringify(kind==='safari'?safariHandle(request,b):handle(request,kind==='chromium'?b:sidebarBrowser(b,kind,id)));
    } catch (error) {
        return JSON.stringify({ok:false,error:error.safeReason||'Browser automation failed; action was not retried',code:Number(error.errorNumber)||0});
    }
}
