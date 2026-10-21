// Native Brave scripting bridge. JSON on stdin and stdout; no page JavaScript.
ObjC.import('Foundation');

// Normalize origin spelling only; path, query and fragment remain significant.
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
    if (request.action === 'list') {
        var list = windows(), tabs = [];
        for (var i=0;i<list.length;i++) {
            var ids = list[i].tabs.id(), titles = list[i].tabs.title(), urls = list[i].tabs.url();
            for (var j=0;j<ids.length;j++) {
                tabs.push({tabId:String(ids[j]), windowId:String(list[i].id()),
                    windowNumber:i+1, index:j+1, title:titles[j] || '', url:urls[j] || ''});
            }
        }
        return {ok:true,tabs:tabs};
    }
    if (request.action === 'selectTab') {
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
        var w = list[0];
        if (!w) {
            w = b.Window().make();
            w.activeTab().url = url;
        } else {
            w.tabs.push(b.Tab({url:url}));
            w.activeTabIndex = w.tabs().length;
        }
        w.index = 1;
        b.activate();
        var result = snapshot(w);
        result.reused = false;
        return result;
    }
    return {ok:false, error:'Unknown browser action'};
}

function run() {
    try {
        var input = $.NSFileHandle.fileHandleWithStandardInput.readDataToEndOfFile;
        var raw = $.NSString.alloc.initWithDataEncoding(input, $.NSUTF8StringEncoding).js;
        var request = JSON.parse(raw);
        var b = Application('Brave Browser');
        if (!b.running()) return JSON.stringify({ok:false,error:'Brave is not running'});
        return JSON.stringify(handle(request, b));
    } catch (error) {
        return JSON.stringify({ok:false,error:'Brave automation failed',code:Number(error.errorNumber)||0});
    }
}
