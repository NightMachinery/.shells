// Invoked in one identified chat tab. No network calls or OS keyboard events.
function omniDOMSubmit(request) {
    const normalize=value=>String(value||'').replace(/\r\n/g,'\n').trim();
    const origin={chatgpt:'https://chatgpt.com',claude:'https://claude.ai',gemini:'https://gemini.google.com'}[request.destination];
    if(location.origin!==origin)return JSON.stringify(request.initial?{status:'waiting',navigationPending:true}:{status:'cancelled',reason:'Query tab navigated away'});
    const key='__omniSubmissionV1';
    let job=window[key];
    if(job && job.nonce===request.nonce && job.sent)return JSON.stringify({status:'submitted'});
    const paths={chatgpt:['/'],claude:['/new','/new/'],gemini:['/app','/app/']};
    const conversation=request.destination==='chatgpt' && /^\/c\/[^/]+\/?$/.test(location.pathname);
    if(!paths[request.destination].includes(location.pathname) && !conversation)return JSON.stringify(request.initial?{status:'waiting',navigationPending:true}:{status:'cancelled',reason:'Query tab navigated away'});
    if(!job || job.nonce!==request.nonce)job=window[key]={nonce:request.nonce};
    const visible=e=>e && e.isConnected && e.getClientRects().length>0;
    const find=selector=>Array.from(document.querySelectorAll(selector)).find(visible);
    const label=e=>(e.getAttribute('aria-label')||e.innerText||'').trim();
    if(request.destination==='gemini') {
        const picker=find('[aria-label^="Open mode picker, currently "]');
        if(!picker)return JSON.stringify({status:'waiting'});
        const items=Array.from(document.querySelectorAll('[role="menuitem"],[role="option"]')).filter(visible);
        const selected=e=>e.classList.contains('selected') || e.getAttribute('aria-selected')==='true' || e.getAttribute('aria-checked')==='true';
        const choose=(e,action)=>{
            // Wait for the prior click to be reflected before issuing another.
            if(job.action===action) {
                if(Date.now()-job.actionAt>3000)return JSON.stringify({status:'native-required'});
                return JSON.stringify({status:'waiting'});
            }
            job.actions=job.actions||{};
            if(job.actions[action])return JSON.stringify({status:'native-required'});
            job.actions[action]=true;job.action=action;job.actionAt=Date.now();
            e.click();return JSON.stringify({status:'waiting'});
        };
        if(/currently (?:4 )?Argon High$/.test(label(picker))) {
            if(items.length)return choose(picker,'close-picker');
            job.action=null;
        } else {
            const argon=items.find(e=>/^(?:Selected )?4 Argon(?:\s|$)/.test(label(e)));
            const thinking=items.find(e=>label(e)==='Thinking level');
            const high=items.find(e=>/^(?:Selected )?High(?:\s|$)/.test(label(e)));
            if(high) {
                if(selected(high))return choose(picker,'close-high');
                return choose(high,'choose-high');
            }
            if(argon) {
                if(!selected(argon))return choose(argon,'choose-argon');
                if(thinking)return choose(thinking,'open-thinking');
            }
            if(items.length)return JSON.stringify({status:'failed',reason:'Gemini Argon High is unavailable'});
            return choose(picker,job.action==='choose-argon'?'reopen-picker':'open-picker');
        }
    }
    const selectors={chatgpt:'#prompt-textarea,[contenteditable="true"][aria-label="Ask ChatGPT"],textarea[name="prompt-textarea"]',
        claude:'[data-testid="chat-input"],[contenteditable="true"][aria-label="Write your prompt to Claude"]',
        gemini:'[contenteditable="true"][aria-label="Enter a prompt for Gemini"]'};
    const editor=find(selectors[request.destination]);
    if(!editor)return JSON.stringify({status:'waiting'});
    const value=()=>editor.tagName==='TEXTAREA'?editor.value:editor.innerText;
    const actual=normalize(value()),wanted=normalize(request.query);
    if(conversation && !actual) {
        const messages=Array.from(document.querySelectorAll('[data-message-author-role="user"]'));
        if(messages.length && normalize(messages[messages.length-1].innerText)===wanted) {
            job.sent=true;return JSON.stringify({status:'submitted'});
        }
    }
    if(actual && actual!==wanted)return JSON.stringify({status:'cancelled',reason:'Composer contains a different draft'});
    if(!actual) {
        // ChatGPT may allocate a conversation URL before prefill is rendered.
        // Never insert into an empty conversation: wait for exact draft/message.
        if(conversation)return JSON.stringify({status:'waiting'});
        if(job.filled)return JSON.stringify({status:'waiting'});
        job.filled=true;
        if(editor.tagName==='TEXTAREA') {
            Object.getOwnPropertyDescriptor(HTMLTextAreaElement.prototype,'value').set.call(editor,request.query);
            editor.dispatchEvent(new InputEvent('input',{bubbles:true,inputType:'insertText',data:request.query}));
        } else {
            // execCommand integrates with Chromium editing/React input handling.
            // DOM focus in this tab does not activate its browser window or tab.
            editor.focus({preventScroll:true});
            const range=document.createRange();range.selectNodeContents(editor);
            const selection=window.getSelection();selection.removeAllRanges();selection.addRange(range);
            if(!document.execCommand('insertText',false,request.query))return JSON.stringify({status:'failed',reason:'Composer did not accept insertion'});
        }
        return JSON.stringify({status:'waiting'});
    }
    const buttons={chatgpt:'button[data-testid="send-button"],button[aria-label="Send"],button[aria-label="Send prompt"],button[aria-label="Send message"]',
        claude:'button[data-testid="chat-input-send"],button[aria-label="Send message"]',
        gemini:'button[aria-label="Send message"],button[aria-label="Send prompt"],button[aria-label="Submit"]'};
    const send=find(buttons[request.destination]);
    if(!send || send.disabled || send.getAttribute('aria-disabled')==='true')return JSON.stringify({status:'waiting'});
    if(normalize(value())!==wanted)return JSON.stringify({status:'cancelled',reason:'Composer changed before submission'});
    // Mark before clicking: a lost Apple Event reply must never cause a second send.
    job.sent=true;
    try{send.click();}catch(_){return JSON.stringify({status:'failed',reason:'Send outcome uncertain; not retried'});}
    return JSON.stringify({status:'submitted'});
}
