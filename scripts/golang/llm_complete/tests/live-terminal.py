#!/usr/bin/env python3
"""Opt-in live checks. Targets must be dedicated scratch CLIs; never submit text."""
import argparse, http.server, json, os, pathlib, re, subprocess, tempfile, threading, time, unicodedata
BINARY=os.path.expanduser('~/go/bin/llm_complete')
ROOT=pathlib.Path(__file__).resolve().parents[3]
KITTEN=str(ROOT/'configFiles/kitty/kittens/agent_complete.py')

def run(args, text=None, env=None):
    p=subprocess.run(args,input=text,text=True,capture_output=True,env=env,timeout=35)
    if p.returncode: raise RuntimeError(f'{pathlib.Path(args[0]).name}: {p.stderr.strip()}')
    return p.stdout

class Target:
    def __init__(self,source,socket,target,agent):self.source,self.socket,self.target,self.agent=source,socket,target,agent
    def rc(self,*args,text=None):
        cmd=['kitten','@','--to',self.socket] if self.source=='kitty' else ['tmux','-S',self.socket]
        return run(cmd+list(args),text)
    def capture(self):
        return self.rc('get-text','--match','id:'+self.target) if self.source=='kitty' else self.rc('capture-pane','-p','-J','-t',self.target)
    def prompt(self):
        marker='❯' if self.agent=='claude' else '›'
        rows=[l.lstrip(' │┃') for l in self.capture().splitlines() if l.lstrip(' │┃').startswith(marker)]
        return unicodedata.normalize('NFC',rows[-1][1:].lstrip(' \xa0').rstrip()) if rows else ''
    def wait(self,predicate,limit=5):
        until=time.monotonic()+limit
        while time.monotonic()<until:
            value=self.capture()
            if predicate(value):return value
            time.sleep(.025)
        raise AssertionError(f'{self.source}/{self.agent}: screen readiness timeout')
    def wait_error(self,message):
        deadline=time.monotonic()+5
        while time.monotonic()<deadline:
            listing=json.loads(self.rc('ls'))
            for oswin in listing:
                for tab in oswin['tabs']:
                    if not any(str(w['id'])==self.target for w in tab['windows']):continue
                    for window in tab['windows']:
                        id=str(window['id'])
                        if id!=self.target and message in self.rc('get-text','--match','id:'+id):
                            self.rc('close-window','--match','id:'+id)
                            return
            time.sleep(.025)
        raise AssertionError('scratch error overlay did not show '+message)
    def key(self,*keys):
        if self.source=='kitty':self.rc('send-key','--match','id:'+self.target,*keys)
        else:self.rc('send-keys','-t',self.target,*[{'ctrl+u':'C-u','ctrl+e':'C-e','esc':'Escape','backspace':'BSpace','left':'Left'}.get(k,k) for k in keys])
    def text(self,text):
        assert '\n' not in text and '\r' not in text
        if self.source=='kitty':self.rc('send-text','--match','id:'+self.target,'--stdin',text=text)
        else:
            self.rc('load-buffer','-b','llm-complete-live-test','-',text=text)
            self.rc('paste-buffer','-d','-b','llm-complete-live-test','-t',self.target)
    def set(self,text):
        self.key('ctrl+e','ctrl+u')
        self.wait(lambda _:self.prompt() in ('','Ask Codex to do anything','Find and fix a bug in @filename','Improve documentation in @filename'))
        self.text(text);self.wait(lambda _:self.prompt()==unicodedata.normalize('NFC',text))
    def snapshot(self):
        raw=self.rc('get-text','--match','id:'+self.target,'--add-cursor','--add-wrap-markers')
        match=list(re.finditer(r'\x1b\[(\d+);(\d+)H',raw))[-1]
        rows=raw[:match.start()].rstrip('\n').split('\n')
        listing=json.loads(self.rc('ls'))
        window=next(w for o in listing for t in o['tabs'] for w in t['windows'] if str(w['id'])==self.target)
        process=next(p for p in window['foreground_processes'] if any(pathlib.Path(x).name in (self.agent,self.agent+'.exe') for x in p['cmdline'][:3]))
        return {'source':'kitty','socket':self.socket,'target':self.target,'kitty_pid':int(re.search(r'kitty-(\d+)\.sock',self.socket)[1]),'cwd':window['cwd'],'kitten':KITTEN,'screen':{'agent':self.agent,'process_id':process['pid'],'cursor_y':int(match[1])-1,'cursor_x':int(match[2])-1,'lines':[{'text':r.rstrip('\r'),'wrapped':r.endswith('\r')} for r in rows]}}
    def operation(self,op,env=None,background=False):
        if self.source=='kitty' and env is None:
            return self.rc('kitten','--match','id:'+self.target,KITTEN,op)
        args=[BINARY,'terminal',op]
        text=None
        if self.source=='tmux':args+=['tmux',self.socket,self.target]
        else:text=json.dumps(self.snapshot())
        if background:
            proc=subprocess.Popen(args,stdin=subprocess.PIPE,stdout=subprocess.PIPE,stderr=subprocess.PIPE,text=True,env=env)
            if text is not None:proc.stdin.write(text)
            proc.stdin.close();proc.stdin=None
            return proc
        return run(args,text,env)
    def checks(self):
        if self.agent=='claude':
            self.key('esc');self.wait(lambda text:'-- INSERT --' not in text);self.key('A');self.wait(lambda text:'-- INSERT --' in text)
        base='Review alpha alphaLong alphaNext '
        self.set(base+'al')
        started=time.monotonic();self.operation('dabbrev');self.wait(lambda _:self.prompt()==base+'alphaNext')
        elapsed=(time.monotonic()-started)*1000
        self.operation('dabbrev');self.wait(lambda _:self.prompt()==base+'alphaLong')
        print(f'PASS: {self.source}/{self.agent} expansion and cycling ({elapsed:.0f} ms observed)')
        # Editing invalidates cycling, rather than deleting the old remainder.
        self.text('Z');self.wait(lambda _:self.prompt()==base+'alphaLongZ')
        if self.agent=='claude':
            self.set(base+'al');self.key('esc');self.wait(lambda text:'-- INSERT --' not in text)
            if self.source=='kitty':
                self.operation('dabbrev');self.wait_error('enter INSERT mode first')
            else:
                p=subprocess.run([BINARY,'terminal','dabbrev','tmux',self.socket,self.target],capture_output=True)
                assert p.returncode==1
            assert self.prompt()==base+'al'
            self.key('A');self.wait(lambda text:'-- INSERT --' in text)
            print(f'PASS: {self.source}/claude vim NORMAL refuses insertion')
        for word in ['سلام','e\u0301','😀']:
            self.set('inert '+word)
            self.key('backspace')
            # Different editors delete graphemes or code points. Measure rather
            # than assuming; keep production non-ASCII cycling disabled.
            expected='inert '
            self.wait(lambda _:self.prompt()!=unicodedata.normalize('NFC','inert '+word))
            remaining=self.prompt()
            print(f'Observed: {self.source}/{self.agent} backspace of {word!r} leaves {remaining!r}')
        self.set('Review inertDraft')

class Handler(http.server.BaseHTTPRequestHandler):
    received=threading.Event();release=threading.Event();body=None
    def log_message(self,*_):pass
    def do_POST(self):
        Handler.body=json.loads(self.rfile.read(int(self.headers['Content-Length'])))
        Handler.received.set();Handler.release.wait(8)
        self.send_response(200);self.end_headers();self.wfile.write(b'{"choices":[{"message":{"content":" inertCompletion"}}]}')

def stale_checks(target):
    server=http.server.ThreadingHTTPServer(('127.0.0.1',0),Handler)
    threading.Thread(target=server.serve_forever,daemon=True).start()
    with tempfile.TemporaryDirectory(prefix='llm-complete-live-',dir='/private/tmp') as d:
        config=pathlib.Path(d)/'providers.json'
        config.write_text(json.dumps({'providers':{'codestral':{'endpoint':f'http://127.0.0.1:{server.server_port}','key_env':''}}}))
        env=dict(os.environ,LLM_COMPLETE_CONFIG=str(config),LLM_COMPLETE_LOG_DIR=str(pathlib.Path(d)/'logs'))
        target.set('Review inertDraft')
        Handler.received.clear();Handler.release.clear()
        proc=target.operation('fim',env=env,background=True)
        assert Handler.received.wait(5)
        assert Handler.body['stop']=='\n'
        target.text('Z');target.wait(lambda _:target.prompt()=='Review inertDraftZ')
        Handler.release.set();_,err=proc.communicate(timeout=10)
        assert proc.returncode==1 and target.prompt()=='Review inertDraftZ',err
        if target.source=='kitty':target.wait_error('discarded completion')
        log=(pathlib.Path(d)/'logs/completion.log').read_text()
        assert Handler.body['prompt']+'⟦CURSOR⟧' in log and 'inertCompletion' in log
        print(f'PASS: {target.source}/{target.agent} delayed response discarded; exact request logged')
        Handler.received.clear();Handler.release.set();target.set('Review inertDraft')
        target.operation('fim',env=env)
        target.wait(lambda _:target.prompt()=='Review inertDraft inertCompletion')
        print(f'PASS: {target.source}/{target.agent} unchanged prompt accepts FIM')
    server.shutdown()

if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('--source',choices=['kitty','tmux'],required=True);p.add_argument('--socket',required=True);p.add_argument('--claude',required=True);p.add_argument('--codex',required=True);p.add_argument('--stale-only',action='store_true');p.add_argument('--basic-only',action='store_true');args=p.parse_args()
    for agent in ['claude','codex']:
        target=Target(args.source,args.socket,getattr(args,agent),agent)
        if not args.stale_only:target.checks()
        if not args.basic_only:stale_checks(target)
