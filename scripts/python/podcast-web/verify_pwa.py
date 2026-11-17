#!/usr/bin/env python3
from pathlib import Path
from http.server import SimpleHTTPRequestHandler,ThreadingHTTPServer
import argparse,json,tempfile,threading,urllib.parse
from playwright.sync_api import sync_playwright

def main():
    parser=argparse.ArgumentParser(description="Verify podcast PWA installability, offline behavior and authentication in Chromium.")
    parser.add_argument('--screenshots',type=Path,help="Optional directory for phone screenshots")
    args=parser.parse_args()
    if args.screenshots:args.screenshots.mkdir(parents=True,exist_ok=True)
    root=Path(__file__).resolve().parent
    state={'authorized':True,'manifests':[],'posts':0}
    data={'title':'Private podcast fixture','episodes':[{'id':'abcdefghijk','title':'PRIVATE EPISODE TITLE','archived':False,'audio_url':None,'published':'20200101','duration':120,'job':None}],'playlists':[],'memberships':{'uploads':['abcdefghijk']},'refreshes':{},'sources':{},'feed_url':'https://example.com/feed.rss'}
    class Login(SimpleHTTPRequestHandler):
     def do_GET(self):
      content=b'<h1>Sign in required</h1>';self.send_response(200);self.send_header('Content-Type','text/html');self.end_headers();self.wfile.write(content)
     def log_message(self,*args):pass
    login=ThreadingHTTPServer(('127.0.0.1',0),Login)
    class App(SimpleHTTPRequestHandler):
     def __init__(self,*args,**kwargs):super().__init__(*args,directory=str(root),**kwargs)
     def do_GET(self):
      path=urllib.parse.urlsplit(self.path).path
      if not state['authorized'] or 'pwa-test=yes' not in self.headers.get('Cookie',''):
       self.send_response(302);self.send_header('Location',f'http://127.0.0.1:{login.server_port}/login');self.end_headers();return
      if path.endswith('/manifest.webmanifest'):state['manifests'].append(self.headers.get('Cookie',''))
      if path=='/podcasts/hola/api/catalog':
       body=json.dumps(data).encode();self.send_response(200);self.send_header('Content-Type','application/json');self.send_header('Cache-Control','no-store');self.end_headers();self.wfile.write(body);return
      if not path.startswith('/podcasts/hola/'):
       self.send_error(404);return
      self.path=self.path.replace('/podcasts/hola/','/',1);super().do_GET()
     def do_POST(self):state['posts']+=1;self.send_response(403);self.end_headers()
     def end_headers(self):self.send_header('Cache-Control','no-cache');super().end_headers()
     def log_message(self,*args):pass
    app=ThreadingHTTPServer(('127.0.0.1',0),App)
    for server in (login,app):threading.Thread(target=server.serve_forever,daemon=True).start()
    url=f'http://127.0.0.1:{app.server_port}/podcasts/hola/'
    with tempfile.TemporaryDirectory(prefix='podcast-pwa-profile-') as profile,sync_playwright() as p:
     context=p.chromium.launch_persistent_context(profile,headless=True,viewport={'width':390,'height':950})
     context.add_cookies([{'name':'pwa-test','value':'yes','url':url}])
     page=context.pages[0];errors=[];page.on('pageerror',lambda e:errors.append(str(e)))
     page.goto(url);page.wait_for_selector('.episode')
     scope=page.evaluate('navigator.serviceWorker.ready.then(r=>r.scope)')
     assert scope==url,scope
     page.wait_for_function('navigator.serviceWorker.controller !== null')
     page.evaluate("caches.open('other-app-keep').then(c=>c.put('/other-app/keep',new Response('keep'))) ")
     cdp=context.new_cdp_session(page);cdp.send('Page.enable')
     manifest=cdp.send('Page.getAppManifest')
     assert not manifest.get('errors'),manifest
     parsed=json.loads(manifest['data'])
     assert urllib.parse.urljoin(manifest['url'],parsed['start_url'])==url
     assert urllib.parse.urljoin(manifest['url'],parsed['scope'])==url
     assert parsed['display']=='standalone'
     assert {i['sizes'] for i in parsed['icons']}>={'192x192','512x512'}
     assert state['manifests'] and all('pwa-test=yes' in c for c in state['manifests'])
     installability=cdp.send('Page.getInstallabilityErrors')
     print('Installability',json.dumps(installability),flush=True)
     assert not installability['installabilityErrors'],installability
     assert page.locator('#install-app').is_visible()
     page.evaluate("""() => { window.installCalls=0;const event=new Event('beforeinstallprompt',{cancelable:true});event.prompt=()=>{window.installCalls++;return Promise.resolve()};event.userChoice=Promise.resolve({outcome:'dismissed'});window.dispatchEvent(event); }""")
     page.locator('#install-app').click();page.wait_for_function('window.installCalls === 1')
     assert page.locator('#install-help').is_visible()
     assert page.evaluate('document.documentElement.scrollWidth<=innerWidth')
     if args.screenshots:page.screenshot(path=str(args.screenshots/'install-phone.png'))
     page.locator('#theme').select_option('dark');page.wait_for_function("document.querySelector('meta[name=\"theme-color\"]').content==='#111a1a'")
     assert page.evaluate('caches.keys()')==['other-app-keep']
     context.set_offline(True)
     page.reload(wait_until='load')
     assert 'You’re offline' in page.locator('h1').inner_text()
     assert 'PRIVATE EPISODE TITLE' not in page.content()
     assert page.evaluate('caches.keys()')==['other-app-keep']
     if args.screenshots:page.screenshot(path=str(args.screenshots/'offline-phone.png'))
     failed=page.evaluate("fetch('api/download',{method:'POST',body:'{}'}).then(()=>false,()=>true)")
     assert failed and state['posts']==0
     context.set_offline(False);page.goto(url);page.wait_for_selector('.episode')
     state['authorized']=False
     page.reload(wait_until='load')
     assert page.url.startswith(f'http://127.0.0.1:{login.server_port}/login'),page.url
     assert page.locator('h1').inner_text()=='Sign in required'
     assert not errors,errors
     print(json.dumps({'manifest_and_cookie_auth':'passed','installability':'no errors','worker_scope':scope,'native_prompt_handler':'passed','phone_layout':'passed','offline_generic_page':'passed','private_content_and_mutations_not_cached':'passed','other_app_cache':'preserved','cross_origin_login_redirect':'passed','browser_errors':errors}),flush=True)
     context.close()
    for server in (app,login):server.shutdown();server.server_close()

if __name__ == "__main__":
    main()
