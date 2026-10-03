#!/usr/bin/env python3
"""Local stub, argv privacy, real ZLE readiness and byte contract."""
import http.server, json, os, pathlib, subprocess, tempfile, threading
root = pathlib.Path(__file__).resolve().parents[3]
key = 'inert-key-sentinel-llm-complete'
buffer = 'inert-buffer-sentinel-llm-complete'
seen = []
class Handler(http.server.BaseHTTPRequestHandler):
    def log_message(self, *args): pass
    def do_POST(self):
        body = json.loads(self.rfile.read(int(self.headers['Content-Length'])))
        # Inspect names+argv without ever printing the potentially secret listing.
        listing = subprocess.check_output(['ps','-axo','pid=,args='], text=True)
        assert key not in listing and buffer not in listing, 'request leaked to argv'
        seen.append(body)
        self.send_response(200); self.end_headers()
        self.wfile.write(b'{"choices":[{"message":{"content":" 0"}}]}')
server = http.server.ThreadingHTTPServer(('127.0.0.1',0), Handler)
threading.Thread(target=server.serve_forever, daemon=True).start()
with tempfile.TemporaryDirectory(prefix='llm-complete-contract-',dir='/private/tmp') as d:
    config = pathlib.Path(d)/'providers.json'
    config.write_text(json.dumps({'providers':{'stub':{'endpoint':f'http://127.0.0.1:{server.server_port}', 'model':'stub','key_env':'stub_key','extract':'chat'}}}))
    env = dict(os.environ, LLM_COMPLETE_CONFIG=str(config), stub_key=key)
    setup = f'''source "{root}/zshlang/basic/basic.plugin.zsh"; source "{root}/zshlang/basic/colors.zsh"; source "{root}/zshlang/basic/conditions-personal.zsh"; source "{root}/zshlang/basic/proxy.zsh"; source "{root}/zshlang/auto-load/others/fim.zsh"; fim_provider=stub; fim_proxy_p=n; local text="$(command cat)"; fim-get "$text"'''
    p = subprocess.run(['zsh','-f','-c',setup],input=buffer.encode(),stdout=subprocess.PIPE,stderr=subprocess.PIPE,env=env,timeout=30)
    assert (p.returncode,p.stdout,p.stderr)==(0,b' 0',b''), (p.returncode,p.stdout,p.stderr)
    try:
        p = subprocess.run(['zsh','-f',str(root/'golang/llm_complete/tests/zpty-contract.zsh')], env=env,capture_output=True,timeout=25)
    except subprocess.TimeoutExpired as e:
        print((e.stderr or b'').decode()); raise
    assert p.returncode==0, (p.stdout,p.stderr)
    print(p.stdout.decode(),end='')
    assert seen[0]['prompt']==buffer
    print('PASS: pipe bytes and key/buffer argv privacy')
server.shutdown()
