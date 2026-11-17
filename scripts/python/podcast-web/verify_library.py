#!/usr/bin/env python3
"""Headless integration check for podcast switching, source feeds and URL migration."""
from pathlib import Path
from http.server import SimpleHTTPRequestHandler
from urllib.parse import urlsplit
import json
import tempfile
import threading
import time
from unittest.mock import patch
import xml.etree.ElementTree as ET
from playwright.sync_api import sync_playwright
from library import Library
from server import Catalog, Handler, ThreadingHTTPServer

PRIMARY, SECOND, THIRD = ['UC' + c*22 for c in 'abc']
OLD, NEW = 'abcdefghijk', 'lmnopqrstuv'
ROOT = Path(__file__).parent


def main():
    with tempfile.TemporaryDirectory(prefix='podcast-library-qa-') as temporary:
        base=Path(temporary)
        first=Catalog(base/'state',base/'audio'/'original','https://example.com/yt/original',PRIMARY,'Original podcast')
        library=Library(first,Catalog,policy_file=base/'policy.json',public_app_path='/podcasts/',pwa_id='/podcasts/hola/')
        def populate(catalog):
            with catalog.db() as db:
                db.executemany('INSERT OR IGNORE INTO episodes(id,title,duration) VALUES(?,?,?)',[(OLD,'Downloaded episode',60),(NEW,'An older conversation',120)])
                db.executemany("INSERT OR IGNORE INTO membership VALUES('uploads',?,?)",[(OLD,0),(NEW,1)])
                db.execute("INSERT OR REPLACE INTO sources VALUES('uploads',?)",(time.time(),))
            path=catalog.audio/f'Downloaded episode [{OLD}].m4a';path.write_bytes(b'fixture')
            catalog.add_to_feed({'id':OLD,'title':'Downloaded episode','upload_date':'20260304'},path)
            catalog.enqueue=lambda vid:catalog.job(vid,'queued')
        populate(first)
        with patch.object(library,'resolve',return_value=(SECOND,'Second podcast','https://www.youtube.com/@second')),patch.object(library,'schedule_catalog'):
            library.add('@second')
        populate(library.catalog(SECOND));library.rebuild_feeds()
        library.resolve=lambda value:(THIRD,'Added podcast','https://www.youtube.com/@added')
        library.schedule_catalog=populate
        class App(Handler):
            def do_GET(self):
                path=urlsplit(self.path).path
                if path in ('/podcasts/hola/','/podcasts/hola','/podcasts/hola/index.html'):
                    self.send_response(308);self.send_header('Location','/podcasts/');self.end_headers();return
                if path.startswith('/podcasts/api/'):
                    self.path=self.path.removeprefix('/podcasts/api');return super().do_GET()
                if path=='/podcasts/manifest.webmanifest':
                    self.path='/manifest.webmanifest';return super().do_GET()
                if not path.startswith('/podcasts/'):
                    self.reply({'error':'Not found'},404);return
                name='index.html' if path.endswith('/') else path.rsplit('/',1)[-1]
                if name not in ('index.html','app.js','style.css','theme.js','pwa.js','sw.js','icon.svg','icon-192.png','icon-512.png','icon-maskable-512.png','apple-touch-icon.png'):
                    self.reply({'error':'Not found'},404);return
                mime={'.js':'text/javascript','.css':'text/css','.html':'text/html','.png':'image/png','.svg':'image/svg+xml'}[Path(name).suffix]
                self.reply((ROOT/name).read_bytes(),content_type=mime)
            def do_POST(self):
                self.path=self.path.removeprefix('/podcasts/api');super().do_POST()
            def log_message(self,*args):pass
        server=ThreadingHTTPServer(('127.0.0.1',0),App);server.catalog=first;server.library=library
        threading.Thread(target=server.serve_forever,daemon=True).start()
        try:
            with sync_playwright() as p:
                browser=p.chromium.launch(headless=True)
                page=browser.new_page(viewport={'width':390,'height':950})
                errors=[];page.on('pageerror',lambda e:errors.append(str(e)))
                url=f'http://127.0.0.1:{server.server_port}/podcasts/'
                page.goto(url+'hola/');page.wait_for_selector('.episode')
                assert page.url==url
                assert page.locator('#podcast option').count()==2
                assert page.locator('#auto-download').is_checked()
                original_feed=page.locator('#podcast-feed-url').input_value()
                assert original_feed.endswith('/original/feed.rss')
                page.get_by_role('button',name='Archive',exact=True).first.click()
                page.wait_for_function("document.getElementById('count').textContent==='(0 of 2)'")
                page.locator('#podcast').select_option(SECOND);page.wait_for_selector('.episode')
                assert not page.locator('#auto-download').is_checked()
                page.locator('#exclude-global').check()
                page.wait_for_function("document.getElementById('library-status').textContent.includes('Excluded')")
                assert next(r for r in library.podcasts() if r['id']==SECOND)['excluded_global']
                page.locator('#exclude-global').uncheck()
                page.wait_for_function("document.getElementById('library-status').textContent.includes('Included')")
                assert page.locator('#title').inner_text()=='Second podcast'
                assert page.locator('#count').inner_text()=='(1 of 2)'
                page.get_by_role('button',name='Download',exact=True).click()
                page.wait_for_function("document.getElementById('queue-count').textContent==='(1 active)'")
                assert 'Second podcast' in page.locator('#queue-active').inner_text()
                page.locator('#manage-podcasts').evaluate('(e)=>e.open=true')
                page.locator(f'#feed-sources input[value="{PRIMARY}"]').check()
                page.locator(f'#feed-sources input[value="{SECOND}"]').check()
                page.locator('#create-feed').click();page.wait_for_selector('#combined-feed:visible')
                feed=page.locator('#combined-feed-url').input_value();assert '/feeds/' in feed
                mixed=ET.parse(library.audio_root/'feeds'/feed.rsplit('/',1)[-1]).findall('./channel/item')
                assert len(mixed)==2
                global_items=ET.parse(library.audio_root/'feeds/global.rss').findall('./channel/item')
                assert len(global_items)==1 and global_items[0].findtext('title').startswith('Second podcast:')
                assert 'except Original podcast' in page.locator('label[for="global-feed-url"]').inner_text()
                page.locator('#podcast-url').fill('@added');page.locator('#add-podcast button').click()
                page.wait_for_function("document.getElementById('title').textContent==='Added podcast'")
                assert not page.locator('#auto-download').is_checked()
                assert page.locator('#podcast option').count()==3
                page.reload();page.wait_for_function("document.getElementById('title').textContent==='Added podcast'")
                page.locator('#auto-download').check();page.wait_for_function("document.getElementById('library-status').textContent.includes('enabled')")
                assert next(r for r in library.podcasts() if r['id']==THIRD)['auto_download']
                page.locator('#auto-download').uncheck()
                page.wait_for_function("document.getElementById('library-status').textContent.includes('disabled')")
                assert page.evaluate('document.documentElement.scrollWidth<=innerWidth')
                manifest=page.evaluate("fetch('manifest.webmanifest').then(r=>r.json())")
                assert manifest['id']=='/podcasts/hola/' and manifest['start_url']=='/podcasts/' and manifest['scope']=='/podcasts/'
                assert not errors,errors
                print(json.dumps({'podcast_switch_and_reload':'passed','archive_isolation':'passed','global_queue':'passed','combined_feed':'2 sources','global_exclusion':'passed','add_auto_off_and_toggle':'passed','canonical_redirect_and_pwa_identity':'passed','phone_overflow':'none','browser_errors':errors}))
                browser.close()
        finally:
            server.shutdown();server.server_close();library.refresh_pool.shutdown(wait=True);first.pool.shutdown(wait=True)


if __name__=='__main__':
    main()
