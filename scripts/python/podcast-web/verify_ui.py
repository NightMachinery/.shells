#!/usr/bin/env python3
"""Check stable pages during downloads and render compact mixed-direction rows."""
import argparse
from copy import deepcopy
from functools import partial
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
import json
from pathlib import Path
import threading
import time

from playwright.sync_api import sync_playwright
from podcast_dates import format_upload_date
from presentation import episode_title

ROOT=Path(__file__).parent


def main():
    parser=argparse.ArgumentParser();parser.add_argument('--screenshots',type=Path)
    args=parser.parse_args()
    if args.screenshots:args.screenshots.mkdir(parents=True,exist_ok=True)
    class Quiet(SimpleHTTPRequestHandler):
        def log_message(self,*args):pass
    server=ThreadingHTTPServer(('127.0.0.1',0),partial(Quiet,directory=str(ROOT)))
    threading.Thread(target=server.serve_forever,daemon=True).start()
    source='tg_sample_audio'
    titles=['چطور با جدایی کنار بیاییم.mp3','شناخت احساسات و رابطه سالم.mp3','گفتگو_در_زمان_اختلاف.mp3','A conversation about family']
    episodes=[dict(id='tg_'+str(i+1),title=titles[i%4],display_title=episode_title(titles[i%4], 'telegram'),duration=1500+i*15,
                   published='20240921',published_display=format_upload_date('20240921'),
                   source_label='Telegram',source_url=f'https://t.me/sample_audio/{i+1}',
                   archived=False,is_short=False,local=False,
                   audio_url='https://example.com/old.mp3' if i==0 else None,job=None) for i in range(90)]
    fixture=dict(title='Listening library',artwork_url='icon-512.png?v=fixture',source_type='telegram',current_podcast=source,
                 podcasts=[dict(id=source,title='Listening library',auto_download=False,excluded_from_global=True,
                                feed_url='https://example.com/feed.rss',downloaded=1,source_type='telegram')],
                 episodes=episodes,memberships={'uploads':[e['id'] for e in episodes]},
                 playlists=[],refreshes={},sources={},feed_url='https://example.com/feed.rss',
                 global_feed_url='https://example.com/global.rss',settings_sync={'configured':True,'sync_enabled':True,'pending':False,'error':None})
    results=[]
    try:
        with sync_playwright() as p:
            browser=p.chromium.launch(headless=True)
            for name,width in [('desktop',1280),('phone',390),('narrow',320)]:
                data=deepcopy(fixture);context=browser.new_context(viewport={'width':width,'height':900})
                page=context.new_page();errors=[];page.on('pageerror',lambda e:errors.append(str(e)))
                def api(route):
                    if route.request.method=='GET':route.fulfill(json=data);return
                    request=route.request.post_data_json
                    episode=next(e for e in data['episodes'] if e['id']==request['id'])
                    episode['job']={'status':'downloading','error':None,'updated':time.time()}
                    route.fulfill(status=202,json={'ok':True})
                page.route('**/api/*',api)
                page.goto(f'http://127.0.0.1:{server.server_port}/');page.wait_for_selector('.episode')
                page.wait_for_function("document.getElementById('podcast-cover').naturalWidth === 512")
                assert page.locator('#count').inner_text()=='(89 of 90)'
                assert 'September9/2024' in page.locator('.meta time').first.inner_text()
                assert 'Shahrivar6/31' in page.locator('.meta time').first.inner_text()
                assert not any('.mp3' in t or '_' in t for t in page.locator('.episode h3').all_text_contents())
                if args.screenshots:page.screenshot(path=str(args.screenshots/f'{name}-controls.png'))
                page.locator('#next-top').click()
                assert page.locator('#page').inner_text()==page.locator('#page-top').inner_text()=='Page 2 of 3'
                ids=lambda:page.locator('.episode').evaluate_all('(rows)=>rows.map(e=>e.dataset.episode)')
                before=ids();identifier=before[0]
                row=page.locator(f'.episode[data-episode="{identifier}"]')
                row.get_by_role('button',name='Download',exact=True).click()
                page.wait_for_function("document.querySelector('.episode .badge')?.textContent==='Downloading…'")
                following=page.locator('.episode').nth(1);before_y=following.bounding_box()['y']
                entry=next(e for e in data['episodes'] if e['id']==identifier)
                entry['audio_url']='https://example.com/completed.mp3';entry['job']['status']='done'
                page.evaluate('load()')
                assert ids()==before,(name,'completed download changed page contents')
                assert abs(following.bounding_box()['y']-before_y)<=1,(name,'completed download shifted following rows')
                assert row.get_by_role('button',name='Listen',exact=True).is_visible()
                assert '1 completed download' in page.locator('#download-retention').inner_text()
                page.locator('.episode').nth(20).scroll_into_view_if_needed()
                assert abs(page.locator('.list-toolbar').bounding_box()['y'])<=1
                page.locator('#next').click();page.locator('#previous-top').click()
                assert ids()==before
                page.locator('#refresh-list').click()
                page.wait_for_function("document.getElementById('count').textContent==='(88 of 90)'")
                assert identifier not in ids()
                # An externally/automatically completed episode has no queued badge.
                before=ids();automatic=next(e for e in data['episodes'] if e['id']==before[0])
                before_y=page.locator('.episode').nth(1).bounding_box()['y']
                automatic['audio_url']='https://example.com/automatic.mp3'
                page.evaluate('load()')
                assert ids()==before
                assert abs(page.locator('.episode').nth(1).bounding_box()['y']-before_y)<=1,(name,'automatic completion shifted rows')
                page.reload();page.wait_for_selector('.episode')
                assert page.locator('#count').inner_text()=='(87 of 90)'
                for theme in ('light','dark'):
                    page.locator('#theme').select_option(theme)
                    page.locator('#episode-list').scroll_into_view_if_needed()
                    assert page.evaluate('document.documentElement.scrollWidth<=innerWidth'),(name,theme,'overflow')
                    if args.screenshots:page.screenshot(path=str(args.screenshots/f'{name}-{theme}.png'))
                assert not errors,errors
                results.append({'viewport':name,'stable_manual_and_auto_downloads':'passed','refresh_hides_completed':'passed','sticky_and_bottom_paging':'passed','dual_dates':'passed','overflow':'none'})
                context.close()
            browser.close()
    finally:server.shutdown();server.server_close()
    print(json.dumps(results))


if __name__=='__main__':main()
