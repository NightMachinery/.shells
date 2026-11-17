#!/usr/bin/env python3
"""Phone-browser check for combined Shorts, length and saved list filters."""
from functools import partial
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
import json
import threading
from playwright.sync_api import sync_playwright

ROOT = Path(__file__).parent

def main():
    server = ThreadingHTTPServer(('127.0.0.1', 0), partial(SimpleHTTPRequestHandler, directory=str(ROOT)))
    threading.Thread(target=server.serve_forever, daemon=True).start()
    episodes = [dict(id=char*11, title=title, duration=seconds, is_short=short,
                     archived=archived, audio_url='https://example.com/audio.m4a' if downloaded else None, job=None)
                for char,title,seconds,short,archived,downloaded in [
                    ('s','A YouTube Short',60,True,False,False), ('r','Short regular video',60,False,False,False),
                    ('m','Ten minutes',600,False,False,False), ('x','Twenty minutes',1200,False,False,False),
                    ('u','Unknown length',None,False,False,False), ('a','Archived',900,False,True,False),
                    ('d','Downloaded',900,False,False,True)]]
    data=dict(title='Filter check',episodes=episodes,memberships={'uploads':[e['id'] for e in episodes]},playlists=[],refreshes={},sources={},feed_url='https://example.com/feed.rss')
    try:
        with sync_playwright() as p:
            browser=p.chromium.launch(headless=True)
            page=browser.new_page(viewport={'width':390,'height':900})
            errors=[];page.on('pageerror',lambda e:errors.append(str(e)))
            page.route('**/api/catalog*',lambda route:route.fulfill(json=data))
            page.goto(f'http://127.0.0.1:{server.server_port}/')
            page.wait_for_selector('.episode')
            assert page.locator('#hide-shorts').is_checked()
            assert page.locator('#count').inner_text()=='(4 of 7)'
            assert 'Short regular video' in page.locator('#episodes').inner_text()
            page.locator('#min-length').fill('10');page.locator('#max-length').fill('20')
            assert page.locator('#count').inner_text()=='(2 of 7)'
            page.reload();page.wait_for_selector('.episode')
            assert page.locator('#min-length').input_value()=='10'
            assert page.locator('#max-length').input_value()=='20'
            assert page.locator('#count').inner_text()=='(2 of 7)'
            page.locator('#min-length').fill('1');page.locator('#max-length').fill('1')
            assert page.locator('#count').inner_text()=='(1 of 7)'
            page.locator('#hide-shorts').uncheck()
            assert page.locator('#count').inner_text()=='(2 of 7)'
            assert page.locator('.episode .badge').filter(has_text='YouTube Short').count()==1
            page.locator('#min-length').fill('25')
            assert page.locator('#count').inner_text()=='(0 of 7)'
            assert page.locator('#length-error').inner_text()
            page.locator('#show-all').click()
            assert page.locator('#count').inner_text()=='(7 of 7)'
            assert page.locator('#min-length').input_value()==page.locator('#max-length').input_value()==''
            assert not page.locator('#hide-shorts').is_checked()
            page.locator('#min-length').fill('0.5');page.locator('#max-length').fill('1.5')
            assert page.locator('#count').inner_text()=='(2 of 7)'
            assert page.evaluate('document.documentElement.scrollWidth<=innerWidth')
            assert not errors,errors
            print(json.dumps({'default_shorts_hidden':'passed','regular_short_video_visible':'passed','inclusive_length_bounds':'passed','unknown_lengths_excluded':'passed','saved_filters':'passed','invalid_bounds':'passed','show_all':'passed','phone_overflow':'none'}))
            browser.close()
    finally:
        server.shutdown();server.server_close()

if __name__=='__main__':main()
