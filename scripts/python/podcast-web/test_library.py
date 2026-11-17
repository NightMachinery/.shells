import importlib.util
import json
from pathlib import Path
import tempfile
import threading
import time
import unittest
from unittest.mock import patch
import xml.etree.ElementTree as ET

from library import Library, channel_url
from podcast_policy import managed_by_library

spec = importlib.util.spec_from_file_location("podcast_test_catalog", Path(__file__).with_name("server.py"))
web = importlib.util.module_from_spec(spec)
spec.loader.exec_module(web)
PRIMARY, SECOND, THIRD = ["UC" + char * 22 for char in ("a", "b", "c")]
OLD, NEW = "abcdefghijk", "lmnopqrstuv"


class LibraryTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = Path(self.tmp.name)
        self.primary = web.Catalog(self.root/"state", self.root/"audio"/"original", "https://example.com/yt/original", PRIMARY, "Original podcast")
        self.policy = self.root/"policy.json"
        self.library = Library(self.primary, web.Catalog, policy_file=self.policy)

    def tearDown(self):
        self.library.stop.set()
        self.library.refresh_pool.shutdown(wait=True)
        self.library.playlist_pool.shutdown(wait=True)
        self.primary.pool.shutdown(wait=True)
        self.tmp.cleanup()

    def add(self, identifier=SECOND, title="Second podcast"):
        with patch.object(self.library, "resolve", return_value=(identifier,title,"https://www.youtube.com/channel/"+identifier)), patch.object(self.library,"schedule_catalog"):
            return self.library.add(identifier)

    def episode(self, catalog, vid=OLD, title="An episode"):
        with catalog.db() as db:
            db.execute("INSERT OR IGNORE INTO episodes(id,title) VALUES(?,?)", (vid,title))
            db.execute("INSERT OR IGNORE INTO membership VALUES('uploads',?,0)", (vid,))

    def wait_for_refreshes(self, catalog):
        deadline = time.monotonic() + 5
        while any(r["status"] == "loading" for r in catalog.refreshes.values()):
            self.assertLess(time.monotonic(), deadline, catalog.refreshes)
            time.sleep(0.01)

    def test_new_source_prefetches_discovered_playlist_contents_without_audio(self):
        self.add();catalog=self.library.catalog(SECOND)
        def extract(url):
            if url.endswith(('/podcasts','/playlists')):
                return {'entries':[{'id':'PLfirstplaylist','title':'First'},{'id':'PLemptyplaylist','title':'Empty'}]}
            if 'list=PLfirstplaylist' in url:
                return {'entries':[{'id':OLD,'title':'Older episode'}]}
            if 'list=UU' in url:
                return {'entries':[{'id':NEW,'title':'Upload'}]}
            return {'entries':[]}
        with patch.object(catalog,'extract',side_effect=extract),patch.object(catalog,'enqueue') as download:
            self.library.schedule_catalog(catalog)
            self.wait_for_refreshes(catalog)
            snap=catalog.snapshot()
            self.assertEqual(snap['playlist_cache'],{'total':2,'cached':2,'loading':0,'failed':0})
            self.assertEqual(snap['memberships']['PLfirstplaylist'],[OLD])
            self.assertIn('PLemptyplaylist',snap['sources'],'Successful empty playlists are cached too')
            download.assert_not_called()

    def test_startup_prefetches_all_sources_independently_of_auto_download(self):
        self.add();other=self.library.catalog(SECOND)
        for catalog in (self.primary,other):
            with catalog.db() as db:
                db.execute("INSERT INTO playlists VALUES('PLpendinglist','Pending')")
                db.executemany('INSERT INTO sources VALUES(?,?)',[(source,time.time()) for source in ('playlists','shorts')])
        with patch.object(self.primary,'extract',return_value={'entries':[{'id':OLD,'title':'Primary'}]}),patch.object(other,'extract',return_value={'entries':[{'id':NEW,'title':'Second'}]}):
            self.library.maintain_catalogs()
            self.wait_for_refreshes(self.primary);self.wait_for_refreshes(other)
        self.assertEqual(self.primary.snapshot()['memberships']['PLpendinglist'],[OLD])
        self.assertEqual(other.snapshot()['memberships']['PLpendinglist'],[NEW])
        self.assertFalse(next(r for r in self.library.podcasts() if r['id']==SECOND)['auto_download'])

    def test_prefetch_is_bounded_deduplicated_and_keeps_failed_cache(self):
        catalog=self.primary
        with catalog.db() as db:
            db.executemany('INSERT INTO playlists VALUES(?,?)',[(f'PLplaylist{i:03d}',str(i)) for i in range(5)])
            db.execute("INSERT INTO membership VALUES('PLplaylist004',?,0)",(OLD,))
            db.execute("INSERT INTO sources VALUES('PLplaylist004',0)")
        release=threading.Event();lock=threading.Lock();active=0;peak=0;calls=[]
        def extract(url):
            nonlocal active,peak
            with lock:
                calls.append(url);active+=1;peak=max(peak,active)
            try:
                self.assertTrue(release.wait(5))
                if url.endswith('PLplaylist004'):raise ValueError('Temporarily unavailable')
                return {'entries':[]}
            finally:
                with lock:active-=1
        with patch.object(catalog,'extract',side_effect=extract),self.assertLogs(level='ERROR'):
            self.library.prefetch_playlists(catalog)
            self.library.prefetch_playlists(catalog)
            release.set();self.wait_for_refreshes(catalog)
        self.assertEqual(len(calls),5)
        self.assertLessEqual(peak,2)
        snap=catalog.snapshot()
        self.assertEqual(snap['playlist_cache'],{'total':5,'cached':5,'loading':0,'failed':1})
        self.assertEqual(snap['memberships']['PLplaylist004'],[OLD])
        self.assertEqual(snap['sources']['PLplaylist004'],0)
        with patch.object(catalog,'schedule_refresh') as schedule:
            self.library.prefetch_playlists(catalog)
            self.assertEqual(schedule.call_count,1,'Only the failed stale cache should be retried')

    def feed_episode(self, catalog, vid=OLD):
        path=catalog.audio/(vid+".m4a")
        path.write_bytes(b"fixture")
        catalog.add_to_feed({"id":vid,"title":"An episode","description":"Original description","duration":30,"upload_date":"20260304"},path)

    def test_new_channel_auto_off_idempotent_and_primary_feed_preserved(self):
        before=(self.primary.audio/"feed.rss").read_bytes()
        self.add();self.library.set_auto(SECOND,True);self.add()
        rows={r["id"]:r for r in self.library.podcasts()}
        self.assertTrue(rows[PRIMARY]["auto_download"])
        self.assertTrue(rows[SECOND]["auto_download"],"Adding a duplicate must preserve its setting")
        self.add(THIRD,"Third")
        self.assertFalse(next(r for r in self.library.podcasts() if r["id"]==THIRD)["auto_download"])
        self.assertEqual(before,(self.primary.audio/"feed.rss").read_bytes())
        self.assertEqual(len(self.library.podcasts()),3)
        self.assertNotEqual(self.library.catalog(SECOND).state,self.primary.state)
        self.assertIs(self.library.catalog(SECOND).pool,self.primary.pool)

    def test_archives_are_isolated_and_queue_reports_source(self):
        self.add();other=self.library.catalog(SECOND)
        self.episode(self.primary);self.episode(other)
        self.primary.archive([OLD],True)
        other.job(OLD,"queued")
        snapshot=self.library.snapshot(SECOND)
        self.assertFalse(next(e for e in snapshot["episodes"] if e["id"]==OLD)["archived"])
        self.assertEqual(snapshot["queue"][0]["podcast_id"],SECOND)
        self.assertEqual(snapshot["current_podcast"],SECOND)
        self.assertTrue(next(e for e in self.primary.snapshot()["episodes"] if e["id"]==OLD)["archived"])

    def test_combined_feed_stable_identity_and_global_exclusion(self):
        self.add();other=self.library.catalog(SECOND)
        self.feed_episode(self.primary,OLD);self.feed_episode(other,NEW)
        first=self.library.create_mix([PRIMARY,SECOND])
        self.assertEqual(first,self.library.create_mix([SECOND,PRIMARY,SECOND]))
        path=self.library.audio_root/"feeds"/first.rsplit("/",1)[1]
        mixed=ET.parse(path).findall("./channel/item")
        self.assertEqual(len(mixed),2)
        before=path.read_bytes();self.library.rebuild_feeds();self.assertEqual(before,path.read_bytes())
        global_items=ET.parse(self.library.audio_root/"feeds/global.rss").findall("./channel/item")
        self.assertEqual(len(global_items),1)
        self.assertTrue(global_items[0].findtext("title").startswith("Second podcast:"))
        original=ET.parse(other.audio/"feed.rss").find("./channel/item")
        self.assertEqual(global_items[0].findtext("guid"),original.findtext("guid"))
        self.assertEqual(global_items[0].findtext("pubDate"),original.findtext("pubDate"))
        self.assertEqual(global_items[0].findtext("description"),original.findtext("description"))
        self.add(THIRD,"Third podcast");third=self.library.catalog(THIRD);self.feed_episode(third,"12345678901")
        self.library.rebuild_feeds()
        self.assertEqual(len(ET.parse(self.library.audio_root/"feeds/global.rss").findall("./channel/item")),2)
        self.assertEqual(len(ET.parse(path).findall("./channel/item")),2,"Saved selections must not grow when another source is added")

    def test_auto_enable_baselines_existing_and_downloads_only_new(self):
        self.add();catalog=self.library.catalog(SECOND);self.episode(catalog,OLD)
        self.library.set_auto(SECOND,True)
        class YDL:
            def __init__(self,*args):pass
            def __enter__(self):return self
            def __exit__(self,*args):pass
            def extract_info(self,*args,**kwargs):return {"entries":[{"id":NEW,"title":"New"},{"id":OLD,"title":"Old"}]}
        with patch("yt_dlp.YoutubeDL",YDL),patch.object(catalog,"enqueue") as enqueue:
            self.library.poll_auto(SECOND)
            enqueue.assert_called_once_with(NEW)
            self.library.poll_auto(SECOND)
            self.assertEqual(enqueue.call_count,1)
            self.library.set_auto(SECOND,False)
            self.library.poll_auto(SECOND)
            self.assertEqual(enqueue.call_count,1)

    def test_first_auto_check_never_downloads_a_historical_catalog(self):
        class YDL:
            def __init__(self,*args):pass
            def __enter__(self):return self
            def __exit__(self,*args):pass
            def extract_info(self,*args,**kwargs):return {"entries":[{"id":OLD,"title":"Old"}]}
        with patch("yt_dlp.YoutubeDL",YDL),patch.object(self.primary,"enqueue") as enqueue:
            self.library.poll_auto(PRIMARY)
            enqueue.assert_not_called()

    def test_existing_enabled_primary_keeps_its_known_baseline(self):
        primary=web.Catalog(self.root/"legacy-state",self.root/"audio"/"legacy","https://example.com/yt/legacy",PRIMARY,"Existing")
        self.episode(primary,OLD)
        library=Library(primary,web.Catalog,policy_file=self.root/"legacy-policy.json")
        class YDL:
            def __init__(self,*args):pass
            def __enter__(self):return self
            def __exit__(self,*args):pass
            def extract_info(self,*args,**kwargs):return {"entries":[{"id":NEW,"title":"New"},{"id":OLD,"title":"Old"}]}
        try:
            with patch("yt_dlp.YoutubeDL",YDL),patch.object(primary,"enqueue") as enqueue:
                library.poll_auto(PRIMARY)
                enqueue.assert_called_once_with(NEW)
        finally:
            library.refresh_pool.shutdown(wait=True);library.playlist_pool.shutdown(wait=True);primary.pool.shutdown(wait=True)

    def test_legacy_cron_yields_to_library_for_on_and_off_channels(self):
        self.add()
        with patch.dict("os.environ",{"PODCAST_AUTO_POLICY":str(self.policy)}):
            self.assertTrue(managed_by_library(PRIMARY))
            self.assertTrue(managed_by_library(SECOND))
            self.assertFalse(managed_by_library(THIRD))
            self.assertTrue(managed_by_library("https://www.youtube.com/channel/"+PRIMARY))

    def test_download_completion_publishes_the_global_feed(self):
        self.add();catalog=self.library.catalog(SECOND);self.episode(catalog,OLD)
        path=catalog.audio/f"Downloaded [{OLD}].m4a"
        class YDL:
            def __init__(self,*args):pass
            def __enter__(self):return self
            def __exit__(self,*args):pass
            def extract_info(self,*args,**kwargs):
                path.write_bytes(b"fixture")
                return {"id":OLD,"title":"Downloaded","upload_date":"20260304"}
            def prepare_filename(self,info):return str(path.with_suffix(".webm"))
        with patch("yt_dlp.YoutubeDL",YDL):catalog.download(OLD)
        items=ET.parse(self.library.audio_root/"feeds/global.rss").findall("./channel/item")
        self.assertEqual(len(items),1)
        self.assertTrue(items[0].findtext("title").startswith("Second podcast:"))
        self.assertIn("Original upload date:",items[0].findtext("description"))

    def test_settings_and_saved_feed_selection_survive_restart(self):
        self.add();url=self.library.create_mix([PRIMARY,SECOND]);self.library.set_auto(PRIMARY,False)
        restored=Library(self.primary,web.Catalog,policy_file=self.policy)
        try:
            self.assertFalse(next(r for r in restored.podcasts() if r["id"]==PRIMARY)["auto_download"])
            self.assertEqual(restored.create_mix([SECOND,PRIMARY]),url)
        finally:
            restored.refresh_pool.shutdown(wait=True)
            restored.playlist_pool.shutdown(wait=True)

    def test_mix_validation_and_channel_url_validation(self):
        for value in ([],[THIRD],["../../secret"],"all"):
            with self.assertRaises(ValueError):self.library.create_mix(value)
        self.assertEqual(channel_url("@example"),"https://www.youtube.com/@example")
        self.assertEqual(channel_url("https://youtube.com/@example?utm_source=chat"),"https://www.youtube.com/@example")
        self.assertEqual(channel_url("youtube.com/@example"),"https://www.youtube.com/@example")
        self.assertTrue(channel_url("@آموزش").startswith("https://www.youtube.com/@%"))
        for value in ("https://attacker.example/@example","https://www.youtube.com/watch?v="+OLD,"file:///tmp/x"):
            with self.assertRaises(ValueError):channel_url(value)


if __name__=="__main__":
    unittest.main()
