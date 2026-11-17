import importlib.util
import json
from pathlib import Path
import tempfile
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
        self.primary.pool.shutdown(wait=True)
        self.tmp.cleanup()

    def add(self, identifier=SECOND, title="Second podcast"):
        with patch.object(self.library, "resolve", return_value=(identifier,title,"https://www.youtube.com/channel/"+identifier)), patch.object(self.library,"schedule_catalog"):
            return self.library.add(identifier)

    def episode(self, catalog, vid=OLD, title="An episode"):
        with catalog.db() as db:
            db.execute("INSERT OR IGNORE INTO episodes(id,title) VALUES(?,?)", (vid,title))
            db.execute("INSERT OR IGNORE INTO membership VALUES('uploads',?,0)", (vid,))

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
            library.refresh_pool.shutdown(wait=True);primary.pool.shutdown(wait=True)

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
        finally:restored.refresh_pool.shutdown(wait=True)

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
