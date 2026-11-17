import http.client
import importlib.util
import json
from pathlib import Path
import tempfile
import threading
import time
import unittest
from unittest.mock import patch
import xml.etree.ElementTree as ET
from podcast_dates import append_upload_date, format_upload_date

spec = importlib.util.spec_from_file_location("podcast_web", Path(__file__).with_name("server.py"))
web = importlib.util.module_from_spec(spec)
spec.loader.exec_module(web)
ID = "abcdefghijk"
CHANNEL = "UC" + "x" * 22


class CatalogTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.catalog = web.Catalog(self.root / "state", self.root / "audio", "https://example.com/audio", CHANNEL, "Test library")

    def tearDown(self):
        self.catalog.pool.shutdown(wait=True)
        self.temp.cleanup()

    def seed(self, vid=ID, title="An older episode"):
        with self.catalog.db() as db:
            db.execute("INSERT OR IGNORE INTO episodes(id,title) VALUES(?,?)", (vid, title))
            db.execute("INSERT OR IGNORE INTO membership VALUES('uploads',?,0)", (vid,))

    def test_downloaded_matching_and_archive_survive_refresh(self):
        self.seed(title="سلام: یک گفت‌وگو")
        path = self.catalog.audio / "سلام یک گفتوگو.m4a"
        path.write_bytes(b"audio fixture")
        snap = self.catalog.snapshot()
        remote = next(e for e in snap["episodes"] if e["id"] == ID)
        self.assertIn("%", remote["audio_url"])
        self.assertEqual(len(snap["episodes"]), 1, "Local and catalog rows must not duplicate the same audio")
        self.catalog.archive([ID], True)
        self.catalog.extract = lambda url: {"entries": [{"id": ID, "title": "Renamed title"}]}
        self.catalog.refresh("uploads")
        restarted = web.Catalog(self.catalog.state, self.catalog.audio, self.catalog.base_url, CHANNEL, "Test library")
        try:
            self.assertTrue(next(e for e in restarted.snapshot()["episodes"] if e["id"] == ID)["archived"])
            restarted.archive([ID], False)
            self.assertFalse(next(e for e in restarted.snapshot()["episodes"] if e["id"] == ID)["archived"])
        finally:
            restarted.pool.shutdown()
        self.assertTrue(path.exists())

    def test_existing_metadata_maps_renamed_audio_by_video_id(self):
        self.seed(title="New YouTube title")
        (self.catalog.audio / "Old title.m4a").write_bytes(b"fixture")

        class Media(dict):
            info = type("Info", (), {"length": 120})()

        media = Media(title=["Old title"], comment=[f"https://www.youtube.com/watch?v={ID}"], date=["2020-02-03"])
        with patch("mutagen.File", return_value=media):
            snap = self.catalog.snapshot()
        rows = [e for e in snap["episodes"] if e["audio_url"]]
        self.assertEqual(len(rows), 1)
        self.assertEqual(rows[0]["id"], ID)

    def test_complete_catalog_playlist_and_failed_refresh_preserves_cache(self):
        entries = [{"id": f"{i:011d}", "title": f"Episode {i}"} for i in range(123)]
        self.catalog.extract = lambda url: {"entries": entries}
        self.catalog.refresh("uploads")
        self.assertEqual(len(self.catalog.snapshot()["memberships"]["uploads"]), 123)
        self.catalog.extract = lambda url: {"entries": [{"id": "PLabcdefghijk", "title": "A playlist"}]}
        self.catalog.refresh("playlists")
        self.catalog.extract = lambda url: {"entries": entries[100:]}
        self.catalog.refresh("PLabcdefghijk")
        self.assertEqual(len(self.catalog.snapshot()["memberships"]["PLabcdefghijk"]), 23)
        self.catalog.extract = lambda url: (_ for _ in ()).throw(ValueError("offline"))
        with self.assertLogs(level="ERROR"):
            self.catalog.refresh("uploads")
        self.assertEqual(len(self.catalog.snapshot()["memberships"]["uploads"]), 123)
        self.assertEqual(self.catalog.refreshes["uploads"]["status"], "error")

    def test_feed_update_preserves_old_guids_and_is_idempotent(self):
        feed = self.catalog.audio / "feed.rss"
        feed.write_text('<rss version="2.0"><channel><title>Keep me</title><item><title>Old</title><guid>stable-guid</guid><enclosure url="https://example.com/old.m4a"/></item></channel></rss>')
        audio = self.catalog.audio / f"New title [{ID}].m4a"
        audio.write_bytes(b"fixture")
        info = {"id": ID, "title": "New title", "description": "<b>Text</b>", "duration": 123, "upload_date": "20260304"}
        self.catalog.add_to_feed(info, audio)
        once = feed.read_bytes()
        self.catalog.add_to_feed(info, audio)
        self.assertEqual(feed.read_bytes(), once)
        tree = ET.parse(feed)
        self.assertEqual(tree.findtext("./channel/title"), "Keep me")
        self.assertEqual(tree.findtext("./channel/item/guid"), "stable-guid")
        self.assertEqual(len(tree.findall("./channel/item")), 2)
        self.assertTrue(tree.findall("./channel/item")[1].findtext("description").endswith("1404/Esfand12/13 Wednesday 4/March3/2026"))

    def test_original_date_format_and_no_invented_dates(self):
        self.assertEqual(format_upload_date("20260304"), "1404/Esfand12/13 Wednesday 4/March3/2026")
        self.assertEqual(format_upload_date("2024-03-20"), "1403/Farvardin1/1 Wednesday 20/March3/2024")
        once = append_upload_date("Description", "20260304")
        self.assertEqual(append_upload_date(once, "20260304"), once)
        self.assertEqual(append_upload_date("Description", None), "Description")
        self.assertEqual(append_upload_date("Description", "2024"), "Description")
        self.assertEqual(append_upload_date("Description", "20260230"), "Description")

    def test_cli_metadata_and_feed_use_original_date_not_fake_date(self):
        import importlib.util
        import sys
        parent = Path(__file__).resolve().parents[1]
        sys.path.insert(0, str(parent))
        cli_spec = importlib.util.spec_from_file_location("podcaster_cli", parent / "podcaster.py")
        cli = importlib.util.module_from_spec(cli_spec)
        cli_spec.loader.exec_module(cli)
        episode = {"url": "https://example.com/test.m4a", "title": "Test", "description": "Original", "upload_date": "20260304", "date": 1, "file_path": "test.m4a", "size": 10, "duration": 20}
        feed = self.root / "cli.rss"
        cli.generate_rss_feed([dict(episode), dict(episode, url="https://example.com/second.m4a")], title="Test", description="Test", author="Test", album_art_url=None, output_path=str(feed), link_url="https://example.com", fake_dates=True)
        for item in ET.parse(feed).findall("./channel/item"):
            self.assertTrue(item.findtext("description").endswith("1404/Esfand12/13 Wednesday 4/March3/2026"))
            self.assertNotEqual(item.findtext("pubDate"), "Wed, 04 Mar 2026 00:00:00 GMT")

    def test_backfill_changes_only_descriptions_and_is_idempotent(self):
        import backfill_descriptions
        audio = self.catalog.audio / "original.m4a"
        audio.write_bytes(b"fixture")
        feed = self.catalog.audio / "feed.rss"
        feed.write_text('<rss><channel><item><title>Original</title><description>Keep this text</description><guid>stable-id</guid><pubDate>Mon, 01 Jan 2024 00:00:00 GMT</pubDate><enclosure url="https://example.com/original.m4a" length="7" type="audio/mp4"/></item></channel></rss>')
        with patch.object(backfill_descriptions, "File", return_value={"date": ["20260304"]}):
            preview = backfill_descriptions.backfill(self.catalog.audio)
            self.assertEqual(preview["updated"], 1)
            self.assertNotIn("Original upload date", feed.read_text())
            result = backfill_descriptions.backfill(self.catalog.audio, apply=True)
            self.assertEqual(result["updated"], 1)
            once = feed.read_bytes()
            self.assertEqual(backfill_descriptions.backfill(self.catalog.audio, apply=True)["unchanged"], 1)
            self.assertEqual(feed.read_bytes(), once)
        item = ET.parse(feed).find("./channel/item")
        self.assertEqual(item.findtext("guid"), "stable-id")
        self.assertEqual(item.findtext("pubDate"), "Mon, 01 Jan 2024 00:00:00 GMT")
        self.assertEqual(item.find("enclosure").get("length"), "7")
        self.assertTrue(item.findtext("description").startswith("Keep this text\n\nOriginal upload date:"))
        self.assertTrue(feed.with_name("feed.before-upload-dates.rss").exists())

    def test_channel_without_playlist_tab_and_manual_playlist(self):
        def extract(url):
            if url.endswith("/podcasts"):
                return {"entries": [{"id": "PLpodcastabcde", "title": "Podcast collection"}]}
            raise ValueError("This channel does not have a playlists tab")
        self.catalog.extract = extract
        self.catalog.refresh("playlists")
        self.assertEqual(self.catalog.refreshes["playlists"]["status"], "ready")
        self.assertEqual(self.catalog.snapshot()["playlists"][0]["title"], "Podcast collection")
        with patch.object(self.catalog, "schedule_refresh") as refresh:
            pid = self.catalog.add_playlist("https://www.youtube.com/playlist?list=PLabcdefghijk")
            refresh.assert_called_once_with("PLabcdefghijk")
            self.assertEqual(pid, "PLabcdefghijk")
        with self.assertRaises(ValueError):
            self.catalog.add_playlist("https://attacker.example/playlist?list=PLabcdefghijk")

    def test_backfill_reads_legacy_unescaped_filename(self):
        import backfill_descriptions
        audio = self.catalog.audio / "shorts #5870.m4a"
        audio.write_bytes(b"fixture")
        feed = self.catalog.audio / "feed.rss"
        feed.write_text('<rss><channel><item><description>Original</description><guid>stable</guid><enclosure url="https://example.com/shorts #5870.m4a"/></item></channel></rss>')
        with patch.object(backfill_descriptions, "File", return_value={"date": ["20260304"]}) as read:
            self.assertEqual(backfill_descriptions.backfill(self.catalog.audio, apply=True)["updated"], 1)
            self.assertEqual(read.call_args.args[0], audio)
        item = ET.parse(feed).find("./channel/item")
        self.assertEqual(item.find("enclosure").get("url"), "https://example.com/shorts #5870.m4a")
        self.assertEqual(item.findtext("guid"), "stable")

    def test_queue_deduplicates_and_respects_cli_lock(self):
        self.seed()
        started, release = threading.Event(), threading.Event()
        original = self.catalog.download_locked
        def download(vid):
            started.set()
            release.wait(3)
            self.catalog.job(vid, "done")
        self.catalog.download_locked = download
        lock = (self.catalog.audio.parent / ".podcaster.lock").open("a")
        web.fcntl.flock(lock, web.fcntl.LOCK_EX)
        try:
            self.catalog.enqueue(ID)
            self.catalog.enqueue(ID)
            self.assertFalse(started.wait(.15), "Worker must wait for the CLI's feed lock")
            self.assertEqual(len(self.catalog.busy), 1)
            web.fcntl.flock(lock, web.fcntl.LOCK_UN)
            self.assertTrue(started.wait(2))
        finally:
            release.set()
            lock.close()
            self.catalog.pool.shutdown(wait=True)
            self.catalog.download_locked = original

    def test_mock_download_updates_audio_feed_and_shared_archive(self):
        self.seed()
        audio = self.catalog.audio / f"An older episode [{ID}].m4a"
        class YDL:
            def __init__(self, options): self.options = options
            def __enter__(self): return self
            def __exit__(self, *args): pass
            def extract_info(self, url, download):
                audio.write_bytes(b"fixture")
                return {"id": ID, "title": "An older episode", "duration": 45}
            def prepare_filename(self, info): return str(audio.with_suffix(".webm"))
        with patch("yt_dlp.YoutubeDL", YDL):
            self.catalog.download(ID)
        row = next(e for e in self.catalog.snapshot()["episodes"] if e["id"] == ID)
        self.assertTrue(row["audio_url"].endswith(".m4a"))
        self.assertEqual(row["job"]["status"], "done")
        self.assertIn(f"youtube {ID}", (self.catalog.audio.parent / ".downloaded.txt").read_text())
        self.assertEqual(len(ET.parse(self.catalog.audio / "feed.rss").findall("./channel/item")), 1)

    def test_http_rejects_cross_origin_and_invalid_payloads(self):
        self.seed()
        server = web.ThreadingHTTPServer(("127.0.0.1", 0), web.Handler)
        server.catalog = self.catalog
        worker = threading.Thread(target=server.serve_forever, daemon=True)
        worker.start()
        host = f"127.0.0.1:{server.server_port}"
        def post(path, body, extra):
            connection = http.client.HTTPConnection("127.0.0.1", server.server_port)
            connection.request("POST", path, json.dumps(body), headers={"Content-Type": "application/json", **extra})
            response = connection.getresponse()
            result = response.status
            response.read()
            connection.close()
            return result
        try:
            self.assertEqual(post("/archive", {"ids": [ID], "archived": True}, {}), 403)
            self.assertEqual(post("/archive", {"ids": [ID], "archived": True}, {"X-Podcast-Action": "1", "Origin": "https://attacker.example"}), 403)
            headers = {"X-Podcast-Action": "1", "Origin": f"http://{host}"}
            self.assertEqual(post("/archive", {"ids": [ID], "archived": True}, headers), 202)
            self.assertEqual(post("/archive", {"ids": [ID], "archived": "false"}, headers), 400)
            self.assertEqual(post("/download", {"id": "$(printf sentinel)"}, headers), 400)
            self.assertEqual(post("/refresh", {"source": []}, headers), 400)
        finally:
            server.shutdown()
            server.server_close()
            worker.join()


if __name__ == "__main__":
    unittest.main()
