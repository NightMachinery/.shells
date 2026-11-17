"""Multi-channel registry, downloaded-audio feeds and opt-in new-upload checks."""
import concurrent.futures
from contextlib import contextmanager
from copy import deepcopy
import email.utils
import hashlib
import json
import logging
from pathlib import Path
import re
import sqlite3
import threading
import time
from urllib.parse import quote, unquote, urlsplit
import xml.etree.ElementTree as ET

from podcast_policy import write_policy

CHANNEL_ID = re.compile(r"UC[A-Za-z0-9_-]{22}\Z")
SOURCE_ID = re.compile(r"(?:UC[A-Za-z0-9_-]{22}|tg_[a-z][a-z0-9_]{4,31})\Z")


def channel_url(value):
    if not isinstance(value, str) or len(value) > 1000:
        raise ValueError("Enter a YouTube channel URL, handle or channel ID")
    value = value.strip()
    if value.startswith(("youtube.com/", "www.youtube.com/", "m.youtube.com/")):
        value = "https://" + value
    if CHANNEL_ID.fullmatch(value):
        return "https://www.youtube.com/channel/" + value
    if not value.startswith("https://"):
        if not re.fullmatch(r"@?[\w.-]{1,100}", value):
            raise ValueError("Expected a YouTube channel handle or URL")
        return "https://www.youtube.com/@" + quote(value.lstrip("@"), safe="._-")
    parsed = urlsplit(value)
    if parsed.hostname not in ("youtube.com", "www.youtube.com", "m.youtube.com") or parsed.port not in (None, 443) or parsed.username:
        raise ValueError("Only YouTube channel URLs are supported")
    path = unquote(parsed.path).rstrip("/")
    if not re.fullmatch(r"/(?:@[\w.-]+|(?:channel|c|user)/[\w.-]+)(?:/(?:videos|streams|shorts|podcasts|playlists))?", path):
        raise ValueError("Use a channel URL, rather than an episode or playlist URL")
    path = re.sub(r"/(?:videos|streams|shorts|podcasts|playlists)$", "", path)
    return "https://www.youtube.com" + quote(path, safe="/@._-")


class Library:
    def __init__(self, primary, catalog_class, *, policy_file=None, public_app_path=None, pwa_id=None,
                 settings_path=None, settings_sync=None):
        self.primary = primary.channel
        self.state = primary.state
        self.audio_root = primary.audio.parent
        self.url_root = primary.base_url.rsplit("/", 1)[0]
        self.catalog_class = catalog_class
        self.catalogs = {self.primary: primary}
        self.pool = primary.pool
        self.lock = threading.RLock()
        self.publish_lock = threading.RLock()
        self.policy_file = policy_file
        self.settings = None
        self.public_app_path, self.pwa_id = public_app_path, pwa_id
        self.refresh_pool = concurrent.futures.ThreadPoolExecutor(max_workers=2)
        # Large playlist histories must not hold up a newly added source's catalog.
        self.playlist_pool = concurrent.futures.ThreadPoolExecutor(max_workers=2)
        self.stop = threading.Event()
        with self.db() as db:
            db.executescript("""
            CREATE TABLE IF NOT EXISTS podcasts(id TEXT PRIMARY KEY, title TEXT NOT NULL, url TEXT NOT NULL,
              auto_download INTEGER NOT NULL DEFAULT 0, auto_ready INTEGER NOT NULL DEFAULT 0,
              checked REAL, error TEXT);
            CREATE TABLE IF NOT EXISTS auto_seen(podcast TEXT, video TEXT, PRIMARY KEY(podcast,video));
            CREATE TABLE IF NOT EXISTS mixes(id TEXT PRIMARY KEY, title TEXT NOT NULL, members TEXT NOT NULL);
            """)
            if "excluded_global" not in {r[1] for r in db.execute("PRAGMA table_info(podcasts)")}:
                db.execute("ALTER TABLE podcasts ADD COLUMN excluded_global INTEGER NOT NULL DEFAULT 0")
            created = db.execute("INSERT OR IGNORE INTO podcasts(id,title,url,auto_download) VALUES(?,?,?,1)",
                       (self.primary, primary.title, "https://www.youtube.com/channel/" + self.primary)).rowcount
            if created:
                # Preserve an already-enabled primary source's known baseline,
                # including the ability to pick up uploads newer than its cache.
                with primary.db() as existing:
                    known = [r[0] for r in existing.execute("SELECT video FROM membership WHERE playlist='uploads'")]
                db.executemany("INSERT OR IGNORE INTO auto_seen VALUES(?,?)", [(self.primary, vid) for vid in known])
                db.execute("UPDATE podcasts SET auto_ready=? WHERE id=?", (bool(known), self.primary))
            db.execute("UPDATE podcasts SET excluded_global=1 WHERE id=?", (self.primary,))
        self.configure_catalog(primary)
        if settings_path:
            from settings import SettingsFile
            settings = SettingsFile(settings_path, settings_sync)
            if not settings.path.exists():
                settings.write(self.export_settings())
            self.settings = settings
            self.apply_settings(settings.read())
        for row in self.podcasts():
            self.catalog(row["id"])
        self.write_policy()
        self.rebuild_feeds()

    @contextmanager
    def db(self):
        db = sqlite3.connect(self.state / "library.sqlite", timeout=30)
        db.row_factory = sqlite3.Row
        try:
            yield db
            db.commit()
        except BaseException:
            db.rollback()
            raise
        finally:
            db.close()

    def podcasts(self):
        with self.db() as db:
            return [dict(r) for r in db.execute("SELECT * FROM podcasts ORDER BY title COLLATE NOCASE")]

    def write_policy(self):
        write_policy(self.podcasts(), self.policy_file)

    def configure_catalog(self, catalog):
        catalog.on_download = self.rebuild_feeds
        catalog.on_playlists = self.prefetch_playlists
        catalog.on_archive = lambda ids, archived: self.save_archives(catalog.channel, ids, archived)
        catalog.on_playlist_added = lambda playlist: self.save_playlist(catalog.channel, playlist)

    def export_settings(self):
        podcasts = []
        for row in self.podcasts():
            catalog = self.catalog(row["id"])
            with catalog.db() as db:
                archived = [r[0] for r in db.execute("SELECT id FROM episodes WHERE archived=1 ORDER BY id")]
                playlists = [r[0] for r in db.execute("SELECT id FROM playlists ORDER BY id")]
            podcasts.append({"id": row["id"], "title": row["title"], "url": row["url"],
                             "auto_download": bool(row["auto_download"]), "excluded_from_global": bool(row["excluded_global"]),
                             "archived": archived, "playlists": playlists})
        with self.db() as db:
            feeds = [{"title": row["title"], "sources": json.loads(row["members"])} for row in db.execute("SELECT * FROM mixes")]
        return {"version": 1, "primary": self.primary, "podcasts": podcasts, "feeds": feeds}

    def apply_settings(self, document):
        from settings import mix_id
        if document["primary"] != self.primary:
            raise ValueError("Settings primary source does not match the configured audio library")
        previous = {r["id"]: r for r in self.podcasts()}
        identifiers = [p["id"] for p in document["podcasts"]]
        with self.db() as db:
            db.execute("DELETE FROM podcasts WHERE id NOT IN (" + ",".join("?" for _ in identifiers) + ")", identifiers)
            for podcast in document["podcasts"]:
                db.execute("""INSERT INTO podcasts(id,title,url,auto_download,excluded_global) VALUES(?,?,?,?,?)
                    ON CONFLICT(id) DO UPDATE SET title=excluded.title,url=excluded.url,
                    auto_download=excluded.auto_download,excluded_global=excluded.excluded_global""",
                    (podcast["id"],podcast["title"],podcast["url"],podcast["auto_download"],podcast["excluded_from_global"]))
            db.execute("DELETE FROM mixes")
            db.executemany("INSERT INTO mixes VALUES(?,?,?)", [(mix_id(f["sources"]), f["title"], json.dumps(sorted(set(f["sources"])))) for f in document["feeds"]])
        self.catalogs = {key: value for key,value in self.catalogs.items() if key in identifiers}
        for podcast in document["podcasts"]:
            catalog = self.catalog(podcast["id"])
            catalog.title = podcast["title"]
            with catalog.db() as db:
                db.execute("UPDATE episodes SET archived=0")
                db.executemany("INSERT INTO episodes(id,title,archived) VALUES(?,?,1) ON CONFLICT(id) DO UPDATE SET archived=1", [(vid,vid) for vid in podcast["archived"]])
                db.executemany("INSERT OR IGNORE INTO playlists VALUES(?,?)", [(pid,pid) for pid in podcast["playlists"]])
                known = [r[0] for r in db.execute("SELECT video FROM membership WHERE playlist='uploads'")]
            if podcast["auto_download"] and not previous.get(podcast["id"],{}).get("auto_download"):
                with self.db() as db:
                    db.execute("DELETE FROM auto_seen WHERE podcast=?", (podcast["id"],))
                    db.executemany("INSERT INTO auto_seen VALUES(?,?)", [(podcast["id"],vid) for vid in known])
                    db.execute("UPDATE podcasts SET auto_ready=? WHERE id=?", (bool(known),podcast["id"]))

    def save_archives(self, identifier, ids, archived):
        if self.settings:
            def change(document):
                podcast = next(p for p in document["podcasts"] if p["id"] == identifier)
                values = set(podcast["archived"])
                values.update(ids) if archived else values.difference_update(ids)
                podcast["archived"] = sorted(values)
            self.settings.update(change)

    def save_playlist(self, identifier, playlist):
        if self.settings:
            def change(document):
                podcast = next(p for p in document["podcasts"] if p["id"] == identifier)
                podcast["playlists"] = sorted(set(podcast["playlists"]) | {playlist})
            self.settings.update(change)

    def catalog(self, identifier=None):
        identifier = identifier or self.primary
        with self.lock:
            if identifier in self.catalogs:
                return self.catalogs[identifier]
            with self.db() as db:
                row = db.execute("SELECT * FROM podcasts WHERE id=?", (identifier,)).fetchone()
            if row is None:
                raise ValueError("Unknown podcast")
            cls = self.catalog_class
            if identifier.startswith("tg_"):
                from telegram_source import TelegramCatalog
                cls = TelegramCatalog
            catalog = cls(self.state / "channels" / identifier,
                self.audio_root / "channels" / identifier, self.url_root + "/channels/" + identifier,
                identifier, row["title"], pool=self.pool, on_download=self.rebuild_feeds)
            self.configure_catalog(catalog)
            self.catalogs[identifier] = catalog
            self.ensure_feed(catalog)
            return catalog

    def ensure_feed(self, catalog):
        path = catalog.audio / "feed.rss"
        if path.exists():
            return
        root = ET.Element("rss", version="2.0")
        channel = ET.SubElement(root, "channel")
        for key, value in (("title", catalog.title), ("description", "Downloaded episodes from " + catalog.title),
                           ("link", catalog.channel_url)):
            ET.SubElement(channel, key).text = value
        self.write_xml(path, root)

    @staticmethod
    def write_xml(path, root):
        path.parent.mkdir(parents=True, exist_ok=True)
        content = ET.tostring(root, encoding="utf-8", xml_declaration=True)
        if path.exists() and path.read_bytes() == content:
            return
        temporary = path.with_suffix(path.suffix + ".tmp")
        temporary.write_bytes(content)
        temporary.replace(path)

    @staticmethod
    def resolve(value):
        if isinstance(value, str) and re.match(r"(?:https://)?(?:t\.me|telegram\.me)/", value.strip()):
            from telegram_source import resolve
            return resolve(value)
        from yt_dlp import YoutubeDL
        url = channel_url(value)
        with YoutubeDL({"extract_flat": "in_playlist", "playlistend": 1, "skip_download": True,
                        "quiet": True, "socket_timeout": 30, "retries": 1}) as ydl:
            info = ydl.extract_info(url + "/videos", download=False)
        identifier = info.get("channel_id", "")
        if not CHANNEL_ID.fullmatch(identifier):
            raise ValueError("YouTube did not return a channel ID")
        return identifier, info.get("channel") or info.get("uploader") or info.get("title") or identifier, url

    def add(self, value, *, excluded=None):
        if excluded is not None and not isinstance(excluded, bool):
            raise ValueError("Global feed exclusion must be true or false")
        try:
            identifier, title, url = self.resolve(value)
        except Exception as exc:
            raise ValueError("Could not read that channel. Check its URL and retry. " + str(exc)[-250:]) from exc
        with self.lock:
            if self.settings:
                def change(document):
                    if not any(p["id"] == identifier for p in document["podcasts"]):
                        document["podcasts"].append({"id":identifier,"title":title,"url":url,"auto_download":False,
                                                     "excluded_from_global":bool(excluded),"archived":[],"playlists":[]})
                self.settings.update(change)
            with self.db() as db:
                db.execute("INSERT OR IGNORE INTO podcasts(id,title,url,excluded_global) VALUES(?,?,?,?)", (identifier, title, url, bool(excluded)))
            catalog = self.catalog(identifier)
            self.write_policy()
        with catalog.db() as db:
            cached = db.execute("SELECT 1 FROM sources WHERE id='uploads'").fetchone()
        if not cached and catalog.refreshes.get("uploads", {}).get("status") != "loading":
            self.schedule_catalog(catalog)
        self.rebuild_feeds()
        return identifier

    def schedule_catalog(self, catalog):
        sources = ("uploads",) if catalog.source_type == "telegram" else ("uploads", "playlists", "shorts")
        with catalog.lock:
            for source in sources:
                catalog.refreshes[source] = {"status": "loading", "error": None}
        def fetch():
            for source in sources:
                catalog.refresh(source)
        self.refresh_pool.submit(fetch)

    def prefetch_playlists(self, catalog, max_age=21600):
        if catalog.source_type != "youtube":
            return
        with catalog.db() as db:
            playlists = list(db.execute("""SELECT p.id,s.updated FROM playlists p
                LEFT JOIN sources s ON s.id=p.id ORDER BY p.id"""))
        for playlist in playlists:
            if self.stop.is_set():
                return
            if playlist["updated"] is None or time.time() - playlist["updated"] > max_age:
                catalog.schedule_refresh(playlist["id"], executor=self.playlist_pool)

    def maintain_catalogs(self, max_age=21600):
        for row in self.podcasts():
            catalog = self.catalog(row["id"])
            with catalog.db() as db:
                uploads = db.execute("SELECT 1 FROM sources WHERE id='uploads'").fetchone()
            if not uploads and catalog.refreshes.get("uploads", {}).get("status") != "loading":
                self.schedule_catalog(catalog)
            if catalog.source_type != "youtube":
                continue
            with catalog.db() as db:
                cached = {r["id"]: r["updated"] for r in db.execute("SELECT id,updated FROM sources WHERE id IN ('playlists','shorts')")}
            for source in ("playlists", "shorts"):
                if time.time() - cached.get(source, 0) > max_age:
                    catalog.schedule_refresh(source, executor=self.refresh_pool)
            self.prefetch_playlists(catalog, max_age)

    def set_auto(self, identifier, enabled):
        if not isinstance(enabled, bool):
            raise ValueError("auto_download must be true or false")
        catalog = self.catalog(identifier)
        identifier = catalog.channel
        if self.settings:
            self.settings.set_source(identifier, "auto_download", enabled)
        with self.lock, self.db() as db:
            old = db.execute("SELECT auto_download FROM podcasts WHERE id=?", (identifier,)).fetchone()
            if enabled and not old[0]:
                db.execute("DELETE FROM auto_seen WHERE podcast=?", (identifier,))
                with catalog.db() as episodes:
                    current = [r[0] for r in episodes.execute("SELECT video FROM membership WHERE playlist='uploads'")]
                db.executemany("INSERT OR IGNORE INTO auto_seen VALUES(?,?)", [(identifier, vid) for vid in current])
                db.execute("UPDATE podcasts SET auto_ready=? WHERE id=?", (bool(current), identifier))
            db.execute("UPDATE podcasts SET auto_download=?,error=NULL WHERE id=?", (enabled, identifier))
        self.write_policy()

    def set_excluded(self, identifier, excluded):
        if not isinstance(excluded, bool):
            raise ValueError("Global feed exclusion must be true or false")
        catalog = self.catalog(identifier)
        if catalog.channel == self.primary and not excluded:
            raise ValueError("The original podcast is always excluded from the global feed")
        if self.settings:
            self.settings.set_source(catalog.channel, "excluded_from_global", excluded)
        with self.db() as db:
            db.execute("UPDATE podcasts SET excluded_global=? WHERE id=?", (excluded, catalog.channel))
        self.rebuild_feeds()

    def snapshot(self, identifier=None):
        selected = self.catalog(identifier)
        data = selected.snapshot()
        channels, queue = [], []
        for row in self.podcasts():
            catalog = self.catalog(row["id"])
            snap = data if catalog is selected else catalog.snapshot()
            channels.append({"id": row["id"], "title": row["title"], "url": row["url"],
                "auto_download": bool(row["auto_download"]), "auto_error": row["error"],
                "feed_url": snap["feed_url"], "downloaded": sum(bool(e["audio_url"]) for e in snap["episodes"]),
                "episodes": len(snap["memberships"].get("uploads", [])), "excluded_from_global": bool(row["excluded_global"]),
                "source_type": catalog.source_type, "exclusion_locked": row["id"] == self.primary,
                "playlist_cache": snap["playlist_cache"],
                "loading": any(r.get("status") == "loading" for r in snap["refreshes"].values())})
            queue.extend(dict(e, podcast_id=row["id"], podcast_title=row["title"]) for e in snap["episodes"] if e.get("job"))
        data.update(podcasts=channels, current_podcast=selected.channel, queue=queue,
                    global_feed_url=self.url_root + "/feeds/global.rss",
                    settings_sync=self.settings.status() if self.settings else {"configured": False})
        return data

    def create_mix(self, identifiers):
        if not isinstance(identifiers, list) or not identifiers or len(identifiers) > 100:
            raise ValueError("Check at least one podcast")
        if not all(isinstance(i, str) and SOURCE_ID.fullmatch(i) for i in identifiers):
            raise ValueError("Invalid podcast ID")
        members = sorted(set(identifiers))
        known = {row["id"] for row in self.podcasts()}
        if not set(members) <= known:
            raise ValueError("Unknown podcast in feed selection")
        key = hashlib.sha256("\n".join(members).encode()).hexdigest()[:24]
        if self.settings:
            from settings import mix_id
            def change(document):
                if not any(mix_id(feed["sources"]) == key for feed in document["feeds"]):
                    document["feeds"].append({"title":"Selected podcasts","sources":members})
            self.settings.update(change)
        with self.db() as db:
            db.execute("INSERT OR IGNORE INTO mixes VALUES(?,?,?)", (key, "Selected podcasts", json.dumps(members)))
        self.rebuild_feeds()
        return self.url_root + "/feeds/" + key + ".rss"

    def rebuild_feeds(self):
        with self.publish_lock:
            sources = self.podcasts()
            for source in sources:
                self.ensure_feed(self.catalog(source["id"]))
            with self.db() as db:
                mixes = [dict(r) for r in db.execute("SELECT * FROM mixes")]
            mixes.append({"id": "global", "title": "All podcasts", "members": json.dumps([r["id"] for r in sources if not r["excluded_global"]])})
            for mix in mixes:
                root = ET.Element("rss", version="2.0")
                channel = ET.SubElement(root, "channel")
                ET.SubElement(channel, "title").text = mix["title"]
                ET.SubElement(channel, "link").text = self.url_root
                ET.SubElement(channel, "description").text = "Downloaded episodes from the selected podcast sources."
                members, entries, seen = set(json.loads(mix["members"])), [], set()
                try:
                    for source in sources:
                        if source["id"] not in members:
                            continue
                        for item in ET.parse(self.catalog(source["id"]).audio / "feed.rss").findall("./channel/item"):
                            enclosure = item.find("enclosure")
                            if enclosure is None:
                                continue
                            identity = item.findtext("guid") or enclosure.get("url")
                            if identity in seen:
                                continue
                            seen.add(identity)
                            item = deepcopy(item)
                            title = item.find("title")
                            if title is not None:
                                title.text = source["title"] + ": " + (title.text or "")
                            try:
                                stamp = email.utils.parsedate_to_datetime(item.findtext("pubDate")).timestamp()
                            except (ValueError, TypeError, AttributeError):
                                stamp = 0
                            entries.append((stamp, item))
                    for _, item in sorted(entries, key=lambda entry: entry[0], reverse=True):
                        channel.append(item)
                    self.write_xml(self.audio_root / "feeds" / (mix["id"] + ".rss"), root)
                except (ET.ParseError, OSError):
                    logging.exception("Feed is temporarily unavailable; retaining its previous aggregate")

    def poll_auto(self, identifier):
        from yt_dlp import YoutubeDL
        with self.db() as db:
            row = db.execute("SELECT * FROM podcasts WHERE id=?", (identifier,)).fetchone()
        if row is None or not row["auto_download"]:
            return
        catalog = self.catalog(identifier)
        try:
            if catalog.source_type == "telegram":
                info = {"entries": catalog.recent_entries()}
            else:
                with YoutubeDL({"extract_flat": "in_playlist", "playlistend": 50, "skip_download": True,
                            "quiet": True, "socket_timeout": 30, "retries": 1}) as ydl:
                    info = ydl.extract_info("https://www.youtube.com/playlist?list=UU" + identifier[2:], download=False)
            from server import EPISODE
            entries = [e for e in info.get("entries", []) if e and EPISODE.fullmatch(e.get("id", ""))]
            with self.lock, self.db() as db:
                current = db.execute("SELECT auto_download,auto_ready FROM podcasts WHERE id=?", (identifier,)).fetchone()
                if not current["auto_download"]:
                    return
                with catalog.db() as episodes:
                    existing = [r[0] for r in episodes.execute("SELECT video FROM membership WHERE playlist='uploads' ORDER BY position")]
                    for e in entries:
                        catalog.upsert_episode(episodes, e)
                    ids = [e["id"] for e in entries]
                    order = ids + [vid for vid in existing if vid not in set(ids)]
                    episodes.execute("DELETE FROM membership WHERE playlist='uploads'")
                    episodes.executemany("INSERT INTO membership VALUES('uploads',?,?)", [(vid, i) for i, vid in enumerate(order)])
                seen = {r[0] for r in db.execute("SELECT video FROM auto_seen WHERE podcast=?", (identifier,))}
                if not current["auto_ready"]:
                    fresh = []
                    baseline = set(existing) | set(ids)
                    db.executemany("INSERT OR IGNORE INTO auto_seen VALUES(?,?)", [(identifier, vid) for vid in baseline])
                else:
                    fresh = [vid for vid in reversed(ids) if vid not in seen]
                    db.executemany("INSERT OR IGNORE INTO auto_seen VALUES(?,?)", [(identifier, vid) for vid in fresh])
                db.execute("UPDATE podcasts SET auto_ready=1,checked=?,error=NULL WHERE id=?", (time.time(), identifier))
            for vid in fresh:
                catalog.enqueue(vid)
        except Exception as exc:
            logging.exception("Automatic upload check failed for %s", identifier)
            with self.db() as db:
                db.execute("UPDATE podcasts SET checked=?,error=? WHERE id=?", (time.time(), str(exc)[-400:], identifier))

    def start(self, interval=1800):
        if self.settings:
            self.settings.start()
        def run():
            while not self.stop.is_set():
                self.maintain_catalogs()
                for row in self.podcasts():
                    if row["auto_download"]:
                        self.poll_auto(row["id"])
                self.rebuild_feeds()
                self.stop.wait(interval)
        threading.Thread(target=run, name="podcast-auto", daemon=True).start()
