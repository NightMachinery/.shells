#!/usr/bin/env python3
"""Private podcast catalog. Deployment-specific values come from the environment."""
import argparse
import concurrent.futures
import fcntl
import email.utils
import hashlib
import json
import logging
import os
from pathlib import Path
import re
import sqlite3
import threading
import time
import unicodedata
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import quote, unquote, urlsplit
import xml.etree.ElementTree as ET

VIDEO = re.compile(r"^[A-Za-z0-9_-]{11}$")
PLAYLIST = re.compile(r"^[A-Za-z0-9_-]{10,100}$")


def normalized(title):
    return "".join(c for c in unicodedata.normalize("NFKC", title).replace("ي", "ی").replace("ك", "ک").casefold() if c.isalnum())


class Catalog:
    def __init__(self, state, audio, base_url, channel, title):
        self.state, self.audio = Path(state), Path(audio)
        self.state.mkdir(parents=True, exist_ok=True)
        self.audio.mkdir(parents=True, exist_ok=True)
        self.base_url, self.channel, self.title = base_url.rstrip("/"), channel, title
        self.pool = concurrent.futures.ThreadPoolExecutor(max_workers=1)
        self.lock = threading.RLock()
        self.busy = set()
        self.refreshes = {}
        self.last_scan = 0
        self.audio_by_id, self.audio_by_title = {}, {}
        self.file_cache = {}
        with self.db() as db:
            db.executescript("""
            PRAGMA journal_mode=WAL;
            CREATE TABLE IF NOT EXISTS episodes(id TEXT PRIMARY KEY, title TEXT NOT NULL, duration REAL, published TEXT, archived INTEGER DEFAULT 0);
            CREATE TABLE IF NOT EXISTS playlists(id TEXT PRIMARY KEY, title TEXT NOT NULL);
            CREATE TABLE IF NOT EXISTS membership(playlist TEXT, video TEXT, position INTEGER, PRIMARY KEY(playlist,video));
            CREATE TABLE IF NOT EXISTS sources(id TEXT PRIMARY KEY, updated REAL);
            CREATE TABLE IF NOT EXISTS jobs(id TEXT PRIMARY KEY, status TEXT, error TEXT, updated REAL);
            UPDATE jobs SET status='error', error='Interrupted by restart; retry the download.' WHERE status IN ('queued','downloading');
            """)

    def db(self):
        db = sqlite3.connect(self.state / "catalog.sqlite", timeout=30)
        db.row_factory = sqlite3.Row
        return Transaction(db)

    def extract(self, url):
        from yt_dlp import YoutubeDL
        with YoutubeDL({"extract_flat": "in_playlist", "skip_download": True, "quiet": True,
                        "ignoreerrors": False, "socket_timeout": 30, "retries": 2}) as ydl:
            return ydl.extract_info(url, download=False)

    def schedule_refresh(self, source="uploads"):
        if not isinstance(source, str):
            raise ValueError("Expected a source name")
        if source not in ("uploads", "playlists"):
            with self.db() as db:
                if not db.execute("SELECT 1 FROM playlists WHERE id=?", (source,)).fetchone():
                    raise ValueError("Unknown playlist")
        with self.lock:
            if self.refreshes.get(source, {}).get("status") == "loading":
                return
            self.refreshes[source] = {"status": "loading", "error": None}
            # Catalog fetches do not wait behind audio downloads.
            threading.Thread(target=self.refresh, args=(source,), daemon=True).start()

    def refresh(self, source):
        try:
            if source == "playlists":
                # YouTube can expose podcast collections only on the Podcasts
                # tab, even when the channel has no Playlists tab at all.
                entries = []
                for tab in ("podcasts", "playlists"):
                    try:
                        result = self.extract(f"https://www.youtube.com/channel/{self.channel}/{tab}")
                        entries.extend(e for e in result.get("entries", []) if e)
                    except Exception as exc:
                        if "does not have a" in str(exc) and " tab" in str(exc):
                            continue
                        raise
                info = {"entries": entries}
            else:
                url = f"https://www.youtube.com/playlist?list={'UU' + self.channel[2:] if source == 'uploads' else source}"
                info = self.extract(url)
            if not info or "entries" not in info:
                raise ValueError("YouTube returned no catalog; try refresh again.")
            entries = [e for e in info["entries"] if e]
            with self.db() as db:
                if source == "playlists":
                    for e in entries:
                        pid = e.get("id", "")
                        if PLAYLIST.fullmatch(pid):
                            db.execute("INSERT INTO playlists VALUES(?,?) ON CONFLICT(id) DO UPDATE SET title=excluded.title", (pid, e.get("title") or pid))
                else:
                    if source != "uploads" and info.get("title"):
                        db.execute("UPDATE playlists SET title=? WHERE id=?", (info["title"], source))
                    # Replace membership only after a successful complete fetch.
                    db.execute("DELETE FROM membership WHERE playlist=?", (source,))
                    for pos, e in enumerate(entries):
                        vid = e.get("id", "")
                        if not VIDEO.fullmatch(vid):
                            continue
                        published = e.get("upload_date")
                        if not published and e.get("timestamp"):
                            published = time.strftime("%Y%m%d", time.gmtime(e["timestamp"]))
                        db.execute("INSERT INTO episodes(id,title,duration,published) VALUES(?,?,?,?) ON CONFLICT(id) DO UPDATE SET title=excluded.title,duration=coalesce(excluded.duration,episodes.duration),published=coalesce(excluded.published,episodes.published)",
                                   (vid, e.get("title") or vid, e.get("duration"), published))
                        db.execute("INSERT OR IGNORE INTO membership VALUES(?,?,?)", (source, vid, pos))
                db.execute("INSERT INTO sources VALUES(?,?) ON CONFLICT(id) DO UPDATE SET updated=excluded.updated", (source, time.time()))
            with self.lock:
                self.refreshes[source] = {"status": "ready", "error": None}
        except Exception as exc:
            logging.exception("Catalog refresh failed")
            with self.lock:
                self.refreshes[source] = {"status": "error", "error": str(exc)[-600:]}

    def scan_audio(self, force=False):
        with self.lock:
            if not force and time.monotonic() - self.last_scan < 15:
                return
            by_id, by_title, records = {}, {}, []
            for p in sorted(self.audio.iterdir()):
                if p.suffix.lower() not in (".m4a", ".mp3", ".m4b", ".ogg", ".opus", ".wav") or not p.is_file() or not p.stat().st_size:
                    continue
                url = self.base_url + "/" + quote(p.name, safe="")
                match = re.search(r"\[([A-Za-z0-9_-]{11})\]$", p.stem)
                if match:
                    by_id[match[1]] = url
                by_title[normalized(p.stem)] = url
                if match:
                    by_title[normalized(p.stem[:match.start()].strip())] = url
                fingerprint = (p.stat().st_size, p.stat().st_mtime_ns)
                cached = self.file_cache.get(p.name)
                if not cached or cached[0] != fingerprint:
                    metadata = {}
                    try:
                        from mutagen import File
                        media = File(p, easy=True)
                        if media:
                            metadata = {"title": media.get("title", [p.stem])[0],
                                        "duration": int(media.info.length),
                                        "published": re.sub(r"\D", "", media.get("date", [""])[0])[:8] or None}
                            comments = " ".join(media.get("comment", []) + media.get("purl", []))
                            source = re.search(r"(?:youtu\.be/|[?&]v=)([A-Za-z0-9_-]{11})", comments)
                            if source:
                                metadata["id"] = source[1]
                    except Exception:
                        logging.debug("Could not read audio tags for %s", p.name, exc_info=True)
                    self.file_cache[p.name] = (fingerprint, metadata)
                metadata = self.file_cache[p.name][1]
                vid = match[1] if match else metadata.get("id")
                if vid:
                    by_id[vid] = url
                by_title[normalized(metadata.get("title", p.stem))] = url
                records.append((vid or hashlib.sha256(url.encode()).hexdigest()[:11],
                                metadata.get("title", p.stem), metadata.get("duration"), metadata.get("published")))
            # Existing filenames were sanitized by yt-dlp; RSS retains their real titles.
            feed = self.audio / "feed.rss"
            if feed.exists():
                try:
                    for item in ET.parse(feed).findall("./channel/item"):
                        enclosure = item.find("enclosure")
                        if enclosure is None:
                            continue
                        filename = Path(unquote(urlsplit(enclosure.get("url", "")).path)).name
                        p = self.audio / filename
                        if p.is_file() and p.stat().st_size:
                            by_title[normalized(item.findtext("title", ""))] = self.base_url + "/" + quote(filename, safe="")
                except ET.ParseError:
                    logging.warning("Feed is being rewritten; using filenames until next scan")
            self.audio_by_id, self.audio_by_title = by_id, by_title
            with self.db() as db:
                db.executemany("INSERT OR IGNORE INTO episodes(id,title,duration,published) VALUES(?,?,?,?)", records)
            self.last_scan = time.monotonic()

    def snapshot(self):
        self.scan_audio()
        with self.db() as db:
            episodes = [dict(e) for e in db.execute("SELECT * FROM episodes")]
            memberships = {}
            for m in db.execute("SELECT playlist,video FROM membership ORDER BY position"):
                memberships.setdefault(m["playlist"], []).append(m["video"])
            playlists = [dict(p) for p in db.execute("SELECT * FROM playlists ORDER BY title")]
            jobs = {j["id"]: dict(j) for j in db.execute("SELECT * FROM jobs")}
            sources = {r["id"]: r["updated"] for r in db.execute("SELECT * FROM sources")}
        for e in episodes:
            e["audio_url"] = self.audio_by_id.get(e["id"]) or self.audio_by_title.get(normalized(e["title"]))
            e["archived"] = bool(e["archived"])
            e["job"] = jobs.get(e["id"])
            e["local"] = bool(e["audio_url"] and hashlib.sha256(e["audio_url"].encode()).hexdigest()[:11] == e["id"])
        remote_urls = {e["audio_url"] for e in episodes if e["audio_url"] and not e["local"]}
        episodes = [e for e in episodes if not e["local"] or e["audio_url"] not in remote_urls]
        with self.lock:
            refreshes = dict(self.refreshes)
        return {"title": self.title, "episodes": episodes, "playlists": playlists, "memberships": memberships,
                "refreshes": refreshes, "sources": sources, "feed_url": self.base_url + "/feed.rss"}

    def archive(self, ids, archived):
        if not isinstance(archived, bool) or not isinstance(ids, list) or not 1 <= len(ids) <= 500 or not all(isinstance(i, str) and VIDEO.fullmatch(i) for i in ids):
            raise ValueError("Expected up to 500 video IDs and a boolean archived value")
        with self.db() as db:
            db.executemany("UPDATE episodes SET archived=? WHERE id=?", [(archived, i) for i in ids])

    def add_playlist(self, value):
        if not isinstance(value, str):
            raise ValueError("Enter a YouTube playlist URL or ID")
        value = value.strip()
        if value.startswith("https://"):
            from urllib.parse import parse_qs
            parts = urlsplit(value)
            if parts.hostname not in ("youtube.com", "www.youtube.com", "m.youtube.com", "music.youtube.com"):
                raise ValueError("Expected a YouTube playlist URL")
            value = parse_qs(parts.query).get("list", [""])[0]
        if not PLAYLIST.fullmatch(value):
            raise ValueError("Expected a YouTube playlist URL or ID")
        with self.db() as db:
            db.execute("INSERT OR IGNORE INTO playlists VALUES(?,?)", (value, "Loading playlist…"))
        self.schedule_refresh(value)
        return value

    def enqueue(self, vid):
        if not isinstance(vid, str) or not VIDEO.fullmatch(vid):
            raise ValueError("Invalid video ID")
        with self.db() as db:
            e = db.execute("SELECT * FROM episodes WHERE id=?", (vid,)).fetchone()
        if not e:
            raise ValueError("Unknown episode")
        self.scan_audio()
        if self.audio_by_id.get(vid) or self.audio_by_title.get(normalized(e["title"])):
            return
        with self.lock:
            if vid in self.busy:
                return
            self.busy.add(vid)
            self.job(vid, "queued")
            self.pool.submit(self.download, vid)

    def job(self, vid, status, error=None):
        with self.db() as db:
            db.execute("INSERT INTO jobs VALUES(?,?,?,?) ON CONFLICT(id) DO UPDATE SET status=excluded.status,error=excluded.error,updated=excluded.updated", (vid, status, error, time.time()))

    def download(self, vid):
        try:
            with (self.audio.parent / ".podcaster.lock").open("a") as lock:
                fcntl.flock(lock, fcntl.LOCK_EX)
                self.download_locked(vid)
        except Exception as exc:
            logging.exception("Download failed for %s", vid)
            self.job(vid, "error", str(exc)[-600:])
        finally:
            with self.lock:
                self.busy.discard(vid)

    def download_locked(self, vid):
        try:
            from yt_dlp import YoutubeDL
            self.scan_audio(force=True)
            with self.db() as db:
                episode = db.execute("SELECT title FROM episodes WHERE id=?", (vid,)).fetchone()
            # An automatic update may have downloaded it while this job waited.
            if self.audio_by_id.get(vid) or self.audio_by_title.get(normalized(episode["title"])):
                self.job(vid, "done")
                return
            self.job(vid, "downloading")
            options = {"format": "bestaudio[ext=m4a]/bestaudio/best", "outtmpl": str(self.audio / "%(title)s [%(id)s].%(ext)s"),
                       "noplaylist": True, "quiet": True, "socket_timeout": 30, "retries": 3, "overwrites": False,
                       "postprocessors": [{"key": "FFmpegExtractAudio", "preferredcodec": "m4a"}, {"key": "FFmpegMetadata"}]}
            with YoutubeDL(options) as ydl:
                info = ydl.extract_info(f"https://www.youtube.com/watch?v={vid}", download=True)
                path = Path(ydl.prepare_filename(info)).with_suffix(".m4a")
            if not path.is_file() or not path.stat().st_size:
                raise ValueError("Download did not produce an audio file")
            self.add_to_feed(info, path)
            # Share the CLI's download archive so future automatic runs skip it.
            archive = self.audio.parent / ".downloaded.txt"
            with archive.open("a") as f:
                f.write(f"youtube {vid}\n")
            self.scan_audio(force=True)
            self.job(vid, "done")
        except Exception as exc:
            logging.exception("Download failed for %s", vid)
            self.job(vid, "error", str(exc)[-600:])

    def add_to_feed(self, info, path):
        feed = self.audio / "feed.rss"
        # The legacy updater writes this same feed. An optimistic compare prevents
        # overwriting its output if it changes while we prepare our atomic update.
        for attempt in range(5):
            original = feed.read_bytes() if feed.exists() else None
            tree = ET.fromstring(original) if original else ET.Element("rss", version="2.0")
            channel = tree.find("channel")
            if channel is None:
                channel = ET.SubElement(tree, "channel")
                for key, value in (("title", self.title), ("link", self.base_url), ("description", self.title)):
                    ET.SubElement(channel, key).text = value
            url = self.base_url + "/" + quote(path.name, safe="")
            if any(Path(unquote(urlsplit(e.get("url", "")).path)).name == path.name for e in channel.findall("item/enclosure")):
                return
            item = ET.SubElement(channel, "item")
            ET.SubElement(item, "title").text = info.get("title") or path.stem
            ET.SubElement(item, "description").text = info.get("description") or ""
            # Match the CLI's raw (unescaped) URL when generating its GUID, so
            # rebuilding the feed later does not make this item look new again.
            ET.SubElement(item, "guid", isPermaLink="false").text = hashlib.md5(f"{unquote(self.base_url)}/{path.name}|{path.stat().st_mtime}".encode()).hexdigest()
            ET.SubElement(item, "enclosure", url=url, length=str(path.stat().st_size), type="audio/mp4")
            ET.SubElement(item, "pubDate").text = email.utils.formatdate(path.stat().st_mtime, usegmt=True)
            if info.get("duration"):
                seconds = int(info["duration"])
                ET.SubElement(item, "{http://www.itunes.com/dtds/podcast-1.0.dtd}duration").text = f"{seconds//3600}:{seconds//60%60:02}:{seconds%60:02}"
            temp = feed.with_name(".feed.web.tmp")
            temp.write_bytes(ET.tostring(tree, encoding="utf-8", xml_declaration=True))
            if (feed.read_bytes() if feed.exists() else None) == original:
                temp.replace(feed)
                return
            time.sleep(0.2)
        raise RuntimeError("Feed changed during update; audio saved. Retry after the automatic updater finishes.")


class Transaction:
    def __init__(self, db): self.db = db
    def __enter__(self): return self.db
    def __exit__(self, kind, value, tb):
        try:
            self.db.rollback() if kind else self.db.commit()
        finally:
            self.db.close()


class Handler(BaseHTTPRequestHandler):
    def reply(self, data, status=200, content_type="application/json; charset=utf-8"):
        body = json.dumps(data, ensure_ascii=False).encode() if content_type.startswith("application/json") else data
        self.send_response(status)
        self.send_header("Content-Type", content_type)
        self.send_header("Cache-Control", "no-store")
        self.send_header("X-Content-Type-Options", "nosniff")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        path = urlsplit(self.path).path
        if path == "/health":
            self.reply({"ok": True})
        elif path == "/catalog":
            self.reply(self.server.catalog.snapshot())
        else:
            self.reply({"error": "Not found"}, 404)

    def do_POST(self):
        # JSON plus this header forces cross-origin requests through a preflight,
        # which this server never permits. Also reject hostile browser origins.
        origin = self.headers.get("Origin")
        host = self.headers.get("X-Forwarded-Host", self.headers.get("Host"))
        if (self.headers.get("X-Podcast-Action") != "1" or
            self.headers.get("Content-Type", "").split(";")[0] != "application/json" or
            (origin and urlsplit(origin).netloc != host) or self.headers.get("Sec-Fetch-Site") == "cross-site"):
            self.reply({"error": "Same-origin JSON request required"}, 403)
            return
        try:
            length = int(self.headers.get("Content-Length", "0"))
            if not 0 < length <= 32768:
                raise ValueError("Invalid request size")
            data = json.loads(self.rfile.read(length))
            if not isinstance(data, dict):
                raise ValueError("Expected a JSON object")
            if self.path == "/archive":
                self.server.catalog.archive(data.get("ids"), data.get("archived"))
            elif self.path == "/download":
                self.server.catalog.enqueue(data.get("id"))
            elif self.path == "/refresh":
                self.server.catalog.schedule_refresh(data.get("source", "uploads"))
            elif self.path == "/playlist":
                playlist = self.server.catalog.add_playlist(data.get("url"))
                self.reply({"ok": True, "id": playlist}, 202)
                return
            else:
                self.reply({"error": "Not found"}, 404)
                return
            self.reply({"ok": True}, 202)
        except (ValueError, TypeError, KeyError) as exc:
            self.reply({"error": str(exc)}, 400)
        except Exception:
            logging.exception("Request failed")
            self.reply({"error": "Server error; please retry."}, 500)


def main():
    logging.basicConfig(level=logging.INFO)
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--state", default=os.environ.get("PODCAST_STATE", "~/.local/state/podcast-web"))
    parser.add_argument("--audio-dir", default=os.environ.get("PODCAST_AUDIO_DIR"), required=not os.environ.get("PODCAST_AUDIO_DIR"))
    parser.add_argument("--base-url", default=os.environ.get("PODCAST_BASE_URL"), required=not os.environ.get("PODCAST_BASE_URL"))
    parser.add_argument("--channel", default=os.environ.get("PODCAST_CHANNEL"), required=not os.environ.get("PODCAST_CHANNEL"))
    parser.add_argument("--title", default=os.environ.get("PODCAST_TITLE", "Podcast library"))
    parser.add_argument("--port", type=int, default=int(os.environ.get("PORT", "8812")))
    args = parser.parse_args()
    if not re.fullmatch(r"UC[A-Za-z0-9_-]{22}", args.channel):
        parser.error("Expected a YouTube channel ID")
    catalog = Catalog(Path(args.state).expanduser(), Path(args.audio_dir).expanduser(), args.base_url, args.channel, args.title)
    server = ThreadingHTTPServer(("127.0.0.1", args.port), Handler)
    server.catalog = catalog
    for source in ("uploads", "playlists"):
        with catalog.db() as db:
            cached = db.execute("SELECT updated FROM sources WHERE id=?", (source,)).fetchone()
        if not cached or time.time() - cached["updated"] > 21600:
            catalog.schedule_refresh(source)
    server.serve_forever()


if __name__ == "__main__":
    main()
