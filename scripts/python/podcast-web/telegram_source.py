"""Public Telegram audio catalogs, with a dedicated read-only user session for files."""
import asyncio
from contextlib import closing
from datetime import datetime
from html.parser import HTMLParser
import logging
import os
from pathlib import Path
import re
import sqlite3
import time
import urllib.request
from urllib.parse import urlsplit

from server import Catalog
from presentation import image_bytes

HANDLE = re.compile(r"[a-z][a-z0-9_]{4,31}\Z")


def channel_handle(value):
    if not isinstance(value, str):
        raise ValueError("Enter a public Telegram channel URL")
    value = value.strip()
    if value.startswith(("t.me/", "telegram.me/")):
        value = "https://" + value
    parsed = urlsplit(value)
    handle = parsed.path.strip("/").removeprefix("s/").lower()
    if (parsed.scheme != "https" or parsed.hostname not in ("t.me", "telegram.me")
            or parsed.username or parsed.port not in (None, 443) or parsed.query or parsed.fragment
            or not HANDLE.fullmatch(handle)):
        raise ValueError("Use a public Telegram channel URL, without a post number or invite link")
    return handle


class Node:
    def __init__(self, tag="", attrs=()):
        self.tag, self.attrs, self.children = tag, dict(attrs), []

    def has(self, name):
        return name in self.attrs.get("class", "").split()

    def find(self, predicate):
        if predicate(self):
            yield self
        for child in self.children:
            if isinstance(child, Node):
                yield from child.find(predicate)

    def text(self):
        return "".join(c.text() if isinstance(c, Node) else c for c in self.children).strip()


class PreviewParser(HTMLParser):
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.root = Node()
        self.stack = [self.root]

    def handle_starttag(self, tag, attrs):
        node = Node(tag, attrs)
        self.stack[-1].children.append(node)
        if tag == "br":
            self.stack[-1].children.append("\n")
        if tag not in {"area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr"}:
            self.stack.append(node)

    def handle_endtag(self, tag):
        for index in range(len(self.stack) - 1, 0, -1):
            if self.stack[index].tag == tag:
                del self.stack[index:]
                break

    def handle_data(self, data):
        self.stack[-1].children.append(data)


def parse_preview(html, handle):
    parser = PreviewParser()
    parser.feed(html)
    root = parser.root
    title = next((n.text() for n in root.find(lambda n: n.has("tgme_channel_info_header_title"))), handle)
    entries, posts = [], []
    for node in root.find(lambda n: n.has("tgme_widget_message") and n.attrs.get("data-post", "").startswith(handle + "/")):
        post = node.attrs["data-post"].split("/")[-1]
        if not re.fullmatch(r"[1-9][0-9]{0,18}", post):
            continue
        posts.append(int(post))
        if not any(node.find(lambda n: n.has("audio") or n.tag == "audio" or n.has("tgme_widget_message_voice"))):
            continue
        caption = next((n.text() for n in node.find(lambda n: n.has("tgme_widget_message_text"))), "")
        heading = next((n.text() for n in node.find(lambda n: n.has("tgme_widget_message_document_title"))), "")
        date = next((n.attrs.get("datetime") for n in node.find(lambda n: n.tag == "time")), None)
        stamp = datetime.fromisoformat(date) if date else None
        entries.append({"id": "tg_" + post, "title": heading or caption.split("\n")[0][:160] or "Audio " + post,
                        "description": caption, "duration": None,
                        "upload_date": stamp.strftime("%Y%m%d") if stamp else None})
    return title, entries, min(posts) if posts else None


def public_html(handle, before=None):
    url = "https://t.me/s/" + handle + ("?before=" + str(before) if before else "")
    request = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(request, timeout=30) as response:
        raw = response.read(8 * 1024 * 1024 + 1)
    if len(raw) > 8 * 1024 * 1024:
        raise ValueError("Telegram preview exceeded its size limit")
    return raw.decode("utf-8")


def public_page(handle, before=None):
    return parse_preview(public_html(handle, before), handle)


def resolve(value):
    handle = channel_handle(value)
    title, _, oldest = public_page(handle)
    if oldest is None:
        raise ValueError("No public channel history is available at that URL")
    return "tg_" + handle, title, "https://t.me/" + handle


def user_client():
    from telethon import TelegramClient
    from telethon.crypto import AuthKey
    from telethon.sessions import StringSession
    path = os.environ.get("PODCAST_TELEGRAM_SESSION")
    if not path:
        raise ValueError("Telegram audio needs a configured downloader user session")
    path = Path(path).expanduser()
    if path.suffix != ".session":
        path = Path(str(path) + ".session")
    with closing(sqlite3.connect(path.resolve().as_uri() + "?mode=ro", uri=True)) as db:
        row = db.execute("SELECT dc_id,server_address,port,auth_key FROM sessions LIMIT 1").fetchone()
    if not row or not row[3]:
        raise ValueError("Telegram downloader session is not authorized")
    session = StringSession()
    session.set_dc(row[0], row[1], row[2])
    session.auth_key = AuthKey(row[3])
    api_id, api_hash = os.environ.get("TELEGRAM_API_ID"), os.environ.get("TELEGRAM_API_HASH")
    if not api_id or not api_hash:
        with (Path.home() / ".telegram-config").open() as config:
            api_id, api_hash = config.readline().strip(), config.readline().strip()
    return TelegramClient(session, int(api_id), api_hash)


def audio_message(message):
    return bool(message and message.file and (message.audio or message.voice))


def message_entry(message):
    title = next((getattr(a, "title", None) for a in message.document.attributes if getattr(a, "title", None)), None)
    return {"id": "tg_" + str(message.id), "title": title or message.file.name or (message.message or "").split("\n")[0][:160] or "Audio " + str(message.id),
            "description": message.message or "", "duration": message.file.duration,
            "upload_date": message.date.strftime("%Y%m%d")}


class TelegramCatalog(Catalog):
    source_type = "telegram"

    def channel_picture(self):
        if not os.environ.get("PODCAST_TELEGRAM_SESSION"):
            parser = PreviewParser()
            parser.feed(public_html(self.handle))
            photo = next(parser.root.find(lambda node: node.has("tgme_page_photo_image")), None)
            avatar = photo.attrs.get("src") if photo else None
            return image_bytes(avatar, host_suffix="telesco.pe") if avatar else None

        async def download():
            client = user_client()
            try:
                await client.connect()
                if not await client.is_user_authorized():
                    raise ValueError("Telegram artwork requires an authorized session")
                return await client.download_profile_photo(self.handle, file=bytes)
            finally:
                await client.disconnect()

        return asyncio.run(asyncio.wait_for(download(), timeout=30))

    @property
    def handle(self):
        return self.channel.removeprefix("tg_")

    @property
    def channel_url(self):
        return "https://t.me/" + self.handle

    def episode_url(self, identifier):
        return self.channel_url + "/" + identifier.removeprefix("tg_")

    def recent_entries(self):
        if os.environ.get("PODCAST_TELEGRAM_SESSION"):
            return asyncio.run(self.fetch_messages(limit=100))
        return public_page(self.handle)[1]

    async def fetch_messages(self, limit=None):
        client = user_client()
        try:
            await client.connect()
            if not await client.is_user_authorized() or (await client.get_me()).bot:
                raise ValueError("Telegram history requires an authorized user session, not a bot")
            return [message_entry(m) async for m in client.iter_messages(self.handle, limit=limit) if audio_message(m)]
        finally:
            await client.disconnect()

    def refresh(self, source):
        try:
            if source != "uploads":
                raise ValueError("Telegram channels have an audio-post catalog, without YouTube playlists")
            if os.environ.get("PODCAST_TELEGRAM_SESSION"):
                entries = asyncio.run(self.fetch_messages())
            else:
                entries, before, seen = [], None, set()
                for _ in range(5000):
                    _, page, oldest = public_page(self.handle, before)
                    entries.extend(e for e in page if e["id"] not in seen)
                    seen.update(e["id"] for e in page)
                    if oldest is None or oldest == 1 or (before is not None and oldest >= before):
                        break
                    before = oldest
                    time.sleep(0.1)
                else:
                    raise ValueError("Telegram history exceeded its page limit; retaining the cached catalog")
            with self.db() as db:
                db.execute("DELETE FROM membership WHERE playlist='uploads'")
                for position, entry in enumerate(entries):
                    self.upsert_episode(db, entry)
                    db.execute("INSERT OR IGNORE INTO membership VALUES('uploads',?,?)", (entry["id"], position))
                db.execute("INSERT INTO sources VALUES('uploads',?) ON CONFLICT(id) DO UPDATE SET updated=excluded.updated", (time.time(),))
            with self.lock:
                self.refreshes[source] = {"status": "ready", "error": None}
        except Exception as exc:
            logging.exception("Telegram catalog refresh failed")
            with self.lock:
                self.refreshes[source] = {"status": "error", "error": str(exc)[-400:]}

    def download_locked(self, identifier):
        async def download():
            client = user_client()
            temporary = None
            try:
                await client.connect()
                if not await client.is_user_authorized() or (await client.get_me()).bot:
                    raise ValueError("Telegram audio downloads require an authorized user session")
                message = await client.get_messages(self.handle, ids=int(identifier.removeprefix("tg_")))
                if not audio_message(message):
                    raise ValueError("This Telegram audio post is no longer available")
                entry = message_entry(message)
                extension = message.file.ext or ".ogg"
                if extension.lower() not in (".mp3", ".m4a", ".m4b", ".ogg", ".opus", ".wav"):
                    raise ValueError("This Telegram attachment is not a supported audio file")
                path = self.audio / ("Audio [" + identifier + "]" + extension.lower())
                temporary = path.with_suffix(path.suffix + ".part")
                self.job(identifier, "downloading")
                if not path.is_file() or not path.stat().st_size:
                    await client.download_media(message, file=str(temporary))
                    if not temporary.is_file() or not temporary.stat().st_size:
                        raise ValueError("Telegram returned an empty audio file")
                    temporary.replace(path)
                self.add_to_feed(entry, path)
                with self.db() as db:
                    self.upsert_episode(db, entry)
                self.scan_audio(force=True)
                self.job(identifier, "done")
                if self.on_download:
                    self.on_download()
            finally:
                if temporary and temporary.exists():
                    temporary.unlink()
                await client.disconnect()
        try:
            asyncio.run(download())
        except Exception as exc:
            logging.exception("Telegram audio download failed")
            self.job(identifier, "error", str(exc)[-400:])
