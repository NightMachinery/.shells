"""Durable, human-readable library settings; fetched metadata stays in SQLite."""
from copy import deepcopy
import fcntl
import hashlib
import json
import logging
import os
from pathlib import Path
import re
import shlex
import subprocess
import tempfile
import threading
import time
import tomllib

SOURCE = re.compile(r"(?:UC[A-Za-z0-9_-]{22}|tg_[a-z][a-z0-9_]{4,31})\Z")
EPISODE = re.compile(r"(?:[A-Za-z0-9_-]{11}|tg_[1-9][0-9]{0,18})\Z")
PLAYLIST = re.compile(r"[A-Za-z0-9_-]{10,100}\Z")


def mix_id(sources):
    return hashlib.sha256("\n".join(sorted(set(sources))).encode()).hexdigest()[:24]


def validate(document):
    if document.get("version") != 1 or not SOURCE.fullmatch(document.get("primary", "")):
        raise ValueError("Unsupported settings version or primary source")
    podcasts = document.get("podcasts")
    if not isinstance(podcasts, list) or not podcasts:
        raise ValueError("Settings must contain registered podcasts")
    known = set()
    for podcast in podcasts:
        identifier = podcast.get("id", "")
        if not SOURCE.fullmatch(identifier) or identifier in known:
            raise ValueError("Invalid or duplicate podcast ID in settings")
        known.add(identifier)
        for field in ("title", "url"):
            if not isinstance(podcast.get(field), str) or not podcast[field]:
                raise ValueError("Settings require podcast " + field)
        for field in ("auto_download", "excluded_from_global"):
            if not isinstance(podcast.get(field), bool):
                raise ValueError("Settings require boolean " + field)
        for field, pattern in (("archived", EPISODE), ("playlists", PLAYLIST)):
            values = podcast.setdefault(field, [])
            if not isinstance(values, list) or not all(isinstance(v, str) and pattern.fullmatch(v) for v in values):
                raise ValueError("Invalid " + field + " in settings")
    if document["primary"] not in known:
        raise ValueError("Primary source is missing from settings")
    primary = next(p for p in podcasts if p["id"] == document["primary"])
    if not primary["excluded_from_global"]:
        raise ValueError("The primary source must stay excluded from the global feed")
    feeds = document.setdefault("feeds", [])
    if not isinstance(feeds, list):
        raise ValueError("Settings feeds must be a list")
    seen = set()
    for feed in feeds:
        sources = feed.get("sources")
        if not isinstance(sources, list) or not sources or not all(isinstance(v, str) and v in known for v in sources):
            raise ValueError("Saved feeds must reference registered sources")
        identifier = mix_id(sources)
        if identifier in seen or not isinstance(feed.get("title"), str):
            raise ValueError("Invalid or duplicate saved feed")
        seen.add(identifier)
    return document


def dumps(document):
    document = validate(deepcopy(document))
    quote = lambda value: json.dumps(value, ensure_ascii=False)
    lines = ["# Persistent podcast settings. Updated by the podcast app.",
             "# Catalogs, fetch times, jobs and credentials are stored separately.",
             "version = 1", "primary = " + quote(document["primary"])]
    for podcast in sorted(document["podcasts"], key=lambda p: p["id"]):
        lines.extend(["", "[[podcasts]]"])
        for field in ("id", "title", "url", "auto_download", "excluded_from_global"):
            lines.append(field + " = " + quote(podcast[field]))
        for field in ("archived", "playlists"):
            values = sorted(set(podcast[field]))
            lines.append(field + " = [" + ("" if values else "]"))
            if values:
                lines.extend("  " + quote(value) + "," for value in values)
                lines.append("]")
    for feed in sorted(document["feeds"], key=lambda f: mix_id(f["sources"])):
        lines.extend(["", "[[feeds]]", "title = " + quote(feed["title"]),
                      "sources = [" + ", ".join(quote(s) for s in sorted(set(feed["sources"]))) + "]"])
    return "\n".join(lines) + "\n"


class SettingsFile:
    def __init__(self, path, sync_command=None):
        self.path = Path(path).expanduser().resolve()
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self.lock = threading.RLock()
        self.sync_command = shlex.split(sync_command) if sync_command else None
        self.pending = threading.Event()
        self.stop = threading.Event()
        self.error = None
        self.last_synced = None

    def read(self):
        with self.path.open("rb") as stream:
            return validate(tomllib.load(stream))

    def write(self, document):
        content = dumps(document)
        if self.path.exists() and self.path.read_text() == content:
            return
        temporary = None
        try:
            with tempfile.NamedTemporaryFile(mode="w", encoding="utf-8", dir=self.path.parent,
                                             prefix="." + self.path.name, suffix=".tmp", delete=False) as stream:
                temporary = Path(stream.name)
                stream.write(content)
                stream.flush()
                os.fsync(stream.fileno())
            temporary.replace(self.path)
        finally:
            if temporary and temporary.exists():
                temporary.unlink()
        self.pending.set()

    def update(self, change):
        with self.lock, self.path.with_suffix(self.path.suffix + ".lock").open("a") as lock:
            fcntl.flock(lock, fcntl.LOCK_EX)
            document = self.read()
            change(document)
            self.write(document)

    def set_source(self, identifier, field, value):
        def change(document):
            podcast = next((p for p in document["podcasts"] if p["id"] == identifier), None)
            if podcast is None:
                raise ValueError("Source was removed from settings; restart to reload")
            podcast[field] = value
        self.update(change)

    def status(self):
        return {"configured": True, "sync_enabled": bool(self.sync_command),
                "pending": self.pending.is_set(), "error": self.error, "last_synced": self.last_synced}

    def sync_once(self):
        if not self.sync_command:
            return
        # Clear before running so edits made during a push schedule another pass.
        self.pending.clear()
        try:
            subprocess.run(self.sync_command, cwd=self.path.parent, check=True, timeout=120,
                           stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                           env=dict(os.environ, GIT_TERMINAL_PROMPT="0"))
            self.error, self.last_synced = None, time.time()
        except (subprocess.SubprocessError, OSError) as exc:
            self.error = "Settings saved locally; Git synchronization failed and will retry."
            self.pending.set()
            logging.warning("Persistent settings sync failed: %s", type(exc).__name__)

    def start(self):
        if not self.sync_command:
            return
        self.pending.set()
        def run():
            while not self.stop.is_set():
                if self.pending.wait(1):
                    self.sync_once()
                    if self.error:
                        self.stop.wait(60)
        threading.Thread(target=run, name="podcast-settings-sync", daemon=True).start()
