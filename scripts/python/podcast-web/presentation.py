"""Episode display titles and podcast feed artwork."""

from functools import lru_cache
import hashlib
from io import BytesIO
from pathlib import Path
import re
import urllib.request
from urllib.parse import urlsplit
import xml.etree.ElementTree as ET

ITUNES = "{http://www.itunes.com/dtds/podcast-1.0.dtd}"
ET.register_namespace("itunes", ITUNES[1:-1])
MEDIA_SUFFIX = re.compile(r"\.(?:mp3|m4a|m4b|ogg|opus|wav|flac|aac)$", re.IGNORECASE)
TRAILING_MARKS = re.compile(r"[\s\u200e\u200f\u202a-\u202e\u2066-\u2069]+$")
ARTWORK_NAME = "podcast-cover.jpg"
MAX_IMAGE_BYTES = 5 * 1024 * 1024


def episode_title(title, source_type):
    """Treat Telegram attachment names and titles ending in audio extensions as filenames."""
    name = TRAILING_MARKS.sub("", title).lstrip()
    stem = MEDIA_SUFFIX.sub("", name)
    if source_type == "telegram" or stem != name:
        return " ".join(stem.replace("_", " ").split()) or title
    return title


def image_bytes(url, *, host_suffix):
    parts = urlsplit(url)
    host = parts.hostname or ""
    if parts.scheme != "https" or not (host == host_suffix or host.endswith("." + host_suffix)):
        raise ValueError("Unexpected channel image host")

    class DirectImage(urllib.request.HTTPRedirectHandler):
        def redirect_request(self, *args, **kwargs):
            return None

    with urllib.request.build_opener(DirectImage()).open(url, timeout=20) as response:
        data = response.read(MAX_IMAGE_BYTES + 1)
    if len(data) > MAX_IMAGE_BYTES:
        raise ValueError("Channel image exceeded its size limit")
    return data


def publish_cover(data, directory):
    from PIL import Image, ImageOps

    with Image.open(BytesIO(data)) as image:
        if image.width * image.height > 16_000_000:
            raise ValueError("Channel image exceeded its pixel limit")
        image = ImageOps.fit(ImageOps.exif_transpose(image).convert("RGB"), (1400, 1400), Image.Resampling.LANCZOS)
        output = BytesIO()
        image.save(output, format="JPEG", quality=88, optimize=True)
    path = directory / ARTWORK_NAME
    directory.mkdir(parents=True, exist_ok=True)
    content = output.getvalue()
    if path.exists() and path.read_bytes() == content:
        return
    temporary = path.with_suffix(".jpg.tmp")
    temporary.write_bytes(content)
    temporary.replace(path)


@lru_cache(maxsize=128)
def cover_revision(path, mtime_ns, size):
    return hashlib.sha256(path.read_bytes()).hexdigest()[:16]


def cover_url(directory, base_url):
    path = directory / ARTWORK_NAME
    try:
        stat = path.stat()
        revision = cover_revision(path, stat.st_mtime_ns, stat.st_size)
    except FileNotFoundError:
        return None
    return f"{base_url}/{ARTWORK_NAME}?v={revision}"


def set_cover(element, url, *, channel=False):
    itunes = element.find(ITUNES + "image")
    if itunes is None:
        itunes = ET.SubElement(element, ITUNES + "image")
    itunes.set("href", url)
    if channel:
        image = element.find("image")
        if image is None:
            image = ET.SubElement(element, "image")
        for key, value in (("url", url), ("title", element.findtext("title")), ("link", element.findtext("link"))):
            child = image.find(key)
            if child is None:
                child = ET.SubElement(image, key)
            child.text = value
