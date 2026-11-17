"""Episode display titles and podcast feed artwork."""

from email.utils import formatdate
from functools import lru_cache
import hashlib
from io import BytesIO
import math
import os
from pathlib import Path
import re
import urllib.request
from urllib.parse import unquote, urlsplit
import xml.etree.ElementTree as ET

ITUNES = "{http://www.itunes.com/dtds/podcast-1.0.dtd}"
ET.register_namespace("itunes", ITUNES[1:-1])
MEDIA_SUFFIX = re.compile(r"\.(?:mp3|m4a|m4b|ogg|opus|wav|flac|aac)$", re.IGNORECASE)
TRAILING_MARKS = re.compile(r"[\s\u200e\u200f\u202a-\u202e\u2066-\u2069]+$")
ARTWORK_NAME = "podcast-cover.jpg"
PUBLISHED_COVER = re.compile(r"podcast-cover(?:-rss)?-[a-f0-9]{16}\.jpg$")
MAX_IMAGE_BYTES = 5 * 1024 * 1024


def episode_title(title, source_type):
    """Treat Telegram attachment names and titles ending in audio extensions as filenames."""
    name = TRAILING_MARKS.sub("", title).lstrip()
    stem = MEDIA_SUFFIX.sub("", name)
    if source_type == "telegram" or stem != name:
        return " ".join(stem.replace("_", " ").split()) or title
    return title


def duration_seconds(value):
    """Read numeric seconds or a legacy RSS clock value."""
    try:
        if isinstance(value, str) and ":" in value:
            parts = value.split(":")
            if len(parts) not in (2, 3):
                return None
            value = sum(float(part) * 60 ** index for index, part in enumerate(reversed(parts)))
        seconds = float(value)
    except (TypeError, ValueError):
        return None
    return int(seconds) if math.isfinite(seconds) and seconds >= 1 else None


def set_duration(item, value):
    element = item.find(ITUNES + "duration")
    seconds = duration_seconds(value) or duration_seconds(element.text if element is not None else None)
    if seconds is not None:
        if element is None:
            element = ET.SubElement(item, ITUNES + "duration")
        element.text = str(seconds)
    elif element is not None:
        item.remove(element)


def mark_feed_updated(channel):
    stamp = channel.find("lastBuildDate")
    if stamp is None:
        stamp = ET.Element("lastBuildDate")
        first_item = channel.find("item")
        channel.insert(list(channel).index(first_item) if first_item is not None else len(channel), stamp)
    stamp.text = formatdate(usegmt=True)


def enclosure_name(url, file_cache):
    name = Path(unquote(urlsplit(url).path)).name
    # Some older feeds used literal # and ? in local filenames.
    return name if name in file_cache else unquote(url.rsplit("/", 1)[-1])


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
    directory.mkdir(parents=True, exist_ok=True)
    write_image(directory / ARTWORK_NAME, output.getvalue())
    prepare_cover_files(directory)


def write_image(path, content):
    if path.exists() and path.read_bytes() == content:
        return
    temporary = path.with_suffix(".jpg.tmp")
    temporary.write_bytes(content)
    temporary.replace(path)


@lru_cache(maxsize=128)
def cover_revision(path, mtime_ns, size):
    return hashlib.sha256(path.read_bytes()).hexdigest()[:16]


def cover_names(directory):
    path = directory / ARTWORK_NAME
    try:
        stat = path.stat()
        revision = cover_revision(path, stat.st_mtime_ns, stat.st_size)
    except FileNotFoundError:
        return None
    return f"podcast-cover-{revision}.jpg", f"podcast-cover-rss-{revision}.jpg"


def prepare_cover_files(directory):
    """Publish immutable URLs for existing covers and the small RSS rendition."""
    names = cover_names(directory)
    if names is None:
        return
    large, small = (directory / name for name in names)
    if not large.exists():
        os.link(directory / ARTWORK_NAME, large)
    if not small.exists():
        from PIL import Image

        with Image.open(large) as image:
            thumbnail = image.resize((144, 144), Image.Resampling.LANCZOS)
        output = BytesIO()
        thumbnail.save(output, "JPEG", quality=88, optimize=True)
        write_image(small, output.getvalue())


def cover_url(directory, base_url, *, rss=False):
    names = cover_names(directory)
    if names:
        name = names[1 if rss else 0]
        if (directory / name).is_file():
            return f"{base_url}/{name}"
    return None


def set_cover(element, url, *, rss_url=None):
    itunes = element.find(ITUNES + "image")
    if itunes is not None and rss_url is None:
        existing = urlsplit(itunes.get("href", "")).path.rsplit("/", 1)[-1]
        if existing and existing != ARTWORK_NAME and not PUBLISHED_COVER.fullmatch(existing):
            return
    if itunes is None:
        itunes = ET.SubElement(element, ITUNES + "image")
    itunes.set("href", url)
    if rss_url is not None:
        image = element.find("image")
        if image is None:
            image = ET.SubElement(element, "image")
        for key, value in (("url", rss_url), ("title", element.findtext("title")), ("link", element.findtext("link")),
                           ("width", "144"), ("height", "144")):
            child = image.find(key)
            if child is None:
                child = ET.SubElement(image, key)
            child.text = value
        first_item = element.find("item")
        if first_item is not None:
            for artwork_tag in (itunes, image):
                if list(element).index(artwork_tag) > list(element).index(first_item):
                    element.remove(artwork_tag)
                    element.insert(list(element).index(first_item), artwork_tag)
