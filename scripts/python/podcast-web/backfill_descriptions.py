#!/usr/bin/env python3
"""Append original upload dates to an existing feed without changing episode identity."""
import argparse
from collections import Counter
import fcntl
import json
from pathlib import Path
from urllib.parse import unquote, urlsplit
import xml.etree.ElementTree as ET

from mutagen import File
from podcast_dates import append_upload_date, parse_upload_date


def backfill(audio_dir, apply=False):
    audio_dir = Path(audio_dir).expanduser()
    feed = audio_dir / "feed.rss"
    counts = Counter()
    with (audio_dir.parent / ".podcaster.lock").open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        original = feed.read_bytes()
        root = ET.fromstring(original)
        for item in root.findall("./channel/item"):
            counts["episodes"] += 1
            enclosure = item.find("enclosure")
            if enclosure is None:
                counts["no_audio"] += 1
                continue
            path = audio_dir / Path(unquote(urlsplit(enclosure.get("url", "")).path)).name
            try:
                media = File(path, easy=True)
                date = parse_upload_date(media.get("date", [None])[0]) if media else None
            except Exception:
                date = None
            if date is None:
                counts["missing_date"] += 1
                continue
            description = item.find("description")
            before = description.text if description is not None else ""
            after = append_upload_date(before, date)
            if before == after:
                counts["unchanged"] += 1
                continue
            if description is None:
                description = ET.SubElement(item, "description")
            description.text = after
            counts["updated"] += 1
        if apply and counts["updated"]:
            # Backup is made once, while the shared feed lock is held.
            backup = feed.with_name("feed.before-upload-dates.rss")
            if not backup.exists():
                backup.write_bytes(original)
            temp = feed.with_name(".feed.dates.tmp")
            temp.write_bytes(ET.tostring(root, encoding="utf-8", xml_declaration=True))
            temp.replace(feed)
    return dict(counts)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--audio-dir", required=True)
    parser.add_argument("--apply", action="store_true", help="Write the feed; otherwise report a preview")
    args = parser.parse_args()
    print(json.dumps(backfill(args.audio_dir, args.apply)))


if __name__ == "__main__":
    main()
