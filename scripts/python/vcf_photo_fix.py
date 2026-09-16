#!/usr/bin/env python3
"""
vcf_photo_fix.py - make contact photos survive a Google Contacts -> anything import.

@broken Not sure if this ever worked.

What it does:
  1. Unfolds the vCard properly (RFC 6350 continuation lines).
  2. Finds PHOTO / LOGO properties.
  3. Downloads URL-valued photos and inlines them as base64.
  4. Re-emits PHOTO in the exact syntax the target vCard version expects.
  5. Re-folds at 75 octets and writes CRLF line endings.
  6. Optionally splits the output into chunks (many importers choke on huge files).

Usage:
  python3 vcf_photo_fix.py in.vcf -o out.vcf                 # inspect + fix in place syntax
  python3 vcf_photo_fix.py in.vcf -o out.vcf --target 4.0
  python3 vcf_photo_fix.py in.vcf -o out.vcf --split 50
  python3 vcf_photo_fix.py in.vcf --report                   # diagnose only, write nothing

No third-party dependencies.
"""

import argparse
import base64
import binascii
import re
import sys
import urllib.request
import urllib.error
from pathlib import Path

UA = "Mozilla/5.0 (compatible; vcf_photo_fix/1.0)"

MAGIC = [
    (b"\xff\xd8\xff", "JPEG", "image/jpeg"),
    (b"\x89PNG\r\n\x1a\n", "PNG", "image/png"),
    (b"GIF87a", "GIF", "image/gif"),
    (b"GIF89a", "GIF", "image/gif"),
    (b"RIFF", "WEBP", "image/webp"),
]


def sniff(data: bytes):
    for sig, name, mime in MAGIC:
        if data.startswith(sig):
            if name == "WEBP" and data[8:12] != b"WEBP":
                continue
            return name, mime
    return "JPEG", "image/jpeg"  # last-resort guess


def unfold(text: str) -> str:
    text = text.replace("\r\n", "\n").replace("\r", "\n")
    # A continuation line starts with a single space or tab; that char is dropped.
    return re.sub(r"\n[ \t]", "", text)


def fold(line: str, limit: int = 75) -> list:
    """Fold a logical line into physical lines of <= limit octets, never splitting
    a UTF-8 code point. Continuation lines get a single leading space."""
    raw = line.encode("utf-8")
    if len(raw) <= limit:
        return [line]
    out = []
    i = 0
    first = True
    while i < len(raw):
        budget = limit if first else limit - 1
        j = min(i + budget, len(raw))
        # do not cut mid code point
        while j > i and j < len(raw) and (raw[j] & 0xC0) == 0x80:
            j -= 1
        chunk = raw[i:j].decode("utf-8")
        out.append(chunk if first else " " + chunk)
        first = False
        i = j
    return out


def parse_params(param_str: str) -> list:
    """Split ';'-separated params, respecting quoted values."""
    parts, cur, in_q = [], "", False
    for ch in param_str:
        if ch == '"':
            in_q = not in_q
            cur += ch
        elif ch == ";" and not in_q:
            parts.append(cur)
            cur = ""
        else:
            cur += ch
    if cur:
        parts.append(cur)
    return [p for p in parts if p]


def fetch(url: str, cookie: str = None, timeout: int = 20) -> bytes:
    req = urllib.request.Request(url, headers={"User-Agent": UA})
    if cookie:
        req.add_header("Cookie", cookie)
    with urllib.request.urlopen(req, timeout=timeout) as r:
        return r.read()


def bump_google_size(url: str, size: int) -> str:
    """googleusercontent URLs accept a size token; ask for a decent resolution."""
    if "googleusercontent.com" not in url:
        return url
    url = re.sub(r"=s\d+(-c)?$", "", url)
    if re.search(r"[?&]sz=\d+", url):
        return re.sub(r"([?&]sz=)\d+", r"\g<1>%d" % size, url)
    return url + "=s%d" % size


def emit_photo(version: str, raw: bytes) -> list:
    """Return the physical lines for one PHOTO property in the given version."""
    name, mime = sniff(raw)
    b64 = base64.b64encode(raw).decode("ascii")
    if version == "2.1":
        lines = fold("PHOTO;ENCODING=BASE64;%s:%s" % (name, b64))
        return lines + [""]          # 2.1 terminates a base64 block with a blank line
    if version == "4.0":
        return fold("PHOTO:data:%s;base64,%s" % (mime, b64))
    return fold("PHOTO;ENCODING=b;TYPE=%s:%s" % (name, b64))   # 3.0


def process(text: str, args):
    stats = {"cards": 0, "url": 0, "inline": 0, "fetched": 0, "failed": 0, "none": 0}
    out_cards = []
    cards = re.split(r"(?=^BEGIN:VCARD\s*$)", unfold(text), flags=re.M)

    for card in cards:
        if "BEGIN:VCARD" not in card:
            continue
        stats["cards"] += 1
        src_ver = "3.0"
        m = re.search(r"^VERSION:(\S+)", card, flags=re.M)
        if m:
            src_ver = m.group(1).strip()
        target = src_ver if args.target == "keep" else args.target

        new_lines = []
        had_photo = False
        for line in card.split("\n"):
            if not line.strip():
                continue
            m = re.match(r"^(PHOTO|LOGO)((?:;[^:]*)?):(.*)$", line, flags=re.I)
            if not m or m.group(1).upper() != "PHOTO":
                if line.upper().startswith("VERSION:"):
                    new_lines.extend(fold("VERSION:" + target))
                else:
                    new_lines.extend(fold(line))
                continue

            had_photo = True
            params = parse_params(m.group(2)[1:] if m.group(2) else "")
            value = m.group(3).strip()
            pflat = ";".join(params).upper()
            raw = None

            if value.lower().startswith("data:"):
                stats["inline"] += 1
                try:
                    raw = base64.b64decode(value.split(",", 1)[1], validate=False)
                except (IndexError, binascii.Error):
                    raw = None
            elif value.lower().startswith(("http://", "https://")) or "VALUE=URI" in pflat or "VALUE=URL" in pflat:
                stats["url"] += 1
                if args.no_fetch:
                    new_lines.extend(fold(line))
                    continue
                url = bump_google_size(value, args.size)
                try:
                    raw = fetch(url, args.cookie)
                    stats["fetched"] += 1
                except (urllib.error.URLError, urllib.error.HTTPError, OSError) as e:
                    stats["failed"] += 1
                    print("  ! could not fetch %s (%s)" % (url[:70], e), file=sys.stderr)
                    new_lines.extend(fold(line))
                    continue
            else:
                stats["inline"] += 1
                try:
                    raw = base64.b64decode(re.sub(r"\s+", "", value), validate=False)
                except binascii.Error:
                    raw = None

            if not raw or len(raw) < 100:
                stats["failed"] += 1
                continue
            if args.max_kb and len(raw) > args.max_kb * 1024:
                print("  ! photo %d KB exceeds --max-kb, dropping" % (len(raw) // 1024), file=sys.stderr)
                continue
            new_lines.extend(emit_photo(target, raw))

        if not had_photo:
            stats["none"] += 1
        out_cards.append("\r\n".join(new_lines))

    return out_cards, stats


def main():
    p = argparse.ArgumentParser()
    p.add_argument("infile")
    p.add_argument("-o", "--out")
    p.add_argument("--target", default="keep", choices=["keep", "2.1", "3.0", "4.0"],
                   help="vCard version to emit PHOTO for (default: same as source)")
    p.add_argument("--split", type=int, default=0, help="max contacts per output file")
    p.add_argument("--size", type=int, default=512, help="requested pixel size for Google photo URLs")
    p.add_argument("--max-kb", type=int, default=0, help="drop photos larger than this")
    p.add_argument("--cookie", help="Cookie header, for URLs that need a signed-in session")
    p.add_argument("--no-fetch", action="store_true", help="never hit the network")
    p.add_argument("--report", action="store_true", help="diagnose only, write nothing")
    args = p.parse_args()

    text = Path(args.infile).read_text(encoding="utf-8", errors="replace")
    if args.report:
        args.no_fetch = True
    cards, s = process(text, args)

    print("cards: %d | inline photos: %d | url photos: %d | fetched: %d | failed: %d | no photo: %d"
          % (s["cards"], s["inline"], s["url"], s["fetched"], s["failed"], s["none"]), file=sys.stderr)

    if args.report:
        return
    if not args.out:
        print("nothing written: pass -o OUT.vcf", file=sys.stderr)
        return

    out = Path(args.out)
    if args.split:
        chunks = [cards[i:i + args.split] for i in range(0, len(cards), args.split)]
        for n, ch in enumerate(chunks, 1):
            fp = out.with_name("%s_%03d%s" % (out.stem, n, out.suffix))
            fp.write_bytes(("\r\n".join(ch) + "\r\n").encode("utf-8"))
            print("wrote %s (%d cards)" % (fp, len(ch)), file=sys.stderr)
    else:
        out.write_bytes(("\r\n".join(cards) + "\r\n").encode("utf-8"))
        print("wrote %s (%d cards)" % (out, len(cards)), file=sys.stderr)


if __name__ == "__main__":
    main()
