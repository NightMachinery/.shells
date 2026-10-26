#!/usr/bin/env python3
"""Cache consistent Noto previews independently of the workstation's fonts."""
import argparse
from concurrent.futures import ThreadPoolExecutor
import json
from pathlib import Path
import re
import struct
import subprocess
import tempfile
import time
from urllib.error import HTTPError, URLError
from urllib.request import urlopen

RELEASE = 'v2026-09-24-unicode18_0'
BASE = f'https://raw.githubusercontent.com/googlefonts/noto-emoji/{RELEASE}/'


def image_name(emoji):
    return 'emoji_u' + '_'.join(f'{ord(c):04x}' for c in emoji if ord(c) != 0xfe0f) + '.png'


def image_asset(emoji, size=128):
    codes = [ord(c) for c in emoji]
    if len(codes) == 2 and all(0x1f1e6 <= c <= 0x1f1ff for c in codes):
        flag = ''.join(chr(c - 0x1f1e6 + 65) for c in codes)
        return f'third_party/region-flags/png/{flag}.png'
    if codes[0] == 0x1f3f4 and len(codes) > 2 and codes[-1] == 0xe007f:
        tag = ''.join(chr(c - 0xe0000) for c in codes[1:-1])
        flag = tag[:2].upper() + '-' + tag[2:].upper()
        return f'third_party/region-flags/png/{flag}.png'
    return f'2D/png/{size}/' + image_name(emoji)


def valid_png(data, limit=128):
    return (len(data) >= 24 and data[:8] == b'\x89PNG\r\n\x1a\n'
            and data[12:16] == b'IHDR'
            and all(0 < size <= limit for size in struct.unpack('>II', data[16:24])))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--data', type=Path, default=Path.home() / 'code/misc/unicode-emoji-json/data-by-emoji.json')
    parser.add_argument('--minimum-version', type=float, default=0)
    parser.add_argument('--size', type=int, choices=(128, 512), default=128)
    parser.add_argument('--emoji', action='append', help='Cache only this emoji; repeat for a selection')
    parser.add_argument('--cache', type=Path)
    args = parser.parse_args()
    if args.cache is None:
        suffix = '' if args.size == 128 else f'-{args.size}'
        args.cache = Path.home() / f'Library/Caches/night-emoji/noto-18.0{suffix}'
    data = json.loads(args.data.read_text())
    requested = set(args.emoji) if args.emoji else None
    if requested and requested.difference(data):
        parser.error('Requested emoji is absent from the installed Unicode data')
    assets = {image_name(emoji): image_asset(emoji, args.size) for emoji, info in data.items()
              if float(info['unicode_version']) >= args.minimum_version
              and (requested is None or emoji in requested)}
    names = sorted(assets)
    args.cache.mkdir(parents=True, exist_ok=True)

    def fetch(name):
        target = args.cache / name
        if target.exists() and valid_png(target.read_bytes(), args.size):
            return
        asset = assets[name]
        for link in range(5):
            for attempt in range(4):
                try:
                    with urlopen(BASE + asset, timeout=30) as response:
                        payload = response.read(1_000_001)
                    break
                except HTTPError as error:
                    if error.code not in (429, 500, 502, 503, 504) or attempt == 3:
                        raise
                    time.sleep(2 ** attempt)
                except (URLError, TimeoutError):
                    if attempt == 3:
                        raise
                    time.sleep(2 ** attempt)
            # raw.githubusercontent serves a Git symlink as its target name.
            # Resolve only simple sibling PNG names within the pinned source.
            if re.fullmatch(rb'[A-Z0-9-]+\.png\n?', payload):
                asset = str(Path(asset).parent / payload.decode().strip())
            else:
                break
        if len(payload) > 1_000_000 or not valid_png(payload, 16384):
            raise ValueError(f'Invalid preview PNG: {name}')
        with tempfile.NamedTemporaryFile(dir=args.cache, suffix='.png', delete=False) as stream:
            temporary = Path(stream.name)
            stream.write(payload)
        try:
            if not valid_png(payload, args.size):
                # Flags can be thousands of pixels wide. Preserve aspect/alpha.
                subprocess.run(['/usr/bin/sips', '--resampleHeightWidthMax', str(args.size),
                                str(temporary)], check=True, capture_output=True)
            if not valid_png(temporary.read_bytes(), args.size):
                raise ValueError(f'Preview resize failed: {name}')
            temporary.replace(target)
        finally:
            temporary.unlink(missing_ok=True)

    with ThreadPoolExecutor(max_workers=6) as pool:
        list(pool.map(fetch, names))
    for name in ('LICENSE', 'README.md'):
        if not (args.cache / name).exists():
            with urlopen(BASE + name, timeout=30) as response:
                (args.cache / name).write_bytes(response.read())
    if not (args.cache / 'LICENSE-region-flags').exists():
        with urlopen(BASE + 'third_party/region-flags/LICENSE', timeout=30) as response:
            (args.cache / 'LICENSE-region-flags').write_bytes(response.read())
    cached = sorted(p.name for p in args.cache.glob('emoji_u*.png') if valid_png(p.read_bytes(), args.size))
    (args.cache / 'source.json').write_text(json.dumps({'release': RELEASE, 'base': BASE, 'size': args.size, 'images': cached}, indent=2) + '\n')
    print(f'NOTO_EMOJI_PREVIEWS_READY: {len(names)} images in {args.cache}')


if __name__ == '__main__':
    main()
