#!/usr/bin/env python3
"""Cache Noto previews for emoji newer than the workstation's Apple font."""
import argparse
from concurrent.futures import ThreadPoolExecutor
import json
from pathlib import Path
import struct
from urllib.request import urlopen

RELEASE = 'v2026-09-24-unicode18_0'
BASE = f'https://raw.githubusercontent.com/googlefonts/noto-emoji/{RELEASE}/'
# Regional flags live in Noto's upstream flag artwork rather than its square
# emoji PNG tree. Keep the actual Sark artwork, rather than substituting a flag.
EXTRA_IMAGES = {'emoji_u1f1e8_1f1f6.png': 'third_party/region-flags/png/CQ.png'}


def image_name(emoji):
    return 'emoji_u' + '_'.join(f'{ord(c):x}' for c in emoji if ord(c) != 0xfe0f) + '.png'


def valid_png(data):
    return (len(data) >= 24 and data[:8] == b'\x89PNG\r\n\x1a\n'
            and data[12:16] == b'IHDR'
            and all(0 < size <= 1024 for size in struct.unpack('>II', data[16:24])))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--data', type=Path, default=Path.home() / 'code/misc/unicode-emoji-json/data-by-emoji.json')
    parser.add_argument('--minimum-version', type=float, default=16)
    parser.add_argument('--cache', type=Path, default=Path.home() / 'Library/Caches/night-emoji/noto-18.0')
    args = parser.parse_args()
    data = json.loads(args.data.read_text())
    names = sorted({image_name(emoji) for emoji, info in data.items()
                    if float(info['unicode_version']) >= args.minimum_version})
    args.cache.mkdir(parents=True, exist_ok=True)

    def fetch(name):
        target = args.cache / name
        if target.exists() and valid_png(target.read_bytes()):
            return
        asset = EXTRA_IMAGES.get(name, '2D/png/128/' + name)
        with urlopen(BASE + asset, timeout=30) as response:
            payload = response.read(1_000_001)
        if len(payload) > 1_000_000 or not valid_png(payload):
            raise ValueError(f'Invalid preview PNG: {name}')
        temporary = target.with_suffix('.tmp')
        temporary.write_bytes(payload)
        temporary.replace(target)

    with ThreadPoolExecutor(max_workers=6) as pool:
        list(pool.map(fetch, names))
    for name in ('LICENSE', 'README.md'):
        with urlopen(BASE + name, timeout=30) as response:
            (args.cache / name).write_bytes(response.read())
    with urlopen(BASE + 'third_party/region-flags/LICENSE', timeout=30) as response:
        (args.cache / 'LICENSE-region-flags').write_bytes(response.read())
    (args.cache / 'source.json').write_text(json.dumps({'release': RELEASE, 'base': BASE, 'images': names}, indent=2) + '\n')
    print(f'NOTO_EMOJI_PREVIEWS_READY: {len(names)} images in {args.cache}')


if __name__ == '__main__':
    main()
