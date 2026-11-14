#!/usr/bin/env python3
"""Compose emoji images into one or more tightly trimmed PNGs.

Reads a JSON spec from the file named by its argument (or stdin) and prints a
JSON report on stdout. Hammerspoon's emoji choosers compute the layout (cells
in arbitrary units, see core/emoji-layout.lua); this script finds the best
source for each emoji, trims its transparent margin, and resamples it once,
straight from the source to its final size.

Spec:
  items:   [{"path": PNG} or {"telegram": {"id", "export", "socket"},
            "fallback": [PNG, ...]}]
  outputs: [{"path": PNG, "cells": [{"i", "x", "y", "s", "rot"}],
             "long": px | "cell": px | "cell": "source", "maxLong": px}]
  trim:    drop transparent margins (default true)
  gap:     share of a cell a glyph may fill when there are several (0.92)
  timeout: seconds each Telegram demand render may take (10)

A Telegram item uses, in order: a rendered large/<id>-<px>.png already in
Purple's export folder; an emoji.render request on Purple's control socket;
an empty requests/<id> file, which Purple scans even with automation off; and
finally the first existing fallback path (the 128px preview, then Unicode).
"""
from concurrent.futures import ThreadPoolExecutor
import json
import math
import os
from pathlib import Path
import re
import socket
import sys
import time

from PIL import Image

LANCZOS = Image.Resampling.LANCZOS
BICUBIC = Image.Resampling.BICUBIC
LARGE_LIMIT = 512
ALPHA_FLOOR = 8  # Ignore near-invisible antialiasing when trimming.
WINDOW = 3  # Demand renders in flight; Purple paces its own queue too.


def large_png(export, emoji_id):
    """The largest finished render Purple has published for this emoji."""
    best = None
    folder = Path(export) / 'large'
    try:
        names = os.listdir(folder)
    except OSError:
        return None
    for name in names:
        match = re.fullmatch(re.escape(emoji_id) + r'-(\d+)\.png', name)
        if not match:
            continue
        px, path = int(match.group(1)), folder / name
        if 0 < px <= LARGE_LIMIT and path.is_file() and not path.is_symlink() \
                and path.stat().st_size > 0 and (best is None or px > best[0]):
            best = (px, str(path))
    return best


def inside_large(export, emoji_id, path):
    folder = os.path.realpath(Path(export) / 'large')
    real = os.path.realpath(path)
    return (os.path.dirname(real) == folder
            and re.fullmatch(re.escape(emoji_id) + r'-\d+\.png', os.path.basename(real))
            and os.path.isfile(real))


class Unavailable(Exception):
    """Purple cannot render on demand now; try the request folder instead."""


def socket_render(path, emoji_id, deadline):
    """One emoji.render call. Returns a path, or raises Unavailable/TimeoutError."""
    while True:
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            raise TimeoutError
        try:
            with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as conn:
                conn.settimeout(remaining)
                conn.connect(path)
                # IDs exceed a double's precision, so send them as strings.
                request = {'v': 1, 'id': 1, 'cmd': 'emoji.render',
                           'args': {'id': emoji_id, 'px': LARGE_LIMIT}}
                conn.sendall((json.dumps(request) + '\n').encode())
                data = b''
                while not data.endswith(b'\n'):
                    chunk = conn.recv(65536)
                    if not chunk:
                        raise Unavailable('Purple closed the control socket')
                    data += chunk
                    if len(data) > 1 << 20:
                        raise Unavailable('Oversized control reply')
        except socket.timeout as error:
            raise TimeoutError from error
        except OSError as error:
            raise Unavailable(str(error)) from error
        reply = json.loads(data)
        if reply.get('ok'):
            return reply['result']['path']
        code = (reply.get('error') or {}).get('code')
        if code == 'busy':
            time.sleep(min(0.6, max(0, deadline - time.monotonic())))
            continue
        if code == 'timeout':
            raise TimeoutError
        raise Unavailable(code or 'unknown error')


def request_render(export, emoji_id, deadline):
    """Leave an empty request file and wait for Purple's next scan to render it."""
    requests = Path(export) / 'requests'
    if not requests.is_dir() or requests.is_symlink():
        return None
    try:
        os.close(os.open(requests / emoji_id, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600))
    except FileExistsError:
        pass
    except OSError:
        return None
    while time.monotonic() < deadline:
        found = large_png(export, emoji_id)
        if found:
            return found[1]
        time.sleep(0.25)
    return None


def resolve(item, timeout):
    """Return (path, source) for one item."""
    if 'path' in item:
        return item['path'], 'path'
    telegram = item.get('telegram')
    if telegram:
        found = resolve_telegram(telegram, timeout)
        if found:
            return found
    for fallback in item.get('fallback') or []:
        if fallback and os.path.isfile(fallback):
            return fallback, 'fallback'
    raise FileNotFoundError(f'No image for emoji {(telegram or {}).get("id", "")}'.rstrip())


def resolve_telegram(telegram, timeout):
    export, emoji_id = telegram['export'], str(telegram['id'])
    if not re.fullmatch(r'\d+', emoji_id):
        raise ValueError('Telegram emoji IDs are decimal strings')
    found = large_png(export, emoji_id)
    if found:
        return found[1], 'large'
    deadline = time.monotonic() + timeout
    if telegram.get('socket'):
        try:
            path = socket_render(telegram['socket'], emoji_id, deadline)
            if inside_large(export, emoji_id, path):
                return path, 'render'
        except TimeoutError:
            deadline = time.monotonic()  # Already waited the item's whole budget.
        except (Unavailable, ValueError, KeyError):
            pass
    path = request_render(export, emoji_id, deadline)
    return (path, 'request') if path else None


def load(path, trim):
    """Return the (trimmed) RGBA image and the source's own long side."""
    with Image.open(path) as source:
        image = source.convert('RGBA')
    px = max(image.size)
    if trim:
        alpha = image.getchannel('A').point(lambda a: 255 if a > ALPHA_FLOOR else 0)
        box = alpha.getbbox()
        if box:
            image = image.crop(box)
    return image, px


def extents(w, h, rot):
    rad = math.radians(rot or 0)
    c, s = abs(math.cos(rad)), abs(math.sin(rad))
    return (w * c + h * s) / 2, (w * s + h * c) / 2


def compose(images, pixels, output, trim, gap):
    cells = output['cells']
    gap = gap if len(cells) > 1 else 1.0
    placed = []
    for cell in cells:
        image = images[cell['i'] - 1]
        w, h = image.size
        side = cell['s'] * gap
        gw, gh = (side, side * h / w) if w >= h else (side * w / h, side)
        ex, ey = extents(gw, gh, cell.get('rot'))
        placed.append((image, cell, gw, gh, ex, ey))
    minx = min(c['x'] - ex for _, c, _, _, ex, _ in placed)
    maxx = max(c['x'] + ex for _, c, _, _, ex, _ in placed)
    miny = min(c['y'] - ey for _, c, _, _, _, ey in placed)
    maxy = max(c['y'] + ey for _, c, _, _, _, ey in placed)
    width, height = maxx - minx, maxy - miny
    if output.get('long'):
        scale = output['long'] / max(width, height)
    else:
        cell_px = output.get('cell') or 'source'
        if cell_px == 'source':
            cell_px = max(pixels[c['i'] - 1] for c in cells)
        scale = cell_px / max(c['s'] * gap for c in cells)
    if output.get('maxLong'):
        scale = min(scale, output['maxLong'] / max(width, height))
    margin = 4
    canvas = Image.new('RGBA', (math.ceil(width * scale) + 2 * margin,
                                math.ceil(height * scale) + 2 * margin), (0, 0, 0, 0))
    for image, cell, gw, gh, _, _ in placed:
        size = (max(1, round(gw * scale)), max(1, round(gh * scale)))
        # Premultiplied, so transparent pixels do not bleed colour into edges.
        glyph = image.convert('RGBa').resize(size, LANCZOS)
        if cell.get('rot'):
            glyph = glyph.rotate(-cell['rot'], resample=BICUBIC, expand=True)
        glyph = glyph.convert('RGBA')
        left = round((cell['x'] - minx) * scale + margin - glyph.width / 2)
        top = round((cell['y'] - miny) * scale + margin - glyph.height / 2)
        layer = Image.new('RGBA', canvas.size, (0, 0, 0, 0))
        layer.paste(glyph, (left, top))
        canvas = Image.alpha_composite(canvas, layer)
    box = canvas.getchannel('A').getbbox() if trim else None
    if box:
        canvas = canvas.crop(box)
    elif not trim:
        canvas = canvas.crop((margin, margin, canvas.width - margin, canvas.height - margin))
    tmp = output['path'] + '.tmp'
    canvas.save(tmp, 'PNG')
    os.replace(tmp, output['path'])
    return {'path': output['path'], 'w': canvas.width, 'h': canvas.height}


def run(spec):
    trim = spec.get('trim', True)
    gap = float(spec.get('gap', 0.92))
    timeout = float(spec.get('timeout', 10))
    items = spec['items']
    if not 1 <= len(items) <= 128:
        raise ValueError('Choose between 1 and 128 emoji')
    with ThreadPoolExecutor(max_workers=WINDOW) as pool:
        resolved = list(pool.map(lambda item: resolve(item, timeout), items))
    loaded = [load(path, trim) for path, _ in resolved]
    images, pixels = [image for image, _ in loaded], [px for _, px in loaded]
    report = {'items': [{'path': path, 'source': source, 'px': px}
                        for (path, source), px in zip(resolved, pixels)], 'outputs': []}
    for output in spec['outputs']:
        report['outputs'].append(compose(images, pixels, output, trim, gap))
    return report


def main():
    try:
        if len(sys.argv) > 1:
            with open(sys.argv[1], encoding='utf-8') as file:
                spec = json.load(file)
        else:
            spec = json.load(sys.stdin)
        report = run(spec)
    except Exception as error:  # Reported to Hammerspoon as one line.
        print(f'{type(error).__name__}: {error}', file=sys.stderr)
        return 1
    json.dump(report, sys.stdout)
    return 0


if __name__ == '__main__':
    sys.exit(main())
