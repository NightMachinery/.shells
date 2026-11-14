# Run from scripts: /opt/homebrew/bin/python3 -I hammerspoon/tests/emoji-compose-test.py
# Synthetic images and a fake control socket; never talks to the real Purple.
import importlib.util
import json
import os
from pathlib import Path
import socket
import tempfile
import threading
import time

from PIL import Image

spec = importlib.util.spec_from_file_location('compose', 'hammerspoon/bin/emoji-compose.py')
compose = importlib.util.module_from_spec(spec)
spec.loader.exec_module(compose)

root = Path(tempfile.mkdtemp(prefix='emoji-compose-test-'))


def glyph(name, size, box, colour=(255, 0, 0, 255)):
    image = Image.new('RGBA', (size, size), (0, 0, 0, 0))
    image.paste(Image.new('RGBA', (box[2] - box[0], box[3] - box[1]), colour), box[:2])
    path = root / name
    image.save(path)
    return str(path)


# A 512px glyph with a wide transparent margin trims to its 256x256 square.
padded = glyph('padded.png', 512, (128, 128, 384, 384))
out = str(root / 'one.png')
report = compose.run({'items': [{'path': padded}],
                      'outputs': [{'path': out, 'long': 512, 'cells': [{'i': 1, 'x': 0, 'y': 0, 's': 1}]}]})
assert report['outputs'][0]['w'] == 512 and report['outputs'][0]['h'] == 512, report
assert report['items'][0]['px'] == 512
with Image.open(out) as image:
    assert image.getpixel((0, 0))[3] == 255, 'The trimmed glyph fills the whole sticker'

# Three cells as a centred triangle: one over two, long side 512, tight crop.
cells = [{'i': 1, 'x': 1, 'y': 0, 's': 1}, {'i': 2, 'x': 0.5, 'y': 1, 's': 1}, {'i': 3, 'x': 1.5, 'y': 1, 's': 1}]
wide = glyph('wide.png', 100, (0, 30, 100, 70), (0, 0, 255, 255))
out = str(root / 'triangle.png')
report = compose.run({'items': [{'path': wide}, {'path': padded}, {'path': padded}],
                      'outputs': [{'path': out, 'long': 512, 'cells': cells}]})
w, h = report['outputs'][0]['w'], report['outputs'][0]['h']
assert max(w, h) <= 512 and max(w, h) >= 500 and h < w, report
with Image.open(out) as image:
    alpha = image.getchannel('A')
    assert alpha.getbbox() == (0, 0, w, h), 'No transparent border survives'
    assert alpha.getpixel((2, 2)) == 0, 'The top row is centred, leaving the corners empty'

# Rotation keeps the whole glyph inside the canvas.
out = str(root / 'rotated.png')
report = compose.run({'items': [{'path': padded}],
                      'outputs': [{'path': out, 'long': 512, 'cells': [{'i': 1, 'x': 0, 'y': 0, 's': 1, 'rot': 45}]}]})
assert abs(report['outputs'][0]['w'] - 512) <= 2, report

# PNG at source resolution: cell size follows the largest resolved source.
out = str(root / 'source.png')
report = compose.run({'items': [{'path': wide}],
                      'outputs': [{'path': out, 'cell': 'source', 'cells': [{'i': 1, 'x': 0, 'y': 0, 's': 1}]}]})
assert report['outputs'][0]['w'] == 100 and report['outputs'][0]['h'] == 40, report

# Telegram: an existing large render wins; then the socket; then the fallback.
export = root / 'export'
(export / 'large').mkdir(parents=True)
(export / 'requests').mkdir()
Image.new('RGBA', (512, 512), (0, 255, 0, 255)).save(export / 'large' / '111-512.png')
Image.new('RGBA', (100, 100), (0, 255, 0, 255)).save(export / 'large' / '111-100.png')
assert compose.resolve({'telegram': {'id': '111', 'export': str(export)}}, 1) == \
    (str(export / 'large' / '111-512.png'), 'large')

sock = str(root / 'control.sock')
server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
server.bind(sock)
server.listen()
calls = []


def answer(conn):
    with conn:
        request = json.loads(conn.makefile().readline())
        calls.append(request)
        emoji_id = request['args']['id']
        assert isinstance(emoji_id, str), 'IDs travel as strings'
        if emoji_id == '222' and len([c for c in calls if c['args']['id'] == '222']) == 1:
            reply = {'ok': False, 'error': {'code': 'busy'}}
        elif emoji_id == '222':
            path = export / 'large' / '222-512.png'
            Image.new('RGBA', (512, 512)).save(path)
            reply = {'ok': True, 'result': {'path': str(path), 'px': 512}}
        elif emoji_id == '333':
            time.sleep(2)  # Longer than the client's per-item budget.
            return
        else:
            reply = {'ok': False, 'error': {'code': 'automation-disabled'}}
        conn.sendall((json.dumps(reply) + '\n').encode())


def serve():
    while True:
        conn, _ = server.accept()
        threading.Thread(target=answer, args=(conn,), daemon=True).start()


threading.Thread(target=serve, daemon=True).start()
item = {'telegram': {'id': '222', 'export': str(export), 'socket': sock}}
assert compose.resolve(item, 3) == (str(export / 'large' / '222-512.png'), 'render'), 'Busy is retried'
preview = glyph('preview-128.png', 128, (10, 10, 118, 118))
start = time.monotonic()
item = {'telegram': {'id': '333', 'export': str(export), 'socket': sock}, 'fallback': [str(root / 'missing.png'), preview]}
assert compose.resolve(item, 0.5) == (preview, 'fallback'), 'A slow render falls back'
assert time.monotonic() - start < 1.5, 'The timeout is per item and is not spent twice'
# Automation off: the request file is left for Purple's scan, then fallback.
item = {'telegram': {'id': '444', 'export': str(export), 'socket': sock}, 'fallback': [preview]}
start = time.monotonic()
assert compose.resolve(item, 0.6) == (preview, 'fallback')
assert time.monotonic() - start >= 0.5, 'After automation-disabled, the request folder gets the item budget'
assert (export / 'requests' / '444').is_file() and (export / 'requests' / '444').stat().st_size == 0
# A socket reply pointing outside large/ is ignored.
assert not compose.inside_large(str(export), '222', preview)
assert compose.resolve({'fallback': [preview]}, 1) == (preview, 'fallback'), 'No catalog: Unicode only'
try:
    compose.run({'items': [{'telegram': {'id': '555', 'export': str(export)}}], 'outputs': []})
    raise AssertionError('No source at all must fail')
except FileNotFoundError:
    pass
print('EMOJI_COMPOSE_TESTS_PASSED')
