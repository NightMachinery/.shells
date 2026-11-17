#!/usr/bin/env python3
"""Write content-hashed asset references; repeated builds are byte-identical."""
import hashlib
from io import BytesIO
from pathlib import Path
import re
from PIL import Image, ImageDraw

root = Path(__file__).parent
path = root / "index.html"
html = path.read_text()
for asset in ("app.js", "theme.js", "pwa.js", "style.css"):
    digest = hashlib.sha256((root / asset).read_bytes()).hexdigest()[:12]
    html = re.sub(re.escape(asset) + r"(?:\?v=[a-f0-9]+)?", asset + "?v=" + digest, html)
if html != path.read_text():
    path.write_text(html)

# Original vector artwork, rendered at high resolution before downsampling.
svg = '''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><rect width="512" height="512" rx="104" fill="#173e31"/><path d="M144 282V230a112 112 0 0 1 224 0v52" fill="none" stroke="#b5e4ce" stroke-width="32" stroke-linecap="round"/><rect x="128" y="245" width="64" height="112" rx="28" fill="#b5e4ce"/><rect x="320" y="245" width="64" height="112" rx="28" fill="#b5e4ce"/></svg>\n'''
icon_svg = root / "icon.svg"
if not icon_svg.exists() or icon_svg.read_text() != svg:
    icon_svg.write_text(svg)
scale = 4
for name, size in (("icon-192.png", 192), ("icon-512.png", 512),
                   ("apple-touch-icon.png", 180), ("icon-maskable-512.png", 512)):
    canvas = Image.new("RGB", (512*scale, 512*scale), "#173e31")
    draw = ImageDraw.Draw(canvas)
    # All important artwork is within the central maskable safe zone.
    draw.arc(tuple(n*scale for n in (128,102,384,358)), 180, 360, fill="#b5e4ce", width=32*scale)
    for bounds in ((128,245,192,357), (320,245,384,357)):
        draw.rounded_rectangle(tuple(n*scale for n in bounds), radius=28*scale, fill="#b5e4ce")
    for x in (144,368):
        draw.line((x*scale,230*scale,x*scale,282*scale), fill="#b5e4ce", width=32*scale)
    canvas = canvas.resize((size,size), Image.Resampling.LANCZOS)
    output = BytesIO()
    canvas.save(output, format="PNG", optimize=True)
    target = root / name
    if not target.exists() or target.read_bytes() != output.getvalue():
        target.write_bytes(output.getvalue())

# The worker keeps a stable URL, but changes whenever its app build changes.
worker_path = root / "sw.js"
worker = re.sub(r"^// Build revision:.*\n", "", worker_path.read_text())
digest = hashlib.sha256(worker.encode())
for name in ("index.html", "app.js", "theme.js", "pwa.js", "style.css", "manifest.webmanifest",
             "icon.svg", "icon-192.png", "icon-512.png", "icon-maskable-512.png", "apple-touch-icon.png"):
    digest.update((root / name).read_bytes())
worker = "// Build revision: " + digest.hexdigest()[:12] + "\n" + worker
if worker != worker_path.read_text():
    worker_path.write_text(worker)
