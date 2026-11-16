#!/usr/bin/env python3
"""Write content-hashed asset references; repeated builds are byte-identical."""
import hashlib
from pathlib import Path
import re

root = Path(__file__).parent
path = root / "index.html"
html = path.read_text()
for asset in ("app.js", "style.css"):
    digest = hashlib.sha256((root / asset).read_bytes()).hexdigest()[:12]
    html = re.sub(re.escape(asset) + r"(?:\?v=[a-f0-9]+)?", asset + "?v=" + digest, html)
if html != path.read_text():
    path.write_text(html)
