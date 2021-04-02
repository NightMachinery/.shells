from datetime import datetime, timedelta
import pathlib
from pathlib import Path
import os
import sys
import re
try:
    from brish import z, zs, zq, bsh
except: pass
try:
    from icecream import ic
except ImportError:  # Graceful fallback if IceCream isn't installed.
    ic = lambda *a: None if not a else (a[0] if len(a) == 1 else a)  # noqa
