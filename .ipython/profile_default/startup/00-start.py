from datetime import datetime, timedelta
import pathlib
from pathlib import Path
import os
import sys
import re
import random
import logging

logger = logging.getLogger("ipy")
try:
    import numpy as np
except:
    pass

try:
    from brish import z, zp, zs, zq, bsh
except:
    pass
try:
    from icecream import ic
except ImportError:  # Graceful fallback if IceCream isn't installed.
    ic = lambda *a: None if not a else (a[0] if len(a) == 1 else a)  # noqa

###
def char_range(c1, c2):
    """Generates the characters from `c1` to `c2`, inclusive."""
    for c in range(ord(c1), ord(c2) + 1):
        yield chr(c)


###
