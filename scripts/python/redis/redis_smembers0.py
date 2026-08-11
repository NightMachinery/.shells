#!/usr/bin/env python3

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from libs.redis_client import redis_client

r = redis_client()

first = True
for mem in r.smembers(sys.argv[1]):
    if not first:
        sys.stdout.buffer.write(b"\0")

    sys.stdout.buffer.write(mem)
    first = False
