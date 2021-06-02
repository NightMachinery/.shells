#!/usr/bin/env python3

import sys
import redis
r = redis.StrictRedis(host='localhost', port=6379, db=0)

first = True
for mem in r.smembers(sys.argv[1]):
    if not first:
        sys.stdout.buffer.write(b"\0")

    sys.stdout.buffer.write(mem)
    first = False
