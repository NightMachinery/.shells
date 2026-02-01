#!/usr/bin/env python3

import sys
import random

text = sys.stdin.read()
items = text.split('\n')

n = 1
if len(sys.argv) >= 2:
    n = int(sys.argv[1])

print("\n".join(random.choices(items, k=n)))
#: @bug this is with replacements (dups are possible)
