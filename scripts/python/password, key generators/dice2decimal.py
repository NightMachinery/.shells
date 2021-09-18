#!/usr/bin/env python3
"""
Usage: dice2decimal 13152366 [6]
Converts dice numbers to a decimal number.
"""

import sys

input = sys.argv[1]

if len(sys.argv) > 2:
    base = int(sys.argv[2])
else:
    base = 6

out = 0
for i, c in enumerate(reversed(list(input))):
    c = int(c)
    c -= 1
    out += (base**i)*c

print(out)
