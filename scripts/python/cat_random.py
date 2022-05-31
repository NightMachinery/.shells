#!/usr/bin/env python3

import sys
import random

text = sys.stdin.read()
items = text.split('\n')

print(random.choice(items))
