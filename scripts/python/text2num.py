#!/usr/bin/env python3

from text_to_num import alpha2digit
import sys

text = sys.stdin.read()
text = text.split('\n')
for line in text:
    if line.strip() == 'o': # replaces this with 0, stupid algo
        print(line)
        continue
    print(alpha2digit(line, "en"))
