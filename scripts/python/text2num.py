#!/usr/bin/env python3

from text_to_num import alpha2digit
import sys

text = sys.stdin.read()
if text.strip() == 'o': # replaces this with 0, stupid algo
    print(text)
    sys.exit(0)
print(alpha2digit(text, "en"))
