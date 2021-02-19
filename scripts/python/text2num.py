#!/usr/bin/env python3

from text_to_num import alpha2digit
import sys

text = sys.stdin.read()
print(alpha2digit(text, "en"))
