#!/usr/bin/env python3

import arabic_reshaper
import sys
from bidi.algorithm import get_display

text_to_be_reshaped = sys.stdin.read()
reshaped_text = arabic_reshaper.reshape(text_to_be_reshaped)
bidi_text = get_display(reshaped_text)
print(bidi_text, end='')
