#!/usr/bin/env python3
"""
An alternative for `perl -Mopen=locale -MHTML::Entities -pe '$_ = decode_entities($_)'` (which you can use by `cpanm HTML::Entities`) and `recode html..`.
"""

import fileinput
import html

for line in fileinput.input():
    print(html.unescape(line.rstrip('\n')))
