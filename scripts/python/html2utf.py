#!/usr/bin/env python3
import fileinput
import html

for line in fileinput.input():
    print(html.unescape(line.rstrip('\n')))
