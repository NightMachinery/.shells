#!/usr/bin/env python3
import fileinput
import urllib.parse
import sys

lines = sys.stdin.readlines()

lines[-1] = lines[-1].rstrip()
# if lines[-1] == '':
#     lines = lines[:-1]
# else:
#     print(repr(lines[-1]))

print(urllib.parse.quote(''.join(lines), safe='/:?=&'))
##
# for line in fileinput.input():
#     print(urllib.parse.quote(line.rstrip(), safe='/:?=&'))
