#!/usr/bin/env python3
import fileinput
import urllib.parse
# import request_uri

for line in fileinput.input():
    # print(request_uri(line))
    print(urllib.parse.quote(line.rstrip(), safe='/:?=&'))
