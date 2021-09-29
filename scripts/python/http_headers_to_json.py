#!/usr/bin/env python3
# @todoing
##
import sys

request_text = sys.stdin.read().encode("utf8")
## @broken idk why people said this works with Python 3.7, it didn't work for me
# from email.parser import BytesParser
# request_line, headers_alone = request_text.split(b'\r\n', 1)
# headers = BytesParser().parsebytes(headers_alone)
##
from kiss_headers import parse_it, dumps
headers = parse_it(request_text)
print(dumps(headers))
##
