#!/usr/bin/env python3
##
import sys
import xmltodict
import json

text = sys.stdin.read()
d = xmltodict.parse(text)
d_json = json.dumps(d)
print(d_json)
