#!/usr/bin/env python3

import sys
import json

input_json = sys.stdin.read()

input_dict = json.loads(input_json)

for key in input_dict.keys():
    val = input_dict[key]
    if not isinstance(val, list):
        input_dict[key] = [val]

print(json.dumps(input_dict))
