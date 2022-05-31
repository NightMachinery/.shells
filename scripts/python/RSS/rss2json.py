#!/usr/bin/env python3

import sys
# import os
import feedparser
import json
from IPython import embed
##
def serialize_safe(obj):
  default = lambda o: f"<non-serializable: {type(o).__qualname__}>"
  return json.dumps(obj, default=default)
##
url = sys.argv[1] #: @input

d = feedparser.parse(url)
# embed()
print(serialize_safe(d))
