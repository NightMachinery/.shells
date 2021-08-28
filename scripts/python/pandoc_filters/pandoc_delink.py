#!/usr/bin/env python3

from pandocfilters import toJSONFilter, Str
from pandas.core.common import flatten
from brish import *

def delink(key, value, format, meta):
    if key == 'Link':
        z("ectty {repr(value)}")

        return Str(' '.join(flatten(value[0]))) # @broken

if __name__ == "__main__":
    toJSONFilter(delink)
