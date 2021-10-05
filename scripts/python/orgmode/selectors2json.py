#!/usr/bin/env python3
# @perf rewrite me in commonlisp
# or not:
#   `time2 goodreads-url-to-json 'https://www.goodreads.com/book/show/4099.The_Pragmatic_Programmer'` takes 1.6s if we do not use `html2org`. But with `html2org`, it shoots up to 9.3s!
#
#  So the LHF is to parallelize the conversion.
###
import sys
import os
import json
from bs4 import BeautifulSoup, SoupStrainer
from brish import brishzn
from pynight.common_iterable import iterable_chunk
# from IPython import embed

html = os.environ.get("selectors2json_py_html", None)
if not html:
    html = sys.stdin.read()

out = dict()
selector_name_pairs = sys.argv[1:]
assert (len(selector_name_pairs) % 2) == 0
selector_name_pairs = iterable_chunk(selector_name_pairs, 2)

soup = BeautifulSoup(html, features='lxml')

for name, selector in selector_name_pairs:
    sels = soup.select(selector)

    out[name] = [brishzn(["html2org"], stdin=str(s)).assert_zero.outrs for s in sels]
    # out[name] = [str(s) for s in sels]

print(json.dumps(out))
