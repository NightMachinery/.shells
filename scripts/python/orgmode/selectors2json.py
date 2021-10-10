#!/usr/bin/env python3
# @perf rewrite me in commonlisp
# or not:
#   `time2 goodreads-url-to-json 'https://www.goodreads.com/book/show/4099.The_Pragmatic_Programmer'` takes 1.6s if we do not use `html2org`. But with `html2org`, it shoots up to 9.3s!
#
#  So the LHF is to parallelize the conversion.
#
#  Update: So I tried both multiprocessing's Pool and ThreadPoolExecutor. They were about the same, with threads being a bit faster, and also able to do nested concurrent maps.
#  This improved the time from ~10s to ~3.5s.
#  To improve it further to ~1.3s, I needed to do away with using brishzn's html2org, and directly shell out to pandoc itself.
#  Note that this loses us some valuable postprocessing from zsh's html2org. So I added a new 'what' mode '->>org' to fall back to the old brishzn method.
# --
# I have a rather simple BeautifulSoup script (https://github.com/NightMachinary/.shells/blob/master/scripts/python/orgmode/selectors2json.py) that is very slow. Can I do anything to improve the performance? I assume BS doesn't use C extensions, so is there a library that does?
###
import sys
import os
import json
from bs4 import BeautifulSoup, SoupStrainer
from pynight.common_external import html2org
from pynight.common_iterable import iterable_chunk
from brish import brishzn
from concurrent.futures import ThreadPoolExecutor

# from pytictoc import TicToc
# t = TicToc()

# from IPython import embed

html = os.environ.get("selectors2json_py_html", None)
if not html:
    html = sys.stdin.read()

selector_tuples = sys.argv[1:]
assert (len(selector_tuples) % 3) == 0
selector_tuples = list(iterable_chunk(selector_tuples, 3))

soup = BeautifulSoup(html, features='lxml')

def selection_extract(sel, what):
    if what == '':
        return str(sel.text.strip())
    elif what == '->org':
        return html2org(str(sel)).assert_zero.outrs
    elif what == '->>org':
        return brishzn(["html2org"], stdin=str(sel)).assert_zero.outrs
    elif what.startswith('attr:'):
        attr_name = what[5:]
        return sel[attr_name]


def process_one(selector_tuple):
    # t.tic() # @warn turn off the concurrent map that calls process_one to get accurate results

    name, selector, what = selector_tuple
    sels = soup.select(selector)

    res = list()
    if sels:
        workers_count = len(sels)
        # with ThreadPoolExecutor(max_workers=workers_count) as p:
        res = list(p.map(lambda sel: selection_extract(sel, what), sels, chunksize=1))

    # t.toc(f"selector {name} took ")

    return res


if selector_tuples:
    processed_list = None
    workers_count = len(selector_tuples)
    if False:
        from multiprocessing import Pool
        with Pool(int(workers_count / 2)) as p:
            processed_list = (p.map(process_one, selector_tuples))
    else:
        p = ThreadPoolExecutor(max_workers=32)

        # with ThreadPoolExecutor(max_workers=workers_count) as p:
        processed_list = list(p.map(process_one, selector_tuples, chunksize=4))
        ##
        # processed_list = list(map(process_one, selector_tuples))

    out = dict()
    for i, key in enumerate(map(lambda x: x[0], selector_tuples)):
        out[key] = processed_list[i]

    print(json.dumps(out))
else:
    print(json.dumps(dict()))
