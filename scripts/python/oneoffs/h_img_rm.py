#!/usr/bin/env python3

from IPython import embed
from bs4 import BeautifulSoup, SoupStrainer
import sys

html = sys.stdin.read()
soup = BeautifulSoup(html, features='lxml')
##
sels = soup.select('img[width="11"][height="11"]')
for s in sels:
    s.extract() # remove this element

sels = soup.select('img[width][height]')
for s in sels:
    h = float(s['height'])
    w = float(s['width'])
    if w and h and (h / w) >= 3:
        s.extract() # remove this elemen
##
# remove all padding_left?

print(soup)
##
# sys.stdin = open('/dev/tty') ; embed()



