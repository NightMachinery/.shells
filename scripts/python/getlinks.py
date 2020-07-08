#!/usr/bin/env python3
### Usage: gl_tag=<html-tag=a> gl_prop=<html-property=href> gl_s=<selector=> <url> [<html-file>]
### Use prop WHOLE to return whole tag

from IPython import embed
from bs4 import BeautifulSoup, SoupStrainer
import requests, sys
from urllib.parse import urlparse
from plumbum import local
from plumbum.cmd import cat

url = sys.argv[1]

if len(sys.argv) >= 3:
    data = cat(sys.argv[2])
else:
    page = requests.get(url)
    data = page.text
soup = BeautifulSoup(data, features='lxml')

parts = urlparse(url)

page_links = []
sel = soup
gl_s = local.env.get('gl_s', '')
tag = local.env.get('gl_tag', 'a')
prop = local.env.get('gl_prop', 'href')
if gl_s != '':
    sel = soup.select_one(gl_s)
# We can use select to get an array of matches

# embed()
if  prop == "WHOLE":
    page_links = sel.find_all(tag)
else:
    for link in [h.get(prop) for h in sel.find_all(tag)]:
        if link is None:
            continue
        if link.startswith('http'):
            page_links.append(link)
        elif link.startswith('/'):
            page_links.append(parts.scheme + '://' + parts.netloc + link)
        else:
            page_links.append(url + link)
for link in page_links:
    print(link)
