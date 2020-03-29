#!/usr/bin/env python3
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
#embed()
sel = soup
gl_s = local.env.get('gl_s', '')
if gl_s != '':
	sel = soup.select_one(gl_s)
# We can use select to get an array of matches
for link in [h.get('href') for h in sel.find_all('a')]:
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
