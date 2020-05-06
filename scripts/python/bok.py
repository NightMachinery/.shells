#!/usr/bin/env python3
from IPython import embed
import sys
import requests
import re
from urllib.parse import urlparse
from brish import z

def get_filename_from_cd(cd, default="defaultname"):
    """
    Get filename from content-disposition
    """
    if not cd:
        return default
    fname = re.findall(r'filename="?([^"]+)"?', cd)
    if len(fname) == 0 or not fname[0]:
        return default
    return fname[0]

url=sys.argv[1]
parts = urlparse(url)

s = requests.Session()
bookpage = s.get(url, allow_redirects=True)
bookpage_content = str(bookpage.content)
r=re.search('href="(/dl/[^"]*)"', bookpage_content)
#dlurl = f"https://b-ok.cc{r.group(1)}"
dlurl = parts.scheme + '://' + parts.netloc + r.group(1)
res = s.get(dlurl, allow_redirects=True)
filename = get_filename_from_cd(res.headers.get('content-disposition'), default=None)
if not filename:
    # quota finished
    exit(1)
open(filename, 'wb').write(res.content)
print(filename)
exit(0)
