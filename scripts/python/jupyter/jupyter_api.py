#!/usr/bin/env python3
#: @broken
##
import requests
import sys
import os


url = sys.argv[1]
password = sys.argv[2]

s = requests.Session()
if not url.endswith('/'):
    url += '/'

url_login = f'{url}login/'
url_status = f'{url}api/status/'

resp = s.get(url_login)
xsrf_cookie = resp.cookies['_xsrf']

params={'_xsrf':xsrf_cookie, 'password': password}
print(s.post(url_login, data=params))

print(s.post(url_status, data=params))
