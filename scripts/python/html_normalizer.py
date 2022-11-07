#!/usr/bin/env python3

import sys
import os
from bs4 import BeautifulSoup, XMLParsedAsHTMLWarning

import warnings
warnings.filterwarnings('ignore', category=XMLParsedAsHTMLWarning)

html = sys.stdin.read()
soup = BeautifulSoup(html, features='lxml')

print(soup)
